************************************************************************
* Program Name      : ZRPP901R_WOSUM_CHECK
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.12.08.
* Specifications By : B. Choi
* Pattern           : 1.1
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Checking Data Between ZTPP_WOSUM and SAP.
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report ZRPP901R_WOSUM_CHECK  no standard page heading line-size 355 .

tables: ztpp_wosum,  "ERP_WO QTY SUMMARY
        ztpp_status,  "Status ID Mapping Between Legarcy and SAP
        ausp,  "Characteristic Values
        cabn.  "Characteristic

data: begin of it_wosum occurs 0,
        matnr type objnum.  "Work Order Header Name.
        include structure ztpp_wosum.
data: end of it_wosum.
data: begin of st_vm_rp,
         vm_rp01tq type ztpp_wosum-rp01tq,
         vm_rp02tq type ztpp_wosum-rp02tq,
         vm_rp03tq type ztpp_wosum-rp03tq,
         vm_rp04tq type ztpp_wosum-rp04tq,
         vm_rp05tq type ztpp_wosum-rp05tq,
         vm_rp06tq type ztpp_wosum-rp06tq,
         vm_rp07tq type ztpp_wosum-rp07tq,
         vm_rp08tq type ztpp_wosum-rp08tq,
         vm_rp09tq type ztpp_wosum-rp09tq,
         vm_rp10tq type ztpp_wosum-rp10tq,
         vm_rp11tq type ztpp_wosum-rp11tq,
         vm_rp12tq type ztpp_wosum-rp12tq,
         vm_rp13tq type ztpp_wosum-rp13tq,
         vm_rp14tq type ztpp_wosum-rp14tq,
         vm_rp15tq type ztpp_wosum-rp15tq,
         vm_rp16tq type ztpp_wosum-rp16tq,
* *      See You Later~~~!!
*        vm_rp01dq TYPE ztpp_wosum-rp01dq,
*        vm_rp02dq TYPE ztpp_wosum-rp02dq,
*        vm_rp03dq TYPE ztpp_wosum-rp03dq,
*        vm_rp04dq TYPE ztpp_wosum-rp04dq,
*        vm_rp05dq TYPE ztpp_wosum-rp05dq,
*        vm_rp06dq TYPE ztpp_wosum-rp06dq,
*        vm_rp07dq TYPE ztpp_wosum-rp07dq,
*        vm_rp08dq TYPE ztpp_wosum-rp08dq,
*        vm_rp09dq TYPE ztpp_wosum-rp09dq,
*        vm_rp10dq TYPE ztpp_wosum-rp10dq,
*        vm_rp11dq TYPE ztpp_wosum-rp11dq,
*        vm_rp12dq TYPE ztpp_wosum-rp12dq,
*        vm_rp13dq TYPE ztpp_wosum-rp13dq,
*        vm_rp14dq TYPE ztpp_wosum-rp14dq,
*        vm_rp15dq TYPE ztpp_wosum-rp15dq,
*        vm_rp16dq TYPE ztpp_wosum-rp16dq,
*        VM_T01PQ  TYPE ZTPP_WOSUM-T01PQ,
*        VM_T06PQ  TYPE ZTPP_WOSUM-T06PQ,
*        VM_T08PQ  TYPE ZTPP_WOSUM-T08PQ,
*        VM_T12PQ  TYPE ZTPP_WOSUM-T12PQ,
*        VM_T17PQ  TYPE ZTPP_WOSUM-T17PQ,
*        VM_T20PQ  TYPE ZTPP_WOSUM-T20PQ,
*        VM_T01DQ  TYPE ZTPP_WOSUM-T01DQ,
*        VM_T06DQ  TYPE ZTPP_WOSUM-T06DQ,
*        VM_T08DQ  TYPE ZTPP_WOSUM-T08DQ,
*        VM_T12DQ  TYPE ZTPP_WOSUM-T12DQ,
*        VM_T17DQ  TYPE ZTPP_WOSUM-T17DQ,
*        VM_T20DQ  TYPE ZTPP_WOSUM-T20DQ.
       end of st_vm_rp.

data: begin of it_data occurs 0,
        matnr type objnum.  "Work Order Header or Color Name.
        include structure ztpp_wosum.
        include structure zspp_wo_class.
        include structure st_vm_rp.
data: end of it_data.

include <icon>.
field-symbols <tq> type any.

selection-screen begin of block b1 with frame.
select-options: s_wo_ser for ztpp_wosum-wo_ser,
                s_nation for ztpp_wosum-nation,
                s_dealer for ztpp_wosum-dealer,
                s_extc   for ztpp_wosum-extc,
                s_intc   for ztpp_wosum-intc,
                s_crdat  for ztpp_wosum-wocredate,
                s_modat  for ztpp_wosum-womoddate,
                s_erdat  for ztpp_wosum-erdat.

selection-screen end of block b1.



***********************************************
initialization.
***********************************************
  clear: s_erdat.
  concatenate sy-datum(06) '01'
    into s_erdat-low .
  move sy-datum to s_erdat-high.
  append s_erdat.

***********************************************
start-of-selection.
***********************************************
  perform read_ztpp_wosum.
  perform setting_wo_data.

***********************************************
end-of-selection.
***********************************************
  perform display_data.

***********************************************
at line-selection.
***********************************************
  data: l_field_name01(20).
  get cursor field l_field_name01.

  if sy-lsind < 2 and l_field_name01(12) = 'ICON_RED_LIG'.
    perform detail_data using it_data-matnr .
  endif.

***********************************************
top-of-page during line-selection  .
***********************************************


***********************************************
top-of-page.
***********************************************
  perform top_of_page.
*&---------------------------------------------------------------------*
*&      Form  read_ztpp_wosum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztpp_wosum.
  select *
    into corresponding fields of table it_wosum
    from ztpp_wosum
    where wo_ser in s_wo_ser and
          nation in s_nation and
          dealer in s_dealer and
          extc   in s_extc   and
          intc   in s_intc   and
          wocredate in s_crdat and
          womoddate in s_modat and
          erdat  in s_erdat   .
  sort it_wosum by wo_ser nation dealer extc intc .
  loop at it_wosum.
    concatenate it_wosum-wo_ser
                it_wosum-nation
                it_wosum-dealer
      into it_wosum-matnr.  "work order header creation.
    modify it_wosum index sy-tabix.
  endloop.
endform.                    " read_ztpp_wosum
*&---------------------------------------------------------------------*
*&      Form  setting_wo_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form setting_wo_data.
  data: l_wo_class type zspp_wo_class,
        l_tabix type sy-tabix.
  data: l_vm_rp like st_vm_rp.

  clear: it_data, it_data[].
  loop at it_wosum.
    at new matnr .
      sum.
      clear: it_data,
             l_wo_class,
             l_vm_rp.
*     Work Order Header's Information.
      perform read_wo_class using l_wo_class
                                  it_wosum-matnr .
*     Vehicle Master's Information.
      perform read_vm_rp using l_vm_rp
                               it_wosum-matnr
                               'H' .
      move-corresponding: it_wosum   to it_data,
                          l_wo_class to it_data,
                          l_vm_rp    to it_data.
      append it_data.
    endat.

    clear: it_data,
           l_wo_class,
           l_vm_rp.
*   Work Order Color's Information.
    concatenate it_wosum-matnr
                it_wosum-extc
                it_wosum-intc
      into it_wosum-matnr .
    perform read_wo_class using l_wo_class
                                it_wosum-matnr.
*   Vehicle Master's Information.
    perform read_vm_rp using l_vm_rp
                             it_wosum-matnr
                             'C' .
    move-corresponding: it_wosum   to it_data,
                        l_wo_class to it_data,
                        l_vm_rp    to it_data.
    append it_data.
  endloop.
endform.                    " setting_wo_data
*&---------------------------------------------------------------------*
*&      Form  read_wo_class
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WOSUM_MATNR  text
*      -->P_L_WO_HEADER  text
*----------------------------------------------------------------------*
form read_wo_class using   p_class like zspp_wo_class
                           p_matnr  .
  data: l_it_class type table of zspp_wo_class with header line.
  data: l_error type n .
  clear: l_it_class, l_it_class[].
  call function 'Z_FPP_READ_WO_CLASSIFICATION'
       exporting
            matnr          = p_matnr
       tables
            classification = l_it_class
       exceptions
            no_data        = 1 .

  if sy-subrc <> 0.
  else.
    read table l_it_class index 1.
    move-corresponding l_it_class to p_class .
  endif.
*
*CALL FUNCTION 'Z_FPP_READ_WO_CLASSIFICATION'
*  EXPORTING
*    matnr                =
*  tables
*    classification       =
** EXCEPTIONS
**   NO_DATA              = 1
**   OTHERS               = 2
*          .
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
endform.                    " read_wo_class
*&---------------------------------------------------------------------*
*&      Form  read_vm_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_VM_RP  text
*      -->P_IT_WOSUM_MATNR  text
*      -->P_0307   text
*----------------------------------------------------------------------*
form read_vm_rp using    p_vm_rp like st_vm_rp
                         p_matnr
                         p_type.
  data: l_vm_no   type ausp-objek ,
        l_woh type ausp-atinn ,
        l_int type ausp-atinn ,
        l_ext type ausp-atinn .
* Search ATINNs.
  perform call_function_conversion using 'P_WORK_ORDER'
                                         l_woh .
  perform call_function_conversion using 'P_EXT_COLOR'
                                         l_int .
  perform call_function_conversion using 'P_INT_COLOR'
                                         l_ext .
* Search The V/M No.
  select objek
    into l_vm_no
    from ausp
    where atinn = l_woh       and
          atwrt = p_matnr(14) and
          klart = '002' .
    if p_type <> 'H'.
      select single objek
        into l_vm_no
        from ausp
        where objek = l_vm_no and
              atinn = l_ext   and
              atwrt = p_matnr+14(02) .
      if sy-subrc = 0.
        select single objek
          into l_vm_no
          from ausp
          where objek = l_vm_no and
                atinn = l_int   and
                atwrt = p_matnr+16(02).
        if sy-subrc = 0.
*         Work Order Color
          perform sum_qty using p_vm_rp
                                l_vm_no .
        else.
          continue.
        endif.
      else.
        continue.
      endif.
    else.
*     Work Order Header
      perform sum_qty using p_vm_rp
                            l_vm_no .
    endif.
  endselect.
endform.                    " read_vm_rp
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_MATNR(14)  text
*      -->P_L_ATINN  text
*----------------------------------------------------------------------*
form call_function_conversion using    p_char_c
                                       p_numb_n.
  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_char_c
       importing
            output = p_numb_n.

endform.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  sum_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VM_RP  text
*      -->P_L_VM_NO  text
*----------------------------------------------------------------------*
form sum_qty using    p_vm_rp like st_vm_rp
                      p_vm_no.
  data: l_pre(04) type c value 'P_RP',
        l_num(02) type n,
        l_tag(12) type c value '_ACTUAL_DATE',
        l_act_date(18) type c ,
        l_atwrt type ausp-atwrt ,
        l_wo_point type ztpp_status-id_point ,
        l_part01(14) type c value 'P_VM_RP-VM_RP',
        l_part02(02) type c value 'TQ',
        l_rptq(18) type c .

  clear l_num.
  do 28 times.
    l_num = l_num + 1 .
    concatenate l_pre l_num l_tag
      into l_act_date .
    select single atwrt
      into l_atwrt
      from ( ausp as au
           inner join cabn as ca on au~atinn = ca~atinn )
      where au~objek = p_vm_no and
            ca~atnam = l_act_date and
            au~atwrt <> space .
    if sy-subrc = 0.
      select single id_point
        into l_wo_point
        from ztpp_status
        where rp_point = l_num  .
      concatenate l_part01 l_wo_point l_part02
        into l_rptq   .
      assign (l_rptq) to <tq>.
      <tq> = <tq> + 1 .
    endif.
  enddo.




endform.                    " sum_qty
*&---------------------------------------------------------------------*
*&      Form  Top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_of_page.
  set left scroll-boundary column 25.
  write:/ 'Checking Data Between Legacy and SAP'.
  skip.
  skip.
  format color col_positive intensified on.
  write:/(347) sy-uline.
  write:/ '|' no-gap,
          (04) ' ' no-gap,
          (18) 'Work Order' no-gap,
          '|' no-gap,
          (08) 'Init QTY' no-gap,
          '|' no-gap,
          (07) 'Mod.QTY' no-gap,
          '|' no-gap,
          (07) 'Seq.QTY' no-gap,
          '|' no-gap,
          (08) 'Plan QTY' no-gap,
          '|' no-gap,
          (12) 'Forecast QTY' no-gap,
          '|' no-gap,
          (08) 'MITU QTY' no-gap,
          '|' no-gap,
          (12) 'W/O Cre.Date' no-gap,
          '|' no-gap,
          (12) 'W/O Mod.Date' no-gap,
          '|' no-gap,
          (18) 'FSC' no-gap,
          '|' no-gap,
          (03) 'Ver' no-gap,
          '|' no-gap,
          (09) 'Sales No.' no-gap,
          '|' no-gap,
          (12) 'RP01 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP02 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP03 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP04 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP05 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP06 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP07 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP08 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP09 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP10 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP11 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP12 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP13 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP14 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP15 Tot.QTY' no-gap,
          '|' no-gap,
          (12) 'RP16 Tot.QTY' no-gap,
          '|' no-gap.
  write:/(347) sy-uline.
endform.                    " Top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
  data: l_flag.
  data: l_color.
  loop at it_data.
    clear l_flag.
    if l_color <> 'X'.
      format color col_positive intensified off.
      l_color = 'X'.
    else.
      format color col_positive intensified on.
      l_color = ' '.
    endif.

    perform check_error changing l_flag.
    if l_flag <> space.
      write:/ '|' no-gap,
              (04) icon_red_light as icon no-gap.
    else.
      write:/ '|' no-gap,
              (04) icon_green_light as icon no-gap.
    endif.
    write:  (18) it_data-matnr no-gap,
            '|' no-gap,
            (08) it_data-initqty no-gap,
            '|' no-gap,
            (07) it_data-modqty no-gap,
            '|' no-gap,
            (07) it_data-seqqty no-gap,
            '|' no-gap,
            (08) it_data-planqty no-gap,
            '|' no-gap,
            (12) it_data-forecastqty no-gap,
            '|' no-gap,
            (08) it_data-mituqty no-gap,
            '|' no-gap,
            (12) it_data-wocredate no-gap,
            '|' no-gap,
            (12) it_data-womoddate no-gap,
            '|' no-gap,
            (18) it_data-fsc no-gap,
            '|' no-gap,
            (03) it_data-version no-gap,
            '|' no-gap,
            (09) it_data-sales no-gap,
            '|' no-gap,
            (12) it_data-rp01tq no-gap,
            '|' no-gap,
            (12) it_data-rp02tq no-gap,
            '|' no-gap,
            (12) it_data-rp03tq no-gap,
            '|' no-gap,
            (12) it_data-rp04tq no-gap,
            '|' no-gap,
            (12) it_data-rp05tq no-gap,
            '|' no-gap,
            (12) it_data-rp06tq no-gap,
            '|' no-gap,
            (12) it_data-rp07tq no-gap,
            '|' no-gap,
            (12) it_data-rp08tq no-gap,
            '|' no-gap,
            (12) it_data-rp09tq no-gap,
            '|' no-gap,
            (12) it_data-rp10tq no-gap,
            '|' no-gap,
            (12) it_data-rp11tq no-gap,
            '|' no-gap,
            (12) it_data-rp12tq no-gap,
            '|' no-gap,
            (12) it_data-rp13tq no-gap,
            '|' no-gap,
            (12) it_data-rp14tq no-gap,
            '|' no-gap,
            (12) it_data-rp15tq no-gap,
            '|' no-gap,
            (12) it_data-rp16tq no-gap,
            '|' no-gap.
    hide: it_data-matnr .
  endloop.
  write:/(347) sy-uline.

endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  check_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
form check_error changing p_flag.
  data: l_char01(20),
        l_char02(20),
        l_num01(20) type n,
        l_num02(20) type n.
* Sales Order
  move: it_data-sales to l_char01,
        it_data-p_sales_order to l_char02.
  if l_char01 <> l_char02 .
    p_flag = 'E'.
    exit.
  endif.
* Initial Quantity
  move: it_data-initqty to l_num01,
        it_data-p_init_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* Modified Quantity
  move: it_data-modqty to l_num01,
        it_data-p_mod_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* Sequence Quantity
  move: it_data-seqqty to l_num01,
        it_data-p_seq_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* Planned Quantity
  move: it_data-planqty to l_num01,
        it_data-p_plan_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* Forecast Quantity
  move: it_data-forecastqty to l_num01,
        it_data-p_forecast_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* MITU Quantity
  move: it_data-mituqty to l_num01,
        it_data-p_mitu_qty to l_num02.
  if l_num01 <> l_num02 .
    p_flag = 'E'.
    exit.
  endif.
* W/O Creation Date
  move: it_data-wocredate to l_char01,
        it_data-p_wo_create_date to l_char02.
  if l_char01 <> l_char02 .
    p_flag = 'E'.
    exit.
  endif.
* W/O Modification Date
  move: it_data-womoddate to l_char01,
        it_data-p_wo_modi_date to l_char02.
  if l_char01 <> l_char02 .
    p_flag = 'E'.
    exit.
  endif.
* FSC
  move it_data-fsc to l_char01 .
  concatenate it_data-p_model_year
              it_data-dest_code
              it_data-p_mi
    into l_char02 .
  concatenate l_char02 it_data-p_ocn
    into l_char02 separated by ' '.
  if l_char01 <> l_char02 .
    p_flag = 'E'.
    exit.
  endif.
* Version
  move: it_data-version to l_char01,
        it_data-p_version to l_char02.
  if l_char01 <> l_char02 .
    p_flag = 'E'.
    exit.
  endif.
* Sales Document No.
* RP01 Total QTY.
  move: it_data-rp01tq to l_num01,
        it_data-vm_rp01tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP02 Total QTY.
  move: it_data-rp02tq to l_num01,
        it_data-vm_rp02tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP03 Total QTY.
  move: it_data-rp03tq to l_num01,
        it_data-vm_rp03tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP04 Total QTY.
  move: it_data-rp04tq to l_num01,
        it_data-vm_rp04tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP05 Total QTY.
  move: it_data-rp05tq to l_num01,
        it_data-vm_rp05tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP06 Total QTY.
  move: it_data-rp06tq to l_num01,
        it_data-vm_rp06tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP07 Total QTY.
  move: it_data-rp07tq to l_num01,
        it_data-vm_rp07tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP08 Total QTY.
  move: it_data-rp08tq to l_num01,
        it_data-vm_rp08tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP09 Total QTY.
  move: it_data-rp09tq to l_num01,
        it_data-vm_rp09tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP10 Total QTY.
  move: it_data-rp10tq to l_num01,
        it_data-vm_rp10tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP11 Total QTY.
  move: it_data-rp11tq to l_num01,
        it_data-vm_rp11tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP12 Total QTY.
  move: it_data-rp12tq to l_num01,
        it_data-vm_rp12tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP13 Total QTY.
  move: it_data-rp13tq to l_num01,
        it_data-vm_rp13tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP14 Total QTY.
  move: it_data-rp14tq to l_num01,
        it_data-vm_rp14tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP15 Total QTY.
  move: it_data-rp15tq to l_num01,
        it_data-vm_rp15tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
* RP16 Total QTY.
  move: it_data-rp16tq to l_num01,
        it_data-vm_rp16tq to l_num02.
  if l_num01 <> l_num02.
    p_flag = 'E'.
    exit.
  endif.
endform.                   " check_error
*&---------------------------------------------------------------------*
*&      Form  detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_MATNR  text
*----------------------------------------------------------------------*
form detail_data using    p_matnr.
  read table it_data with key matnr = p_matnr.
  data: l_fsc(20).
  write:/  'Data of ZTPP_WOSUM and SAP'.
  skip.
  skip.
  write:/(64) sy-uline.
  write:/ '|' no-gap, (20) it_data-matnr no-gap,
          '|' no-gap, (20) 'ZTPP_WOSUM' no-gap,
          '|' no-gap, (20) 'SAP' no-gap, '|' no-gap.
  write:/(64) sy-uline.
* Initial QTY
  write:/ '|' no-gap,
          (20) 'Initial Order Quantity' no-gap,
          '|' no-gap,
          (20) it_data-initqty no-gap,
          '|' no-gap,
          (20) it_data-p_init_qty no-gap,
          '|' no-gap.
* Modified QTY
  write:/ '|' no-gap,
          (20) 'Modified Order Quantity' no-gap,
          '|' no-gap,
          (20) it_data-modqty no-gap,
          '|' no-gap,
          (20) it_data-p_mod_qty no-gap,
          '|' no-gap.
* Sequence QTY
  write:/ '|' no-gap,
          (20) 'Sequence Quantity' no-gap,
          '|' no-gap,
          (20) it_data-seqqty no-gap,
          '|' no-gap,
          (20) it_data-p_seq_qty no-gap,
          '|' no-gap.
* Planned QTY
  write:/ '|' no-gap,
          (20) 'Planned Quantity' no-gap,
          '|' no-gap,
          (20) it_data-planqty no-gap,
          '|' no-gap,
          (20) it_data-p_plan_qty no-gap,
          '|' no-gap.
* Forecast QTY
  write:/ '|' no-gap,
          (20) 'Forecast Quantity' no-gap,
         '|' no-gap,
          (20) it_data-forecastqty no-gap,
         '|' no-gap,
          (20) it_data-p_forecast_qty no-gap,
          '|' no-gap.
* MITU QTY
  write:/ '|' no-gap,
          (20) 'MITU Quantity' no-gap,
         '|' no-gap,
          (20) it_data-mituqty no-gap,
         '|' no-gap,
          (20) it_data-p_mitu_qty no-gap,
          '|' no-gap.
* W/O Creation Date
  write:/ '|' no-gap,
          (20) 'W/O Creation Date' no-gap,
         '|' no-gap,
          (20) it_data-wocredate no-gap,
         '|' no-gap,
          (20) it_data-p_wo_create_date no-gap,
          '|' no-gap.
* W/O Modification Date
  write:/ '|' no-gap,
          (20) 'W/O Modification Date' no-gap,
         '|' no-gap,
          (20) it_data-womoddate no-gap,
         '|' no-gap,
          (20) it_data-p_wo_modi_date no-gap,
          '|' no-gap.
* FSC
  clear l_fsc .
  concatenate it_data-p_model_year
              it_data-dest_code
              it_data-p_mi
    into l_fsc.
  concatenate l_fsc it_data-p_ocn
    into l_fsc separated by ' '.
  write:/ '|' no-gap,
          (20) 'FSC' no-gap,
          '|' no-gap,
          (20) it_data-fsc no-gap,
          '|' no-gap,
          (20) l_fsc no-gap,
          '|' no-gap.
* Version
  write:/ '|' no-gap,
          (20) 'Version' no-gap,
          '|' no-gap,
          (20) it_data-version no-gap,
          '|' no-gap,
          (20) it_data-p_version no-gap,
          '|' no-gap.
* RP01 Total QTY
  write:/ '|' no-gap,
          (20) 'RP01 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp01tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp01tq no-gap,
          '|' no-gap.
* RP02 Total QTY
  write:/ '|' no-gap,
          (20) 'RP02 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp02tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp02tq no-gap,
          '|' no-gap.
* RP03 Total QTY
  write:/ '|' no-gap,
          (20) 'RP03 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp03tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp03tq no-gap,
          '|' no-gap.
* RP04 Total QTY
  write:/ '|' no-gap,
          (20) 'RP04 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp04tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp04tq no-gap,
          '|' no-gap.
* RP05 Total QTY
  write:/ '|' no-gap,
          (20) 'RP05 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp05tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp05tq no-gap,
          '|' no-gap.
* RP06 Total QTY
  write:/ '|' no-gap,
          (20) 'RP06 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp06tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp06tq no-gap,
          '|' no-gap.
* RP07 Total QTY
  write:/ '|' no-gap,
          (20) 'RP07 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp07tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp07tq no-gap,
          '|' no-gap.
* RP08 Total QTY
  write:/ '|' no-gap,
          (20) 'RP08 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp08tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp08tq no-gap,
          '|' no-gap.
* RP09 Total QTY
  write:/ '|' no-gap,
          (20) 'RP09 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp09tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp09tq no-gap,
          '|' no-gap.
* RP10 Total QTY
  write:/ '|' no-gap,
          (20) 'RP10 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp10tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp10tq no-gap,
          '|' no-gap.
* RP11 Total QTY
  write:/ '|' no-gap,
          (20) 'RP11 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp11tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp11tq no-gap,
          '|' no-gap.
* RP12 Total QTY
  write:/ '|' no-gap,
          (20) 'RP12 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp12tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp12tq no-gap,
          '|' no-gap.
* RP13 Total QTY
  write:/ '|' no-gap,
          (20) 'RP13 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp13tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp13tq no-gap,
          '|' no-gap.
* RP14 Total QTY
  write:/ '|' no-gap,
          (20) 'RP14 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp14tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp14tq no-gap,
          '|' no-gap.
* RP15 Total QTY
  write:/ '|' no-gap,
          (20) 'RP15 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp15tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp15tq no-gap,
          '|' no-gap.
* RP16 Total QTY
  write:/ '|' no-gap,
          (20) 'RP16 Total QTY' no-gap,
          '|' no-gap,
          (20) it_data-rp16tq no-gap,
          '|' no-gap,
          (20) it_data-vm_rp16tq no-gap,
          '|' no-gap.
  write:/(64) sy-uline.
endform.                    " detail_data
