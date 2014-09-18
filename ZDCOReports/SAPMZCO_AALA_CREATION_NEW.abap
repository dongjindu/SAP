************************************************************************
* Program Name      : SAPMZCO_AALA_CREATION
* Author            : Byung Sung Bae
* Creation Date     : 2005.01.10.
* Specifications By : HS Cho, Andy Choi
* Pattern           : 2.1
* Development Request No : UD1K913724
* Addl Documentation:
* Description       : AALA Part Portion Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*03/27/06    Manju        UD1K919862   AALA :- Exclude cost
*                                      elements
*
* exclude sub-material
* to-do : unit of measure (info, std)
************************************************************************

report  sapmzco_aala_creation.
include <icon>.

tables: mara, t001, marc, a902, konp,
        ztco_shop_pln,ztco_shop_pln_cc,
        ztco_aala_creation_9000, ztco_aala_creation_9000_tab,
        ztco_aala_mip.

data: zsco_aala_creation_9100 like zsco_aala_creation_9100.

*---// Internal tables
data: begin of it_duty occurs 0,
        matnr   like  mara-matnr,
        dduty   like  ztmm_analy-dduty,
      end   of it_duty.

data: it_source like ztco_aala_source occurs 0 with header line.

data: begin of it_shop occurs 0.
        include structure ztco_shop_pln.
data:   mipcd   like ztco_aala_parts-mipcd,
*       elemt   like ztco_shop_pln_cc-elemt,
      end   of it_shop.

data: it_shop_additive like it_shop occurs 0 with header line.
data: it_shop_mip like it_shop occurs 0 with header line.

data: it_parts  like ztco_aala_parts  occurs 0 with header line.

data: it_mip    like ztco_aala_mip    occurs 0 with header line.

data: it_fsc    like ztco_aala_fsc    occurs 0 with header line.

data: it_model  like ztco_aala_model  occurs 0 with header line.

data: it_fsc_alt_qty   like ztco_aala_creation_9000_tab occurs 0
                                               with header line.

data: gt_itab type standard table of zsco_aala_creation_9100
                                               with header line.

data: begin of it_matnr occurs 0,
        matnr   like   mara-matnr,
      end   of it_matnr.

data: begin of it_egntm occurs 0,
        matnr   like   mara-matnr,
      end   of it_egntm.

data: it_tckh2 like tckh2 occurs 0 with header line.

data: begin of it_origin occurs 0,
        matnr   like   mara-matnr,             "FSC Code
        mipcd   like   mara-matnr,             "MIP Code
        shop    like   ztco_shop_pln-shop,     "Shop
        urzla   like   eina-urzla,             "Country Code
        wrbtr   like   bseg-wrbtr,             "Amount
        index   type   i,
      end   of it_origin.
data : wa_ftab(72) type c,
       ftab        like table of wa_ftab.


*---// Work areas
data: w_klvar   like   tck03-klvar,
      w_versn   like   ztco_shopcost-versn,
      w_period  like   ztmm_analy-period,
      w_total(10),
      w_success(10),
      w_error(10).

*---// Constants
constants: c_check                             value 'X',
           c_trimshop  like ztco_shop_pln-shop value '__T%',
           c_bodyshop  like ztco_shop_pln-shop value '__B%',
           c_paintshop like ztco_shop_pln-shop value '__P%',
           c_pressshop like ztco_shop_pln-shop value '__S%',
           c_egnshop   like ztco_shop_pln-shop value '__E%',
           c_trim      like ztco_shop_pln-shop value 'MXTX',
           c_mip       like cska-kstar         value '0000540300',
           c_mcost_f   like cska-kstar         value '0000540000',
           c_mcost_t   like cska-kstar         value '0000540199',
           c_sub_cos_ele   like cska-kstar     value '0000540200',
           c_activity  like cska-kstar         value '0000800000',
           c_elehk     like tckh2-elehk        value 'H1',
           c_bukrs     like t001-bukrs         value 'H201',
           c_success(4)                        value icon_green_light,
           c_error(4)                          value icon_red_light.

*---// Ranges
ranges: r_fsc   for    mara-matnr.

*---// Table controls
controls: tc_9000 type tableview using screen 9000.

*-----/// ALV Control : START
* Control Framework Basic Class
class cl_gui_cfw      definition load.

* Declare reference variables, the container and internal table
data: wc_control_9100   type        scrfname value 'CC_9100_ALV',
      wc_alv_9100       type ref to cl_gui_alv_grid,
      wc_container_9100 type ref to cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class lcl_event_receiver definition deferred. "/ALV Event Handling

data : event_receiver type ref to lcl_event_receiver.

* Interal tables for ALV GRID
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line.

* Global variable for ALV GRID
data : w_is_layout type lvc_s_layo,
       w_variant   type disvariant,          "for parameter IS_VARIANT
       w_fieldname like line of it_fieldcat,
       w_repid     like sy-repid,
       w_cnt       type i,                   "Field count
       w_save      type c   value 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

data: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure like dd02l-tabname.

field-symbols: <container> type ref to cl_gui_custom_container,
               <control>   type        scrfname,
               <alv>       type ref to cl_gui_alv_grid,
               <itab>      type standard table.

constants: c_structure(100) value 'ZSCO_AALA_CREATION_'.

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
class lcl_event_receiver definition.
  public section.
    methods:

    handle_double_click
        for event double_click of cl_gui_alv_grid
            importing e_row
                      e_column
                      es_row_no.
endclass.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
class lcl_event_receiver implementation.
  method handle_double_click.
    perform dbl_click_9000 using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click
endclass.

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status output.
  case sy-dynnr.
    when 9000.
      set pf-status '9000'.
      set titlebar  '9000'.
    when 9100.
      set pf-status '9100'.
      set titlebar  '9000'.
  endcase.
endmodule.                 " STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  move_itab_to_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module move_itab_to_screen output.
  read table it_fsc_alt_qty index tc_9000-current_line.
  if sy-subrc eq 0.
    move-corresponding it_fsc_alt_qty to ztco_aala_creation_9000_tab.
  else.
    exit from step-loop.
  endif.
endmodule.                 " move_itab_to_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  case sy-ucomm.
    when 'EXIT' or 'CANC'.
      clear: sy-ucomm.
      leave to screen 0.
  endcase.
endmodule.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_table_control_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_table_control_9000 input.
  perform check_matnr_9000.
  perform check_atlmt_9000.

  if not ztco_aala_creation_9000_tab-matnr is initial.
    move: 'EA' to ztco_aala_creation_9000_tab-meins.
  endif.
endmodule.                 " check_table_control_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_matnr_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_matnr_9000.
  select single maktx into ztco_aala_creation_9000_tab-maktx
    from makt
   where matnr = ztco_aala_creation_9000_tab-matnr
     and spras = sy-langu.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.

* by ig.moon 1/8/08 {

*  IF ztco_aala_creation_9000_tab-matnr+6(2) NE
*     ztco_aala_creation_9000-model.
*    MESSAGE e000(zz) WITH text-m16.
*  ENDIF.

  if ztco_aala_creation_9000_tab-matnr+13(1) eq space.
    if ztco_aala_creation_9000_tab-matnr+6(2) ne
       ztco_aala_creation_9000-model.
      message e000(zz) with text-m16.
    endif.
  else.
    if ztco_aala_creation_9000_tab-matnr+5(2) ne
       ztco_aala_creation_9000-model.
      message e000(zz) with text-m16.
    endif.
  endif.

* }
  check ztco_aala_creation_9000_tab-altmt eq space.

  perform check_shop_cost using ztco_aala_creation_9000_tab-matnr.
endform.                    " check_matnr_9000
*&---------------------------------------------------------------------*
*&      Form  check_atlmt_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_atlmt_9000.
  check not ztco_aala_creation_9000_tab-altmt is initial.

  select single *
    from mara
   where matnr = ztco_aala_creation_9000_tab-altmt.
  if sy-subrc ne 0.
    message e000(zz) with text-m03.
  endif.

  if ztco_aala_creation_9000_tab-altmt+6(2) ne
     ztco_aala_creation_9000-model.
    message e000(zz) with text-m16.
  endif.

  check ztco_aala_creation_9000_tab-altmt ne space.

  perform check_shop_cost using ztco_aala_creation_9000_tab-altmt.
endform.                    " check_atlmt_9000
*&---------------------------------------------------------------------*
*&      Module  MOVE_SCREEN_TO_ITAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module move_screen_to_itab input.
  read table it_fsc_alt_qty index tc_9000-current_line.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.

  move: ztco_aala_creation_9000_tab to it_fsc_alt_qty.

  modify it_fsc_alt_qty index tc_9000-current_line.
endmodule.                 " MOVE_SCREEN_TO_ITAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  case sy-ucomm.
    when 'BACK'.
      clear: sy-ucomm.
      leave to screen 0.
    when 'EXCUTE'.
      clear: sy-ucomm.
      perform excute_9000_rtn.
    when 'INSERT'.
      clear: sy-ucomm.
      clear: it_fsc_alt_qty.
      append it_fsc_alt_qty.
    when 'DELETE'.
      clear: sy-ucomm.
      delete it_fsc_alt_qty where check = c_check.
  endcase.
endmodule.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_initial_value output.
*----- Set Controlling area
  call function 'K_KOKRS_SET'
       importing
            e_kokrs   = ztco_aala_creation_9000-kokrs
       exceptions
            not_found = 1
            others    = 2.
  if sy-subrc <> 0.
    if sy-msgty = 'E' or sy-msgty = 'A' or sy-msgty = 'X'.
      message id sy-msgid type 'S' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      leave program.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

*---// Set period
  if ztco_aala_creation_9000-bdatj is initial.
    ztco_aala_creation_9000-bdatj = sy-datum(4).
  endif.

  if ztco_aala_creation_9000-poper is initial.
    ztco_aala_creation_9000-poper = sy-datum+4(2).
  endif.

  if ztco_aala_creation_9000-gjahr is initial.
    ztco_aala_creation_9000-gjahr = sy-datum(4).
  endif.

*---// Get company infomation
  select single * from t001 where bukrs = c_bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.
endmodule.                 " set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_9000_RTN
*&---------------------------------------------------------------------*
form excute_9000_rtn.

  perform checing_before_excute.

  perform get_data.

  perform calculate_data.

  perform update_data.

  call screen 9100.

endform.                    " EXCUTE_9000_RTN
*&---------------------------------------------------------------------*
*&      Form  set_fsc_of_shopcost
*&---------------------------------------------------------------------*
form set_fsc_of_shopcost.
  clear: r_fsc, r_fsc[].

  loop at it_fsc_alt_qty.
    move: 'I'  to r_fsc-sign,
          'EQ' to r_fsc-option.

    if it_fsc_alt_qty-altmt is initial.
      move: it_fsc_alt_qty-matnr to r_fsc-low.
    else.
      move: it_fsc_alt_qty-altmt to r_fsc-low.
    endif.

    append r_fsc.
  endloop.
endform.                    " set_fsc_of_shopcost
*&---------------------------------------------------------------------*
*&      Form  get_source_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_source_data.
  clear: it_source, it_source[].
  select *
    into table it_source
    from ztco_aala_source
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message e000(zz) with text-m04 ztco_aala_creation_9000-versn.
  endif.

  read table it_source with key vflag = space.
  if sy-subrc eq 0.
    message e000(zz) with it_source-matnr text-m25.
  endif.

  read table it_source index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.

  move: it_source-klvar to w_klvar.

  sort it_source by matnr.

  clear: w_versn.
  case w_klvar.
    when 'ZPCP'.              "Annual Plan Cost
      move: '311' to w_versn.

      concatenate ztco_aala_creation_9000-bdatj '05' into w_period.
    when 'PPC1'.              "Standard Cost
      move: '000' to w_versn.

      case ztco_aala_creation_9000-poper.
        when '001' or '002' or '003'.
          concatenate ztco_aala_creation_9000-bdatj '01' into w_period.
        when '004' or '005' or '006'.
          concatenate ztco_aala_creation_9000-bdatj '02' into w_period.
        when '007' or '008' or '009'.
          concatenate ztco_aala_creation_9000-bdatj '03' into w_period.
        when '010' or '011' or '012'.
          concatenate ztco_aala_creation_9000-bdatj '04' into w_period.
      endcase.
  endcase.
endform.                    " get_source_data
*&---------------------------------------------------------------------*
*&      Form  get_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_shopcost.

  perform set_fsc_of_shopcost.

  perform get_fsc_shopcost.
  perform get_additive_shopcost.
  perform get_mip_shopcost.

  append lines of it_shop_mip to it_shop.

  perform replace_alternative_fsc.

endform.                    " get_shopcost
*&---------------------------------------------------------------------*
*&      Module  CHECK_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_9000 input.
endmodule.                 " CHECK_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DUTY
*&---------------------------------------------------------------------*
form get_duty.
*NOT VALID - ANDY
*  clear: it_matnr, it_matnr[].
  loop at it_shop.
*    move: it_shop-llv_matnr to it_matnr-matnr.
*
*    collect it_matnr.
*
    if it_shop-shop is initial.
      move: c_trim to it_shop-shop.
    endif.

    modify it_shop.
  endloop.

*  if sy-subrc ne 0.
*    message e000(zz) with text-m05.
*  endif.
*
*  SELECT matnr dduty
*    INTO CORRESPONDING FIELDS OF TABLE it_duty
*    FROM ztmm_analy
*     FOR ALL ENTRIES IN it_matnr
*   WHERE period EQ w_period
*     AND matnr  EQ it_matnr-matnr.
endform.                    " GET_DUTY
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
form get_data.

  perform get_select_filed.
  perform get_source_data.
  perform get_shopcost.
  perform get_duty.
  perform read_cost_component.

endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  calculate_data
*&---------------------------------------------------------------------*
form calculate_data.

  perform set_gt_itab.

* if error... stop processing
  perform calculate_error_count.

  perform set_it_mip.
  perform set_it_fsc_n_it_model.

endform.                    " calculate_data
*&---------------------------------------------------------------------*
*&      Form  set_gt_itab
*&---------------------------------------------------------------------*
form set_gt_itab.
  clear: gt_itab, gt_itab[].

  loop at it_shop.
    clear: gt_itab.

    if it_shop-typps = 'M'.
      perform set_material_cost_for_parts using it_shop.
    elseif it_shop-typps = 'E'.
      perform set_activity_cost_for_parts using it_shop.
    else.
*     break-point.
    endif.
  endloop.

  check 1 = 2.
  loop at it_shop_additive.
    clear: gt_itab.
*   perform get_additive_cost using it_shop_additive-kstar
*                                   it_shop_additive-elemt
*                                   gt_itab-zzmsg.
    perform set_material_cost_for_additive using it_shop_additive.
  endloop.

endform.                    " set_gt_itab
*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_additive_cost using p_kstar
                             p_elemt
                             p_zzmsg.

  check p_kstar >= c_mcost_f and
        p_kstar <= c_mcost_t.

*TEMP FIX
  if p_kstar = '0000540199'.
    p_elemt = '050'.  "assign dummy ccs.
  else.

    loop at it_tckh2 where kstav <= p_kstar
                       and kstab >= p_kstar.
    endloop.
    if sy-subrc ne 0.
      concatenate text-m09 p_kstar text-m10 into p_zzmsg
        separated by space.
    endif.

    move: it_tckh2-elemt to p_elemt.
  endif.
endform.                    " READ_COST_COMPONENT

*&---------------------------------------------------------------------*
*&      Form  APPEND_MATERIAL_COST_FOR_MIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_material_cost_for_mip.
*  LOOP AT
endform.                    " APPEND_MATERIAL_COST_FOR_MIP
*&---------------------------------------------------------------------*
*&      Form  get_mip_lower_level_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MIP  text
*----------------------------------------------------------------------*
form get_mip_lower_level_parts using p_mip structure it_shop_mip
                                     p_fsc p_mipcd   p_menge.
  data: lt_shop_mip like it_shop_mip occurs 0 with header line.

* read activity
  select (ftab)
    into corresponding fields of table lt_shop_mip
    from ztco_shop_pln as a
*   INNER JOIN ztco_shop_pln_cc AS b
*      ON a~kokrs     EQ   b~kokrs
*     AND a~bdatj     EQ   b~bdatj
*     AND a~poper     EQ   b~poper
*     AND a~klvar     EQ   b~klvar
*     AND a~artnr     EQ   b~artnr
*     AND a~typps     EQ   b~typps
*     AND a~kstar     EQ   b~kstar
*     AND a~resou     EQ   b~resou
   where a~kokrs     eq   ztco_aala_creation_9000-kokrs
     and a~bdatj     eq   ztco_aala_creation_9000-bdatj
     and a~poper     eq   ztco_aala_creation_9000-poper
     and a~klvar     eq   w_klvar
*     AND a~versn     EQ   w_versn
     and a~artnr     eq   p_mip-llv_matnr
     and a~typps     =    'E'.
*---// Read Material Cost(Include MIP)
  select (ftab)
    appending corresponding fields of table lt_shop_mip
    from ztco_shop_pln as a
*   INNER JOIN ztco_shop_pln_cc AS b
*      ON a~kokrs     EQ   b~kokrs
*     AND a~bdatj     EQ   b~bdatj
*     AND a~poper     EQ   b~poper
*     AND a~klvar     EQ   b~klvar
*     AND a~artnr     EQ   b~artnr
*     AND a~typps     EQ   b~typps
*     AND a~kstar     EQ   b~kstar
*     AND a~resou     EQ   b~resou
   where a~kokrs     eq   ztco_aala_creation_9000-kokrs
     and a~bdatj     eq   ztco_aala_creation_9000-bdatj
     and a~poper     eq   ztco_aala_creation_9000-poper
     and a~klvar     eq   w_klvar
*     AND a~versn     EQ   w_versn
     and a~artnr     eq   p_mip-llv_matnr
     and a~typps     =    'M'
     and a~kstar     <>  c_sub_cos_ele.
*     AND kstar     BETWEEN c_mcost_f and c_mcost_t.   "UD1K919862

*-if no record...?WHY???
*  IF sy-subrc NE 0.
*    IF p_mip-shop = 'MXSX'.
*      MOVE p_mip TO it_shop.
*      it_shop-kstar = '0000540199'.
*      APPEND it_shop.
*    ENDIF.
*    EXIT.
*  ENDIF.

*  SORT lt_shop_mip BY kokrs bdatj poper klvar versn record_type
*                      fsc_matnr shop llv_matnr.
*  SORT lt_shop_mip BY kokrs bdatj poper klvar
*                      artnr shop llv_matnr.
*
*  LOOP AT lt_shop_mip WHERE kstar EQ c_mip.
*    AT NEW llv_matnr.
*      CONTINUE.
*    ENDAT.
*
*    DELETE lt_shop_mip.
*  ENDLOOP.

  loop at lt_shop_mip.
*WHY??? - ANDY
*    read table it_shop with key lt_shop_mip.
*    if sy-subrc eq 0.
*      delete it_shop index sy-tabix.
*    endif.

    if lt_shop_mip-shop is initial.
      move: p_mip-shop to lt_shop_mip-shop.
    endif.

    if lt_shop_mip-typps = 'E'.
      lt_shop_mip-fevor = p_mip-fevor.
      lt_shop_mip-menge = lt_shop_mip-menge * p_menge.
      perform set_shop_mip_for_act_cost using lt_shop_mip
                                              p_fsc
                                              p_mipcd.
    else.
      if lt_shop_mip-fevor = space.  "END part
        lt_shop_mip-fevor = p_mip-fevor.
        lt_shop_mip-menge = lt_shop_mip-menge * p_menge.
        perform set_shop_mip_for_matl_cost using lt_shop_mip
                                                 p_fsc
                                                 p_mipcd.
      else.  "MIP
        lt_shop_mip-menge = lt_shop_mip-menge * p_menge.
        perform get_mip_lower_level_parts using lt_shop_mip
                                                p_fsc
                                                p_mipcd
                                                lt_shop_mip-menge.
      endif.
    endif.
  endloop.
endform.                    " get_mip_lower_level_parts
*&---------------------------------------------------------------------*
*&      Form  get_fsc_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_fsc_shopcost.
  clear: it_shop, it_shop[].

*---// Read Activity Cost
  select (ftab)
    into corresponding fields of table it_shop
    from ztco_shop_pln as a
*   inner join ztco_shop_pln_cc as b
*      on a~kokrs     eq   b~kokrs
*     and a~bdatj     eq   b~bdatj
*     and a~poper     eq   b~poper
*     and a~klvar     eq   b~klvar
*     and a~artnr     eq   b~artnr
*     and a~typps     eq   b~typps
*     and a~kstar     eq   b~kstar
*     and a~resou     eq   b~resou
   where a~kokrs     eq ztco_aala_creation_9000-kokrs
     and a~bdatj     eq ztco_aala_creation_9000-bdatj
     and a~poper     eq ztco_aala_creation_9000-poper
     and a~klvar     eq w_klvar
*     AND A~versn     EQ w_versn
     and a~artnr     in r_fsc
     and a~typps     eq 'E'
*     AND NOT ( kstar BETWEEN c_mcost_f and c_mcost_t or
*               KSTAR      eq C_MIP )
     and ( a~shop    like c_pressshop or
           a~shop    like c_bodyshop  or
           a~shop    like c_paintshop or
           a~shop    like c_egnshop      ).

  move: 1 to it_shop-menge.
  modify it_shop transporting menge where artnr >= space.

*---// Read Material Cost(Except MIP)

  select (ftab)
    appending corresponding fields of table it_shop
    from ztco_shop_pln as a
*   inner join ztco_shop_pln_cc as b
*      on a~kokrs     eq   b~kokrs
*     and a~bdatj     eq   b~bdatj
*     and a~poper     eq   b~poper
*     and a~klvar     eq   b~klvar
*     and a~artnr     eq   b~artnr
*     and a~typps     eq   b~typps
*     and a~kstar     eq   b~kstar
*     and a~resou     eq   b~resou
   where a~kokrs     eq   ztco_aala_creation_9000-kokrs
     and a~bdatj     eq   ztco_aala_creation_9000-bdatj
     and a~poper     eq   ztco_aala_creation_9000-poper
     and a~klvar     eq   w_klvar
*     AND a~versn     EQ   w_versn
     and a~artnr     in   r_fsc
     and a~typps     eq  'M'
     and a~fevor     =   space
     and a~kstar     <>  c_sub_cos_ele.
*    AND a~kstar     BETWEEN c_mcost_f AND c_mcost_t.

endform.                    " get_fsc_shopcost
*&---------------------------------------------------------------------*
*&      Form  get_mip_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_mip_shopcost.
  data: lt_shop_mip like it_shop_mip occurs 0 with header line.

  clear: it_shop_mip, it_shop_mip[].

* read MIP material only
  select (ftab)
    into corresponding fields of table lt_shop_mip
    from ztco_shop_pln as a
*   inner join ztco_shop_pln_cc as b
*      on a~kokrs     eq   b~kokrs
*     and a~bdatj     eq   b~bdatj
*     and a~poper     eq   b~poper
*     and a~klvar     eq   b~klvar
*     and a~artnr     eq   b~artnr
*     and a~typps     eq   b~typps
*     and a~kstar     eq   b~kstar
*     and a~resou     eq   b~resou
   where a~kokrs     eq   ztco_aala_creation_9000-kokrs
     and a~bdatj     eq   ztco_aala_creation_9000-bdatj
     and a~poper     eq   ztco_aala_creation_9000-poper
     and a~klvar     eq   w_klvar
*     AND A~versn     EQ   w_versn
     and a~artnr     in   r_fsc
     and a~typps     =    'M'
     and a~fevor     <>   space.    "MIP
*    AND a~kstar     EQ   c_mip.

*  SORT lt_shop_mip BY kokrs bdatj poper klvar versn record_type
*                      fsc_matnr shop llv_matnr.
*  sort lt_shop_mip by kokrs bdatj poper klvar artnr shop llv_matnr.
*
*  loop at lt_shop_mip.
*    at new llv_matnr.
*      continue.
*    endat.
*
*    delete lt_shop_mip.
*  endloop.

  loop at lt_shop_mip.
*WHY??? - ANDY
*    if lt_shop_mip-shop+2(1) eq 'E'.
*      move: lt_shop_mip-llv_matnr to it_egntm-matnr.
*      append it_egntm.
*    endif.

*    read table it_shop with key lt_shop_mip.
*    if sy-subrc eq 0.
*      delete it_shop index sy-tabix.
*    endif.

*---default SHOP
    if lt_shop_mip-shop is initial.
      move: c_trim to lt_shop_mip-shop.
    endif.

*---recursive...call
    perform get_mip_lower_level_parts using lt_shop_mip
                                            lt_shop_mip-artnr
                                            lt_shop_mip-llv_matnr
                                            lt_shop_mip-menge.
  endloop.
endform.                    " get_mip_shopcost
*&---------------------------------------------------------------------*
*&      Form  set_material_cost_for_parts
*&---------------------------------------------------------------------*
form set_material_cost_for_parts using p_shop structure it_shop.

  data $subrc.

  move: p_shop-kokrs                  to gt_itab-kokrs,
        p_shop-artnr                  to gt_itab-matnr,
        ztco_aala_creation_9000-gjahr to gt_itab-gjahr,
        p_shop-llv_matnr              to gt_itab-idnrk,
        p_shop-kstar                  to gt_itab-kstar,
*       p_shop-elemt                  to gt_itab-elemt,
        p_shop-fevor                  to gt_itab-fevor,
        p_shop-mipcd                  to gt_itab-mipcd,
        p_shop-shop                   to gt_itab-shop,
        p_shop-menge                  to gt_itab-menge,
        p_shop-meeht                  to gt_itab-meins,
        sy-uname                      to gt_itab-ernam,
        sy-datum                      to gt_itab-erdat,
        sy-uzeit                      to gt_itab-erzet,
        sy-uname                      to gt_itab-aenam,
        sy-datum                      to gt_itab-aedat,
        sy-uzeit                      to gt_itab-aezet.

  clear it_source.

  read table it_source with key matnr = p_shop-llv_matnr
                            binary search.
*                               kokrs = p_shop-kokrs
*                               versn = ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    move: text-m11 to gt_itab-zzmsg.
    $subrc = 'E'.
  endif.

  if it_source-netpr eq 0.
    move: text-m23 to gt_itab-zzmsg.
  endif.

  if it_source-urzla eq space.
    move: text-m24 to gt_itab-zzmsg.
  endif.

  if it_source-kstar eq space.
    move: text-m26 to gt_itab-zzmsg.
  endif.

  if $subrc eq space.
    move: it_source-kstar   to gt_itab-kstar,
          it_source-versn   to gt_itab-versn,
          it_source-stprs   to gt_itab-stprs,  "Source: Shop cost
          it_source-urzla   to gt_itab-urzla,  "upload
          it_source-aaprs   to gt_itab-aaprs,  "upload
          it_source-netpr   to gt_itab-netpr,  "upload
          it_source-peinh   to gt_itab-peinh,
          it_source-meeht   to gt_itab-meeht.
  else.
    move:
          ztco_aala_creation_9000-versn to gt_itab-versn.
  endif.
  clear it_duty.
  if $subrc eq space.
    read table it_duty with key matnr = p_shop-llv_matnr.
    if sy-subrc ne 0.
      perform read_duty_from_hts_code using p_shop gt_itab-dduty.
    else.
      move: it_duty-dduty to gt_itab-dduty.
    endif.
  endif.

** 1/10/08 by ig.moon {
  if gt_itab-netpr eq 0 or gt_itab-peinh eq 0.
*    exit.
  endif.
**
  append gt_itab.
  clear gt_itab.

endform.                    " set_material_cost_for_parts
*&---------------------------------------------------------------------*
*&      Form  set_material_cost_for_additive
*&---------------------------------------------------------------------*
form set_material_cost_for_additive using p_shop structure it_shop.

  move: p_shop-kokrs                  to gt_itab-kokrs,
        p_shop-artnr                  to gt_itab-matnr,
        ztco_aala_creation_9000-gjahr to gt_itab-gjahr,
        p_shop-llv_matnr              to gt_itab-idnrk,
        p_shop-kstar                  to gt_itab-kstar,
*       p_shop-elemt                  to gt_itab-elemt,
        p_shop-mipcd                  to gt_itab-mipcd,
        p_shop-fevor                  to gt_itab-fevor,
        p_shop-shop                   to gt_itab-shop,
        p_shop-menge                  to gt_itab-menge,
        p_shop-meeht                  to gt_itab-meins,
        sy-uname                      to gt_itab-ernam,
        sy-datum                      to gt_itab-erdat,
        sy-uzeit                      to gt_itab-erzet,
        sy-uname                      to gt_itab-aenam,
        sy-datum                      to gt_itab-aedat,
        sy-uzeit                      to gt_itab-aezet.

  append gt_itab.

endform.                    " set_material_cost_for_additive
*&---------------------------------------------------------------------*
*&      Form  set_activity_cost_for_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP  text
*----------------------------------------------------------------------*
form set_activity_cost_for_parts using p_shop structure it_shop.
  clear: gt_itab.

  read table gt_itab with key matnr = p_shop-artnr
                              idnrk = p_shop-llv_matnr
                              mipcd = p_shop-mipcd
                              shop  = p_shop-shop
                              kstar = p_shop-kstar
*                             elemt = p_shop-elemt
                           binary search.
*                             kokrs = p_shop-kokrs
*                             gjahr = ztco_aala_creation_9000-gjahr
*                             versn = ztco_aala_creation_9000-versn.
  if sy-subrc eq 0.
    move: 'US'                          to gt_itab-urzla,
          1                             to gt_itab-netpr,
          1                             to gt_itab-aaprs.

    gt_itab-stprs = gt_itab-stprs +
                    p_shop-wertn / p_shop-lot_size. " *p_shop-menge.
*                   p_shop-wertn * p_shop-menge.

    modify gt_itab index sy-tabix.
  else.
    move: p_shop-kokrs                  to gt_itab-kokrs,
          p_shop-artnr                  to gt_itab-matnr,
          ztco_aala_creation_9000-gjahr to gt_itab-gjahr,
          p_shop-llv_matnr              to gt_itab-idnrk,
          p_shop-kstar                  to gt_itab-kstar,
*         p_shop-elemt                  to gt_itab-elemt,
          p_shop-mipcd                  to gt_itab-mipcd,
          p_shop-shop                   to gt_itab-shop,
          p_shop-fevor                  to gt_itab-fevor,
          'US'                          to gt_itab-urzla,
          1                             to gt_itab-netpr,
          1                             to gt_itab-aaprs,
          1                             to gt_itab-peinh,
          ztco_aala_creation_9000-versn to gt_itab-versn,
          'EA'                          to gt_itab-meins,
          sy-uname                      to gt_itab-ernam,
          sy-datum                      to gt_itab-erdat,
          sy-uzeit                      to gt_itab-erzet,
          sy-uname                      to gt_itab-aenam,
          sy-datum                      to gt_itab-aedat,
          sy-uzeit                      to gt_itab-aezet.

    gt_itab-stprs = p_shop-wertn / p_shop-lot_size.
*   gt_itab-stprs = p_shop-wertn * p_shop-menge.

    move: 1                             to gt_itab-menge.

    if it_source-kstar eq space.
      move: text-m26 to gt_itab-zzmsg.
    endif.

    append gt_itab.
    sort gt_itab by matnr idnrk mipcd shop kstar.  " elemt.
  endif.
endform.                    " set_activity_cost_for_parts
*&---------------------------------------------------------------------*
*&      Form  get_additive_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cost_component.
  select *
    into table it_tckh2
    from tckh2
   where ktopl = t001-ktopl
     and elehk = c_elehk.
  if sy-subrc ne 0.
    message e000(zz) with text-m07 text-m08.
  endif.
endform.                    " get_additive_cost
*&---------------------------------------------------------------------*
*&      Form  set_it_mip
*&---------------------------------------------------------------------*
form set_it_mip.

  perform set_aala_amount_for_mip.
  perform set_origin_for_mip.

endform.                    " set_it_mip
*&---------------------------------------------------------------------*
*&      Form  set_mip_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mip_data.
  if gt_itab-shop+2(1) eq 'T'.
    perform set_mip_data_for_trim.
  else.
    perform set_mip_data_for_other_shop.
  endif.
endform.                    " set_mip_data
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin
*&---------------------------------------------------------------------*
*       calcluate origin and collect to it_origin
*----------------------------------------------------------------------*
form set_mip_origin using p_mipcd.
  clear: it_origin.

  if gt_itab-shop+2(1) eq 'T'.
    perform set_mip_origin_for_trim using p_mipcd.
  else.
    perform set_mip_origin_for_other_shop using p_mipcd.
  endif.

endform.                    " set_mip_origin
*&---------------------------------------------------------------------*
*&      Form  set_aala_amount_for_mip
*&---------------------------------------------------------------------*
form set_aala_amount_for_mip.
  data: lw_mipcd like it_mip-mipcd,
        lw_index like sy-index.

  refresh: it_mip, it_origin.

  sort gt_itab by matnr shop mipcd matnr kstar. " elemt.
  loop at gt_itab.
    clear: it_mip.

    if gt_itab-fevor <> space.
      lw_mipcd = gt_itab-mipcd. "idnrk.
    else.
      clear lw_mipcd.
    endif.
*    if gt_itab-shop+2(1) eq 'E'.   "ENGINE
*      read table it_egntm with key matnr = gt_itab-mipcd.
*      if sy-subrc eq 0.                  "Engine/TM
*        move: it_egntm-matnr to lw_mipcd.
*      else.
*        clear: lw_mipcd.
*      endif.
*    else.
*      clear: lw_mipcd.
*    endif.

    read table it_mip with key matnr = gt_itab-matnr
                               mipcd = lw_mipcd
                               shop  = gt_itab-shop.
*                           binary search.
*                               kokrs = gt_itab-kokrs
*                               gjahr = gt_itab-gjahr
*                               versn = gt_itab-versn.
    if sy-subrc eq 0.
      move: sy-tabix to lw_index.

      perform set_mip_data.
      perform set_mip_origin using lw_mipcd.

      modify it_mip index lw_index.

*---add to MIP table
    else.
      move: gt_itab-kokrs to it_mip-kokrs,
            gt_itab-gjahr to it_mip-gjahr,
            gt_itab-versn to it_mip-versn,

            gt_itab-matnr to it_mip-matnr,
            lw_mipcd      to it_mip-mipcd,
            gt_itab-shop  to it_mip-shop.

      perform set_mip_data.
      perform set_mip_origin using lw_mipcd.

*-----ANDY: only MIP append
*      IF NOT lw_mipcd IS INITIAL.
      append it_mip.
      sort it_mip by matnr mipcd shop.
    endif.
  endloop.

  sort it_mip by matnr mipcd shop.

endform.                    " set_aala_amount_for_mip
*&---------------------------------------------------------------------*
*&      Form  set_origin_for_mip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_origin_for_mip.
* SORT it_origin BY matnr mipcd shop wrbtr.  "UD1K919880
  delete it_origin where urzla eq 'US'
                      or urzla eq 'CA'.


* by ig.moon {
*  sort it_origin ascending by  matnr mipcd shop             "UD1K919880
*  descending   wrbtr. ""UD1K919880

  sort it_origin by  matnr mipcd shop ascending
                     wrbtr descending .
* }

  data: lw_index type i.
  loop at it_origin.
    at new shop.
      clear: lw_index.
    endat.

    lw_index = lw_index + 1.
    move: lw_index to it_origin-index.

    modify it_origin.
  endloop.

  loop at it_mip.
    loop at it_origin where matnr = it_mip-matnr
                        and mipcd = it_mip-mipcd
                        and shop  = it_mip-shop.
      case it_origin-index.
        when 1.
          it_mip-fstld = it_origin-urzla.
          it_mip-fstpp = it_origin-wrbtr / it_mip-wrbtr * 100.
          it_mip-fsttr = it_origin-wrbtr.
        when 2.
          it_mip-sndld = it_origin-urzla.
          it_mip-sndpp = it_origin-wrbtr / it_mip-wrbtr * 100.
          it_mip-sndtr = it_origin-wrbtr.
      endcase.
    endloop.

    move: sy-uname to it_mip-ernam,
          sy-datum to it_mip-erdat,
          sy-uzeit to it_mip-erzet,
          sy-uname to it_mip-aenam,
          sy-datum to it_mip-aedat,
          sy-uzeit to it_mip-aezet.

    modify it_mip.
  endloop.
endform.                    " set_origin_for_mip
*&---------------------------------------------------------------------*
*&      Form  set_shop_mip_for_matl_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SHOP_MIP  text
*      -->P_P_MIP  text
*      -->P_P_MIPCD  text
*----------------------------------------------------------------------*
form set_shop_mip_for_matl_cost using p_shop_mip structure it_shop_mip
                                      p_fsc
                                      p_mipcd.
* check again.. to make it sure...
  select single *
    from ztco_shop_pln
   where kokrs     eq   ztco_aala_creation_9000-kokrs
     and bdatj     eq   ztco_aala_creation_9000-bdatj
     and poper     eq   ztco_aala_creation_9000-poper
     and klvar     eq   w_klvar
*     AND versn     EQ   w_versn
     and artnr     eq   p_shop_mip-llv_matnr.
  if sy-subrc eq 0.
    perform get_mip_lower_level_parts using p_shop_mip
                                            p_fsc
                                            p_mipcd
                                            p_shop_mip-menge.
  else.
    clear: it_shop_mip.
    move: p_shop_mip to it_shop_mip,
          p_fsc      to it_shop_mip-artnr,
          p_mipcd    to it_shop_mip-mipcd.

    if it_shop_mip-shop is initial.
      move: c_trim to it_shop_mip-shop.
    endif.

    append it_shop_mip.
  endif.
endform.                    " set_shop_mip_for_matl_cost
*&---------------------------------------------------------------------*
*&      Form  set_shop_mip_for_act_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_SHOP_MIP  text
*      -->P_P_MIP  text
*      -->P_P_MIPCD  text
*----------------------------------------------------------------------*
form set_shop_mip_for_act_cost using p_shop_mip structure it_shop_mip
                                     p_fsc
                                     p_mipcd.
  clear: it_shop_mip.

  move: p_shop_mip to it_shop_mip,
        p_fsc      to it_shop_mip-artnr,
        p_mipcd    to it_shop_mip-mipcd.

  if it_shop_mip-shop is initial.
    move: c_trim to it_shop_mip-shop.
  endif.

  append it_shop_mip.
endform.                    " set_shop_mip_for_act_cost
*&---------------------------------------------------------------------*
*&      Form  set_it_fsc_n_it_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_fsc_n_it_model.
  read table it_source index 1.

  refresh: it_fsc, it_model.
  call function 'Z_FCO_AALA_CALC_FSC_MODEL'
       exporting
            i_makrt        = ztco_aala_creation_9000-makrt
            i_bdatj        = ztco_aala_creation_9000-bdatj
            i_poper        = ztco_aala_creation_9000-poper
            i_zver_des     = it_source-zver_des
       tables
            t_fsc_qty      = it_fsc_alt_qty
            t_mip          = it_mip
            t_fsc          = it_fsc
            t_model        = it_model
       exceptions
            quantity_error = 1
            program_error  = 2
            others         = 3.
  if sy-subrc <> 0.
    message e000(zz) with text-m13.
  endif.

endform.                    " set_it_fsc_n_it_model
*&---------------------------------------------------------------------*
*&      Form  update_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_data.
  perform set_it_parts.
  perform delete_data.
  perform insert_data.
endform.                    " update_data
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_data.
  perform set_fsc_for_delete.

  delete from ztco_aala_parts
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and matnr in r_fsc.

  delete from ztco_aala_mip
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and matnr in r_fsc.

  delete from ztco_aala_mip_bk
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and matnr in r_fsc.

  delete from ztco_aala_fsc
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and matnr in r_fsc.

  delete from ztco_aala_fsc_bk
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and matnr in r_fsc.

  delete from ztco_aala_model
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and model = ztco_aala_creation_9000-model.

  delete from ztco_aala_mdl_bk
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn
     and model = ztco_aala_creation_9000-model.

  commit work and wait.
endform.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  replace_alternative_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form replace_alternative_fsc.
  loop at it_fsc_alt_qty where altmt > space.
    loop at it_shop where artnr eq it_fsc_alt_qty-altmt.
      move: it_fsc_alt_qty-matnr to it_shop-artnr.
      append it_shop.
    endloop.
  endloop.
endform.                    " replace_alternative_fsc
*&---------------------------------------------------------------------*
*&      Form  insert_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_data.
  insert ztco_aala_parts from table it_parts accepting duplicate keys.
  if sy-subrc ne 0.
    rollback work.
    message e000(zz) with text-m14.
  endif.


  loop at it_mip.
  ztco_aala_mip = it_mip.
  insert ztco_aala_mip." accepting duplicate keys.

  if sy-subrc ne 0.
  endif.

  endloop.

*  insert ztco_aala_mip from table it_mip accepting duplicate keys.
*
*  if sy-subrc ne 0.
*    rollback work.
*    message e000(zz) with text-m14.
*  endif.

  insert ztco_aala_fsc from table it_fsc accepting duplicate keys.
  if sy-subrc ne 0.
    rollback work.
    message e000(zz) with text-m14.
  endif.

  insert ztco_aala_model from table it_model accepting duplicate keys.

  if sy-subrc ne 0.
    rollback work.
    message e000(zz) with text-m14.
  else.
    commit work and wait.
    message s000(zz) with text-m15.
  endif.
endform.                    " insert_data
*&---------------------------------------------------------------------*
*&      Form  calculate_error_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_error_count.
  data: lw_msg01(50),
        lw_msg02(50).

  clear: w_error, w_total, w_success.

  describe table gt_itab lines w_total.
  loop at gt_itab.

    if gt_itab-zzmsg is initial.
      if gt_itab-urzla is initial.
        move: text-m22 to gt_itab-zzmsg.
        w_error = w_error + 1.
        move: c_error to gt_itab-icon.
      else.
        move: c_success to gt_itab-icon.
      endif.
    else.
      w_error = w_error + 1.
      move: c_error to gt_itab-icon.
    endif.

    modify gt_itab.
  endloop.

  w_success = w_total - w_error.

*temp!!!
  exit.

  check w_error > 0.

  concatenate text-m18 w_total
         into lw_msg01 separated by space.

  concatenate text-m19 w_success text-m20 w_error
         into lw_msg02 separated by space.

  message s000(zz) with lw_msg01 lw_msg02.

  call screen 9100.

  leave to screen sy-dynnr.

endform.                    " calculate_error_count
*&---------------------------------------------------------------------*
*&      Form  set_it_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_parts.
  refresh it_parts.

  loop at gt_itab.
    clear: it_parts.

    move-corresponding gt_itab to it_parts.

*   APPEND it_parts.            "UD1K919868
    collect it_parts.                                       "UD1K919868
  endloop.
endform.                    " set_it_parts
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_object output.
  concatenate: 'WC_CONTAINER_' sy-dynnr into w_container.
  assign:      (w_container)            to   <container>.

  if <container> is initial. "/Not Created Control for ALV GRID
    perform create_container_n_object.
    perform set_attributes_alv_grid.
    perform build_field_catalog.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    perform assign_itab_to_alv.
    perform sssign_event.
  endif.
endmodule.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
form dbl_click_9000 using    p_e_column_fieldname
                             p_es_row_no_row_id.

endform.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  concatenate: 'WC_CONTAINER_' sy-dynnr into w_container,
               'WC_CONTROL_'   sy-dynnr into w_control,
               'WC_ALV_'       sy-dynnr into w_alv.

  assign: (w_container) to <container>,
          (w_control)   to <control>,
          (w_alv)       to <alv>.

  create object <container>
         exporting container_name = <control>
         exceptions
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object <alv>
         exporting i_parent      = <container>
                   i_appl_events = 'X'.
endform.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid.
  case sy-dynnr.
    when '9100'.
      perform set_attributes_alv_9100.
  endcase.
endform.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog.
  case sy-dynnr.
    when '9100'.
      perform build_field_catalog_9100.
  endcase.
endform.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv.
* Display data
  concatenate: 'WC_ALV_'    sy-dynnr      into w_alv,
               c_structure  sy-dynnr      into w_structure,
               'GT_ITAB'    '[]' into w_itab.

  assign: (w_alv)       to <alv>,
          (w_itab)      to <itab>.


  call method <alv>->set_table_for_first_display
     exporting i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     changing  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
endform.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sssign_event.

endform.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9100.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog_9100.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  perform set_fieldname.

  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MIPCD'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  ' ' 'EMPHASIZE'   'C400',
                                  'E' 'KEY'         'X',

                                  'S' 'SHOP'        ' ',
                                  'E' 'NO_SIGN'     'X',

                                  'S' 'KOKRS'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'GJAHR'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERNAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERZET'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AENAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEZET'       ' ',
                                  'E' 'NO_OUT'      'X'.

endform.                    " build_field_catalog_9100
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldname.
  data: lw_itab type slis_tabname.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  move: sy-repid to w_repid.
  concatenate c_structure sy-dynnr into lw_itab.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       changing
            ct_fieldcat        = it_fieldname.
endform.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_3884   text
*      -->P_3885   text
*      -->P_3886   text
*----------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat structure it_fieldcat
                      using    p_gubun
                               p_field
                               p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_fieldcat.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check filed catalog'.
    endif.

    move: w_fieldname-fieldname to p_fieldcat-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_FIELDCAT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    if p_fieldcat-col_pos is initial.
      add 1 to w_cnt.
      p_fieldcat-col_pos = w_cnt.
    endif.
    append p_fieldcat.
  endif.
endform.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9100 input.
  case sy-ucomm.
    when 'BACK'.
      clear sy-ucomm.
      leave to screen 0.
  endcase.
endmodule.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  checing_before_excute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form checing_before_excute.
  read table it_fsc_alt_qty index 1.
  if sy-subrc ne 0.
    message s000(zz) with text-m21.
    leave to screen sy-dynnr.
  endif.
endform.                    " checing_before_excute
*&---------------------------------------------------------------------*
*&      Form  set_mip_data_for_trim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mip_data_for_trim.
  data: lw_mod   type f,
        lw_propo like ztco_aala_model-propo.

  if gt_itab-netpr = 0.
    lw_propo = 0.
  else.
    lw_propo = gt_itab-aaprs / gt_itab-netpr * 100.
  endif.

  if lw_propo >= 70.
    lw_propo = 100.
  else.
    lw_mod = lw_propo mod 5.
    lw_propo = lw_propo - lw_mod.

    if lw_mod >= '2.5'.
      lw_propo = lw_propo + 5.
    endif.
  endif.

*TEMP...{{{{{{{{{{{{{{{
  if gt_itab-peinh = 0.
    gt_itab-peinh = 1.
  endif.


  it_mip-wrbtr =     it_mip-wrbtr +
                 ( ( gt_itab-stprs - gt_itab-dduty ) *
                     gt_itab-menge / gt_itab-peinh ).

*  IF ( gt_itab-kstar >= c_mcost_f AND      "Material Cost
*       gt_itab-kstar <= c_mcost_t )
*  OR ( gt_itab-kstar = c_mip AND gt_itab-mipcd = space ).
  if gt_itab-kstar >= c_activity.
    it_mip-acost =   it_mip-acost +
       ( gt_itab-stprs * gt_itab-menge / gt_itab-peinh ).

    it_mip-aaprs =   it_mip-aaprs +
       ( gt_itab-stprs * gt_itab-menge / gt_itab-peinh ).
  else.
    it_mip-mcost =     it_mip-mcost +
                   ( ( gt_itab-stprs - gt_itab-dduty ) *
                       gt_itab-menge / gt_itab-peinh ).

    it_mip-aaprs =     it_mip-aaprs +
                 ( ( ( gt_itab-stprs - gt_itab-dduty ) *
                       gt_itab-menge / gt_itab-peinh ) *
                       lw_propo / 100 ).
  endif.
endform.                    " set_mip_data_for_trim
*&---------------------------------------------------------------------*
*&      Form  set_mip_data_for_other_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mip_data_for_other_shop.

*TEMP>>>>>>>>>>>>>>>>>>>>>
  if gt_itab-peinh = 0.
    gt_itab-peinh = 1.
  endif.

  it_mip-wrbtr =     it_mip-wrbtr +
                 ( ( gt_itab-stprs - gt_itab-dduty ) *
                     gt_itab-menge / gt_itab-peinh ).

*  IF ( gt_itab-kstar >= c_mcost_f AND      "Material Cost
*      gt_itab-kstar <= c_mcost_t )
*  OR ( gt_itab-kstar = c_mip AND gt_itab-mipcd = space ).

  if gt_itab-kstar >= c_activity.
    it_mip-acost =   it_mip-acost +
          ( gt_itab-stprs * gt_itab-menge / gt_itab-peinh ).

    it_mip-aaprs =   it_mip-aaprs +
          ( gt_itab-stprs * gt_itab-menge / gt_itab-peinh ).
  else.
    it_mip-mcost =     it_mip-mcost +
                   ( ( gt_itab-stprs - gt_itab-dduty ) *
                       gt_itab-menge / gt_itab-peinh ).

    if gt_itab-netpr = 0.
    else.
      it_mip-aaprs =     it_mip-aaprs +
                   ( ( ( gt_itab-stprs - gt_itab-dduty ) *
                         gt_itab-menge / gt_itab-peinh ) *
                         gt_itab-aaprs / gt_itab-netpr ).
    endif.
  endif.
endform.                    " set_mip_data_for_other_shop
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin_for_other_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mip_origin_for_other_shop using p_mipcd.
* US/CA -> KR change???
  if gt_itab-urzla eq 'US' or
     gt_itab-urzla eq 'CA'.
    move: gt_itab-matnr to it_origin-matnr,
          p_mipcd       to it_origin-mipcd,
          gt_itab-shop  to it_origin-shop,
          'KR'           to it_origin-urzla.
    it_origin-wrbtr =  ( gt_itab-stprs - gt_itab-dduty ) *
                         gt_itab-menge / gt_itab-peinh -
                   ( ( ( gt_itab-stprs - gt_itab-dduty ) *
                         gt_itab-menge / gt_itab-peinh ) *
                         gt_itab-aaprs / gt_itab-netpr ).

    collect it_origin.
  else.
    move: gt_itab-matnr to it_origin-matnr,
          p_mipcd       to it_origin-mipcd,
          gt_itab-shop  to it_origin-shop,
          gt_itab-urzla to it_origin-urzla.

    it_origin-wrbtr = ( gt_itab-stprs - gt_itab-dduty ) *
                        gt_itab-menge / gt_itab-peinh.

    collect it_origin.
  endif.
endform.                    " set_mip_origin_for_other_shop
*&---------------------------------------------------------------------*
*&      Form  set_mip_origin_for_trim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mip_origin_for_trim using p_mipcd.
  data: lw_mod   type f,
        lw_propo like ztco_aala_model-propo.

* only for US/CA calculation (ANDY)

  if gt_itab-netpr = 0.
    lw_propo = 0.
  else.
    lw_propo = gt_itab-aaprs / gt_itab-netpr * 100.
  endif.

  if lw_propo >= 70.
    lw_propo = 100.
  else.
    lw_mod = lw_propo mod 5.
    lw_propo = lw_propo - lw_mod.

    if lw_mod >= '2.5'.
      lw_propo = lw_propo + 5.
    endif.
  endif.

* US/CA -> KR change???
  if gt_itab-urzla eq 'US' or
     gt_itab-urzla eq 'CA'.
    move: gt_itab-matnr to it_origin-matnr,
          p_mipcd       to it_origin-mipcd,
          gt_itab-shop  to it_origin-shop,
          'KR'          to it_origin-urzla.
    it_origin-wrbtr =  ( gt_itab-stprs - gt_itab-dduty ) *
                         gt_itab-menge / gt_itab-peinh -
                   ( ( ( gt_itab-stprs - gt_itab-dduty ) *
                         gt_itab-menge / gt_itab-peinh ) *
                         lw_propo / 100 ).

    collect it_origin.
  else.
    move: gt_itab-matnr to it_origin-matnr,
          p_mipcd       to it_origin-mipcd,
          gt_itab-shop  to it_origin-shop,
          gt_itab-urzla to it_origin-urzla.

    it_origin-wrbtr = ( gt_itab-stprs - gt_itab-dduty ) *
                        gt_itab-menge / gt_itab-peinh.

    collect it_origin.
  endif.
endform.                    " set_mip_origin_for_trim
*&---------------------------------------------------------------------*
*&      Form  read_duty_from_hts_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_duty_from_hts_code using p_shop structure it_shop
                                   p_duty.
  if it_source-urzla eq 'US' or
     it_source-urzla eq 'CA'.
    p_duty = 0.
    exit.
  endif.

  select single * from marc where matnr = p_shop-llv_matnr
                              and werks = p_shop-werks.
  if sy-subrc ne 0.
    p_duty = 0.
    exit.
  endif.

  select single * from a902 where kappl = 'M'
                              and kschl = 'ZOA1'
                              and stawn = marc-stawn.
  if sy-subrc ne 0.
    p_duty = 0.
    exit.
  endif.

  select single * from konp where knumh = a902-knumh.
  if sy-subrc ne 0.
    p_duty = 0.
    exit.
  endif.

* if p_shop-llv_matnr = 'S721'.
*   break-point.
* endif.

  p_duty = it_source-stprs * konp-kbetr / 1000.

*FIXME
* p_duty = ( it_shop-wertn / ( 1 + konp-kbetr ) ) * konp-kbetr / 1000.

endform.                    " read_duty_from_hts_code
*&---------------------------------------------------------------------*
*&      Form  check_shop_cost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTCO_AALA_CREATION_9000_TAB_MA  text
*----------------------------------------------------------------------*
form check_shop_cost using p_matnr.
  select single klvar into w_klvar
    from ztco_aala_source
   where kokrs = ztco_aala_creation_9000-kokrs
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message s000(zz) with text-m04 ztco_aala_creation_9000-versn.
    leave to screen sy-dynnr.
  endif.

  clear: w_versn.
  case w_klvar.
    when 'ZPCP'.              "Annual Plan Cost
      move: '311' to w_versn.

      concatenate ztco_aala_creation_9000-bdatj '05' into w_period.
    when 'PPC1'.              "Standard Cost
      move: '000' to w_versn.

      case ztco_aala_creation_9000-poper.
        when '001' or '002' or '003'.
          concatenate ztco_aala_creation_9000-bdatj '01' into w_period.
        when '004' or '005' or '006'.
          concatenate ztco_aala_creation_9000-bdatj '02' into w_period.
        when '007' or '008' or '009'.
          concatenate ztco_aala_creation_9000-bdatj '03' into w_period.
        when '010' or '011' or '012'.
          concatenate ztco_aala_creation_9000-bdatj '04' into w_period.
      endcase.
  endcase.

  select single *
    from ztco_shop_pln
   where       kokrs      eq ztco_aala_creation_9000-kokrs
     and       bdatj      eq ztco_aala_creation_9000-bdatj
     and       poper      eq ztco_aala_creation_9000-poper
     and       klvar      eq w_klvar
*     AND       versn      EQ w_versn
     and       artnr      eq p_matnr.
  if sy-subrc ne 0.
    message e000(zz) with p_matnr text-m27.
  endif.
endform.                    " check_shop_cost
*&---------------------------------------------------------------------*
*&      Form  SET_FSC_FOR_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fsc_for_delete.
  clear: r_fsc, r_fsc[].

  loop at it_fsc_alt_qty.
    move: 'I'  to r_fsc-sign,
          'EQ' to r_fsc-option.

    move: it_fsc_alt_qty-matnr to r_fsc-low.

    append r_fsc.
  endloop.
endform.                    " SET_FSC_FOR_DELETE
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module table_control_lines output.
  describe table it_fsc_alt_qty lines tc_9000-lines.
endmodule.                 " TABLE_CONTROL_LINES  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  GET_SELECT_FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_select_filed.
  data : wa_ftab(72) type c.

  clear ftab.
  wa_ftab = 'A~KOKRS'.     append wa_ftab to ftab.
  wa_ftab = 'A~BDATJ'.     append wa_ftab to ftab.
  wa_ftab = 'A~POPER'.     append wa_ftab to ftab.
  wa_ftab = 'A~KLVAR'.     append wa_ftab to ftab.
  wa_ftab = 'A~ARTNR'.     append wa_ftab to ftab.
  wa_ftab = 'A~TYPPS'.     append wa_ftab to ftab.
  wa_ftab = 'A~KSTAR'.     append wa_ftab to ftab.
  wa_ftab = 'A~RESOU'.     append wa_ftab to ftab.
  wa_ftab = 'A~SHOP'.      append wa_ftab to ftab.
  wa_ftab = 'A~FEVOR'.     append wa_ftab to ftab.
  wa_ftab = 'A~LLV_MATNR'. append wa_ftab to ftab.
  wa_ftab = 'A~WERKS'.     append wa_ftab to ftab.
  wa_ftab = 'A~BWKEY'.     append wa_ftab to ftab.
  wa_ftab = 'A~BWTAR'.     append wa_ftab to ftab.
  wa_ftab = 'A~KALNR'.     append wa_ftab to ftab.
  wa_ftab = 'A~KOSTL'.     append wa_ftab to ftab.
  wa_ftab = 'A~LSTAR'.     append wa_ftab to ftab.
  wa_ftab = 'A~PAR_KALNR'. append wa_ftab to ftab.
  wa_ftab = 'A~PAR_WERKS'. append wa_ftab to ftab.
  wa_ftab = 'A~PAR_KADKY'. append wa_ftab to ftab.
  wa_ftab = 'A~CK_KALST'.  append wa_ftab to ftab.
  wa_ftab = 'A~MENGE'.     append wa_ftab to ftab.
  wa_ftab = 'A~LOT_SIZE'.  append wa_ftab to ftab.
  wa_ftab = 'A~MEEHT'.     append wa_ftab to ftab.
  wa_ftab = 'A~PEINH'.     append wa_ftab to ftab.
  wa_ftab = 'A~BESKZ'.     append wa_ftab to ftab.
  wa_ftab = 'A~SOBSL'.     append wa_ftab to ftab.
  wa_ftab = 'A~VSPVB'.     append wa_ftab to ftab.
  wa_ftab = 'A~MTART'.     append wa_ftab to ftab.

  wa_ftab = 'A~WERTN'.     append wa_ftab to ftab.
  wa_ftab = 'A~WRTFX'.     append wa_ftab to ftab.

*  wa_ftab = 'B~ELEMT'.     append wa_ftab to ftab.
*  wa_ftab = 'B~WERTN'.     append wa_ftab to ftab.
*  wa_ftab = 'B~WRTFX'.     append wa_ftab to ftab.
*  wa_ftab = 'B~KKZMA' .    append wa_ftab to ftab.  "additive
endform.                    " GET_SELECT_FILED
*&---------------------------------------------------------------------*
*&      Form  get_additive_shopcost
*&---------------------------------------------------------------------*
form get_additive_shopcost.

  select (ftab)
    into corresponding fields of table it_shop_additive
    from ztco_shop_pln as a
*   inner join ztco_shop_pln_cc as b
*      on a~kokrs     eq   b~kokrs
*     and a~bdatj     eq   b~bdatj
*     and a~poper     eq   b~poper
*     and a~klvar     eq   b~klvar
*     and a~artnr     eq   b~artnr
*     and a~typps     eq   b~typps
*     and a~kstar     eq   b~kstar
*     and a~resou     eq   b~resou
   where a~kokrs     eq ztco_aala_creation_9000-kokrs
     and a~bdatj     eq ztco_aala_creation_9000-bdatj
     and a~poper     eq ztco_aala_creation_9000-poper
     and a~klvar     eq w_klvar
*     AND A~versn     EQ w_versn
     and a~artnr     in r_fsc
     and a~typps     eq 'V'.

endform.                    " get_additive_shopcost
