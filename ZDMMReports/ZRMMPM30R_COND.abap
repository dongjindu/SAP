************************************************************************
* Program Name      : ZRMMPM30R_COND
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.02.23.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K907463
* Addl Documentation:
* Description       : Condition Status by Material/Vendor
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.02.23.     Sung-Tae Lim     UD1K907463     Initial Coding
* 2005.06.25.     chris Li                        Add buniness plan cost
* 2005.08.15.     Andy Choi                       Fix bug, add mapping
************************************************************************
* TODO item
* - change ABP costing logic
************************************************************************
report zrmmpm30r_cond no standard page heading
                     line-size 600
                     message-id zmmm.

tables: t416, ztco_shopcost.
**---
include : zrmmpmxxr_incl.
**--- Types
types : z_amnt like konp-kbetr,
        z_qnty like ztmm_cond-menge.


*TABLES: mara, mast,cabn.

**--- Internal Tables
data : begin of it_itab occurs 0,
         matnr like mara-matnr,
         maktx like makt-maktx,
         werks like marc-werks,
*         matnr_p LIKE mara-matnr,
*         stlkn LIKE stpo-stlkn,
         lifnr like lfa1-lifnr,
         name1 like lfa1-name1,
         meins like ztmm_cond-meins,
         waers like eine-waers,
*--- Shop cost plan amount per vehicle
         sourc(2) type c,
         mtart like mara-mtart,
         cos_u type z_amnt,     " unit price
         cos_p like ekpo-peinh, " Price unit
         cos_q type z_qnty,     " quantity
         cos_a type z_amnt,     " amount
*--- total purchasing amount per vehicle
         tot_u type z_amnt,     " unit price
         tot_p like ekpo-peinh, " Price unit
         tot_q type z_qnty,     " quantity
         tot_a type z_amnt,     " amount
         eff_p type eine-effpr, " eff.price
         eff_a type z_amnt,
*--- Difference per vehicle
         dif_u type z_amnt,     " unit price
         dif_p like ekpo-peinh, " Price unit
         dif_q type z_qnty,     " quantity
         dif_a type z_amnt,     " amount
*--- packaging amortization cost - zp01
         zp01a type z_amnt,     " price
         zp01b type z_amnt,     " amount
         zp01c type z_qnty,     " depreciation quantity
         zp01d type z_qnty,     " amortization quantity
*--- tooling amortization cost - zp02
         zp02a type z_amnt,     " price
         zp02b type z_amnt,     " amount
         zp02c type z_qnty,     " depreciation quantity
         zp02d type z_qnty,     " amortization quantity
*--- development amortizatino cost - zp03
         zp03a type z_amnt,     " price
         zp03b type z_amnt,     " amount
         zp03c type z_qnty,     " depreciation quantity
         zp03d type z_qnty,     " amortization quantity
*--- raw materials cost - zp04
         zp04a type z_amnt,     " price
         zp04b type z_amnt,     " amount
*--- freight cost - zp05
         zp05a type z_amnt,     " price
         zp05b type z_amnt,     " amount
*--- CC cost - zp06
         zp06a type z_amnt,     " price
         zp06b type z_amnt,     " amount
*--- sequencing cost - zp07
         zp07a type z_amnt,     " price
         zp07b type z_amnt,     " amount
*--- direct labor cost - zp08
         zp08a type z_amnt,     " price
         zp08b type z_amnt,     " amount
*--- machine cost - zp09
         zp09a type z_amnt,     " price
         zp09b type z_amnt,     " amount
*--- MBE cost - zp10
         zp10a type z_amnt,     " price
         zp10b type z_amnt,     " amount
*--- WBE cost - zp11
         zp11a type z_amnt,     " price
         zp11b type z_amnt,     " amount
*--- NAFTA cost - zp12
         zp12a type z_amnt,     " price
         zp12b type z_amnt,     " amount
*--- AALA cost - zp13
         zp13a type z_amnt,     " price
         zp13b type z_amnt,     " amount
*--- By Up Packaging - zp14
         zp14a type z_amnt,     " price
         zp14b type z_amnt,     " amount
       end of it_itab.

data: begin of it_header occurs 0,
        matnr   like   mara-matnr,
        werks   like   marc-werks,
        stlan   like   mast-stlan,
      end   of it_header.

data: begin of it_lifnr occurs 0,
        lifnr   like   lfa1-lifnr,
        knumh   like   konp-knumh,
        kbetr   like   konp-kbetr,
        kpein   like   konp-kpein,
        price   type   f,
        effpr   type   f,
        peinh   like   eine-peinh,
      end   of it_lifnr.

data: begin of it_condition occurs 0,
        kbetr like zvmm_info_condi-kbetr,
        kpein like zvmm_info_condi-kpein,
        kschl like zvmm_info_condi-kschl,
      end   of it_condition.

data dynpread like dynpread occurs 0 with header line.

data: begin of valuetab occurs 0,
          value(80).
data: end of valuetab.

data: begin of fields occurs 0.
        include structure help_value.
data: end of fields.

data: begin of dynpfields  occurs 0.
        include structure dynpread.
data: end of dynpfields.

data: begin of select_values occurs 0.
        include structure help_vtab.
data: end of select_values.

data: it_shopcost like ztco_shopcost occurs 0 with header line.
data: it_all_comp like ztco_shopcost occurs 0 with header line.

*---// Global variables and structures
data: select_index like sy-tabix.

*---// Constants
constants: c_check                    value 'X',
           c_kokrs   like tka01-kokrs value 'H201',
           c_mtart   like mara-mtart  value 'ROH',
           c_mtart1   like mara-mtart  value 'ROH1',
           c_capid   like rc29l-capid value 'PP01', "Application
           c_cuobj   like marc-cuobj  value '999999999999999999',
           c_werks   like marc-werks  value 'E001',
           c_ekorg   like ekko-ekorg  value 'PU01',
           c_kschl   like konp-kschl  value 'PB00',
           c_roh     type i           value 1,
           c_stlal   like mast-stlal  value '01',
           c_module  type i           value 2,
           c_klvar   like ztco_shopcost-klvar value 'ZPCP',
           c_subpart type i           value 3,
           c_module_stlan like mast-stlan value '2'.
constants: c_mip_cos_ele(10)        value '0000540300',
           c_mip_cos_com like ztco_shopcost-elemt value '070'.

*---// Ranges
ranges: r_lifnr for lfa1-lifnr.

*--- ALV
type-pools: slis.
data : w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.

*---- ALV

**--- Macro
define append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
*  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
end-of-definition.

*---// Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters: p_werks like t001w-werks obligatory memory id wrk,
            p_stlan like mast-stlan  obligatory memory id csv,
            p_stlal like mast-stlal  obligatory,
            p_datuv like rc29n-datuv default sy-datum.
selection-screen skip.
parameters: p_lifnr like lfa1-lifnr,
            p_matnr like mara-matnr  obligatory visible length 18.
selection-screen skip.
parameters: p_atwre like  cawn-atwrt obligatory visible length 3,
            p_atwri like  cawn-atwrt obligatory visible length 3.
selection-screen end   of block bl1.

selection-screen begin of block bl2 with frame title text-t04.
selection-screen end   of block bl2.

selection-screen skip.
parameters: p_disp  as checkbox.
parameters: p_bfsc  like mara-matnr  visible length 18,
            p_year  like ztco_shopcost-bdatj,
            p_perid like ztco_shopcost-poper.

start-of-selection.

at selection-screen on value-request for p_atwre.
  perform help_request_p_atwre.

at selection-screen on value-request for p_atwri.
  perform help_request_p_atwri.

top-of-page.
  perform write_top.

*---// Check input fields & Read data
at selection-screen.
  check sy-ucomm eq 'ONLI'.
  perform check_input_value.
  perform read_data.

*---// Display data
start-of-selection.
  perform display_data.
*&---------------------------------------------------------------------*
*&      Form  help_request_p_atwre
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form help_request_p_atwre.
  data it_ksml   like table of ksml  with header line.
  data : begin of it_cawn occurs 0,
          atwrt   like  cawn-atwrt,
          atwtb   like  cawnt-atwtb.
  data : end of it_cawn.

  data :  l_cuobj   like  inob-cuobj,
          l_clint   like  klah-clint.

  clear: mara.
*  SELECT SINGLE * FROM mara WHERE matnr EQ p_matnr.
*  CHECK mara-mtart EQ 'FERT'.

  clear dynpread. refresh dynpread.
  clear valuetab. refresh valuetab.
  clear fields.   refresh fields.

  perform value_read using: 'P_MATNR'.
  loop at dynpread.
    case sy-tabix.
      when 1. p_matnr = dynpread-fieldvalue.
    endcase.
  endloop.

  select single cuobj
         into l_cuobj
         from inob
         where klart eq '300'
           and obtab eq 'MARA'
           and objek eq p_matnr.

  select single clint
         into l_clint
         from kssk
         where objek eq l_cuobj
           and mafid eq 'O'
           and klart eq '300'.

  select *
         into table it_ksml
         from ksml
         where clint eq l_clint.

  data l_tabix   like sy-tabix.
  loop at it_ksml.
    l_tabix = sy-tabix.
    select single *
              from cabn
              where atinn eq it_ksml-imerk
                and atnam eq 'COLOREXT'.
    if sy-subrc ne 0.
      delete it_ksml index l_tabix.
    endif.
  endloop.

  read table it_ksml index 1.
  select a~atwrt
         b~atwtb
         into table it_cawn
         from cawn as a inner join cawnt as b
                        on  a~atinn eq b~atinn
                        and a~atzhl eq b~atzhl
         where a~atinn eq it_ksml-omerk.
  sort it_cawn.
  it_cawn-atwrt = 'No entry'.
  insert it_cawn index 1.
  clear: it_cawn.
  loop at it_cawn.
    valuetab-value = it_cawn-atwrt.
    append valuetab. clear valuetab.
    valuetab-value = it_cawn-atwtb.
    append valuetab. clear valuetab.
  endloop.

  perform add_fields using: 'CAWN'  'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  perform help_values_get.

  if select_index > 0.
    read table it_cawn   index select_index.
    perform value_update using:
            'X'   'P_ATWRE' it_cawn-atwrt 0.
  endif.
endform.                    " help_request_p_atwre
*&---------------------------------------------------------------------*
*&      Form  add_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0390   text
*      -->P_0391   text
*      -->P_0392   text
*----------------------------------------------------------------------*
form add_fields using  p_tabname p_fieldname p_flag.
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  append fields.      clear fields.
endform.                    " add_fields
*&---------------------------------------------------------------------*
*&      Form  help_values_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form help_values_get.
  call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
       exporting
            display                   = ' '
       importing
            index                     = select_index
       tables
            fields                    = fields
            select_values             = select_values
            valuetab                  = valuetab
       exceptions
            field_not_in_ddic         = 1
            more_then_one_selectfield = 2
            no_selectfield            = 3
            others                    = 4.
endform.                    " help_values_get
*&---------------------------------------------------------------------*
*&      Form  help_request_p_atwri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form help_request_p_atwri.
  data it_ksml   like table of ksml  with header line.
  data : begin of it_cawn occurs 0,
          atwrt   like  cawn-atwrt,
          atwtb   like  cawnt-atwtb.
  data : end of it_cawn.

  data :  l_cuobj   like  inob-cuobj,
          l_clint   like  klah-clint.

  clear: mara.
*  SELECT SINGLE * FROM mara WHERE matnr EQ p_matnr.
*  CHECK mara-mtart EQ 'FERT'.

  clear dynpread. refresh dynpread.
  clear valuetab. refresh valuetab.
  clear fields.   refresh fields.

  perform value_read using: 'P_MATNR'.
  loop at dynpread.
    case sy-tabix.
      when 1. p_matnr = dynpread-fieldvalue.
    endcase.
  endloop.

  select single cuobj
         into l_cuobj
         from inob
         where klart eq '300'
           and obtab eq 'MARA'
           and objek eq p_matnr.

  select single clint
         into l_clint
         from kssk
         where objek eq l_cuobj
           and mafid eq 'O'
           and klart eq '300'.

  select *
         into table it_ksml
         from ksml
         where clint eq l_clint.

  data l_tabix   like sy-tabix.
  loop at it_ksml.
    l_tabix = sy-tabix.
    select single *
              from cabn
              where atinn eq it_ksml-imerk
                and atnam eq 'COLORINT'.
    if sy-subrc ne 0.
      delete it_ksml index l_tabix.
    endif.
  endloop.

  read table it_ksml index 1.
  select a~atwrt
         b~atwtb
         into table it_cawn
         from cawn as a inner join cawnt as b
                        on  a~atinn eq b~atinn
                        and a~atzhl eq b~atzhl
         where a~atinn eq it_ksml-omerk.
  sort it_cawn.
  it_cawn-atwrt = 'No entry'.
  insert it_cawn index 1.
  clear: it_cawn.
  loop at it_cawn.
    valuetab-value = it_cawn-atwrt.
    append valuetab. clear valuetab.
    valuetab-value = it_cawn-atwtb.
    append valuetab. clear valuetab.
  endloop.

  perform add_fields using: 'CAWN' 'ATWRT'  'X',
                            'CAWNT' 'ATWTB' ' '.

  perform help_values_get.

  if select_index > 0.
    read table it_cawn   index select_index.
    perform value_update using:
            'X'   'P_ATWRI' it_cawn-atwrt 0.
  endif.
endform.                    " help_request_p_atwri
*&---------------------------------------------------------------------*
*&      Form  value_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0297   text
*----------------------------------------------------------------------*
form value_read using  p_name.
  dynpread-fieldname = p_name.
  append dynpread.

  call function 'DYNP_VALUES_READ'
       exporting
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       tables
            dynpfields           = dynpread
       exceptions
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            invalid_parameter    = 7
            undefind_error       = 8
            double_conversion    = 9
            others               = 10.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " value_read
*&---------------------------------------------------------------------*
*&      Form  value_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0479   text
*      -->P_0480   text
*      -->P_IT_CAWN_ATWRT  text
*      -->P_0      text
*----------------------------------------------------------------------*
form value_update using  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
  clear dynpfields.
  dynpfields-fieldname  = p_fieldname.
  dynpfields-fieldvalue = p_fieldvalue.
  if p_stepl > 0.
    dynpfields-stepl = p_stepl.
  endif.
  append dynpfields.      clear dynpfields.

  if p_process eq 'X'.
    call function 'DYNP_VALUES_UPDATE'
         exporting
              dyname               = sy-cprog
              dynumb               = sy-dynnr
         tables
              dynpfields           = dynpfields
         exceptions
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              others               = 8.
    refresh dynpfields.
  endif.
endform.                    " value_update
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_input_value.
  perform check_werks.
  perform check_stlan.
  perform check_matnr.
  perform check_bfsc.
  perform check_lifnr.
  perform check_color.
endform.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  get_bom_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bom_data.
  data: lw_odseq like zspp_zrpp301r-odseq.
  data: lw_index like sy-tabix,
        lw_knnam like cukb-knnam.

*  PERFORM set_target_material.
*  PERFORM set_bom_header.

  data: lt_stb type  stpox occurs 0 with header line.
  data: lw_topmat like cstmat,
        lw_stlan like stpox-stlan,
        lw_subrc like sy-subrc.

  call function 'CS_BOM_EXPL_MAT_V2'
       exporting
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            cuobj                 = c_cuobj
            datuv                 = p_datuv
            mktls                 = 'X'
            mtnrv                 = p_matnr
            stpst                 = 0
            stlan                 = p_stlan
            stlal                 = p_stlal
            svwvo                 = 'X'
            werks                 = p_werks
            vrsvo                 = 'X'
       importing
            topmat                = lw_topmat
       tables
            stb                   = lt_stb
       exceptions
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            others                = 9.
  if sy-subrc <> 0 or  lw_topmat-stlal ne p_stlal.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  loop at lt_stb.
*    check lt_stb-idnrk = 'AU51'.
    clear: it_itab, lw_knnam.

    read table it_itab with key matnr = lt_stb-idnrk
                                werks = lt_stb-werks.
    if sy-subrc eq 0.
      move: sy-tabix to lw_index.

      perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            lw_knnam.
      if lw_knnam eq space and lt_stb-knobj > 0.
        continue.
      endif.

      it_itab-tot_q = it_itab-tot_q + lt_stb-menge.
      modify it_itab index lw_index.
      continue.
    else.
      read table it_header with key matnr = lt_stb-idnrk
                                    werks = lt_stb-werks
                                    stlan = p_stlan
                           binary search.
      if sy-subrc ne 0.
        check lt_stb-mtart eq c_mtart or lt_stb-mtart eq c_mtart1.

        perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk lt_stb-werks
                                              lw_knnam.
        if lw_knnam eq space and lt_stb-knobj > 0.
          continue.
        endif.

        move: lt_stb-idnrk to it_itab-matnr,
              p_werks      to it_itab-werks,
              lt_stb-menge to it_itab-tot_q,
              lt_stb-ojtxp to it_itab-maktx.

        append it_itab.
      else.
        case lt_stb-zinfo.
          when 'ENG'.
            perform bom_explosion using c_werks lt_stb-idnrk
                                        lt_stb-menge.
          when others.
            perform bom_explosion using p_werks lt_stb-idnrk
                                        lt_stb-menge.
        endcase.

      endif.
    endif.
  endloop.
endform.                    " get_bom_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
  sort it_itab by matnr.

  if p_disp = 'X'.
    perform display_out.
  else.
    perform write_data.
  endif.
endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  check_werks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_werks.
  select single * from t001w where werks = p_werks.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.
endform.                    " check_werks
*&---------------------------------------------------------------------*
*&      Form  check_stlan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_stlan.
  select single * from t416 where stlan = p_stlan.
  if sy-subrc ne 0.
    message e000(zz) with text-m03.
  endif.
endform.                    " check_stlan
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_matnr.
  select single * from mara where matnr eq p_matnr.
  if sy-subrc ne 0.
    message e000(zz) with text-m04.
  endif.

  if mara-mtart ne 'FERT'.
    message e000(zz) with text-m10.
  endif.

  select single * from makt where matnr = p_matnr
                              and spras = sy-langu.
endform.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_WERKS  text
*      -->P_LT_STB_IDNRK  text
*----------------------------------------------------------------------*
form bom_explosion using pw_werks pw_matnr pw_menge.
  data: lt_stb type  stpox occurs 0 with header line.
  data: lw_topmat like cstmat,
        lw_index  like sy-tabix,
        lw_subrc  like sy-subrc,
        lw_knnam  like cukb-knnam,
        lw_menge  like stpo-menge.

  call function 'CS_BOM_EXPL_MAT_V2'
       exporting
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = p_datuv
            mktls                 = 'X'
            cuobj                 = c_cuobj
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = p_stlan
            stlal                 = c_stlal
            svwvo                 = 'X'
            werks                 = pw_werks
            vrsvo                 = 'X'
       importing
            topmat                = lw_topmat
       tables
            stb                   = lt_stb
       exceptions
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            others                = 9.
  if sy-subrc <> 0 or  lw_topmat-stlal ne c_stlal.
    exit.
  endif.

  loop at lt_stb.
    clear: it_itab, lw_knnam.

    read table it_itab with key matnr   = lt_stb-idnrk
                                werks   = pw_werks.
    if sy-subrc eq 0.
      move: sy-tabix to lw_index.
      perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            lw_knnam.
      if lw_knnam eq space and lt_stb-knobj > 0.
        continue.
      endif.

      it_itab-tot_q = it_itab-tot_q + lt_stb-menge * pw_menge.
      modify it_itab index lw_index.
      continue.
    else.
      lw_menge = pw_menge * lt_stb-menge.

      read table it_header with key matnr = lt_stb-idnrk
                                    werks = pw_werks
                                    stlan = p_stlan
                           binary search.
      if sy-subrc ne 0.
        check lt_stb-mtart eq c_mtart or lt_stb-mtart eq c_mtart1.


        perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk pw_werks
                                              lw_knnam.
        if lw_knnam eq space and lt_stb-knobj > 0.
          continue.
        endif.
        move: lt_stb-idnrk to it_itab-matnr,
              pw_werks     to it_itab-werks,
              lt_stb-ojtxp to it_itab-maktx,
              lw_menge     to it_itab-tot_q.

        append it_itab.
      else.
        case lt_stb-zinfo.
          when 'ENG'.
            perform bom_explosion using pw_werks lt_stb-idnrk lw_menge.
          when others.
            perform bom_explosion using pw_werks lt_stb-idnrk lw_menge.
        endcase.
      endif.
    endif.
  endloop.
endform.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_KNOBJ  text
*----------------------------------------------------------------------*
form check_object_dependency using pw_knobj pw_mtart
                                   pw_matnr pw_werks
                                   pw_knnam.
  check pw_knobj > 0.

  case pw_mtart.
    when 'ROH'.
      select single * from marc where matnr = pw_matnr
                                  and werks = pw_werks.
      if sy-subrc ne 0.
        message e000(zz) with text-m01.
      endif.

      if marc-dispo eq 'M01'.         "Module Part
        move: p_atwri to pw_knnam.
        perform compare_dependency using c_module pw_knnam pw_knobj.
      else.
        perform get_dependency using pw_knnam pw_werks.
        perform compare_dependency using c_roh pw_knnam pw_knobj.
      endif.
    when 'ROH1'.
      move: p_atwre to pw_knnam.
      perform compare_dependency using c_subpart pw_knnam pw_knobj.
  endcase.
endform.                    " CHECK_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  COMPARE_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_MODULE  text
*      -->P_LW_KNNAM  text
*----------------------------------------------------------------------*
form compare_dependency using pw_type pw_knnam pw_knobj.
  data: l_knnam like cukb-knnam.
  data: lt_cuob like cuob occurs 0 with header line.

  select * into table lt_cuob
    from cuob
   where kntab =  'STPO'
     and knobj =  pw_knobj
     and datuv <= p_datuv.
  if sy-subrc ne 0.
    move: '9999999999' to lt_cuob-knnum.
    append lt_cuob.
  endif.

  case pw_type.
    when c_roh.
      write: p_atwre  to l_knnam(3),
             pw_knnam to l_knnam+3.
      select knnam into pw_knnam
        from cukb
         for all entries in lt_cuob
       where knnum =  lt_cuob-knnum
         and adzhl =  lt_cuob-adzhl
         and knnam =  l_knnam
         and datuv <= p_datuv.
      endselect.
      if sy-subrc ne 0.
        write: p_atwri  to l_knnam(3),
               pw_knnam to l_knnam+3.
        select knnam into pw_knnam
          from cukb
           for all entries in lt_cuob
         where knnum =  lt_cuob-knnum
           and adzhl =  lt_cuob-adzhl
           and knnam =  l_knnam
           and datuv <= p_datuv.
        endselect.
        if sy-subrc ne 0.
          clear: pw_knnam.
        endif.
      endif.
    when c_module or c_subpart.
      move: pw_knnam to l_knnam.

      select knnam into pw_knnam
        from cukb
         for all entries in lt_cuob
       where knnum =  lt_cuob-knnum
         and adzhl =  lt_cuob-adzhl
         and knnam =  l_knnam
         and datuv <= p_datuv.
      endselect.
      if sy-subrc ne 0.
        clear: pw_knnam.
      endif.
  endcase.
endform.                    " COMPARE_dependency
*&---------------------------------------------------------------------*
*&      Form  get_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_KNNAM  text
*----------------------------------------------------------------------*
form get_dependency using pw_knnam pw_werks.
  data: l_in_recno like ibin-in_recno.

  data: begin of lt_cabn occurs 7,
          atwrt type v_ibin_syval-atwrt,
          atnam type cabn-atnam,
        end   of lt_cabn.

  select single * from marc where matnr = p_matnr
                              and werks = p_werks.
  if sy-subrc ne 0.
    clear: pw_knnam. exit.
  endif.

  select single in_recno into l_in_recno
                         from ibin
                        where instance eq marc-cuobj.
  if sy-subrc ne 0.
    clear: pw_knnam. exit.
  endif.

  select a~atwrt b~atnam
    into table lt_cabn
    from v_ibin_syval as a inner join cabn as b
                              on a~atinn eq b~atinn
   where a~in_recno eq l_in_recno.

  sort lt_cabn by atnam.

  loop at lt_cabn where atnam ne 'COLOREXT'
                    and atnam ne 'COLORINT'
                    and atnam ne 'COLOR_MI'.
    if lt_cabn-atwrt eq '-'.
      clear: lt_cabn-atwrt.
    endif.

    concatenate pw_knnam lt_cabn-atwrt into pw_knnam.
  endloop.

  write: pw_knnam to pw_knnam+10,
         space    to pw_knnam(10).

  read table lt_cabn with key atnam = 'COLOR_MI'.
  if sy-subrc eq 0.
    write: lt_cabn-atwrt to pw_knnam(10).
  endif.

*  SHIFT pw_knnam.
endform.                    " get_dependency
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data.
  perform get_bom_header.
  perform get_bom_data.
  perform set_info_record.
* Requested by Mr.Kim changed by chris
* add shop cost data into this report.
  perform read_shopcost.
  perform add_shopcost_data.
* read the material type
  perform read_mtart.
* end of change on 06/20/2005


endform.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  set_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_info_record.
  data: lw_subrc.
  tables: zmmr31.
  data: i_zmmr31 like zmmr31 occurs 0 with header line.
  select * into table i_zmmr31 from zmmr31.
  loop at it_itab.
    clear: lw_subrc.

* check mapping table
    read table i_zmmr31 with key matnr = it_itab-matnr.
    if sy-subrc = 0.
      it_itab-matnr = i_zmmr31-matn2.
    endif.

* get info record
    perform get_info_record using lw_subrc.

*fixme description
    perform get_vendor_desc using it_itab-lifnr.
    it_itab-name1 = lfa1-name1.

    if lw_subrc eq space.
      modify it_itab.
    endif.
  endloop.
endform.                    " set_info_record
*&---------------------------------------------------------------------*
*&      Form  get_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_info_record using pw_subrc.
*---// If S/A does not exist and the material has 2 more info record,
*---// select info record that has chipper price .
*---// MM guy said
*---// "All of material's purchaning UoM is same as BOM's UoM,
*---//  and all of currency is the same"
*---// So below logic does not include any conversion logic.
  data: lw_lifnr like lfa1-lifnr.

  clear: it_lifnr, it_lifnr[].

  select lifnr b~effpr b~peinh
    into corresponding fields of it_lifnr
    from eina as a inner join eine as b
      on a~infnr = b~infnr
   where a~matnr =  it_itab-matnr
     and a~lifnr in r_lifnr
     and a~loekz =  ' '
     and b~werks =  ' '
     and b~ekorg =  c_ekorg
     and b~loekz =  ' '.

    select single *
      from a018
     where kappl =  'M'
       and kschl =  'PB00'
       and matnr =  it_itab-matnr
       and lifnr =  it_lifnr-lifnr
       and ekorg =  c_ekorg
       and esokz =  '0'
       and datab <= p_datuv
       and datbi >= p_datuv.
    if sy-subrc eq 0.
      move: a018-knumh to it_lifnr-knumh.

      append it_lifnr.
    endif.
  endselect.
  if sy-subrc ne 0.
    move: 'X' to pw_subrc.
    exit.
  endif.

*---// Select one Vendor from S/A
  read table it_lifnr index 2.
  if sy-subrc eq 0.
    perform select_vendor tables it_lifnr
                          using  lw_lifnr.
    perform select_vendor_from_sa tables it_lifnr
                                  using  lw_lifnr.
    if lw_lifnr ne space.
      delete it_lifnr where lifnr ne lw_lifnr.
    endif.
  endif.

  loop at it_lifnr.
    select single kbetr kpein
      into corresponding fields of it_lifnr
      from zvmm_info_condi
     where knumh = it_lifnr-knumh
       and kschl = c_kschl
       and loevm_ko = ' '.
    if sy-subrc ne 0.
      delete it_lifnr.
      continue.
    endif.

    it_lifnr-price = it_lifnr-kbetr / it_lifnr-kpein.

    modify it_lifnr.
  endloop.

  sort it_lifnr by price.
  read table it_lifnr index 1.
  delete it_lifnr where lifnr ne it_lifnr-lifnr.
  move: it_lifnr-lifnr to it_itab-lifnr.

* eff.
  it_itab-eff_p = it_lifnr-effpr / it_lifnr-peinh.
  it_itab-eff_a = ( it_lifnr-effpr / it_lifnr-peinh ) * it_itab-tot_q.

*---// Read price and conditions
  clear: it_condition, it_condition[].

  select kbetr kpein kzust kschl
    into corresponding fields of table it_condition
    from zvmm_info_condi
   where knumh = it_lifnr-knumh
     and ( kschl =    c_kschl or
           kschl like 'ZP%' )
     and loevm_ko = ' '.

  data: lw_index(2) type n,
        lw_price(50),
        lw_amount(50).
  field-symbols: <price>, <amount>.

  loop at it_condition.
    case it_condition-kschl.
* net price
      when c_kschl.
        move: it_condition-kbetr to it_itab-tot_u,
              it_condition-kpein to it_itab-tot_p.
        it_itab-tot_a = it_condition-kbetr / it_condition-kpein *
                        it_itab-tot_q.
      when others.
        move: it_condition-kschl+2(2) to lw_index.

        concatenate: 'IT_ITAB-ZP' lw_index 'A' into lw_price,
                     'IT_ITAB-ZP' lw_index 'B' into lw_amount.

        assign: (lw_price)  to <price>,
                (lw_amount) to <amount>.
        if sy-subrc ne 0. continue. endif.

        if it_itab-tot_p eq 0.
          <price> = 0. <amount> = 0.
        else.
          <price>  = it_condition-kbetr / it_itab-tot_p.
          <amount> = it_condition-kbetr / it_itab-tot_p *
                     it_itab-tot_q.
        endif.
    endcase.
  endloop.

  data: lw_menge like mseg-menge.

  perform get_gr_qty using lw_menge.
  perform get_depreciation_qty using 'ZP01'        it_itab-zp01c
                                     it_itab-zp01d lw_menge.
  perform get_depreciation_qty using 'ZP02'        it_itab-zp02c
                                     it_itab-zp02d lw_menge.
  perform get_depreciation_qty using 'ZP03'        it_itab-zp03c
                                     it_itab-zp03d lw_menge.
endform.                    " get_info_record
*&---------------------------------------------------------------------*
*&      Form  select_vendor_from_sa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LIFNR  text
*----------------------------------------------------------------------*
form select_vendor_from_sa tables pt_lifnr structure it_lifnr
                           using  pw_lifnr.

  data: begin of lt_sa_price occurs 0,
          lifnr   like   lfa1-lifnr,
          netpr   like   ekpo-netpr,
          peinh   like   ekpo-peinh,
          price   type   f,
        end   of lt_sa_price.

  select lifnr netpr peinh
    into corresponding fields of table lt_sa_price
    from ekko as a inner join ekpo as b
                      on a~mandt eq b~mandt
                     and a~ebeln eq b~ebeln
     for all entries in pt_lifnr
   where matnr   eq it_itab-matnr
     and lifnr   eq pt_lifnr-lifnr
     and a~bstyp eq 'L'
     and werks   eq p_werks
     and a~loekz eq space
     and a~autlf eq space
     and b~loekz eq space
     and b~elikz eq space
     and kdatb   <= p_datuv
     and kdate   >= p_datuv.
  if sy-subrc eq 0.
    loop at lt_sa_price.
      lt_sa_price-price = lt_sa_price-netpr / lt_sa_price-peinh.

      modify lt_sa_price.
    endloop.
  endif.

  sort lt_sa_price by price.

  read table lt_sa_price index 1.

  move: lt_sa_price-lifnr to pw_lifnr.
endform.                    " select_vendor_from_sa
*&---------------------------------------------------------------------*
*&      Form  write_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_top.
*---
  write : / text-101,
          / text-102.

  skip.

  perform get_vendor_desc using p_lifnr.

  write:/     text-132, p_datuv,
          100 text-131, p_werks.
  write:/     text-119, p_lifnr, lfa1-name1,
          100 text-133, p_stlan.

  write:/     text-130, (18) p_matnr, makt-maktx,
          100 text-134, p_stlal.

  write:/     text-135, (03) p_atwre,
              text-136, (03) p_atwri.

  skip.

  format color col_heading.     " INTENSIFIED OFF.

  uline.

  set left scroll-boundary column 63.

  perform write_first_line_01.
  perform write_second_line_01.
  perform write_third_line_01.

  uline.
endform.                    " write_top
*&---------------------------------------------------------------------*
*&      Form  write_first_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_first_line_01.
  write : /     '|' no-gap, (18) text-s18 no-gap centered,
                '|' no-gap, (30) text-s30 no-gap centered,
                '|' no-gap, (10) text-s10 no-gap centered,

                '|' no-gap, (8) text-s35 no-gap centered,
                '|' no-gap, (10) text-s35 no-gap centered,
                '|' no-gap, (38) text-204 no-gap centered,

                '|' no-gap, (38) text-103 no-gap centered,
                '|' no-gap, (38) text-205 no-gap centered,
                '|' no-gap, (43) text-104 no-gap centered,
                '|' no-gap, (43) text-105 no-gap centered,
                '|' no-gap, (43) text-106 no-gap centered,
                '|' no-gap, (21) text-107 no-gap centered, " Raw Materia
                '|' no-gap, (21) text-108 no-gap centered, " Freight
                '|' no-gap, (21) text-109 no-gap centered, " CC Cost
                '|' no-gap, (21) text-110 no-gap centered, " Sequencing
                '|' no-gap, (21) text-111 no-gap centered, " Direct Labo
                '|' no-gap, (21) text-112 no-gap centered, " Machine
                '|' no-gap, (21) text-113 no-gap centered, " MBE Cost
                '|' no-gap, (21) text-114 no-gap centered, " WBE Cost
                '|' no-gap, (21) text-115 no-gap centered, " NAFTA Cost
                '|' no-gap, (21) text-116 no-gap centered, " AALA Cost
                '|' no-gap, (21) text-127 no-gap centered, " By Up
                '|' no-gap.
endform.                    " write_first_line_01
*&---------------------------------------------------------------------*
*&      Form  write_second_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_second_line_01.
*---
  write : /     '|' no-gap,  (18) text-117 no-gap centered,
                '|' no-gap,  (30) text-118 no-gap centered,
                '|' no-gap,  (10) text-129 no-gap centered,
                '|' no-gap,  (8)  text-201 no-gap centered,
                '|' no-gap,  (10) text-202 no-gap centered,
                '|' no-gap, (488) sy-uline no-gap,
                '|' no-gap.
endform.                    " write_second_line_01
*&---------------------------------------------------------------------*
*&      Form  write_third_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_third_line_01.
*---
  write : /     '|' no-gap, (18) text-s18 no-gap centered,
                '|' no-gap, (30) text-s30 no-gap centered,
                '|' no-gap, (10) text-s10 no-gap centered,
                '|' no-gap, (8)  text-s35 no-gap centered,
                '|' no-gap, (10) text-203 no-gap centered,

                '|' no-gap, (10) text-121 no-gap centered, " unit price
                '|' no-gap, (05) text-128 no-gap centered, " unit price
                '|' no-gap, (10) text-122 no-gap centered, " quantity
                '|' no-gap, (10) text-123 no-gap centered, " amount


                '|' no-gap, (10) text-121 no-gap centered, " unit price
                '|' no-gap, (05) text-128 no-gap centered, " unit price
                '|' no-gap, (10) text-122 no-gap centered, " quantity
                '|' no-gap, (10) text-123 no-gap centered, " amount

                '|' no-gap, (10) text-121 no-gap centered, " unit price
                '|' no-gap, (05) text-128 no-gap centered, " unit price
                '|' no-gap, (10) text-122 no-gap centered, " quantity
                '|' no-gap, (10) text-123 no-gap centered, " amount


                '|' no-gap, (10) text-124 no-gap centered, " price
                '|' no-gap, (10) text-123 no-gap centered, " amount
                '|' no-gap, (10) text-125 no-gap centered, " Deprc. Qty
                '|' no-gap, (10) text-126 no-gap centered, " Amort. Qty

                '|' no-gap, (10) text-124 no-gap centered,
                '|' no-gap, (10) text-123 no-gap centered,
                '|' no-gap, (10) text-125 no-gap centered,
                '|' no-gap, (10) text-126 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered,
                '|' no-gap, (10) text-123 no-gap centered,
                '|' no-gap, (10) text-125 no-gap centered,
                '|' no-gap, (10) text-126 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " Raw Materia
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " Freight
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " CC Cost
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " Sequencing
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " Direct Labo
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " Machine
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " MBE Cost
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " WBE Cost
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " NAFTA Cost
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " AALA Cost
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap, (10) text-124 no-gap centered, " By Up
                '|' no-gap, (10) text-123 no-gap centered,

                '|' no-gap.
endform.                    " write_third_line_01
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_data.
*---
  data : l_int(1).

  loop at it_itab.
    perform change_color changing l_int.
    write : /
        '|' no-gap,  (18) it_itab-matnr no-gap color col_heading,
        '|' no-gap,  (30) it_itab-maktx no-gap color col_heading,
        '|' no-gap,  (10) it_itab-lifnr no-gap,
        '|' no-gap,  (08) it_itab-sourc no-gap,
        '|' no-gap,  (10) it_itab-mtart no-gap,

*--- Annual Business Plan amount per vehicle
        '|' no-gap,  (10) it_itab-cos_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-cos_p no-gap,
        '|' no-gap,  (10) it_itab-cos_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-cos_a currency it_itab-waers no-gap,
*--- total purchasing amount per vehicle
        '|' no-gap,  (10) it_itab-tot_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-tot_p no-gap,
        '|' no-gap,  (10) it_itab-tot_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-tot_a currency it_itab-waers no-gap,
*--- Difference  per vehicle
        '|' no-gap,  (10) it_itab-dif_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-dif_p no-gap,
        '|' no-gap,  (10) it_itab-dif_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-dif_a currency it_itab-waers no-gap,
*--- packaging amortization cost - zp01
        '|' no-gap,  (10) it_itab-zp01a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp01b currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp01c unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-zp01d unit it_itab-meins no-gap,
*--- tooling amortization cost - zp02
        '|' no-gap,  (10) it_itab-zp02a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp02b currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp02c unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-zp02d unit it_itab-meins no-gap,
*--- development amortizatino cost - zp03
        '|' no-gap,  (10) it_itab-zp03a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp03b currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp03c unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-zp03d unit it_itab-meins no-gap,
*--- raw materials cost - zp04
        '|' no-gap,  (10) it_itab-zp04a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp04b currency it_itab-waers no-gap,
*--- freight cost - zp05
        '|' no-gap,  (10) it_itab-zp05a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp05b currency it_itab-waers no-gap,
*--- CC cost - zp06
        '|' no-gap,  (10) it_itab-zp06a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp06b currency it_itab-waers no-gap,
*--- sequencing cost - zp07
        '|' no-gap,  (10) it_itab-zp07a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp07b currency it_itab-waers no-gap,
*--- direct labor cost - zp08
        '|' no-gap,  (10) it_itab-zp08a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp08b currency it_itab-waers no-gap,
*--- machine cost - zp09
        '|' no-gap,  (10) it_itab-zp09a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp09b currency it_itab-waers no-gap,
*--- MBE cost - zp10
        '|' no-gap,  (10) it_itab-zp10a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp10b currency it_itab-waers no-gap,
*--- WBE cost - zp11
        '|' no-gap,  (10) it_itab-zp11a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp11b currency it_itab-waers no-gap,
*--- NAFTA cost - zp12
        '|' no-gap,  (10) it_itab-zp12a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp12b currency it_itab-waers no-gap,
*--- AALA cost - zp13
        '|' no-gap,  (10) it_itab-zp13a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp13b currency it_itab-waers no-gap,
*--- By Up Packaging
        '|' no-gap,  (10) it_itab-zp14a currency it_itab-waers no-gap,
        '|' no-gap,  (10) it_itab-zp14b currency it_itab-waers no-gap,
        '|' no-gap.

    at last.
      sum.
*      clear unit price, price total value
      clear: it_itab-cos_u, it_itab-cos_p.
      clear: it_itab-tot_u, it_itab-tot_p.
      clear: it_itab-dif_u, it_itab-dif_p.

      uline.

      perform display_sum.

      uline.
    endat.
  endloop.
endform.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  change_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_INT  text
*----------------------------------------------------------------------*
form change_color changing p_l_int.
*---
  if p_l_int eq space.
    format color 2 intensified off.
    move : 'X' to p_l_int.
  else.
    format color 2 intensified on.
    clear : p_l_int.
  endif.
endform.                    " change_color
*&---------------------------------------------------------------------*
*&      Form  GET_BOM_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bom_header.
  select a~matnr b~werks c~stlan
    into corresponding fields of table it_header
    from mara as a inner join marc as b
                      on a~matnr eq b~matnr
                     and b~lvorm eq space
                   inner join mast as c
                      on b~matnr eq c~matnr
                     and b~werks eq c~werks
                     and c~stlal eq c_stlal
                   inner join stko as d
                      on c~stlnr eq d~stlnr
                     and c~stlal eq d~stlal
                     and d~stlty eq 'M'
                     and d~lkenz eq space
                     and d~loekz eq space
                     and d~stlst eq '01'
                     and d~datuv <= p_datuv
   where a~mtart ne 'ROH' and a~mtart ne c_mtart1.

  sort it_header by matnr werks stlan.
endform.                    " GET_BOM_HEADER
*&---------------------------------------------------------------------*
*&      Form  GET_GR_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_gr_qty using pw_menge.
  select sum( lbkum )
    into pw_menge
    from ckmlhd as a inner join mlcd as b
                      on a~kalnr = b~kalnr
                     and b~categ = 'ZU'
                     and b~ptyp  = 'BB'
   where a~matnr = it_itab-matnr
     and a~bwkey = it_itab-werks.
endform.                    " GET_GR_QTY
*&---------------------------------------------------------------------*
*&      Form  get_depreciation_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2665   text
*      -->P_LW_MENGE  text
*----------------------------------------------------------------------*
form get_depreciation_qty using pw_kschl pw_qty_c pw_qty_d pw_menge.
  data: lw_mod like mseg-menge.

  select single menge
    into pw_qty_c
    from ztmm_cond
   where matnr = it_itab-matnr
     and lifnr = it_itab-lifnr
     and ekorg = c_ekorg
     and kschl = pw_kschl.

  if pw_qty_c eq 0.
    move: 0 to lw_mod.
  else.
    lw_mod = pw_menge mod pw_qty_c.
  endif.

  pw_qty_d = pw_qty_c - lw_mod.
endform.                    " get_depreciation_qty
*&---------------------------------------------------------------------*
*&      Form  check_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_lifnr.
  check p_lifnr ne space.

  move: 'I'     to r_lifnr-sign,
        'EQ'    to r_lifnr-option,
        p_lifnr to r_lifnr-low.

  append r_lifnr.
endform.                    " check_lifnr
*&---------------------------------------------------------------------*
*&      Form  display_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_sum.
  format color col_total intensified on.

  write : /
      '|' no-gap,  (80) 'Total'       no-gap,

*--- total Annual Business Plan amount per vehicle
        '|' no-gap,  (10) it_itab-cos_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-cos_p no-gap,
        '|' no-gap,  (10) it_itab-cos_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-cos_a currency it_itab-waers no-gap,
*--- total purchasing amount per vehicle
        '|' no-gap,  (10) it_itab-tot_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-tot_p no-gap,
        '|' no-gap,  (10) it_itab-tot_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-tot_a currency it_itab-waers no-gap,
*--- total Difference  per vehicle
        '|' no-gap,  (10) it_itab-dif_u currency it_itab-waers no-gap,
        '|' no-gap,  (05) it_itab-dif_p no-gap,
        '|' no-gap,  (10) it_itab-dif_q unit it_itab-meins no-gap,
        '|' no-gap,  (10) it_itab-dif_a currency it_itab-waers no-gap,
*--- packaging amortization cost - zp01
      '|' no-gap,  (10) it_itab-zp01a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp01b currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp01c unit it_itab-meins no-gap,
      '|' no-gap,  (10) it_itab-zp01d unit it_itab-meins no-gap,
*--- tooling amortization cost - zp02
      '|' no-gap,  (10) it_itab-zp02a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp02b currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp02c unit it_itab-meins no-gap,
      '|' no-gap,  (10) it_itab-zp02d unit it_itab-meins no-gap,
*--- development amortizatino cost - zp03
      '|' no-gap,  (10) it_itab-zp03a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp03b currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp03c unit it_itab-meins no-gap,
      '|' no-gap,  (10) it_itab-zp03d unit it_itab-meins no-gap,
*--- raw materials cost - zp04
      '|' no-gap,  (10) it_itab-zp04a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp04b currency it_itab-waers no-gap,
*--- freight cost - zp05
      '|' no-gap,  (10) it_itab-zp05a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp05b currency it_itab-waers no-gap,
*--- CC cost - zp06
      '|' no-gap,  (10) it_itab-zp06a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp06b currency it_itab-waers no-gap,
*--- sequencing cost - zp07
      '|' no-gap,  (10) it_itab-zp07a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp07b currency it_itab-waers no-gap,
*--- direct labor cost - zp08
      '|' no-gap,  (10) it_itab-zp08a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp08b currency it_itab-waers no-gap,
*--- machine cost - zp09
      '|' no-gap,  (10) it_itab-zp09a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp09b currency it_itab-waers no-gap,
*--- MBE cost - zp10
      '|' no-gap,  (10) it_itab-zp10a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp10b currency it_itab-waers no-gap,
*--- WBE cost - zp11
      '|' no-gap,  (10) it_itab-zp11a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp11b currency it_itab-waers no-gap,
*--- NAFTA cost - zp12
      '|' no-gap,  (10) it_itab-zp12a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp12b currency it_itab-waers no-gap,
*--- AALA cost - zp13
      '|' no-gap,  (10) it_itab-zp13a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp13b currency it_itab-waers no-gap,
*--- By Up Packaging
      '|' no-gap,  (10) it_itab-zp14a currency it_itab-waers no-gap,
      '|' no-gap,  (10) it_itab-zp14b currency it_itab-waers no-gap,
      '|' no-gap.
endform.                    " display_sum
*&---------------------------------------------------------------------*
*&      Form  check_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_color.
  translate: p_atwre to upper case,
             p_atwri to upper case.
endform.                    " check_color
*&---------------------------------------------------------------------*
*&      Form  select_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*      -->P_LW_LIFNR  text
*----------------------------------------------------------------------*
form select_vendor tables pt_lifnr structure it_lifnr
                   using  pw_lifnr.
  perform select_vendor_from_sa tables pt_lifnr
                                using  pw_lifnr.

  check pw_lifnr eq space.

  perform select_vendor_from_po tables pt_lifnr
                                using  pw_lifnr.
endform.                    " select_vendor
*&---------------------------------------------------------------------*
*&      Form  select_vendor_from_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_LIFNR  text
*      -->P_PW_LIFNR  text
*----------------------------------------------------------------------*
form select_vendor_from_po tables pt_lifnr structure it_lifnr
                           using  pw_lifnr.

  data: begin of lt_sa_price occurs 0,
          lifnr   like   lfa1-lifnr,
          netpr   like   ekpo-netpr,
          peinh   like   ekpo-peinh,
          price   type   f,
        end   of lt_sa_price.

  select lifnr netpr peinh
    into corresponding fields of table lt_sa_price
    from ekko as a inner join ekpo as b
                      on a~mandt eq b~mandt
                     and a~ebeln eq b~ebeln
     for all entries in pt_lifnr
   where matnr   eq it_itab-matnr
     and lifnr   eq pt_lifnr-lifnr
     and a~bstyp eq 'F'
     and werks   eq p_werks
     and a~loekz eq space
     and a~autlf eq space
     and b~loekz eq space
     and b~elikz eq space.
  if sy-subrc eq 0.
    loop at lt_sa_price.
      lt_sa_price-price = lt_sa_price-netpr / lt_sa_price-peinh.

      modify lt_sa_price.
    endloop.
  endif.

  sort lt_sa_price by price.

  read table lt_sa_price index 1.

  move: lt_sa_price-lifnr to pw_lifnr.
endform.                    " select_vendor_from_po
*&---------------------------------------------------------------------*
*&      Form  read_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_shopcost.
  data:  lt_shopcost like it_shopcost occurs 0 with header line.
  data:  l_kstar(10) type c value '%000054%'.


* Read first level components of FSC
  select * into table lt_shopcost
    from ztco_shopcost
    where fsc_matnr = p_bfsc       and
          bdatj     = p_year       and
          poper     = p_perid      and
          kstar  like l_kstar      and
          klvar     = c_klvar      and
          kokrs     = c_kokrs      and
          typps     = 'M'.
  if sy-subrc ne 0.
    exit.      "NO PLAN EXIST
  endif.


* Check the raw materials: Save raw materials and
* get the halb materials with cost component value '070'
* for next level reading
  refresh it_shopcost.
  loop at lt_shopcost.

    if lt_shopcost-kstar ne c_mip_cos_ele .
      append lt_shopcost to it_shopcost.
      delete lt_shopcost.
    elseif lt_shopcost-elemt ne c_mip_cos_com.
      delete lt_shopcost.
    endif.

  endloop.

* READ ALL COMPONENTS OF THIS FSC
  refresh it_all_comp.
  perform read_all_components tables lt_shopcost.
  sort it_all_comp by fsc_matnr llv_matnr.

  loop at it_all_comp.
    if it_all_comp-kstar = c_mip_cos_ele  and
       it_all_comp-elemt ne c_mip_cos_com.
      delete it_all_comp.
    endif.

  endloop.
  delete adjacent duplicates from it_all_comp
     comparing fsc_matnr llv_matnr.


* Search for the raw materials of the MIP parts
  loop at lt_shopcost.

    perform search_mip_raw using lt_shopcost.

  endloop.

  free it_all_comp.

* Summarize the raw materials' qty and value
  refresh lt_shopcost.  clear lt_shopcost.
  loop at it_shopcost.

    lt_shopcost-llv_matnr  = it_shopcost-llv_matnr.
    lt_shopcost-wertn      = it_shopcost-wertn.
    lt_shopcost-menge      = it_shopcost-menge.
    collect lt_shopcost.

  endloop.

  refresh it_shopcost.
  it_shopcost[]  = lt_shopcost[].
endform.                    " read_shopcost
*&---------------------------------------------------------------------*
*&      Form  SERACH_MIP_RAW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_mip_raw using pw_sc like it_shopcost.

  data: lt_mip   like it_shopcost occurs 0 with header line,
        l_lines  type i.


  loop at it_all_comp where fsc_matnr = pw_sc-llv_matnr.

    append it_all_comp to lt_mip.

  endloop.

*  SELECT * INTO TABLE LT_MIP
*   FROM ZTPP_SHOPCOST
*   WHERE FSC_MATNR = PW_SC-LLV_MATNR
*     AND BDATJ     = P_YEAR
*     AND POPER     = P_PERID
*     AND RECORD_TYPE = P_PLAN
*     AND KOKRS     = C_KOKRS
*     AND TYPPS     = 'M'.
  if sy-subrc ne 0.
    exit.      "NO MORE RAW MATERIALS
  endif.

* Check the raw materials
  loop at lt_mip.

*   Derive the qty and value
    lt_mip-wertn  = lt_mip-wertn * pw_sc-menge.
    lt_mip-menge  = lt_mip-menge * pw_sc-menge.

    if lt_mip-kstar  ne c_mip_cos_ele.

      append lt_mip to it_shopcost.
      delete lt_mip.

    else.

      modify lt_mip.

    endif.

  endloop.


  describe table lt_mip lines l_lines.
  if l_lines = 0.
    exit.       "NO MORE MIP MATERIALS
  endif.

* Search next level
  loop at lt_mip.

    perform search_mip_raw using lt_mip.

  endloop.

endform.                    " SERACH_MIP_RAW
*&---------------------------------------------------------------------*
*&      Form  READ_ALL_COMPONENTS
*&---------------------------------------------------------------------*
*     OUTPUT INCLUDES ALL RAW MATERIALS AND HALB MATERIALS WITH
*           COST COMPONENT VALUE '070'
*----------------------------------------------------------------------*
*      -->P_LT_SHOPCOST  text
*----------------------------------------------------------------------*
form read_all_components tables pt_shopcost structure ztco_shopcost.

  data: lt_sc  like it_shopcost occurs 0 with header line.

  select * into table lt_sc
     from ztco_shopcost
     for all entries in pt_shopcost
     where fsc_matnr     = pt_shopcost-llv_matnr
       and bdatj       = p_year
       and poper       = p_perid
       and klvar       = c_klvar
       and kokrs       = c_kokrs
       and typps       = 'M'.
  if sy-subrc ne 0.
    exit.   " NO MORE COMPONENTS
  endif.

  sort lt_sc by fsc_matnr llv_matnr.

*  Get all raw materials and HALB materials with component '070'
  loop at lt_sc.
    if lt_sc-kstar = c_mip_cos_ele   and
       lt_sc-elemt ne c_mip_cos_com.
      delete lt_sc.
    else.
      append lt_sc to it_all_comp.
    endif.
  endloop.


*  READ NEXT LEVEL COMPONENTS
  perform read_all_components tables lt_sc.

endform.          " READ_ALL_COMPONENTS
*&---------------------------------------------------------------------*
*&      Form  add_shopcost_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_shopcost_data.
  data: wa_itab like it_itab.
  data: l_tabix like sy-tabix.

  loop at it_shopcost.
    clear: it_itab.
    read table it_itab with key matnr = it_shopcost-llv_matnr.
    l_tabix = sy-tabix.
    if sy-subrc eq 0.

      it_itab-cos_u = it_shopcost-wertn / it_shopcost-menge.
      it_itab-cos_p = 1.
      it_itab-cos_q = it_shopcost-menge.
      it_itab-cos_a = it_shopcost-wertn.


      modify it_itab index l_tabix.

    else.

      wa_itab-matnr = it_shopcost-llv_matnr.
      wa_itab-cos_u = it_shopcost-wertn / it_shopcost-menge.
      wa_itab-cos_p = 1.
      wa_itab-cos_q = it_shopcost-menge.
      wa_itab-cos_a = it_shopcost-wertn.

*      get the material descriptiong
      select single maktx into wa_itab-maktx
        from makt
        where matnr  = wa_itab-matnr
          and spras  = 'EN'.
      append  wa_itab  to it_itab.
      clear: wa_itab.
    endif.

  endloop.

* calculate the difference
  loop at it_itab.
    if it_itab-tot_p ne 0.

      it_itab-dif_u = it_itab-tot_u / it_itab-tot_p
                      - it_itab-cos_u.
      it_itab-dif_p = 1.
      it_itab-dif_q = it_itab-tot_q - it_itab-cos_q.
      it_itab-dif_a = it_itab-tot_a - it_itab-cos_a.

    else.

      it_itab-dif_u = - it_itab-cos_u.
      it_itab-dif_p = 1.
      it_itab-dif_q = it_itab-tot_q - it_itab-cos_q.
      it_itab-dif_a = it_itab-tot_a - it_itab-cos_a.

    endif.

    modify it_itab .


  endloop.

endform.                    " add_shopcost_data
*&---------------------------------------------------------------------*
*&      Form  read_mtart
*&---------------------------------------------------------------------*
*       reading material type
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_mtart.
  data: begin of lt_mtart occurs 0,
          matnr  like mara-matnr,
          mtart  like mara-mtart,
          profl  like mara-profl,
        end of lt_mtart.
  select matnr mtart profl
    into corresponding fields of table lt_mtart
    from mara
    for all entries in it_itab
    where matnr  = it_itab-matnr.

  sort lt_mtart by matnr.

  loop at it_itab.
    read table lt_mtart with key matnr = it_itab-matnr
        binary search.
    it_itab-mtart  = lt_mtart-mtart.
    it_itab-sourc  = lt_mtart-profl.
*   derive the vendor if no vendor exist
    if it_itab-lifnr is initial.
      select single lifnr into it_itab-lifnr
        from a018
        where matnr = it_itab-matnr
          and datab le p_datuv
          and datbi ge p_datuv.
    endif.
    modify it_itab.
  endloop.
endform.                    " read_mtart
*&---------------------------------------------------------------------*
*&      Form  CHECK_BFSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* Check ABP FSC
*----------------------------------------------------------------------*
form check_bfsc.
  check p_bfsc <> space.

  clear: mara, ztco_shopcost.
  select single * from mara
   where matnr = p_bfsc.
  if mara-mtart ne 'FERT'.
    message e000 with 'Not a FSC material'.
  endif.
  select single * from ztco_shopcost
    where fsc_matnr  = p_bfsc      and
          bdatj     = p_year       and
          poper     = p_perid      and
          klvar     = c_klvar      and
          typps     = 'M'.
  if sy-subrc ne 0.
    message e000 with 'No business plan for this FSC'.
  endif.
endform.                    " CHECK_BFSC
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
form display_out.
* ==> build field category
  perform field_setting tables gt_fieldcat using :
 'SOURC' 'SOURC'        '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'MATNR' 'MATNR'        '18' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'LIFNR' 'LIFNR'        '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
 'MAKTX' 'MAKTX'        '25' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'WERKS' 'WERKS'        '04' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'NAME1' 'NAME1'        '25' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'MTART' 'MTART'        '04' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'MEINS' 'MEINS'        '05' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'WAERS' 'WAERS'        '05' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
 'COS_U' 'Plan_Unt'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'COS_P' 'Plan_Prc'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'COS_Q' 'Plan_Qty'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'COS_A' 'Plan_Amt'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'TOT_U' 'Purchase_Unt' '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'TOT_P' 'Purchase_Pri' '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'TOT_Q' 'Purchase_Qty' '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'TOT_A' 'Purchase_Amt' '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'EFF_P' 'Effect_Price' '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'EFF_A' 'Effect_Amt'   '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'DIF_U' 'Diff_Unt'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'DIF_P' 'Diff_Prc'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'DIF_Q' 'Diff_Qty'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'DIF_A' 'Diff_Amt'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP01A' 'Packg_Prc'    '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP01B' 'Packg_Amt'    '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP01C' 'PackgQ1'      '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP01D' 'PackgQ2'      '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP02A' 'TooL_P'       '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP02B' 'TooL_A'       '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP02C' 'TooLQ1'       '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP02D' 'TooLQ2'       '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP03A' 'Devlop_P'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP03B' 'Devlop_A'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP03C' 'DevlopQ1'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP03D' 'DevlopQ2'     '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP04A' 'RAW_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP04B' 'RAW_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP05A' 'FRT_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP05B' 'FRT_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP06A' 'CC_P '        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP06B' 'CC_A '        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP07A' 'SEQ_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP07B' 'SEQ_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP08A' 'MAN_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP08B' 'MAN_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP09A' 'MCH_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP09B' 'MCH_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP10A' 'MBE_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP10B' 'MBE_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP11A' 'WBE_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP11B' 'WBE_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP12A' 'NFT_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP12B' 'NFT_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP13A' 'AAL_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP13B' 'AAL_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP14A' 'BYU_P'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
 'ZP14B' 'BYU_A'        '15' ' ' 'R'  ' '  ' '  ' '  ' '  'X'.

  g_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
   exporting
     i_callback_program                = g_repid
*    i_callback_pf_status_set          = 'ALV_EVENT_PF_STATUS_SET'
*    i_callback_user_command           = 'ALV_EVENT_USER_COMMAND'
     it_fieldcat                       = gt_fieldcat
     i_save                            = 'A'
    tables
      t_outtab                         = it_itab.
endform.
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
