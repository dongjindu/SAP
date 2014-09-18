************************************************************************
* Program Name      : ZRMMPM31R_PRICE
* Author            : Jae-Sung, Lee
* Creation Date     : 2004.03.25.
* Specifications By : Jae-Sung, Lee
* Pattern           : Report 1-1
* Development Request No : UD1K908634
* Addl Documentation:
* Description       : Price Status per Vehicle by
*                     Condition
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.03.25.     Jae-Sung Lee     UD1K908634     Initial Coding
* 2005.06.25.     chris Li                        Add buniness plan cost

************************************************************************

report zrmmpm31r_price.
tables: t685t, marc, a018, mara, cabn,
        zsmm_mmpm31r_fsc.
data: zsmm_mmpm31r_9000 like zsmm_mmpm31r_9000.

*---// Internal tables
data: it_9000 type standard table of zsmm_mmpm31r_9000
                                     with header line.

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
      end   of it_lifnr.

data: begin of it_condition occurs 0,
        kbetr like zvmm_info_condi-kbetr,
        kpein like zvmm_info_condi-kpein,
        konwa like zvmm_info_condi-konwa,
        kschl like zvmm_info_condi-kschl,
      end   of it_condition.

data it_ksml   like table of ksml  with header line.
data : begin of it_cawn occurs 0,
        atwrt   like  cawn-atwrt,
        atwtb   like  cawnt-atwtb.
data : end of it_cawn.

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

*---// Work area
data: w_werks   like   marc-werks,
      w_kschl   like   konp-kschl,
      w_datuv   like   rc29n-datuv,
*      w_bfsc    like   mara-matnr,
      w_year    like   ztco_shopcost-bdatj,
      w_perid   like   ztco_shopcost-poper.

data: select_index like sy-tabix.
data: w_field(50).

*---// Contants
constants: c_check   type c           value 'x',
           c_mtart   like mara-mtart  value 'ROH',
           c_mtart1  like mara-mtart  value 'ROH1',
           c_capid   like rc29l-capid value 'PP01', "Application
           c_cuobj   like marc-cuobj  value '999999999999999999',
           c_werks   like marc-werks  value 'E001',
           c_ekorg   like ekko-ekorg  value 'PU01',
           c_kschl   like konp-kschl  value 'PB00',
           c_stlal   like mast-stlal  value '01',
           c_roh     type i           value 1,
           c_klvar   like ztco_shopcost-klvar value 'ZPCP',
           c_mip_cos_ele(10)        value '0000540300',
           c_mip_cos_com like ztco_shopcost-elemt value '070',
           c_kokrs   like tka01-kokrs value 'H201',
           c_module  type i           value 2,
           c_subpart type i           value 3,
           c_module_stlan like mast-stlan value '2'.

*---// For Listbox variable
type-pools: vrm.
data: name  type vrm_id,
      list  type vrm_values,
      value like line of list.


*-----/// ALV Control : START
* Control Framework Basic Class
class cl_gui_cfw      definition load.

* Declare reference variables, the container and internal table
data: wc_control_9000   type        scrfname value 'CC_9000_ALV',
      wc_alv_9000       type ref to cl_gui_alv_grid,
      wc_container_9000 type ref to cl_gui_custom_container.

data: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure like dd02l-tabname.

field-symbols: <container> type ref to cl_gui_custom_container,
               <control>   type        scrfname,
               <alv>       type ref to cl_gui_alv_grid,
               <itab>      type standard table.

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

constants: c_structure(100) value 'ZSMM_MMPM31R_'.

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

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
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
endclass.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
class lcl_event_receiver implementation.
  method handle_double_click.
    perform dbl_click_9000 using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click

endclass.

start-of-selection.
  move: sy-datum to w_datuv,
        'P001'   to w_werks.

  call screen 9000.

*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status output.
  case sy-dynnr.
    when 9000.
      set pf-status '9000'.
      set titlebar  '9000'.
  endcase.
endmodule.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_listbox  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_listbox output.
  perform set_listbox_kschl.
endmodule.                 " set_listbox  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_listbox_kschl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_listbox_kschl.
  clear: name, value, list.

  name = 'W_KSCHL'.

  select * from t685t where kvewe = 'A'
                        and kappl = 'M'
                        and spras = sy-langu
                        and kschl like 'ZP%'.
    move: t685t-kschl to value-key,
          t685t-vtext to value-text.
    append value to list.
  endselect.

  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = list.
endform.                    " set_listbox_kschl
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
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  case sy-ucomm.
    when 'BACK'.
      clear sy-ucomm.
      leave to screen 0.
    when 'REFRESH'.
      clear sy-ucomm.
      perform refresh_rtn.
  endcase.
endmodule.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_rtn.
  clear: it_9000, it_9000[].

  perform read_bom_header.
  perform read_bom.
  perform read_info_record.
* added by chris requested by MR.Kim
  perform read_shopcost.
  perform add_shopcost.
  perform read_mtart.
* added on 06/27/2005
endform.                    " REFRESH_RTN
*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_bom.
  perform read_fsc_bom using '01'
                             zsmm_mmpm31r_fsc-fsc01
                             zsmm_mmpm31r_fsc-ext01
                             zsmm_mmpm31r_fsc-int01
                             zsmm_mmpm31r_fsc-usg01
                             zsmm_mmpm31r_fsc-alt01.
  perform read_fsc_bom using '02'
                             zsmm_mmpm31r_fsc-fsc02
                             zsmm_mmpm31r_fsc-ext02
                             zsmm_mmpm31r_fsc-int02
                             zsmm_mmpm31r_fsc-usg02
                             zsmm_mmpm31r_fsc-alt02.
  perform read_fsc_bom using '03'
                             zsmm_mmpm31r_fsc-fsc03
                             zsmm_mmpm31r_fsc-ext03
                             zsmm_mmpm31r_fsc-int03
                             zsmm_mmpm31r_fsc-usg03
                             zsmm_mmpm31r_fsc-alt03.
  perform read_fsc_bom using '04'
                             zsmm_mmpm31r_fsc-fsc04
                             zsmm_mmpm31r_fsc-ext04
                             zsmm_mmpm31r_fsc-int04
                             zsmm_mmpm31r_fsc-usg04
                             zsmm_mmpm31r_fsc-alt04.
  perform read_fsc_bom using '05'
                             zsmm_mmpm31r_fsc-fsc05
                             zsmm_mmpm31r_fsc-ext05
                             zsmm_mmpm31r_fsc-int05
                             zsmm_mmpm31r_fsc-usg05
                             zsmm_mmpm31r_fsc-alt05.
  perform read_fsc_bom using '06'
                             zsmm_mmpm31r_fsc-fsc06
                             zsmm_mmpm31r_fsc-ext06
                             zsmm_mmpm31r_fsc-int06
                             zsmm_mmpm31r_fsc-usg06
                             zsmm_mmpm31r_fsc-alt06.
endform.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  read_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_bom_header.
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
                     and d~datuv <= w_datuv
   where a~mtart ne 'ROH' and a~mtart ne c_mtart1.

  sort it_header by matnr werks stlan.
endform.                    " read_bom_header
*&---------------------------------------------------------------------*
*&      Form  read_fsc_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSMM_MMPM31R_FSC_FSC01  text
*      -->P_ZSMM_MMPM31R_FSC_EXT01  text
*      -->P_ZSMM_MMPM31R_FSC_INT01  text
*----------------------------------------------------------------------*
form read_fsc_bom using pw_index pw_matnr pw_atwre
                        pw_atwri pw_stlan pw_stlal.
  data: lw_odseq like zspp_zrpp301r-odseq.
  data: lw_index like sy-tabix,
        lw_knnam like cukb-knnam,
        lw_quantity(50).

  data: lt_stb type  stpox occurs 0 with header line.
  data: lw_topmat like cstmat,
        lw_stlan  like stpox-stlan,
        lw_subrc  like sy-subrc.

  field-symbols: <quantity>.

  perform display_progress_bar using pw_matnr pw_index.

  call function 'CS_BOM_EXPL_MAT_V2'
       exporting
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = w_datuv
            cuobj                 = c_cuobj
            mktls                 = 'X'
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = pw_stlal
            svwvo                 = 'X'
            werks                 = w_werks
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
  if sy-subrc <> 0 or  lw_topmat-stlal ne pw_stlal.
    exit.
  endif.

  loop at lt_stb.
    clear: it_9000, lw_knnam.

    concatenate 'IT_9000-QTY' pw_index into lw_quantity.
    assign (lw_quantity) to <quantity>.
    if sy-subrc ne 0.
      continue.
    endif.

    read table it_9000 with key matnr = lt_stb-idnrk
                                werks = lt_stb-werks.
    if sy-subrc eq 0.
      move: sy-tabix to lw_index.

      perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            pw_atwre     pw_atwri
                                            lw_knnam     pw_matnr.
      if lw_knnam eq space and lt_stb-knobj > 0.
        continue.
      endif.

      <quantity> = <quantity> + lt_stb-menge.
      modify it_9000 index lw_index.
    else.
      read table it_header with key matnr = lt_stb-idnrk
                                    werks = lt_stb-werks
                                    stlan = pw_stlan
                           binary search.
      if sy-subrc ne 0.
        check lt_stb-mtart eq c_mtart or lt_stb-mtart eq c_mtart1.

        perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk lt_stb-werks
                                              pw_atwre     pw_atwri
                                              lw_knnam     pw_matnr.
        if lw_knnam eq space and lt_stb-knobj > 0.
          continue.
        endif.

        move: lt_stb-idnrk to it_9000-matnr,
              lt_stb-werks to it_9000-werks,
              lt_stb-menge to <quantity>,
              lt_stb-ojtxp to it_9000-maktx,
              lt_stb-meins to it_9000-meins.

        append it_9000.
      else.
        case lt_stb-zinfo.
          when 'ENG'.
** Changed by Furong on 12/19/11 for E002
*           perform bom_explosion using pw_index c_werks lt_stb-idnrk
*                                        pw_stlan lt_stb-menge
*                                        pw_atwre pw_atwri pw_matnr.
            perform bom_explosion using pw_index lt_stb-WERKS lt_stb-idnrk
                                        pw_stlan lt_stb-menge
                                        pw_atwre pw_atwri pw_matnr.
** END ON 12/19/11

          when others.
            perform bom_explosion using pw_index w_werks lt_stb-idnrk
                                        pw_stlan lt_stb-menge
                                        pw_atwre pw_atwri pw_matnr.
        endcase.

      endif.
    endif.
  endloop.
endform.                    " read_fsc_bom
*&---------------------------------------------------------------------*
*&      Form  check_object_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_KNOBJ  text
*      -->P_LT_STB_MTART  text
*      -->P_LT_STB_IDNRK  text
*      -->P_LT_STB_WERKS  text
*      -->P_LW_KNNAM  text
*----------------------------------------------------------------------*
form check_object_dependency using pw_knobj pw_mtart
                                   pw_matnr pw_werks
                                   pw_atwre pw_atwri
                                   pw_knnam pw_fsc.
  check pw_knobj > 0.

  case pw_mtart.
    when 'ROH'.
      select single * from marc where matnr = pw_matnr
                                  and werks = pw_werks.
      if sy-subrc ne 0.
        message e000(zz) with text-m01.
      endif.

      if marc-dispo eq 'M01'.         "Module Part
        move: pw_atwri to pw_knnam.
        perform compare_dependency using c_module pw_knnam
                                         pw_atwre pw_atwri pw_knobj.
      else.
        perform get_dependency using pw_fsc pw_knnam pw_werks.
        perform compare_dependency using c_roh    pw_knnam
                                         pw_atwre pw_atwri pw_knobj.
      endif.
    when 'ROH1'.
      move: pw_atwre to pw_knnam.
      perform compare_dependency using c_subpart pw_knnam
                                       pw_atwre  pw_atwri pw_knobj.
  endcase.
endform.                    " check_object_dependency
*&---------------------------------------------------------------------*
*&      Form  compare_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_MODULE  text
*      -->P_PW_KNNAM  text
*      -->P_PW_KNOBJ  text
*----------------------------------------------------------------------*
form compare_dependency using pw_type  pw_knnam
                              pw_atwre pw_atwri pw_knobj.
  data: l_knnam like cukb-knnam.
  data: lt_cuob like cuob occurs 0 with header line.

  select * into table lt_cuob
    from cuob
   where kntab =  'STPO'
     and knobj =  pw_knobj
     and datuv <= w_datuv.
  if sy-subrc ne 0.
    move: '9999999999' to lt_cuob-knnum.
    append lt_cuob.
  endif.

  case pw_type.
    when c_roh.
      write: pw_atwre  to l_knnam(3),
             pw_knnam  to l_knnam+3.
      select knnam into pw_knnam
        from cukb
         for all entries in lt_cuob
       where knnum =  lt_cuob-knnum
         and adzhl =  lt_cuob-adzhl
         and knnam =  l_knnam
         and datuv <= w_datuv.
      endselect.
      if sy-subrc ne 0.
        write: pw_atwri  to l_knnam(3),
               pw_knnam  to l_knnam+3.
        select knnam into pw_knnam
          from cukb
           for all entries in lt_cuob
         where knnum =  lt_cuob-knnum
           and adzhl =  lt_cuob-adzhl
           and knnam =  l_knnam
           and datuv <= w_datuv.
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
         and datuv <= w_datuv.
      endselect.
      if sy-subrc ne 0.
        clear: pw_knnam.
      endif.
  endcase.
endform.                    " compare_dependency
*&---------------------------------------------------------------------*
*&      Form  get_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_KNNAM  text
*      -->P_PW_WERKS  text
*----------------------------------------------------------------------*
form get_dependency using pw_matnr pw_knnam pw_werks.
  data: l_in_recno like ibin-in_recno.

  data: begin of lt_cabn occurs 7,
          atwrt type v_ibin_syval-atwrt,
          atnam type cabn-atnam,
        end   of lt_cabn.

  select single * from marc where matnr = pw_matnr
                              and werks = pw_werks.
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
endform.                    " get_dependency
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_WERKS  text
*      -->P_LT_STB_IDNRK  text
*      -->P_<QUANTITY>  text
*----------------------------------------------------------------------*
form bom_explosion using pw_index pw_werks pw_matnr pw_stlan pw_menge
                         pw_atwre pw_atwri pw_fsc.
  data: lt_stb type  stpox occurs 0 with header line.
  data: lw_index  like sy-tabix,
        lw_topmat like cstmat,
        lw_subrc like sy-subrc,
        lw_knnam like cukb-knnam,
        lw_menge like stpo-menge,
        lw_quantity(50).

  field-symbols <quantity>.


  call function 'CS_BOM_EXPL_MAT_V2'
       exporting
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = w_datuv
            mktls                 = 'X'
            cuobj                 = c_cuobj
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
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
    clear: it_9000, lw_knnam.

    concatenate 'IT_9000-QTY' pw_index into lw_quantity.
    assign (lw_quantity) to <quantity>.
    if sy-subrc ne 0.
      continue.
    endif.

    lw_menge = pw_menge * lt_stb-menge.

    read table it_9000 with key matnr = lt_stb-idnrk
                                werks = lt_stb-werks.
    if sy-subrc eq 0.
      move: sy-tabix to lw_index.

      perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            pw_atwre     pw_atwri
                                            lw_knnam     pw_fsc.
      if lw_knnam eq space and lt_stb-knobj > 0.
        continue.
      endif.

      <quantity> = <quantity> + lw_menge.
      modify it_9000 index lw_index.
    else.
      read table it_header with key matnr = lt_stb-idnrk
                                    werks = lt_stb-werks
                                    stlan = pw_stlan
                           binary search.
      if sy-subrc ne 0.
        check lt_stb-mtart eq c_mtart or lt_stb-mtart eq c_mtart1.

        perform check_object_dependency using lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk lt_stb-werks
                                              pw_atwre     pw_atwri
                                              lw_knnam     pw_fsc.
        if lw_knnam eq space and lt_stb-knobj > 0.
          continue.
        endif.

        move: lt_stb-idnrk to it_9000-matnr,
              lt_stb-werks to it_9000-werks,
              lt_stb-ojtxp to it_9000-maktx,
              lw_menge     to <quantity>,
              lt_stb-meins to it_9000-meins.

        append it_9000.
      else.
        perform bom_explosion using pw_index pw_werks lt_stb-idnrk
                                    pw_stlan lw_menge
                                    pw_atwre pw_atwri
                                    pw_fsc.
      endif.
    endif.
  endloop.
endform.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_object output.
  perform create_alv_object using sy-dynnr.
endmodule.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
form dbl_click_9000 using p_column_name             "Column Name
                          ps_row_no  like sy-tabix. "Numeric Row ID
  data : lw_sel_index like sy-tabix.

  move: ps_row_no to lw_sel_index.

  read table it_9000 index lw_sel_index.
  if sy-subrc ne 0.
    exit.
  endif.

  set parameter id: 'MAT' field it_9000-matnr,
*                    'WRK' FIELD IT_9000-WERKS,
                    'MXX' field 'K'.
  call transaction 'MM03' and skip first screen.
endform.                    " dbl_click_9000

*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
form create_alv_object using p_dynnr.
  concatenate: 'WC_CONTAINER_' p_dynnr into w_container.
  assign:      (w_container)           to   <container>.

  if <container> is initial.          "/Not Created Control for ALV GRID
    perform create_container_n_object using p_dynnr.
    perform set_attributes_alv_grid using p_dynnr.
    perform build_field_catalog using p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    perform assign_itab_to_alv using p_dynnr.
    perform sssign_event using p_dynnr.
  else.
    perform set_attributes_alv_grid using p_dynnr.
    perform build_field_catalog using p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    perform assign_itab_to_alv using p_dynnr.
  endif.
endform.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form create_container_n_object using p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  concatenate: 'WC_CONTAINER_' p_dynnr into w_container,
               'WC_CONTROL_'   p_dynnr into w_control,
               'WC_ALV_'       p_dynnr into w_alv.

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
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form set_attributes_alv_grid using p_dynnr.
  case p_dynnr.
    when '9000'.
      perform set_attributes_alv_9000.
  endcase.
endform.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9000.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
*  w_is_layout-NO_TOTLINE = ' '.
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form build_field_catalog using p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  perform set_fieldname using p_dynnr.
  perform set_screen_fields using p_dynnr.
endform.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form set_fieldname using p_dynnr.
  data: lw_itab type slis_tabname.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  move: sy-repid to w_repid.
  concatenate c_structure p_dynnr into lw_itab.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       changing
            ct_fieldcat        = it_fieldname.
endform.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form set_screen_fields using p_dynnr.
  case p_dynnr.
    when '9000'.
      perform set_screen_fields_9000.
  endcase.
endform.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9000.
  data: lw_text(9).

  move: w_kschl to lw_text.

  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MATNR'       ' ',
                                  ' ' 'OUTPUTLEN'   '18',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'LIFNR'       ' ',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SOURC'       ' ',
                                  ' ' 'OUTPUTLEN'   '6',
                                  ' ' 'SCRTEXT_L'   'Source',
                                  ' ' 'SCRTEXT_M'   'Source',
                                  ' ' 'SCRTEXT_S'   'Source',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MTART'       ' ',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'COS_U'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'COS_P'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'COS_Q'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
*                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'COS_A'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MEINS'       ' ',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'WERKS'       ' ',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'QTY01'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'QTY02'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'QTY03'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'QTY04'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'QTY05'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'QTY06'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NETPR'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'PEINH'       ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'WAERS'       ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'AMT01'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'AMT02'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'AMT03'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'AMT04'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'AMT05'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'AMT06'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C200',

                                  'S' 'KBETR'       ' ',
                                  ' ' 'COLTEXT'     lw_text,
                                  ' ' 'OUTPUTLEN'   '9',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'CON01'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501',

                                  'S' 'CON02'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501',

                                  'S' 'CON03'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501',

                                  'S' 'CON04'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501',

                                  'S' 'CON05'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501',

                                  'S' 'CON06'       ' ',
                                  ' ' 'OUTPUTLEN'   '9',
                                  ' ' 'DO_SUM'      'X',
                                  'E' 'EMPHASIZE'   'C501'.
endform.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form assign_itab_to_alv using p_dynnr.
  data: lw_dynnr   like   sy-dynnr.

  concatenate: 'WC_ALV_'    p_dynnr      into w_alv,
               c_structure  p_dynnr      into w_structure,
               'IT_'        p_dynnr '[]' into w_itab.

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
form sssign_event using p_dynnr.
  data: lw_dynnr   like   sy-dynnr.

  concatenate: 'WC_ALV_'    p_dynnr      into w_alv.
  assign: (w_alv)       to <alv>.

*--  Regist event for Edit
  if sy-batch is initial.
    call method <alv>->register_edit_event
        exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  endif.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  create object event_receiver.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
  set handler event_receiver->handle_double_click  for <alv>.
**    SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID
  .
**    SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID
  .
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR ALV_GRID
  .

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD alv_grid->set_toolbar_interactive.
*
*    CALL METHOD cl_gui_control=>set_focus
*                        EXPORTING control = alv_grid.
endform.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1033   text
*      -->P_1034   text
*      -->P_1035   text
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
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_info_record.
  sort it_9000 by matnr.

  loop at it_9000.
    perform get_info_record.

    modify it_9000.
  endloop.
endform.                    " read_info_record
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_info_record.
*---// If S/A does not exist and the material has 2 more info record,
*---// select info record that has chipper price .
*---// MM guy said
*---// "All of material's purchaning UoM is same as BOM's UoM,
*---//  and all of currency is the same"
*---// So below logic does not include any conversion logic.
  data: lw_lifnr like lfa1-lifnr.

  clear: it_lifnr, it_lifnr[].

  select lifnr
    into corresponding fields of it_lifnr
    from eina as a inner join eine as b
      on a~infnr = b~infnr
   where a~matnr =  it_9000-matnr
     and a~loekz =  ' '
     and b~werks =  ' '
     and b~ekorg =  c_ekorg
     and b~loekz =  ' '.

    select single *
      from a018
     where kappl =  'M'
       and kschl =  'PB00'
       and matnr =  it_9000-matnr
       and lifnr =  it_lifnr-lifnr
       and ekorg =  c_ekorg
       and esokz =  '0'
       and datab <= w_datuv
       and datbi >= w_datuv.
    if sy-subrc eq 0.
      move: a018-knumh to it_lifnr-knumh.

      append it_lifnr.
    endif.
  endselect.
  if sy-subrc ne 0.
    exit.
  endif.

*---// Select one Vendor from S/A
  read table it_lifnr index 2.
  if sy-subrc eq 0.
    perform select_vendor tables it_lifnr
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
  move: it_lifnr-lifnr to it_9000-lifnr.

*---// Read price and conditions
  clear: it_condition, it_condition[].

  select kbetr kpein konwa kschl
    into corresponding fields of table it_condition
    from zvmm_info_condi
   where knumh = it_lifnr-knumh
     and kschl in (c_kschl,w_kschl)
     and loevm_ko = ' '.

  loop at it_condition.
    case it_condition-kschl.
      when c_kschl.
        move: it_condition-kbetr to it_9000-netpr,
              it_condition-kpein to it_9000-peinh,
              it_condition-konwa to it_9000-waers.
      when w_kschl.
        move: it_condition-kbetr to it_9000-kbetr.
    endcase.
  endloop.

*--- Set amount and condition price
  data: lw_index(2) type n,
        lw_quantity(50),
        lw_amount(50),
        lw_condition(50).

  field-symbols: <quantity>, <amount>, <condition>.

  do 6 times.
    move: sy-index to lw_index.

    concatenate: 'IT_9000-QTY' lw_index into lw_quantity,
                 'IT_9000-AMT' lw_index into lw_amount,
                 'IT_9000-CON' lw_index into lw_condition.
    assign: (lw_quantity)  to <quantity>.
    if sy-subrc ne 0.
      continue.
    endif.

    assign: (lw_amount)    to <amount>.
    if sy-subrc ne 0.
      continue.
    endif.
    <amount>    = it_9000-netpr / it_9000-peinh * <quantity>.

    assign: (lw_condition) to <condition>.
    if sy-subrc ne 0.
      continue.
    endif.
    <condition> = it_9000-kbetr / it_9000-peinh * <quantity>.
  enddo.
endform.                    " GET_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  select_vendor_from_sa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*      -->P_LW_LIFNR  text
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
   where matnr   eq it_9000-matnr
     and lifnr   eq pt_lifnr-lifnr
     and a~bstyp eq 'L'
     and werks   eq w_werks
     and a~loekz eq space
     and a~autlf eq space
     and b~loekz eq space
     and b~elikz eq space
     and kdatb   <= w_datuv
     and kdate   >= w_datuv.
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
*&      Module  check_screen_field_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_screen_field_9000 input.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc01
                          zsmm_mmpm31r_fsc-ext01 zsmm_mmpm31r_fsc-int01
                          zsmm_mmpm31r_fsc-alt01 zsmm_mmpm31r_fsc-usg01.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc02
                          zsmm_mmpm31r_fsc-ext02 zsmm_mmpm31r_fsc-int02
                          zsmm_mmpm31r_fsc-alt02 zsmm_mmpm31r_fsc-usg02.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc03
                          zsmm_mmpm31r_fsc-ext03 zsmm_mmpm31r_fsc-int03
                          zsmm_mmpm31r_fsc-alt03 zsmm_mmpm31r_fsc-usg03.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc04
                          zsmm_mmpm31r_fsc-ext04 zsmm_mmpm31r_fsc-int04
                          zsmm_mmpm31r_fsc-alt04 zsmm_mmpm31r_fsc-usg04.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc05
                          zsmm_mmpm31r_fsc-ext05 zsmm_mmpm31r_fsc-int05
                          zsmm_mmpm31r_fsc-alt05 zsmm_mmpm31r_fsc-usg05.
  perform check_fsc using zsmm_mmpm31r_fsc-fsc06
                          zsmm_mmpm31r_fsc-ext06 zsmm_mmpm31r_fsc-int06
                          zsmm_mmpm31r_fsc-alt06 zsmm_mmpm31r_fsc-usg06.
  perform check_fsc using zsmm_mmpm31r_fsc-bfsc 'AA' 'BB' '01' '1'.

  perform check_business_plan using zsmm_mmpm31r_fsc-bfsc
                                    w_year w_perid.

endmodule.                 " check_screen_field_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_fsc using pw_fsc pw_ext pw_int pw_alt pw_usg.
  check pw_fsc ne space.

  select single * from mara where matnr = pw_fsc.
  if sy-subrc ne 0.
    message e000(zz) with text-m02 pw_fsc text-m03.
  endif.

  if mara-lvorm eq 'X'.
    message e000(zz) with text-m02 pw_fsc text-m04.
  endif.

  if mara-mtart ne 'FERT'.
    message e000(zz) with text-m02 pw_fsc text-m05.
  endif.

  if pw_ext eq space.
    message e000(zz) with text-m06 pw_fsc.
  endif.

  if pw_int eq space.
    message e000(zz) with text-m07 pw_fsc.
  endif.

  if pw_alt eq space.
    message e000(zz) with text-m08 pw_fsc.
  endif.

  if pw_usg eq space.
    message e000(zz) with text-m09 pw_fsc.
  endif.
endform.                    " check_fsc
*&---------------------------------------------------------------------*
*&      Form  value_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2773   text
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
*&      Form  add_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2932   text
*      -->P_2933   text
*      -->P_2934   text
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
*&      Form  value_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2955   text
*      -->P_2956   text
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
*&      Module  POV_req_ext_COLOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pov_req_ext_color input.
  get cursor field w_field.

  shift w_field by 20 places.

  perform display_ext_color_pov using w_field.
endmodule.                 " POV_req_ext_COLOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_EXT_COLOR_POV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_FIELD  text
*----------------------------------------------------------------------*
form display_ext_color_pov using pw_field.
  data: lw_cuobj   like  inob-cuobj,
        lw_clint   like  klah-clint,
        lw_fsc(50),
        lw_field(50).
  field-symbols: <fsc>.


  clear dynpread. refresh dynpread.
  clear valuetab. refresh valuetab.
  clear fields.   refresh fields.

  concatenate: 'ZSMM_MMPM31R_FSC-FSC' pw_field into lw_fsc,
               'ZSMM_MMPM31R_FSC-EXT' pw_field into lw_field.

  assign (lw_fsc) to <fsc>.
  if sy-subrc ne 0.
    exit.
  endif.

  perform value_read using: lw_fsc.
  loop at dynpread.
    case sy-tabix.
      when 1. <fsc> = dynpread-fieldvalue.
    endcase.
  endloop.

  select single cuobj
         into lw_cuobj
         from inob
         where klart eq '300'
           and obtab eq 'MARA'
           and objek eq <fsc>.

  select single clint
         into lw_clint
         from kssk
         where objek eq lw_cuobj
           and mafid eq 'O'
           and klart eq '300'.

  select *
         into table it_ksml
         from ksml
         where clint eq lw_clint.

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
            'X'   lw_field it_cawn-atwrt 0.
  endif.
endform.                    " DISPLAY_EXT_COLOR_POV
*&---------------------------------------------------------------------*
*&      Module  POV_req_INT_COLOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pov_req_int_color input.
  get cursor field w_field.

  shift w_field by 20 places.

  perform display_int_color_pov using w_field.
endmodule.                 " POV_req_INT_COLOR  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_INT_COLOR_POV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_FIELD  text
*----------------------------------------------------------------------*
form display_int_color_pov using pw_field.
  data: lw_cuobj   like  inob-cuobj,
        lw_clint   like  klah-clint,
        lw_fsc(50),
        lw_field(50).
  field-symbols: <fsc>.


  clear dynpread. refresh dynpread.
  clear valuetab. refresh valuetab.
  clear fields.   refresh fields.

  concatenate: 'ZSMM_MMPM31R_FSC-FSC' pw_field into lw_fsc,
               'ZSMM_MMPM31R_FSC-INT' pw_field into lw_field.

  assign (lw_fsc) to <fsc>.
  if sy-subrc ne 0.
    exit.
  endif.
  clear dynpread. refresh dynpread.
  clear valuetab. refresh valuetab.
  clear fields.   refresh fields.

  perform value_read using: lw_fsc.
  loop at dynpread.
    case sy-tabix.
      when 1. <fsc> = dynpread-fieldvalue.
    endcase.
  endloop.

  select single cuobj
         into lw_cuobj
         from inob
         where klart eq '300'
           and obtab eq 'MARA'
           and objek eq <fsc>.

  select single clint
         into lw_clint
         from kssk
         where objek eq lw_cuobj
           and mafid eq 'O'
           and klart eq '300'.

  select *
         into table it_ksml
         from ksml
         where clint eq lw_clint.

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
            'X'   lw_field it_cawn-atwrt 0.
  endif.

endform.                    " DISPLAY_INT_COLOR_POV
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_MATNR  text
*----------------------------------------------------------------------*
form display_progress_bar using pw_text pw_index.
  data: lw_text(50),
        lw_percentage(2) type n.

  concatenate text-m10 pw_text into lw_text.

  lw_percentage = pw_index * 10.

  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = lw_percentage
            text       = lw_text.
endform.                    " display_progress_bar
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
   where matnr   eq it_9000-matnr
     and lifnr   eq pt_lifnr-lifnr
     and a~bstyp eq 'F'
     and werks   eq w_werks
     and a~loekz eq space
     and a~autlf eq space
     and b~loekz eq space
     and b~elikz eq space
     and kdatb   <= w_datuv
     and kdate   >= w_datuv.
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
*&      Form  CHECK_BUSINESS_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_BFSC  text
*      -->P_W_YEAR  text
*      -->P_W_PERID  text
*----------------------------------------------------------------------*
form check_business_plan using    p_bfsc
                                  p_year
                                  p_perid.
   data: l_mtnr like mara-matnr.
   select single fsc_matnr into l_mtnr
     from ztco_shopcost
     where fsc_matnr  = zsmm_mmpm31r_fsc-bfsc   and
           bdatj     = w_year       and
           poper     = w_perid      and
           klvar     = c_klvar      and
           typps     = 'M'.
   if sy-subrc ne 0.
     message w000(zz) with 'Business plan does not exist'.
   endif.

endform.                    " CHECK_BUSINESS_PLAN
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

  refresh :it_shopcost, it_all_comp.
  clear:   it_shopcost, it_all_comp.
* Read first level components of FSC
  select * into table lt_shopcost
    from ztco_shopcost
    where fsc_matnr = zsmm_mmpm31r_fsc-bfsc       and
          bdatj     = w_year       and
          poper     = w_perid      and
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
    lt_shopcost-hwaer      = it_shopcost-hwaer.
    lt_shopcost-meeht      = it_shopcost-meeht.
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
       and bdatj       = w_year
       and poper       = w_perid
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
*&      Form  add_shopcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_shopcost.
  data: wa_9000 like it_9000.
  data: l_tabix like sy-tabix.

  loop at it_shopcost.
    clear: it_9000.
    read table it_9000 with key matnr = it_shopcost-llv_matnr.
    l_tabix = sy-tabix.
    if sy-subrc eq 0.

      it_9000-cos_u = it_shopcost-wertn / it_shopcost-menge.
      it_9000-cos_p = 1.
      it_9000-cos_q = it_shopcost-menge.
      it_9000-cos_a = it_shopcost-wertn.


      modify it_9000 index l_tabix.

    else.

      wa_9000-matnr = it_shopcost-llv_matnr.
      wa_9000-cos_u = it_shopcost-wertn / it_shopcost-menge.
      wa_9000-cos_p = 1.
      wa_9000-cos_q = it_shopcost-menge.
      wa_9000-cos_a = it_shopcost-wertn.
      wa_9000-waers = it_shopcost-hwaer.
      wa_9000-meins = it_shopcost-meeht.

*      get the material descriptiong
      select single maktx into wa_9000-maktx
        from makt
        where matnr  = wa_9000-matnr
          and spras  = 'EN'.
      append  wa_9000  to it_9000.
      clear: wa_9000.
    endif.

  endloop.

endform.                    " add_shopcost
*&---------------------------------------------------------------------*
*&      Form  read_mtart
*&---------------------------------------------------------------------*
*       text
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
    for all entries in it_9000
    where matnr  = it_9000-matnr.

  sort lt_mtart by matnr.

  loop at it_9000.
    read table lt_mtart with key matnr = it_9000-matnr
        binary search.
    it_9000-mtart  = lt_mtart-mtart.
    it_9000-sourc  = lt_mtart-profl.
    if it_9000-waers is initial.
      it_9000-waers = 'USD'.
    endif.
*   derive the vendor if vendor does not exsit
    select single lifnr into it_9000-lifnr
      from a018
      where matnr = it_9000-matnr
        and datab le w_datuv
        and datbi ge w_datuv.
    modify it_9000.
  endloop.

  sort it_9000 by matnr.

endform.                    " read_mtart
