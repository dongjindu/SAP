*&---------------------------------------------------------------------*
*& Include ZAPP236M_TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp236m_veh_history   message-id zmpp.

tables: ausp,
        equi,
        zspp_vm_value.
* -
data  begin of IT_VMV_APP236  occurs  0.
        include structure zspp_vin_value.
data  end of IT_VMV_APP236.
* -
data  begin of st_app236 .
        include structure zspp_vm_value.
data  end of st_app236.
* ------------------------------------------------
data: begin of IT_WIP_APP236  occurs 0,
        progress(2)  type  n,
        shop_dat     like  sy-datum,
        act_dat(12)  type  c,
        serial(5)    type  c,
      end of IT_WIP_APP236.
data: begin of IT_WIP  occurs 0,
        progress(2)  type  n,
        shop_dat     like  sy-datum,
        act_dat(12)  type  c,
        serial(5)    type  c,
      end of IT_WIP.
data: G_SHOP_DATE_APP236(20),
      G_SERIAL_APP236(20),
      G_ACT_DATE_APP236(20).
* Engine dupplicate vehicle
data: begin of IT_ENG_APP236  occurs 0,
        objek  like  ausp-objek,
        engno(15),
      end of IT_ENG_APP236.
* WORK ORDER V/C DATA
data  G_CUOBF_APP236   like  equi-equnr.
*DATA  G_CUOBF_APP236   LIKE  mara-cuobf.  "INSTANCE
data  IT_WO_APP236     like  table of zspp_vin_value   with header line.
* 219 option values table
data: begin of IT_219_APP236  occurs 0,
        col  like  ztbm_abxopvdt-clno,
        val  like  ztbm_abxopvdt-valu,
        valtx like ztbm_abxopvdt-vanm,
        coltx like ztbm_abxopvdt-clnm,
*        col  like  ztbm_219_value-serial,
*        val  like  ztbm_219_value-value,
*        valtx like ztbm_219_value-code_name1,
*        coltx like ztbm_219_desc-desc_219,
      end of IT_219_APP236.
* Order table ; Unique Part
data: begin of IT_PART_APP236  occurs 0,
        col  like  ztbm_abxopvdt-clno,
*        col        like  ztbm_219_value-serial,
        code(10)   type  c,
        codetx(40) type  c,
      end of IT_PART_APP236.
*
data: begin of IT_UPART_APP236  occurs 0,
        col  like  ztbm_abxopvdt-clno,
*        col        like  ztbm_219_value-serial,
        code(10)   type  c,
        codetx(40) type  c,
      end of IT_UPART_APP236.
data: begin of IT_CPART_APP236  occurs 0,
        col  like  ztbm_abxopvdt-clno,
*        col        like  ztbm_219_value-serial,
        code(10)   type  c,
        codetx(40) type  c,
      end of IT_CPART_APP236.

* Order table ; Unique Part
data: begin of IT_UCPART_APP236  occurs 0,
        ucgub(1),
        col  like  ztbm_abxopvdt-clno,
*        col        like  ztbm_219_value-serial,
        code(10)   type  c,
        codetx(40) type  c,
      end of IT_UCPART_APP236.

* table for Airbag list
data: begin of IT_ABAG_APP236  occurs 0,
        airbag(30)   type  c,
        code(30)     type  c,
      end of IT_ABAG_APP236.

* table controls ----------------------------------------
controls: tc100 type tableview using screen 100.
controls: tc110 type tableview using screen 110.
controls: tc120 type tableview using screen 120.
controls: tc130 type tableview using screen 130.
* tabstrib
controls: tc_app236_01 type tableview using screen 2118.
controls: tc_app236_02 type tableview using screen 2119.
controls: tc_app236_03 type tableview using screen 2120.
controls: tc_app236_04 type tableview using screen 2121.
* LIST BOX creation
type-pools vrm.
data: name  type vrm_id,
      LIST_APP236  type vrm_values,
      VALUE_APP236 like line of LIST_APP236.
* GUI Icon control table
data: begin of  IT_FUNC_APP236  occurs 0,   "GUI tool bar control
         fcode  like  rsmpe-func,
      end   of  IT_FUNC_APP236.
data: begin of st_key_app236,
       inqopt(15),
       company(10),
      end of st_key_app236.
* undefine screen field
data: begin of st_iss_app236,
       inqopt(15),
       company(10),
       eqktx    like  eqkt-eqktx,
       dupeng1(15), dupeng2(15), dupeng3(15), dupeng4(15),
       reportno(20),
       approval(20),
       tctx(30),               "Problem contents
       appdat   like sy-datum,
       ccdat    like sy-datum,
       lifnr    like lfa1-lifnr,
       lifnm    like lfa1-name1,
      end of st_iss_app236.
data: begin of st_code_app236,
       inqopt  like  st_key_app236-inqopt,
       model   like  st_app236-model,
       bodyno  like  st_app236-bodyno,
       vin     like  st_app236-vin,
       engno   like  st_app236-engno,
       tmno    like  st_app236-tmno,
     end of st_code_app236.

data: P_BODY01_APP236   type  i,
      P_BODY02_APP236   type  i,
      P_PAINT01_APP236  type  i,
      P_PAINT02_APP236  type  i,
      P_TRIM01_APP236   type  c,
      P_TRIM02_APP236   type  c.
*
data: G_EQUNR_APP236    like  equi-equnr,
      G_EQUICHK_APP236  type  c.
data: G_CRSR_FLD_APP236(20).
data: G_ATTR_APP236(1).
data: G_VIN_APP236    like  ausp-atinn,
      G_ENGNO_APP236  like  ausp-atinn,
      g_tmno_APP236   like  ausp-atinn.
data: g_part_APP236(1),      "Unique, Color
      g_parttit_APP236(30).
data: IT_LINES_APP236  type  i.
data: wip_lines type  i.

* FUNCTION CODES FOR TABSTRIP 'SS2106'
constants: begin of c_ss2106,
             tab1 like sy-ucomm value 'SS2106_FC1',
             tab2 like sy-ucomm value 'SS2106_FC2',
             tab3 like sy-ucomm value 'SS2106_FC3',
             tab4 like sy-ucomm value 'SS2106_FC4',
             tab5 like sy-ucomm value 'SS2106_FC5',
             tab6 like sy-ucomm value 'SS2106_FC6',
             tab7 like sy-ucomm value 'SS2106_FC7',
           end of c_ss2106.
* DATA FOR TABSTRIP 'SS2106'
controls:  ss2106 type tabstrip.
data:      begin of g_ss2106,
             subscreen   like sy-dynnr,
             prog        like sy-repid value 'ZAPP236M_VEH_HISTORY',
             pressed_tab like sy-ucomm value c_ss2106-tab1,
           end of g_ss2106.
data:      ok_code like sy-ucomm.
