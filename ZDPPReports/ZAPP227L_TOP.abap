*&---------------------------------------------------------------------*
*& Include ZAPP227L_TOP                                             *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp227m_spec   message-id zmpp    .

tables: ztpp_spec,  "Spec Table
        mara.
controls: tc_app227 type tableview using screen 1209.
controls: tc_new_app227 type tableview using screen 1210.
*
data: begin of it_app227 occurs 0.
        include structure ztpp_spec.
data: check.
data: end of it_app227.
*
data: begin of IT_EXCEL_1209 occurs 0,
        keycode(20),  "KEY CODE
        opdate(20),  "OPERATION DATE
        opcount(20),  "OPERATION COUNT
        mark(20),  "MARKING
        plant(20),  "PLANT
        model(20),  "MODEL CODE
        worder(20),  "WORK ORDER
        extc(20),  "EXTERNAL COLOR
        intc(20),  "INTERNAL COLOR
        erdat(20),  "LAST CHANGED ON
        erzet(20),  "TIME LAST CHANGE WAS MADE
        ernam(20),  "NAME OF PERSON WHO CHANGED OBJECT
      end of IT_EXCEL_1209.

*
data: begin of it_new_app227 occurs 0,
        keycode type ztpp_spec-keycode,
        opdate type ztpp_spec-opdate,
        opcount type ztpp_spec-opcount,
        mark type ztpp_spec-mark,
        plant type ztpp_spec-plant,
        model type ztpp_spec-model,
*
        forder type mara-matnr,
*
        worder type ztpp_spec-worder,
        extc type ztpp_spec-extc,
        intc type ztpp_spec-intc,
*
        zuser type ztpp_spec-zuser,
        erdat type ztpp_spec-erdat,
        erzet type ztpp_spec-erzet,
        ernam type ztpp_spec-ernam,
        aedat type ztpp_spec-aedat,
        aezet type ztpp_spec-aezet,
        aenam type ztpp_spec-aenam,
*
        check,
*
      end of it_new_app227.
*
data: begin of IT_ERROR_1210 occurs 0,
        matnr type mara-matnr,
        forder type mara-matnr,
        worder type ztpp_spec-worder,
        extc type ztpp_spec-extc,
        intc type ztpp_spec-intc,
      end of IT_ERROR_1210.
type-pools: vrm.
*
* PARAMETERS  --- SCREEN 1210
data: p_company(4),
      p_plant type ztpp_spec-plant,
      p_model type ztpp_spec-model,
      p_opdate type ztpp_spec-opdate,
      p_opcount type ztpp_spec-opcount,
      p_worder type ztpp_spec-worder,
      p_tot_count(04) type n.
* PARAMETERS --- SCREEN 1210
data: p_keycode type ztpp_spec-keycode,
      p_erdat like sy-datum,
      p_erzet like sy-uzeit,
      p_ernam like sy-uname.

* internal table for export
data: begin of it_spec occurs 0,
        plant type ztpp_spec-plant,
        opdate type ztpp_spec-opdate,
        opcount type ztpp_spec-opcount,
        worder type ztpp_spec-worder,
        extc type ztpp_spec-extc,
        intc type ztpp_spec-intc,
      end of it_spec.

* DROPDOWN LIST
* P_PLANT
data: name        type vrm_id,
      plant_list   type vrm_values,
      plant_value  like line of plant_list.
ranges: r_plant for ztpp_spec-plant.
*     P_MODEL
data: model_list type vrm_values,
      model_value like line of model_list,
*     P_EXTC
      p_extc(03),
*     P_INTC
      p_intc(03).
ranges: r_model for ztpp_spec-model.

*   ON VALUE-REQUEST
data: begin of reason_tab occurs 0,
        code   like mara-matnr,
*        text   LIKE lfa1-name1,
      end of reason_tab.
data: begin of dynpfields occurs 3.
        include structure dynpread.
data: end of dynpfields.
*
data: ok_code type sy-ucomm,
      save_ok type sy-ucomm.

data: WA_INIT_1209,
      WA_INIT_1210.
