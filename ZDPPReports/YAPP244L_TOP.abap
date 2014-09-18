*&---------------------------------------------------------------------*
*& Include YAPP803L_ZTPPWOSUM_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp244r_prod_res_by_progress  message-id zmpp    .

controls: tc_app244 type tableview using screen 110.

tables: ausp,  "Characteristic Values
        cabn.  "Characteristic

data: begin of it_app244 occurs 0,
*
        objek type ausp-objek,
        model type ausp-atwrt, "P_MODEL
        bodyno type ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
        vin type ausp-atwrt,                                "P_VIN(17)
        wono type ausp-atwrt,  "P_WORK_ORDER(14)
        extc type ausp-atwrt,      "P_EXT_COLOR(03)
        intc type ausp-atwrt,      "P_INT_COLOR(03)
        vendor(10),    "Not Defined
        mi type ausp-atwrt,                                 "P_MI (07)
        ocn type ausp-atwrt,                                "P_OCN (04)
        ver type ausp-atwrt,  "P_VERSION(03)
        act_date like sy-datum,  "P_RPxx_ACTUAL_DATE(08)
        act_time like sy-uzeit,  "P_RPxx_ACTUAL_DATE+08(06)
        serial type ausp-atwrt,  "P_RPxx_SERIAL(06)
        prog type ausp-atwrt,      "P_STATUS

      end of it_app244.

data: begin of it_excel_app244 occurs 0,
        bodyno type ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
        vin type ausp-atwrt,                                "P_VIN(17)
        wono type ausp-atwrt,  "P_WORK_ORDER(14)
        extc type ausp-atwrt,      "P_EXT_COLOR(03)
        intc type ausp-atwrt,      "P_INT_COLOR(03)
        vendor(10),    "Not Defined
        mi type ausp-atwrt,                                 "P_MI (07)
        ocn type ausp-atwrt,                                "P_OCN (04)
        ver type ausp-atwrt,  "P_VERSION(03)
        act_date like sy-datum,  "P_RPxx_ACTUAL_DATE(08)
        act_time like sy-uzeit,  "P_RPxx_ACTUAL_DATE+08(06)
        prog type ausp-atwrt,      "P_STATUS
        serial type ausp-atwrt,  "P_RPxx_SERIAL(06)
*
      end of it_excel_app244.

* Parameters(Screen0110)
data: p_company_app244(04),
      p_plant_app244(08),                 "P_TRIM_PLANT_NO
      p_model_app244(03),                 "P_MODEL
      p_bodyno_app244 type ausp-atwrt,    "P_BODY_SERIAL(09)
      p_line_app244(03),                  "P_TRIM_LINE_NO
      p_prog_app244(08),                         "P_RP_STATUS
      p_wono_app244(14),                         "P_WORK_ORDER
      p_extc_app244(03),                  "P_EXT_COLOR
      p_intc_app244(03),                  "P_INT_COLOR
**
      p_prod_date_app244   like sy-datum,    "P_RPxx_SHOP_DATE
      p_act_date_st_app244 like sy-datum,    "P_RPxx_ACTUAL_DATE(08)
      p_act_time_st_app244 like sy-uzeit,    "P_RPxx_ACTUAL_DATE+08(06)
      p_serial_st_app244   type ausp-atwrt,  "P_RPxx_SERIAL(06)
      p_act_date_en_app244 like sy-datum,    "P_RPxx_ACTUAL_DATE(08)
      p_act_time_en_app244 like sy-uzeit,    "P_RPxx_ACTUAL_DATE+08(06)
      p_serial_en_app244   type ausp-atwrt,  "P_RPxx_SERIAL(06)
**    P_219_xxx
      p_column01_app244(03),
      p_column02_app244(03),
      p_column03_app244(03),
      p_column04_app244(03),
      p_column05_app244(03),
      p_column06_app244(03),
      p_column07_app244(03),
      p_column08_app244(03),
      p_column09_app244(03),
      p_column10_app244(03),
      p_value01_app244 type ausp-atwrt,
      p_value02_app244 type ausp-atwrt,
      p_value03_app244 type ausp-atwrt,
      p_value04_app244 type ausp-atwrt,
      p_value05_app244 type ausp-atwrt,
      p_value06_app244 type ausp-atwrt,
      p_value07_app244 type ausp-atwrt,
      p_value08_app244 type ausp-atwrt,
      p_value09_app244 type ausp-atwrt,
      p_value10_app244 type ausp-atwrt.

* DROPDOWN LIST for Parameters
type-pools: vrm.
data: name        type vrm_id,
*     Plant
      plant_list  type vrm_values,
      plant_value like line of plant_list,
*     Model
      model_list  type vrm_values,
      model_value like line of model_list,
*     Body Serial
      body_ser_list  type vrm_values,
      body_ser_value like line of body_ser_list,
*     Line
      line_list  type vrm_values,
      line_value like line of line_list,
*     Progress
      progress_list  type vrm_values,
      progress_value like line of progress_list,
*     Work Order
      wono_list type vrm_values,
      wono_value like line of wono_list,
*     External Color
      extc_list type vrm_values,
      extc_value like line of extc_list,
*     Internal Color
      intc_list type vrm_values,
      intc_value like line of intc_list,
*     Column01
      column01_list type vrm_values,
      column01_value like line of column01_list,
*     COLUMN02
      column02_list type vrm_values,
      column02_value like line of column02_list,
*     COLUMN03
      column03_list type vrm_values,
      column03_value like line of column03_list,
*     COLUMN04
      column04_list type vrm_values,
      column04_value like line of column04_list,
*     COLUMN05
      column05_list type vrm_values,
      column05_value like line of column05_list,
*     Column06
      column06_list type vrm_values,
      column06_value like line of column06_list,
*     COLUMN07
      column07_list type vrm_values,
      column07_value like line of column07_list,
*     COLUMN08
      column08_list type vrm_values,
      column08_value like line of column08_list,
*     COLUMN09
      column09_list type vrm_values,
      column09_value like line of column09_list,
*     COLUMN10
      column10_list type vrm_values,
      column10_value like line of column10_list.

data: begin of it_objek occurs 0,
        objek type ausp-objek,
      end of it_objek ,
      ok_code type sy-ucomm.

data:  wa_init_flg_app244.
