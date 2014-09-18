*&---------------------------------------------------------------------*
*& Include YAPP803L_ZTPPWOSUM_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp245r_prod_res_by_period  message-id zmpp    .

controls: tc_app245 type tableview using screen 110.

tables: ausp,  "Characteristic Values
        cabn.  "Characteristic

data: begin of it_temp_app245 occurs 0,
*
        objek  type ausp-objek,
*
        suminf type ausp-atwrt,
        extc   type ausp-atwrt,
        intc   type ausp-atwrt,
        date   type sy-datum,
      end of it_temp_app245 .

data: begin of it_app245 occurs 0,
*
        suminf type ausp-atwrt,         "Summary Type
        wono type ausp-atwrt,
        extc type ausp-atwrt,      "P_EXT_COLOR(03)
        intc type ausp-atwrt,      "P_INT_COLOR(03)
*        objek TYPE ausp-objek,
*
        total(05) type p,          "Total Quantity
*
        01qty(05) type p,          "The First Day's QTY
        02qty(05) type p,          "The Second Day's QTY
        03qty(05) type p,
        04qty(05) type p,
        05qty(05) type p,
        06qty(05) type p,
        07qty(05) type p,
        08qty(05) type p,
        09qty(05) type p,
        10qty(05) type p,
        11qty(05) type p,
        12qty(05) type p,
        13qty(05) type p,
        14qty(05) type p,
        15qty(05) type p,
        16qty(05) type p,
        17qty(05) type p,
        18qty(05) type p,
        19qty(05) type p,
        20qty(05) type p,
        21qty(05) type p,
        22qty(05) type p,
        23qty(05) type p,
        24qty(05) type p,
        25qty(05) type p,
        26qty(05) type p,
        27qty(05) type p,
        28qty(05) type p,
        29qty(05) type p,
        30qty(05) type p,
        31qty(05) type p,
*
      end of it_app245.

data: begin of it_excel_app245 occurs 0,
*
        suminf type ausp-atwrt,    "Summary Type
        extc type ausp-atwrt,      "P_EXT_COLOR(03)
        intc type ausp-atwrt,      "P_INT_COLOR(03)
*
        total(20) ,          "Total Quantity
*
        01qty(20) ,          "The First Day's QTY
        02qty(20) ,          "The Second Day's QTY
        03qty(20) ,
        04qty(20) ,
        05qty(20) ,
        06qty(20) ,
        07qty(20) ,
        08qty(20) ,
        09qty(20) ,
        10qty(20) ,
        11qty(20) ,
        12qty(20) ,
        13qty(20) ,
        14qty(20) ,
        15qty(20) ,
        16qty(20) ,
        17qty(20) ,
        18qty(20) ,
        19qty(20) ,
        20qty(20) ,
        21qty(20) ,
        22qty(20) ,
        23qty(20) ,
        24qty(20) ,
        25qty(20) ,
        26qty(20) ,
        27qty(20) ,
        28qty(20) ,
        29qty(20) ,
        30qty(20) ,
        31qty(20) ,
*
      end of it_excel_app245.

* Parameters(Screen0110)
data: p_company_app245(04),
      p_plant_app245(08),                 "P_TRIM_PLANT_NO
      p_model_app245(03),                 "P_MODEL
      p_line_app245(03),                  "P_TRIM_LINE_NO
      p_prog_app245(08),                  "P_RP_STATUS
      p_wono_app245(14),                  "P_WORK_ORDER
      p_extc_app245(03),                  "P_EXT_COLOR
      p_intc_app245(03),                  "P_INT_COLOR
*
      p_type_app245(02),  "Summary Type 1:Order No. 2:Option
      "Whether or not Color is... O: It is, X: It is not.
      p_color_app245(02),
      p_shop_date_app245 type sy-datum,   "P_RPxx_SHOP_DATE
      "The Ending Date of Shop Date
      p_end_date_app245(02) type n,
*     "P_219_xxx
      p_column01_app245(03),
      p_column02_app245(03),
      p_column03_app245(03),
      p_column04_app245(03),
      p_column05_app245(03),
      p_column06_app245(03),
      p_column07_app245(03),
      p_column08_app245(03),
      p_column09_app245(03),
      p_column10_app245(03),
      p_value01_app245 type ausp-atwrt,
      p_value02_app245 type ausp-atwrt,
      p_value03_app245 type ausp-atwrt,
      p_value04_app245 type ausp-atwrt,
      p_value05_app245 type ausp-atwrt,
      p_value06_app245 type ausp-atwrt,
      p_value07_app245 type ausp-atwrt,
      p_value08_app245 type ausp-atwrt,
      p_value09_app245 type ausp-atwrt,
      p_value10_app245 type ausp-atwrt.

* DROPDOWN LIST for Parameters
type-pools: vrm.
data: name        type vrm_id,
*     Plant
      plant_list  type vrm_values,
      plant_value like line of plant_list,
*     Model
      model_list  type vrm_values,
      model_value like line of model_list,
*     Line
      line_list  type vrm_values,
      line_value like line of line_list,
*     Progress
      progress_list  type vrm_values,
      progress_value like line of progress_list,
*     Color : O or X
      color_list  type vrm_values,
      color_value like line of color_list,
*     Summary Type
      type_list  type vrm_values,
      type_value like line of type_list,
*     Ending Date
      endate_list  type vrm_values,
      endate_value like line of endate_list,
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
data: begin of it_date occurs 0,
        date type sy-datum,
        num(02) type n,
      end of it_date.
data: p_d01_app245(02) type n,
      p_d02_app245(02) type n,
      p_d03_app245(02) type n,
      p_d04_app245(02) type n,
      p_d05_app245(02) type n,
      p_d06_app245(02) type n,
      p_d07_app245(02) type n,
      p_d08_app245(02) type n,
      p_d09_app245(02) type n,
      p_d10_app245(02) type n,
      p_d11_app245(02) type n,
      p_d12_app245(02) type n,
      p_d13_app245(02) type n,
      p_d14_app245(02) type n,
      p_d15_app245(02) type n,
      p_d16_app245(02) type n,
      p_d17_app245(02) type n,
      p_d18_app245(02) type n,
      p_d19_app245(02) type n,
      p_d20_app245(02) type n,
      p_d21_app245(02) type n,
      p_d22_app245(02) type n,
      p_d23_app245(02) type n,
      p_d24_app245(02) type n,
      p_d25_app245(02) type n,
      p_d26_app245(02) type n,
      p_d27_app245(02) type n,
      p_d28_app245(02) type n,
      p_d29_app245(02) type n,
      p_d30_app245(02) type n,
      p_d31_app245(02) type n.

data:  wa_init_flg_app245.
