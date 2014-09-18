*&---------------------------------------------------------------------*
*& Include YAPP803L_ZTPPWOSUM_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zapp240_vm_spec_per_progress  MESSAGE-ID zmpp    .

CONTROLS: tc_app240 TYPE TABLEVIEW USING SCREEN 110.

TABLES: ausp,  "Characteristic Values
        cabn.  "Characteristic

DATA: BEGIN OF it_app240 OCCURS 0,
        serial TYPE ausp-atwrt,  "P_RPxx_SERIAL(06)
        bodyno TYPE ausp-atwrt,  "P_MODEL & P_BODY_SERIAL(09)
        wono TYPE ausp-atwrt,    "P_WORK_ORDER(14)
        mi TYPE ausp-atwrt,                                 " P_MI(07)
        ocn TYPE ausp-atwrt,                                " P_OCN(04)
        ver TYPE ausp-atwrt,     "P_VERSION(03)
        extc TYPE ausp-atwrt,    "P_EXT_COLOR(03)
        intc TYPE ausp-atwrt,    "P_INT_COLOR(03)
        alc TYPE ausp-atwrt,     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
        eng TYPE ausp-atwrt,     "P_219_9(02)
        tm TYPE ausp-atwrt,      "P_219_7(02)
        tl TYPE ausp-atwrt,      "P_219_5(02)
        objek TYPE ausp-objek,
      END OF it_app240.

DATA: BEGIN OF it_excel_app240 OCCURS 0,
        serial(10),  "P_RPxx_SERIAL(06)
        bodyno(10),  "P_MODEL & P_BODY_SERIAL(09)
        wono(15),    "P_WORK_ORDER(14)
        mi(07),                                 "P_MI (07)
        ocn(04),                                "P_OCN (04)
        ver(07),     "P_VERSION(03)
        extc(20),    "P_EXT_COLOR(03)
        intc(20),    "P_INT_COLOR(03)
        alc(05),     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
        eng(10),     "P_219_9(02)
        tm(05),      "P_219_7(02)
        tl(05),      "P_219_5(02)
      END OF it_excel_app240.

* Parameters(Screen0110)
DATA: P_COMPANY_APP240(04),
      P_PLANT_APP240(08),  "P_TRIM_PLANT_NO
      P_MODEL_APP240(03),  "P_MODEL
      P_LINE_APP240(03),   "P_TRIM_LINE_NO
      P_PROG_APP240(08),   "P_RP_STATUS
      P_PART_APP240(10),   "U or C
      P_COLUMN_APP240(03), "P_ALC_U_xxx OR P_ALC_C_xxx
      P_BODYNO_APP240(07), "P_BODY_SERIAL
      P_WONO_APP240(14),   "P_WORK_ORDER
      P_EXTC_APP240(03),   "P_EXT_COLOR
      P_INTC_APP240(03).   "P_INT_COLOR

* DROPDOWN LIST for Parameters
TYPE-POOLS: vrm.
DATA: name        TYPE vrm_id,
*     Plant
      plant_list  TYPE vrm_values,
      plant_value LIKE LINE OF plant_list,
*     Model
      model_list  TYPE vrm_values,
      model_value LIKE LINE OF model_list,
*     Line
      line_list  TYPE vrm_values,
      line_value LIKE LINE OF line_list,
*     Progress
      progress_list  TYPE vrm_values,
      progress_value LIKE LINE OF progress_list,
*     Part - U or C
      part_list TYPE vrm_values,
      part_value LIKE LINE OF progress_list,
*     Column
      column_list TYPE vrm_values,
      column_value LIKE LINE OF column_list,
*     Body No.
      bodyno_list TYPE vrm_values,
      bodyno_value LIKE LINE OF bodyno_list,
*     Work Order
      wono_list TYPE vrm_values,
      wono_value LIKE LINE OF wono_list,
*     External Color
      extc_list TYPE vrm_values,
      extc_value LIKE LINE OF extc_list,
*     Internal Color
      intc_list TYPE vrm_values,
      intc_value LIKE LINE OF intc_list.

RANGES: " R_company FOR P_COMPANY_APP240,
        R_PLANT_APP240 FOR ausp-atwrt,   "P_TRIM_PLANT_NO
        R_MODEL_APP240 FOR ausp-atwrt,   "P_MODEL
        R_LINE_APP240 FOR ausp-atwrt,    "P_TRIM_LINE_NO
        R_PROG_APP240 FOR ausp-atwrt,    "P_RP_STATUS
        R_PART_APP240 FOR ausp-atwrt,    "U or C
        R_COLUMN_APP240 FOR ausp-atwrt,  "P_ALC_U_xxx OR P_ALC_C_xxx
        R_BODYNO_APP240 FOR ausp-atwrt,  "P_BODY_SERIAL
        R_WONO_APP240 FOR ausp-atwrt,    "P_WORK_ORDER
        R_EXTC_APP240 FOR ausp-atwrt,    "P_EXT_COLOR
        R_INTC_APP240 FOR ausp-atwrt.    "P_INT_COLOR

DATA: BEGIN OF it_objek OCCURS 0,
        objek TYPE ausp-objek,
      END OF it_objek ,
      ok_code TYPE sy-ucomm.

DATA:  wa_init_flg_app240.
