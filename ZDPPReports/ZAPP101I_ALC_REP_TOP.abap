*&---------------------------------------------------------------------*
*& Include YAPP101L_ALC_REP_TOP                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*REPORT  yapp101m_alc_reprocess   MESSAGE-ID zmpp    .


TABLES: ztpp_wosum,
        ztpp_alc_rerun.  "ZTPP_SALES_WO Error Data Table

DATA: BEGIN OF it_app101 OCCURS 0.
DATA:   MARK,
        WORKORDER(14).
        INCLUDE STRUCTURE ztpp_alc_rerun.
data:   p_flag               type c,
        wo                   type c.
DATA: END OF it_app101.

data: begin of it_sys_message occurs 0,
        text(120),
      end of it_sys_message.

data: it_msg                 like table of bdcmsgcoll  with header line,
      it_bdcdata             like table of bdcdata     with header line,
      wa_mode                type c value 'N',
      wa_date                like sy-datum   ,
      wa_cnt                 type i       ,
      wa_flg                 type c       ,
      wa_error               type c       ,
      wa_wkdate              like sy-datum.

DATA: BEGIN OF it_excel OCCURS 0,
        col01(20),  "WORK ORDER serial
        col02(20),  "Nation
        col03(20),  "Dealer
        col04(20),  "Exterior Color
        col05(20),  "Interior Color
        col06(20),  "Model Year
        col07(20),  "Model Index
        col08(20),  "OCN
        col09(20),  "Version
        col10(20),  "Initial order QTY
        col11(20),  "Modification order QTY
        col12(20),  "REQUESTED DATE
        col13(20),  "CREATED DATE
        col14(20),  "CHANGED DATE
        col15(20),  "Destination Country
        col16(20),  "L/C COUNT
        col17(20),  "L/C No
        col18(20),  "Region-Port
        col19(20),  "Order Zone
        col20(20),                                          "219 values
      END OF it_excel.

CONTROLS: tc_app101 TYPE TABLEVIEW USING SCREEN 110,
          TC_APP101_T TYPE TABLEVIEW USING SCREEN 111.

* Parameters
DATA: p_company(04),    "Default Value = 'HMMA'
      p_workorder(14),  "Work Order = wo_ser + nation + dealer.
*      P_WO_SER TYPE ZTPP_ALC_RERUN-WO_SER,
*      P_NATION TYPE ZTPP_ALC_RERUN-NATION,
*      P_DEALER TYPE ZTPP_ALC_RERUN-DEALER,
      p_extc TYPE ztpp_alc_rerun-extc,  "External Color
      p_intc TYPE ztpp_alc_rerun-intc,  "Internal Color
      p_reqdate_st TYPE ztpp_alc_rerun-req_date,  "Request Date
      p_reqdate_en TYPE ztpp_alc_rerun-req_date.  "Request Date

TYPE-POOLS: vrm.

* DROPDOWN LIST for Parameter
* P_workorder(wo_ser + nation + dealer)
DATA: name        TYPE vrm_id,
      workorder_list  TYPE vrm_values,
      workorder_value LIKE LINE OF workorder_list.
RANGES: r_workorder FOR p_workorder.

DATA  wa_init_flg.
DATA: WA_TOG_FLG .

DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE sy-ucomm.
