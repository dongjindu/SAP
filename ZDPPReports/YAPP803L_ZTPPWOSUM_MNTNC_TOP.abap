*&---------------------------------------------------------------------*
*& Include YAPP803L_ZTPPWOSUM_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp803m_ztppwosum_mntnc  message-id zmpp    .

controls: tc_app803 type tableview using screen 110.

tables: ztpp_wosum.  "ERP_WO QTY SUMMARY

data: begin of it_app803 occurs 0.
        include structure ztpp_wosum.
data:   new,
        upd,
        mark,
        line type sy-tabix,
      end of it_app803.
data: it_upd like table of  it_app803 with header line.
data: it_del like table of  it_app803 with header line.
*data: begin of it_excel occurs 0,
*        col01(20),  "Serial
*        col02(20),  "Body_NO
*        col03(20),  "Current RP
*        col04(20),  "VIN
*        col05(20),  "ORDER_NO(ORDNO + NATION + DEALER)
*        col06(20),  "Ext.Color
*        col07(20),  "Int.Color
*        col08(20),  "Spec
*        col09(20),  "OCN
*        col10(20),  "Date
*        col11(20),  "Bef.OrdNo
*        col12(20),  "Bef.Nation
*        col13(20),  "Bef.Dealer
*        col14(20),  "Bef.Ext.Color
*        col15(20),  "Bef.Ext.Color
*        col16(20),  "Bef.Spec
*        col17(20),  "Bef.OCN
*        col18(20),  "Bef.VIN
*      end of it_excel.

* Parameters(Screen0110)
data: p_wo_ser type ztpp_wosum-wo_ser,  "Work Order
      p_nation type ztpp_wosum-nation,  "Nation
      p_dealer type ztpp_wosum-dealer,  "Dealer
      p_extc type ztpp_wosum-extc,  "External Color
      p_intc type ztpp_wosum-intc.  "Internal Color
* Parameters(Screen0120)
data: p_wo_ser02 type ztpp_wosum-wo_ser,  "Work Order
      p_nation02 type ztpp_wosum-nation,  "Nation
      p_dealer02 type ztpp_wosum-dealer,  "Dealer
      p_extc02 type ztpp_wosum-extc,  "External Color
      p_intc02 type ztpp_wosum-intc.  "Internal Color

* DROPDOWN LIST for Parameter
type-pools: vrm.
data: name        type vrm_id,
*     WORK ORDER(09)
      wo_ser_list  type vrm_values,
      wo_ser_value like line of wo_ser_list,
*     Nation(03)
      nation_list  type vrm_values,
      nation_value like line of nation_list,
*     Dealer(02)
      dealer_list  type vrm_values,
      dealer_value like line of dealer_list,
*     Exterior Color(03)
      extc_list  type vrm_values,
      extc_value like line of extc_list,
*     Interior Color(03)
      intc_list  type vrm_values,
      intc_value like line of intc_list.

data: ok_code type sy-ucomm,
      save_ok type sy-ucomm.

data: wa_current_line type i,
      tf_count(30) .

field-symbols: <f1> type any.

data:  wa_flag,
       wa_init_flg,
       wa_ins_flg,
       wa_del_flg,
       wa_upd_flg.
