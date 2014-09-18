*&---------------------------------------------------------------------*
*& Include YAPP802L_ZTPPSTATUS_MNTNC_TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

program  zapp802m_ztppstatus_mntnc  message-id zmpp    .

controls: tc_app802 type tableview using screen 110.

tables: ztpp_status.  "Status ID Mapping Between Legarcy and SAP

data: begin of it_app802 occurs 0.
        include structure ztpp_status.
data:   new,
        upd,
        mark,
        line type sy-tabix,
      end of it_app802.
data: it_upd like table of  it_app802 with header line.
data: it_del like table of  it_app802 with header line.
data: begin of it_excel occurs 0,
        col01(20),  "Serial
        col02(20),  "Body_NO
        col03(20),  "Current RP
        col04(20),  "VIN
        col05(20),  "ORDER_NO(ORDNO + NATION + DEALER)
        col06(20),  "Ext.Color
        col07(20),  "Int.Color
        col08(20),  "Spec
        col09(20),  "OCN
        col10(20),  "Date
        col11(20),  "Bef.OrdNo
        col12(20),  "Bef.Nation
        col13(20),  "Bef.Dealer
        col14(20),  "Bef.Ext.Color
        col15(20),  "Bef.Ext.Color
        col16(20),  "Bef.Spec
        col17(20),  "Bef.OCN
        col18(20),  "Bef.VIN
      end of it_excel.

* Parameters
data: p_id(04),
      p_wc(08),  "Work Center(arbpl)
      p_ck(01),  "Control Key(bf_usage)
      p_sa(10),  "Supply Area(prvbe)
      p_ss(02),  "Sort String(rp_point)
      p_bf(20).  "Backflush point(usr01)

* DROPDOWN LIST for Parameter
type-pools: vrm.
data: name        type vrm_id,
*     ID
      id_list  type vrm_values,
      id_value like line of id_list,
*     Work Center
      wc_list  type vrm_values,
      wc_value like line of wc_list,
*     Control Key
      ck_list  type vrm_values,
      ck_value like line of ck_list,
*     Supply Area
      sa_list  type vrm_values,
      sa_value like line of sa_list,
*     Sort String
      ss_list  type vrm_values,
      ss_value like line of ss_list,
*     Backflush Point
      bf_list  type vrm_values,
      bf_value like line of bf_list.

* For BDC
data: it_msg_tab like bdcmsgcoll occurs 0 with header line.
data: begin of it_fbdcdata occurs 0.
        include structure bdcdata.
data: end of it_fbdcdata.
data: nodata.

data: ok_code type sy-ucomm,
      save_ok type sy-ucomm.

data: wa_current_line type i.

data:  wa_init_flg,
       wa_ins_flg,
       wa_del_flg,
       wa_upd_flg.
