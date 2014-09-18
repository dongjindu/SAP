************************************************************************
* Program Name      : ZACO19U_SHOP
* Author            : Hyung Jin Youn
* Creation Date     : 14/04/2004
* Specifications By : Hae Sung Cho
* Pattern           : Report 1-1
* Development Request No:UD1K909481
* Add documentation :
* Description       : Making Cost data which analysed by SHOP and Cost
*                     Component - Actaul
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT zaco19u_shop  MESSAGE-ID zmco.

* For TOP include
INCLUDE zaco19l_1top.
* For Sub-Rutine
INCLUDE zaco19l_f001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Set Record Type / Routing Usage
  PERFORM set_date.
  PERFORM set_rec_type_r_usage.

AT SELECTION-SCREEN OUTPUT.
* Modify Screen Att.
  PERFORM mod_screen_att.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM read_tka01.
* Read CCTRs in SHOP
  PERFORM read_cctr_in_shop.
* Read Base Information
  PERFORM read_base_info.
* CALCULATE % (KSBT)
  PERFORM cal_percent_using_ksbt.
* Read PCC data
  PERFORM read_pcc_data.
* Material Data
  PERFORM put_shop_mat_info.
* Read Scrap/Wip
  PERFORM read_scrap_wip_qty.
* read Additional Issue
  PERFORM read_abispost.
* SHOP information By Item Category. (Actual)
  PERFORM set_shop_by_item_cate.
* Delete DB for New Records
  PERFORM del_data_fr_ztco_shopcost_at.

**// Mod. By Hyung Jin Youn
* 1. Add Actual Manufacture Information
* 2. Adjust fractions
* Set Period
  PERFORM set_per_bf_up.
* Cal. Actual Manufacture qty. and Amt.
  PERFORM cal_act_manu_data.
* Disposal of Fractional Amount
  PERFORM disp_fraction_amt.
  PERFORM disp_fraction_by_cate.
  PERFORM disp_fraction_by_mlcc.
**// End of Mod.

* Update/Insert
  PERFORM update_ztco_shopcost_at.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Insert/Update Log.
  PERFORM up_ins_log.


*
