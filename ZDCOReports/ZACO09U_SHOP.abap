************************************************************************
* Program Name      : ZACO09U_SHOP
* Author            : Hyung Jin Youn
* Creation Date     : 19/02/2004
* Specifications By : Hae Sung Cho
* Pattern           : Report 1-1
* Development Request No:UD1K908459
* Add documentation :
* Description       : Making Cost data which analysed by SHOP and Cost
*                     Component
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
* 01/11/2005 wskim issue #20041201-001 standard cost error :
************************************************************************
REPORT zaco09u_shop MESSAGE-ID zmco.

* For TOP include
INCLUDE zaco09l_1top.
* For Sub-Rutine
INCLUDE zaco09l_f001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Default
  PERFORM set_deafult.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Read Version / Costing Variant / Routing Usage
  PERFORM read_set_ver_cver.
* Check Input Values
  PERFORM chk_input_values.

AT SELECTION-SCREEN OUTPUT.
* Read Version / Costing Variant / Routing Usage
  PERFORM read_set_ver_cver.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP ra01.


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
* Read KEKO
  PERFORM read_keko.
* Read Cost Estimated Data
  PERFORM read_itemization.
* Put data to distinguish Shop, Material Data
  PERFORM put_shop_mat_info.
* SHOP information By Item Category.
  PERFORM set_shop_by_item_cate.
* Fractions to 'V' w/o SHOP, CE, CCTR
  PERFORM tre_fraction_to_v.
* Delete DB for New Records
  PERFORM del_data_fr_ztco_shopcost.
* Update/Insert
  PERFORM update_ztco_shopcost.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Insert/Update Log.
  PERFORM up_ins_log.
