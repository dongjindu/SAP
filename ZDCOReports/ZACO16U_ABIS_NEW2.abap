************************************************************************
* Program Name      : ZACO16U_ABIS
* Author            : Hyung Jin Youn
* Creation Date     : 04/11/2003
* Specifications By : Deok-Kie Lee
* Pattern           : Report 1-1
* Development Request No: UD1K903655
* Add documentation :
* Description       : Allocate the costs in Internal order- P001, E001
*                     to PCC by the rate of PCC cost -> Changed
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date       Developer       Request ID
* 16/04/2004 Hyung Jin Youn  UD1K909644
* Description
* Cost By Shop report requires the Additional Issue Data, so this
* program should be revised and changed to be fit to the change of
* Business Process - To PCC by the rate of PCC Quantity Base
* Date      Developer      Request            Description
* 06/06/06  Manju          UD1K920991         Rounding off changes
************************************************************************
REPORT zaco16u_abis MESSAGE-ID zmco.

* For TOP include
INCLUDE ZACO16L_2TOP.
*INCLUDE zaco16l_1top.
* For Sub-Routine
INCLUDE ZACO16L_F002.
*INCLUDE zaco16l_f001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
**// Mod. By Hyung Jin Youn 2004.08.05
* Do not use Default Values
* KSTAR, I/O, Material Type
*  PERFORM SELECT_INIT.
**// End of Mod.
* Set Material Type for Semi-Finished product and FSC
  PERFORM set_mtype_fk.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* information
  PERFORM confirm_message.

AT SELECTION-SCREEN OUTPUT.
* Check Post+Report/Report
  PERFORM ind_post_n_report.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM read_tka01.
* Enqueue
  PERFORM enqueue.

  IF p_post = space.
* Confirmation
    PERFORM conf_replace_data.
* KSTAR - ALPHA_NUMERIC
    PERFORM alpha_kstar.
* Read I/O data from CO document
    PERFORM read_io_data.
* Read Costs from B/F
    PERFORM read_b_f_data.

*// Mod. by Hyung Jin Youn 2004.04.19
* Cal. ratio by Qty. Base not By Cost Base
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
* PERFORM CAL_COST_RATIO.
    PERFORM cal_cost_ratio_02.
    PERFORM cal_qty_ratio_02.
*// End of Mod.

* Making itab to be posted
    PERFORM make_pcc_post_table.
* Adjustment
    PERFORM adjustment_data.
* Update result to CBO table
    PERFORM update_data_to_table .

  ENDIF.

* Posting
  PERFORM post_data.
* Let system Dequeue


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
