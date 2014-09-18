************************************************************************
* Program Name      : ZACO39U_DPSC
* Author            : Hyung Jin Youn
* Creation Date     : 23/09/2003
* Specifications By : Jin-Won Hong
* Pattern           : Report 1-1
* Development Request No: UD1K902281
* Add documentation :
* Description       : Making Standard Unit price from Planned
*                     depreciation cost. Using LTP data for allocation
*                     factors
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO39U_DPSC MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO39L_1TOP.
* For Sub-routine
INCLUDE ZACO39L_F001.
* For Sub-routine for ALV / POSTING
INCLUDE ZACO39L_F002.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
* Set  Init Value
  PERFORM SET_INIT_VAL.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check period range
  PERFORM CHECK_PERIOD_RANGE.
* Check Scenario Version
  PERFORM SEARCH_LTP_VR.
* Check Requirement Type
  PERFORM CHECK_BEDAE.

* Check Version (LTP)
AT SELECTION-SCREEN ON P_VERSB.
  PERFORM CHECK_VERSB.

* Change Plant or AT ?
AT SELECTION-SCREEN ON P_CHG_C.

* Change Screen ATTR.
AT SELECTION-SCREEN OUTPUT.
  PERFORM CHANGE_SCR_ATTR.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Controlling Area Information
  PERFORM READ_TKA01.
* Searching LTP version
  PERFORM SEARCH_LTP_VR.
* Read Vehicle Model DATA
  PERFORM READ_ZTCO_VEHI_TYPE.
* Making KSPP_DATA & AT
  PERFORM MAKING_KSPP_N_AT.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Enqueue
  PERFORM ENQUEUE_ZTCO_PLANDE.
* Read Source data
  PERFORM READ_FR_ZTCO_PLANDEP.
* Read LTP Quantity
  PERFORM READ_LTP_VEH_QUANTITY.
* Read AT quantity in LTP
*  PERFORM READ_LTP_AT_QUAN.
  PERFORM READ_LTP_AT_QUAN_2.
* Re-organizing data
  PERFORM RE_ORG_DATA.
* Building POST data
  PERFORM BUILD_POST_DATA.
* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.
* Enqueue (when Dialogue is changed)


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.
