************************************************************************
* Program Name      : ZACO38U_DPSC
* Author            : Hyung Jin Youn
* Creation Date     : 08/09/2003
* Specifications By : Jin-Won Hong
* Pattern           : Report 1-1
* Development Request No: UD1K902180
* Add documentation :
* Description       : Subtractiing Planned Dep. Cost from CCA
*                     before Making Planned STD price from Planned
*                     depreciation cost.
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO38U_DPSC MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO38L_1TOP.
* For Sub-routine
INCLUDE ZACO38L_F001.
* For ALV report and events
INCLUDE ZACO38L_F002.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* For ALV
  G_REPID = SY-REPID.
* Set  Init Value
  PERFORM SET_INIT_VAL.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check CCtr Group Hierarchy
  PERFORM CHECK_CCTRGROUP_HI.
* Check period range
  PERFORM CHECK_PERIOD_RANGE.
* Check Vehicle Model
  PERFORM CHECK_VEH_MODEL.
* Check Scenario Version
  PERFORM SEARCH_LTP_VR.
* Check Requirement Type
  PERFORM CHECK_BEDAE.

* Check Version (LTP)
AT SELECTION-SCREEN ON P_VERSB.
  PERFORM CHECK_VERSB.

* Searching for CCtr group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CCTR_GROUP.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Searching LTP version
  PERFORM SEARCH_LTP_VR.
* Screening Vehicle model without LTP DATA
  PERFORM REMV_VM_WO_LTP.
* Read Vehicle Model DATA
  PERFORM READ_ZTCO_VEHI_TYPE.
* Enqueue
  PERFORM ENQUEUE_ZTCO_PLANDE.
* Read Chart of Account/Asset Master Data (Vehicle Type)
  PERFORM READ_CHART_ACC.
* Read CCtrs for retrieval.
  PERFORM READ_CCTRS.
* Read Depreciation COST (PLAN Version)
  PERFORM READ_DEP_PLAN_COST.
* Rid off without Activity quantity.
  PERFORM CHK_LTP_AT_QUAN.
* Update Planned Dep. data to ZTCO_PLANDEP
  PERFORM UPDATE_TO_ZTCO_PLANDEP.
* Building Field Cat.
  PERFORM FIELDCAT_INIT .
* Preparing report
  PERFORM PRE_REPORT_ADJ .
* Dequeue will be triggered by BAPI FM
* Dequeue

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.
