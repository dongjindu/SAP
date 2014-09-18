************************************************************************
* Program Name      : ZACO12U_DPDE
* Author            : Hyung Jin Youn
* Creation Date     : 06/08/2003
* Specifications By : Jin-Won Hong
* Pattern           : Report 1-1
* Development Request No: UD1K901848
* Add documentation :
* Description       :
*              This program makes data from Asset Depreciation costs
*              before posting the depreciation costs to PCC
*              This program selects data from Asset Depreciation table
*              and reorganize them through a distribution factor -
*              Vehicle Type in asset master data .
* the BDC structures for BATCH INPUT processing
*              IT_BDCDATA
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
* Comments      : Should check data of t-code 'ZACO12_01'-Vehicle (Car)
*                 type Mapping Master
REPORT ZACO12U_DPDE MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO12L_1TOP.
* For Sub-Rutine
INCLUDE ZACO12L_F001.
* For Sub-Rutine
INCLUDE ZACO12L_F002.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
* Building Field Cat.
  PERFORM FIELDCAT_INIT .


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Check CCtr Group Hierarchy
  PERFORM CHECK_CCTRGROUP_HI.

* Searching for CCtr group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CCTR_GROUP.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Read Chart of Account/Asset Master Data (Vehicle Type)
  PERFORM READ_CHART_ACC.
* Read CCtrs for retrieval.
  PERFORM READ_CCTRS.
* Read Depreciation Costs from Asset Value Table.
  PERFORM READ_ASS_DEP_COST.
* Building Material LIST to use in selecting data
  PERFORM BUILD_MAT_LIST.
* Read Data from PCC
  PERFORM READ_DATA_PCC.
* Calculating to make an Internal Table (For Report/Posting)
  PERFORM CAL_ALC_DEP_COST.
* Preparation to display report.
  PERFORM PRE_REPORT_ADJ.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.


*
