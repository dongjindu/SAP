************************************************************************
* Program Name      : ZACO18U_VRVA
* Author            : Hyung Jin Youn
* Creation Date     : 11/08/2003
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: UD1K901886
* Add documentation :
* Description       : Update the Variable Amount per period
*                     by the change rate of AT quantity
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
* Comments      : Target and Original version should be created before
REPORT ZACO18U_VRVA MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO18L_1TOP.
* For Sub-Rutine (DATA)
INCLUDE ZACO18L_F001.
* For Sub-Rutine (Report/Posting)
INCLUDE ZACO18L_F002.


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
* Check Input Value
  PERFORM CHK_INPUT_VALUE.

* Searching for Cost element group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_CEGRP.
  PERFORM READ_CEGRP_GROUP USING '0102'
                                 P_CEGRP.

* Searching for Cost Center group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CEGRP_GROUP USING '0101'
                                 P_NCOAL.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Read CCtrs from 'HMMA'. All CCtrs should be considered.
  PERFORM READ_CCTRS_FROM_HMMA.
* Read Cost Elements
  PERFORM READ_COST_ELEMENT.
* Read OBJ Key Combination
  PERFORM READ_COM_OBJ.
* Read Quantity Information from COSS
  PERFORM READ_QUAN_INFO.
* Building Allocation Factor From Quantity Data
  PERFORM MAKING_ALL_FAC.
* Read STD data from COSP
  PERFORM READ_AMT_DATA.
* Preparation of Report
  PERFORM PRE_TO_REPORT.
* Preparing report
  PERFORM PRE_REPORT_ADJ.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*
