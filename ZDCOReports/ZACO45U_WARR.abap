************************************************************************
* Program Name      : ZACO45U_WARR
* Author            : Hyung Jin Youn
* Creation Date     : 2004.01.30.
* Specifications By : Jin Won Hong
* Pattern           : Report 1-1
* Development Request No : UD1K906626
* Addl Documentation:
* Description       : reflect the payment of Warranty & Campaign to
*                     CO-PA
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZACO45U_WARR MESSAGE-ID ZMCO.

* For TOP include
INCLUDE ZACO45L_1TOP.
* For Sub-routine
INCLUDE ZACO45L_F001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
* Building Field Cat.
  PERFORM FIELDCAT_INIT .
  PERFORM DR_LIST_FOR_ACC.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN OUTPUT.
* List Box
*  PERFORM DR_LIST_FOR_ACC.
* Block Input
  PERFORM BLOCK_INPUT.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM READ_TKA01.
* Posting Date -> Last Date of Month
  PERFORM CAL_BUDAT.
* Read data from ZTCO_WARR
  PERFORM READ_ZTCO_WARR.
* Read Billing data
  PERFORM READ_BILLING_DOC.
* Read Condition and Amt.
  PERFORM READ_CON_AMT.
* Collect data
  PERFORM COLLECT_DATA.
* Building Tab for posting
  PERFORM BUILDING_TAB_FOR_POSTING.
* Calulate data for Vehicle Type 'COMMON' if it is ...
  PERFORM CAL_COMMON_VEH_TYPE.
* Preparing LIST
  PERFORM PRE_LIST_DATA.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.


*
