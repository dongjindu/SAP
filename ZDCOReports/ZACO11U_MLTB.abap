************************************************************************
* Program Name      : ZACO11U_MLTB
* Author            : Hyung Jin Youn
* Creation Date     : 28/11/2003
* Specifications By : Bong-Doo Moon
* Pattern           : Report 1-1
* Development Request No: UD1K904510
* Add documentation :
* Description       : To make CBO GR/IR Table from Material Ledger
*
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO11U_MLTB MESSAGE-ID ZMCO.

* Top Include
INCLUDE ZACO11L_1TOP.
* For Sub-Routine
INCLUDE ZACO11L_F001.
* For Sub-Routine
INCLUDE ZACO11L_F002.
* For Local Class
* Include ZACO11L_LC01.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON P_PRONO.
* Check Work Process
  PERFORM CHECK_CUR_WPRO.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Check Background Job ?
  PERFORM CHECK_BATCH.
* Enqueue.
  PERFORM ENQUEUE_ZTCO_MATLEDGER.
* Read CCode/Chart of ACC./Plant/Valuation Area/
  PERFORM READ_GENERAL_INFORMATION.
* Read MVT Group.
  PERFORM READ_MVT_GRP.
* Read Process Catagory
  PERFORM READ_PROCESS_CATAGORY.
* Read Costestimate Number and Material Code
* PERFORM READ_MAT_KALNR.
* Read DATA from ML.
  PERFORM READ_ML_MAIN.
* Update
  PERFORM UPDATE_ZTCO_MATLEDGER.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.


*
