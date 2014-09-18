************************************************************************
* Program Name      : ZACO13U_SVAR
* Author            : Hyung Jin Youn
* Creation Date     : 07/06/2003
* Specifications By : Bong-Doo Moon
* Pattern           : Report 1-1
* Development Request No: UD1K910931
* Add documentation :
* Description       : Create data for Cost Variance Report (SHOP, FSC)
*
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO13U_SVAR NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

* Top Include
INCLUDE ZACO13U_1TOP.
* For Sub-Routine
INCLUDE ZACO13U_F001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Controlling Area Information
  PERFORM READ_TKA01.
* Read data (STD)
  PERFORM READ_FR_ZTCO_SHOPCOST.
* Read data (Actual)
  PERFORM READ_FR_ZTCO_SHOPCOST_AT.
* Cal. data
  PERFORM CAL_DATA.
* Delete DB for New Records
  PERFORM DEL_DATA_FR_ZTCO_SHOPVAR.
* Update/Insert
  PERFORM UPDATE_ZTCO_SHOPVAR.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Insert/Update Log.
  PERFORM UP_INS_LOG.
