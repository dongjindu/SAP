************************************************************************
* Program Name  : ZEMMPM29E_CD_CHANGE_SA
* Created  by   : Min-su Park
* Creation on   : 2003.10.27.
* Pattern       :
* Description   : Condition change in Schedule Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.19.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM29E_CD_CHANGE_SAEVT                                  *
*----------------------------------------------------------------------*

**---
AT SELECTION-SCREEN.
  PERFORM get_data.


**---
START-OF-SELECTION.
  PERFORM condition_change.


**---
END-OF-SELECTION.
  PERFORM write_data.

**---
