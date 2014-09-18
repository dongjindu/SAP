************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM10E_ASSIGN_BLANKEVENT                                *
*----------------------------------------------------------------------*
INITIALIZATION.

*Make Templete for comparing between steel mat and blank mat.
  PERFORM get_steel_mat.

*Get BLANK mat.
  PERFORM get_data.

START-OF-SELECTION.
  CALL SCREEN 100.
