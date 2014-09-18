************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
INCLUDE zemmgm10e_assign_blanktop.
INCLUDE zemmgm10e_assign_blankf01.
INCLUDE zemmgm10e_assign_blankpbo.
INCLUDE zemmgm10e_assign_blankpai.

*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.

START-OF-SELECTION.  "event block for creating lists
*Make Templete for comparing between steel mat and blank mat.
  PERFORM get_steel_mat.

*Get BLANK mat.
  PERFORM get_data.

  CALL SCREEN 100.
