************************************************************************
* Program name : SAPMZEMMPM18E_SUB_DELIVERY000
* Created by   : Min-su Park
* Created on   : 2003.08.29.
* Pattern      :
* Description  : Sub-Daily Delivery Schedule z-Table
*
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.29.     Min-su Park      UD1K901873     Initial Coding       *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE MZEMMPM18E_SUB_DELIVERY000I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_fcode =  w_ok_code.
  CLEAR  w_ok_code.
  CASE  w_fcode.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE PROGRAM.
    WHEN 'CREATE'  .  PERFORM create.
    WHEN 'CHANGE'  .  PERFORM change.
    WHEN 'DISPLAY' .  PERFORM display.
    WHEN 'SAVE'    .  PERFORM save.
    WHEN 'DELETE'  .  PERFORM delete.
    WHEN 'SELALL'  .  PERFORM selall.
    WHEN 'DESELALL'.  PERFORM deselall.
    WHEN 'P--' OR 'P-' OR 'P++' OR 'P+'.
      PERFORM page_control.
    WHEN 'COPY'    .  PERFORM copy.
    WHEN OTHERS    .
  ENDCASE.
  PERFORM clear_0_time.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen INPUT.
  MOVE-CORRESPONDING ztmm_delisch TO it_ztmm_delisch.
  MODIFY it_ztmm_delisch INDEX tc_ztmm_delisch-current_line.
ENDMODULE.                 " MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  MARK_CHK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mark_chk INPUT.
  MODIFY it_ztmm_delisch INDEX tc_ztmm_delisch-current_line.
ENDMODULE.                 " MARK_CHK  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.
  w_fcode =  w_ok_code.
  CLEAR  w_ok_code.
  CASE  w_fcode.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'ADOPT'   .
      PERFORM adopt.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_0900  INPUT
