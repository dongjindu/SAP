************************************************************************
* Program name : ZEMMGM01E_BOM_REG                                     *
* Created by   : Min-su Park                                           *
* Created on   : 2003.11.10.                                           *
* Pattern      :                                                       *
* Description  : BOM Registration Request Program                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.10.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZEMMGM01E_BOM_REGPAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
  w_fcode = ok_code.
  CLEAR ok_code.
  CASE w_fcode.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_fcode = ok_code.
  CASE w_fcode.
    WHEN 'DISP'.
      PERFORM display.
    WHEN 'SAVE'.
      PERFORM save.
    WHEN 'P--' OR 'P-' OR 'P++' OR 'P+'.
      PERFORM page_control.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_modify INPUT.
  CASE w_sel.
    WHEN r1. "Request BOM
      PERFORM request_bom_chk.

    WHEN r3. "Manage Sub Part
  ENDCASE.
  MODIFY it_end_part INDEX tc_end_part-current_line.
  IF sy-subrc <> 0.
    APPEND it_end_part.
  ENDIF.
ENDMODULE.                 " SCREEN_MODIFY  INPUT
