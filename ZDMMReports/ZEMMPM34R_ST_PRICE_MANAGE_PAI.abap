************************************************************************
* Program Name   : ZEMMPM34R_ST_PRICE_MANAGE
* Created by     : Min-su Park
* Created on     : 2003.10.16.
* Pattern        :
* Description    :  Manage Standard Price for Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE ZEMMPM34R_ST_PRICE_MANAGE_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN INPUT.
 MODIFY IT_ERROR INDEX TC_ERROR-CURRENT_LINE.
ENDMODULE.                 " MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT_COMMAND INPUT.
FCODE = OK_CODE.
CLEAR OK_CODE.
CASE FCODE.
 WHEN 'EXIT' OR 'CANC'.
   LEAVE PROGRAM.
ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
FCODE = OK_CODE.
CLEAR OK_CODE.
CASE FCODE.
 WHEN 'SAVE'.
*  PERFORM SAVE.
ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
