************************************************************************
*
** Program Name      : ZEMMPM35E_SPE_MANAGE
** Created by        : Min-su Park
** Created on        : 2003.10.21.
** Pattern           :
** Description       :  Manage Error Standard Price for Purchase
*Material
**
** Modification Logs
** Date            Developer        RequestNo      Description
** 2003.10.22.     Min-su Park    UD1K901873     Initial Coding
************************************************************************
*
*
**----------------------------------------------------------------------
*
****INCLUDE ZMINSUPAI .
**----------------------------------------------------------------------
*
**&---------------------------------------------------------------------
*
**&      Module  MODIFY_SCREEN  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE MODIFY_SCREEN INPUT.
*  MOVE-CORRESPONDING ZTMM_SPE TO IT_ZTMM_SPE.
**Modify Current line.
*  MODIFY IT_ZTMM_SPE INDEX TC_SPE-CURRENT_LINE.
*ENDMODULE.                 " MODIFY_SCREEN  INPUT
**&---------------------------------------------------------------------
*
**&      Module  EXIT_COMMAND  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE EXIT_COMMAND INPUT.
*  W_FCODE = OK_CODE.
*  CLEAR OK_CODE.
*  CASE W_FCODE.
**Move before screen.
*    WHEN 'BACK'. SET SCREEN 0.
*    WHEN 'EXIT' OR 'CANC'. LEAVE PROGRAM.
*  ENDCASE.
*ENDMODULE.                 " EXIT_COMMAND  INPUT
**&---------------------------------------------------------------------
*
**&      Module  USER_COMMAND_0100  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE USER_COMMAND_0100 INPUT.
*  W_FCODE = OK_CODE.
*  CLEAR OK_CODE.
*  CASE W_FCODE.
*    WHEN 'SAVE'  . PERFORM SAVE.
*    WHEN 'TRS'   .
*    WHEN 'SELALL'. PERFORM SELECT_ALL.
*    WHEN 'DSELAL'. PERFORM DESELECT_ALL.
*    WHEN 'P--' OR 'P-' OR 'P++' OR 'P+'.
*      PERFORM PAGE_CONTROL.
*    WHEN 'FIND' OR 'FIND+'.
*      PERFORM FIND_STRING.
*  ENDCASE.
*ENDMODULE.                 " USER_COMMAND_0100  INPUT
