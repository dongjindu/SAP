************************************************************************
* Program Name      : ZEMMPM35E_SPE_MANAGE
* Created by        : Min-su Park
* Created on        : 2003.10.21.
* Pattern           :
* Description       :  Manage Error Standard Price for Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.22.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*----------------------------------------------------------------------*
***INCLUDE ZMINSUPBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
   SET PF-STATUS 'MENU'.
*  SET TITLEBAR 'xxx'.
   DESCRIBE TABLE IT_ZTMM_SPE LINES TC_SPE-LINES.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_ADJUST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_ADJUST OUTPUT.
*Get number of lines visible in table
    W_LOOPC = SY-LOOPC.
    MOVE-CORRESPONDING IT_ZTMM_SPE TO ZTMM_SPE.
ENDMODULE.                 " SCREEN_ADJUST  OUTPUT
