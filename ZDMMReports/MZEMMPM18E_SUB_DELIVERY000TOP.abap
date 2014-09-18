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
*&---------------------------------------------------------------------*
*& Include MZEMMPM18E_SUB_DELIVERY000TOP                               *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzemmpm18e_sub_delivery000 MESSAGE-ID zmmm.
*Data Definition.

*Screen
TABLES : ztmm_delisch.
CONTROLS tc_ztmm_delisch TYPE TABLEVIEW USING SCREEN 100.
DATA   : w_ok_code LIKE sy-ucomm,
         w_fcode LIKE sy-ucomm.

*Internal Table
DATA   : BEGIN OF it_ztmm_delisch OCCURS 0.
        INCLUDE STRUCTURE ztmm_delisch  .
DATA   :  mark                            ,
         END OF it_ztmm_delisch.

*General Variable
DATA   : w_firstchk_flg    ,
         w_status VALUE 'N',
         w_loopc LIKE sy-loopc.

*---
DATA : wa_ztmm_delisch LIKE ztmm_delisch.
