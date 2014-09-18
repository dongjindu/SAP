************************************************************************
* Program Name : ZEMMPM38E_MESSAGE
* Created by   : Min-su Park
* Created on   : 2003.11.05.
* Pattern      :
* Description  : PO Warning message for Pallet Qty
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.05.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZEMMPM38E_MESSAGE                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZEMMPM38E_MESSAGE  MESSAGE-ID ZMMM .

PARAMETERS : P_MSG(10).

START-OF-SELECTION.

MESSAGE I017 WITH P_MSG.
