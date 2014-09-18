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

*&---------------------------------------------------------------------*
*& Include ZEMMPM29E_CD_CHANGE_SATOP                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zemmpm27e_cd_change_sa MESSAGE-ID zmmm.

**---
TABLES : ekko.


**---
DATA : BEGIN OF it_a017 OCCURS 0.
        INCLUDE STRUCTURE a017.
DATA : END OF it_a017.

DATA : BEGIN OF it_po_detail OCCURS 0.
        INCLUDE STRUCTURE zvmm_po_detail1.
DATA : END OF it_po_detail.

*--- BDC_DATA
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_message.

DATA : wa_tmp_po_detail LIKE zvmm_po_detail1,
       w_item_index TYPE i,
       w_status VALUE 'N'.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
PARAMETERS : p_datum TYPE d DEFAULT sy-datum.
*SELECTION-SCREEN ULINE.
*PARAMETERS : p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block1.


**---
