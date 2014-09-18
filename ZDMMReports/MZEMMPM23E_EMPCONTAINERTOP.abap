************************************************************************
* Program name : SAPMZEMMPM23E_EMPCONTAINER
* Created by   : Min-su Park
* Created on   : 2003.09.25.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.09.19.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*& Include MZEMMPM23E_EMPCONTAINERTOP                                  *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzemmpm23e_empcontainer MESSAGE-ID zmmm.

TYPE-POOLS vrm.
TABLES : leci_selopt_dyn,
         leci_chkpt_dyn ,
         leci_tra_dyn   ,
         ztmm_container ,
         lein,
         ltak.

*MODE
DATA   : w_mode   VALUE 'C'  ,
         w_status VALUE 'N'  .

*OK_CODE
DATA   : w_ok_code LIKE sy-ucomm,
         w_fcode   LIKE sy-ucomm.

*BDC_DATA
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_message.

*SCREEN FIELD.
DATA : w_cont_reg_numb1 LIKE leci_tra_dyn-cont_reg_numb1.

*Internal Tables
DATA : BEGIN OF it_storage OCCURS 0,
         lgnum LIKE lagp-lgnum, " Warehouse Number
         lgtyp LIKE lagp-lgtyp, " Storage Type
         lgber LIKE lagp-lgber, " Storage section
         lgpla LIKE lagp-lgpla, " Storage bin
         kzler LIKE lagp-kzler, " Indicator empty
       END OF it_storage.

DATA: BEGIN OF it_scrfield OCCURS   0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF it_scrfield.


*General Variable
DATA : w_pass_numb LIKE leci_event-pass_numb, "Serial No
       w_chk_point LIKE leci_event-chk_point VALUE 'GATE 1',
                                               "Check Point
       w_nltyp     LIKE ltap-nltyp, "Storage type
       w_nlber     LIKE ltap-nlber, "Storage section
       w_nlpla     LIKE ltap-nlpla, "Storage bin
       w_txt10(20) VALUE 'Selection Possible',
       w_count_flg TYPE i             .

*BAPI variable
DATA: wa_goodsmvt_header   LIKE bapi2017_gm_head_01.
DATA: wa_goodsmvt_code     LIKE bapi2017_gm_code.
DATA: wa_goodsmvt_headret  LIKE bapi2017_gm_head_ret.
DATA: it_goodsmvt_item     LIKE TABLE OF bapi2017_gm_item_create.
DATA: wa_goodsmvt_item  LIKE LINE OF it_goodsmvt_item.
DATA: it_bapiret2       LIKE TABLE OF bapiret2.
DATA: wa_bapiret2       LIKE LINE OF it_bapiret2.


*Screen Field
DATA: txt10 TYPE char20.

**--- insert by stlim (2004/04/19)
DATA : w_button_click_time TYPE t,
       w_button_click_date TYPE d,
       ex_subrc LIKE sy-subrc,
       w_vbeln like likp-vbeln.
**--- end of insert
