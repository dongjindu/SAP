************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************

*&---------------------------------------------------------------------*
*& Include ZEMMGM10E_ASSIGN_BLANKTOP                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  zemmgm10e_assign_blank MESSAGE-ID zmmm.

*Screen
CONTROLS tc_bom TYPE TABLEVIEW USING SCREEN 100.
DATA : w_loopc LIKE sy-loopc.

* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.


*Internal Table
DATA : BEGIN OF it_bom OCCURS 0,
          matnr LIKE mara-matnr, "BLANK
          characteristic(40)        , "Characteristics
          brgew LIKE mara-brgew, "Usage
          gewei LIKE mara-gewei, "Unit
          steel LIKE mara-matnr, "STEEL Material NO
          werks LIKE marc-werks, "Plant
          aennr like aenr-aennr,  "Change number
          mark,                  "Mark
       END OF it_bom.

DATA : BEGIN OF it_ammat OCCURS 0,
          maktx LIKE makt-maktx  ,
          matnr LIKE mara-matnr  ,
*         A1(10)       ,    "PRS_BLK_COLQ
*         A2(06)       ,
*         A3(10)       ,    "PRS_BLK_COLT
*         A4(10)       ,    "PRS_BLK_COLW
*         A5(10)       ,    "PRS_BLK_COLL
          maktx2 LIKE makt-maktx  ,  "Added by Hakchin(20040108)
                                     "For Steel Mat Matching Purpose
       END OF it_ammat.

*BDC_DATA
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.
DATA : BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_message.
DATA : w_status VALUE 'N'.

* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.




*/ Begin of Added by Hakchin(20040109)
* INDESES
DATA tabix TYPE sy-tabix.
DATA cline TYPE i.

*Dynamic read of screen value
DATA BEGIN OF wa_dynpread.
        INCLUDE STRUCTURE dynpread.
DATA END OF wa_dynpread.
DATA it_dynpread LIKE TABLE OF wa_dynpread.
DATA: f4rc TYPE sy-subrc.
*
DATA BEGIN OF wa_ddshretval.
        INCLUDE STRUCTURE ddshretval.
DATA END OF wa_ddshretval.
DATA it_ddshretval LIKE TABLE OF wa_ddshretval.

*/ End of Added by Hakchin(20040109)

*/ Begin of Added by Hakchin(20040221)
DATA: w_aennr LIKE aenr-aennr.  "Change number
data: w_subrc like sy-subrc.
* For Error index
DATA: w_error_idx TYPE i.
*/ End of Added by Hakchin(20040221)

**** Constants&Vars for Number range object ****************************
CONSTANTS: nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc. No.
* Number range object
DATA:      nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      nro_number  TYPE num10.      " Same type of nro_object
**** App Doc No
DATA: w_zdocno TYPE num10.     "Application Doc no.
