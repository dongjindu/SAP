*&---------------------------------------------------------------------*
*& Include MZEMMPM32E_6010TOP                                          *
*&                                                                     *
*&---------------------------------------------------------------------*

*TABLES: ltak,        "WM transfer order header
*        lein,        "Storage unit header records
*        *ltap,       "Transfer order item
*        t333t,       "Texts for WM Movement Types
*        t307t,       "Text for Storage Unit Type
*        t301t.       "Storage Type Descriptions

TABLES: likp.         "SD Document: Delivery Header Data
TABLES: lips.         "SD document: Delivery: Item data

* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.

* For Return code
DATA: w_subrc LIKE sy-subrc.

* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.
DATA: w_title(80).   "Title

* For Next Button Click Date & Time
DATA w_button_click_date LIKE sy-datum.
DATA w_button_click_time LIKE sy-uzeit.

****(BEGIN) Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
DATA:  wa_func LIKE LINE OF it_func.
****(END) Itab & WA For FOR Dynamic Menu

**** For Inbound Delivelies list without Transfer Order
DATA: BEGIN OF wa_ideliveries_without_post,
       vbeln LIKE likp-vbeln,  "Inbound Delivery
       kostk LIKE vbuk-kostk,  "Overall picking / putaway status
       wbstk LIKE vbuk-wbstk,  "Total goods movement status
" space: Not relevant, 'A' Not yet processed, 'B' Partially processed
" 'C' Completely processed
      END OF wa_ideliveries_without_post.
DATA: it_ideliveries_without_post
               LIKE TABLE OF wa_ideliveries_without_post.

**** For Inbound Delivelies list without Transfer Order (Item Level)
DATA: BEGIN OF wa_iditems_without_post,
       vbeln LIKE lips-vbeln,  "Inbound Delivery
       posnr LIKE lips-posnr,  "Item number of the SD document
       matnr LIKE lips-matnr,  "Material
       werks LIKE lips-werks,  "Plant
       lgort LIKE lips-lgort,  "Storage location
       bwart LIKE lips-bwart,  "Movement type
       lifnr LIKE likp-lifnr,  "Vendor
       lfimg LIKE lips-lfimg,  "Delivery quantity
       meins LIKE lips-meins,  "Base unit of measure
       vgbel LIKE lips-vgbel,  "Reference document (PO)
       vgpos LIKE lips-vgpos,  "Reference item (PO Item)
       kosta LIKE vbup-kosta,  "Picking status/Putaway status
       wbsta LIKE vbup-wbsta,  "Goods movement status
" space: Not relevant, 'A' Not yet processed, 'B' Partially processed
" 'C' Completely processed
      END OF wa_iditems_without_post.
DATA: it_iditems_without_post
               LIKE TABLE OF wa_iditems_without_post.
**** For BDC Processing
*w_BDCMODE
*'A' Display screen
*'E' Display only if an error occurs
*'N' Do not display
*'P' Do not display; debugging possible
DATA: w_bdcmode TYPE c.


**** For BAPI bapi_goodsmvt_create  (/nMIGO_GR)
DATA: wa_goodsmvt_header   LIKE bapi2017_gm_head_01.
DATA: wa_goodsmvt_code     LIKE bapi2017_gm_code.
DATA: wa_goodsmvt_headret  LIKE bapi2017_gm_head_ret.
DATA: it_goodsmvt_item     LIKE TABLE OF bapi2017_gm_item_create.
DATA: wa_goodsmvt_item  LIKE LINE OF it_goodsmvt_item.
DATA: it_bapiret2       LIKE TABLE OF bapiret2.
DATA: wa_bapiret2       LIKE LINE OF it_bapiret2.

****
DATA: w_qty_message TYPE char2.  "'EQ': Same, 'NE': Not Same

**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of w_nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.
