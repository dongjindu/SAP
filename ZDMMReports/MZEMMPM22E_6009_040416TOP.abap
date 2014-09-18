*&---------------------------------------------------------------------*
*& Include MZEMMPM22E_6009TOP                                          *
*&                                                                     *
*&---------------------------------------------------------------------*

*TABLES: ltak,        "WM transfer order header
*        lein,        "Storage unit header records
*        *ltap,       "Transfer order item
*        t333t,       "Texts for WM Movement Types
*        t307t,       "Text for Storage Unit Type
*        t301t.       "Storage Type Descriptions

TABLES: likp.         "SD Document: Delivery Header Data

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

****
DATA: wa_lips LIKE lips.  "Inbound Delivery Item
DATA: w_qty_message TYPE char2.  "'EQ': Same, 'NE': Not Same

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
