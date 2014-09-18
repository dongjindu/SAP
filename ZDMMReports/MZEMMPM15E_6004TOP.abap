*&---------------------------------------------------------------------*
*& Include MZEMMPM15E_6004TOP                                          *
*&                                                                     *
*&---------------------------------------------------------------------*

TABLES: ltak,        "WM transfer order header
        lein,        "Storage unit header records
        *ltap,       "Transfer order item
        t333t,       "Texts for WM Movement Types
        t307t,       "Text for Storage Unit Type
        t301t.       "Storage Type Descriptions

* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.

* For Return code
DATA: w_subrc LIKE sy-subrc.

* For BDC message
DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

**** Constants&Vars for Number range object ****************************
  CONSTANTS: nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      nro_number  TYPE num10.      " Same type of nro_object
