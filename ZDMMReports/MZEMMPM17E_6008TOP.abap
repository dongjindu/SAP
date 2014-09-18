*&---------------------------------------------------------------------*
*& Include MZEMMPM17E_6008TOP                                          *
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
