*&---------------------------------------------------------------------*
*&  Include           ZIPP_PT_WSTOCK_TOP
*&---------------------------------------------------------------------*

TABLES: pgmi, mseg.

DATA : w_msgtxt(100),
       w_result(1),
       w_yymm(6).


CONSTANTS:
* "Interface Destination.
  c_dest(10)  TYPE c            VALUE 'WMHR01',
  c_kd_eng    TYPE pgmi-prgrp   VALUE 'KD-ENG',
  c_kd_tm     TYPE pgmi-prgrp   VALUE 'KD-TM',
  c_sbc3      TYPE lifnr        VALUE 'SBC3',  "Hyundai Motor Company
  c_h27z      TYPE lifnr        VALUE 'H27Z'.  "WIA Auto.Engine(Shandong)Co

DATA: BEGIN OF it_pt_wstock OCCURS 0.
        INCLUDE STRUCTURE ztpp_pt_wstock.
DATA: END OF it_pt_wstock.



* CH.Jeong on 10/21/2013 : Quantity of shipping & in-transit
DATA : it_wstock_qty LIKE TABLE OF it_pt_wstock WITH HEADER LINE.

DATA : BEGIN OF it_pgmi OCCURS 0.
DATA :   pgtyp   TYPE pgmi-pgtyp,
         prgrp   TYPE pgmi-prgrp,     "product group
         werks   TYPE pgmi-werks,
         nrmit   TYPE pgmi-nrmit,     "matnr (char 18)
         wemit   TYPE pgmi-wemit,
         datum   TYPE pgmi-datum,
         vsnda   TYPE pgmi-vsnda,

         zmo01   TYPE zmo01.          "model code(char 2)
DATA : END OF it_pgmi.

DATA : BEGIN OF it_bl OCCURS 0.
DATA :   zfblno  TYPE ztbl-zfblno,    "B/L Document No
         zfblit  TYPE ztblit-zfblit,  "B/L item no.
         zfetd   TYPE ztbl-zfetd,     "E.T.D
         lifnr   TYPE ztbl-lifnr,     "Vendor

         matnr   TYPE ztblit-matnr,   "Material Number
         prgrp   TYPE pgmi-prgrp,     "product group
         zmo01   TYPE zmo01,          "model code(char 2)
         blmenge TYPE ztblit-blmenge, "B/L quantity
         meins   TYPE ztblit-meins.   "Base Unit of Measure
DATA : END OF it_bl.

DATA : BEGIN OF it_curr_transit OCCURS 0.
DATA :   kunde   TYPE vlkpa-kunde,    "Partner number (Vendor)
         parvw   TYPE vlkpa-parvw,    "Partner Function
         lfdat   TYPE vlkpa-lfdat,    "Delivery date

         wbstk   TYPE vbuk-wbstk,     "Total goods movement status
         vbtyp   TYPE vbuk-vbtyp,     "SD document category

         wbsta   TYPE vbup-wbsta,     "Goods movement status

         vbeln   TYPE lips-vbeln,     "Delivery
         posnr   TYPE lips-posnr,     "Delivery item
         matnr   TYPE lips-matnr,     "Material Number
         prgrp   TYPE pgmi-prgrp,     "product group
         zmo01   TYPE zmo01,          "model code(char 2)

         lfimg   TYPE lips-lfimg,     "Actual quantity delivered
         meins   TYPE lips-meins.     "Base Unit of Measure
DATA : END OF it_curr_transit.

DATA : it_past_transit LIKE it_pt_wstock OCCURS 0   WITH HEADER LINE.

DATA : g_date_time  TYPE zdatetime,   "YYYYMMDDhhmmss
       g_cn(2)      TYPE n,
       g_text(50)   TYPE c,
       g_field(50)  TYPE c.

FIELD-SYMBOLS : <fs>.
* End on 10/21/2013
