*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM10E_6006TOP                                          *
*----------------------------------------------------------------------*
**** WA and ITAB for EBELN, EBELP, MATNR, MEINS, WERKS, EKGRP, LIFNR
DATA: BEGIN OF wa_ekpo,
       ebeln    LIKE ekpo-ebeln,   "Purchasing Document Number
       ebelp    LIKE ekpo-ebelp,   "Item Number of Purchasing Document
       matnr    LIKE ekpo-matnr,   "Material Number
       meins    LIKE ekpo-meins,   "Order unit
       werks    LIKE ekpo-werks,   "Plant
       ekgrp    LIKE ekko-ekgrp,   "Purchasing group
       lifnr    LIKE ekko-lifnr,   "Vendor's account number
      END OF wa_ekpo.
FIELD-SYMBOLS: <fs_ekpo> LIKE wa_ekpo.
DATA  it_ekpo LIKE TABLE OF wa_ekpo.

**** WA and ITAB for Steel Requirement
DATA: BEGIN OF wa_eket,
       zsect(3),                   "MIP, VDR
       ebeln    LIKE eket-ebeln,   "Purchasing Document Number
       ebelp    LIKE eket-ebelp,   "Item Number of Purchasing Document
       etenr    LIKE eket-etenr,   "Delivery schedule line counter
       eindt    LIKE eket-eindt,   "Item delivery date
       matnr    LIKE ekpo-matnr,   "Material Number
       werks    LIKE marc-werks,   "Plant
       meins    LIKE ekpo-meins,   "Order unit
       menge    LIKE eket-menge,   "Scheduled quantity
       wemng    LIKE eket-wemng,   "Quantity of goods received
       obmng    LIKE rm06e-obmng,  "Open quantity
       idnrk    LIKE stpox-idnrk,  "BOM component
       ekgrp    LIKE ekko-ekgrp,   "Purchasing group
       lifnr    LIKE ekko-lifnr,   "Vendor's account number
       mnglg    LIKE stpox-mnglg,
       "Calculated Component Quantity in Base Unit of Measure
       mmein    LIKE stpox-mmein,  "Base unit of measure
       month_0  LIKE eket-menge,   "Qty of This month
       month_1  LIKE eket-menge,   "Qty of This month + 1
       month_2  LIKE eket-menge,   "Qty of This month + 2
       month_3  LIKE eket-menge,   "Qty of This month + 3
       month_4  LIKE eket-menge,   "Qty of This month + 4
       month_5  LIKE eket-menge,   "Qty of This month + 5
       month_t  LIKE eket-menge,   "Total Qty of months
      END OF wa_eket.
FIELD-SYMBOLS: <fs_eket> LIKE wa_eket.
DATA  it_eket LIKE TABLE OF wa_eket.

* Steel Requirement by Section
DATA: it_eketmip LIKE it_eket.   "MIP Section
DATA: it_eketvdr LIKE it_eket.   "VDR Section


* last day of month
DATA: w_lastday_month   LIKE sy-datum.   "last day of month (Generally)
DATA: w_lastday_month_0 LIKE sy-datum.   "last day of this month
DATA: w_lastday_month_1 LIKE sy-datum.   "last day of this month + 1
DATA: w_lastday_month_2 LIKE sy-datum.   "last day of this month + 2
DATA: w_lastday_month_3 LIKE sy-datum.   "last day of this month + 3
DATA: w_lastday_month_4 LIKE sy-datum.   "last day of this month + 4
DATA: w_lastday_month_5 LIKE sy-datum.   "last day of this month + 5

* For ALV
DATA: wa_zsmm_6005_01 LIKE zsmm_6005_01.
DATA: it_zsmm_6005_01 LIKE TABLE OF wa_zsmm_6005_01.


* For OK code
DATA: ok_code TYPE sy-ucomm,  save_ok_code LIKE ok_code.

* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.

**** Begin of Menu
* Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
*DATA: BEGIN OF itabfunc OCCURS 0,
*         fcode LIKE rsmpe-func,
*       END OF itabfunc.
DATA:  wa_func LIKE LINE OF it_func.

* Title
DATA: w_title(80).         " Title

**** Custom Control
*Custom Control Name Declaration (FOR ALV GRID)
DATA: cc_name TYPE scrfname VALUE 'CC_0100'.

*Reference Variable Declaration
DATA: crv_custom_container TYPE REF TO cl_gui_custom_container,
      crv_alv_grid         TYPE REF TO cl_gui_alv_grid.

* Variables for ALV
DATA: wa_layout   TYPE lvc_s_layo.
DATA: it_fieldcat TYPE lvc_t_fcat WITH HEADER LINE.
DATA: wa_toolbar  TYPE stb_button.
DATA: wa_sort     TYPE lvc_s_sort.       "For sort
DATA: it_sort     LIKE TABLE OF wa_sort. "For sort
DATA: it_roid     TYPE lvc_t_roid.       "For Selected Row ID
DATA: wa_roid     LIKE LINE OF it_roid.  "For Selected Row ID
DATA: it_row      TYPE lvc_t_row.        "For Selected Row ID:Before 620
DATA: wa_row      LIKE LINE OF it_row.   "For Selected Row ID:Before 620




***Selection Screen*****************************************************
TABLES: eket.            "Scheduling Agreement Schedule Lines

SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
PARAMETERS    : p_eindt LIKE eket-eindt DEFAULT sy-datum OBLIGATORY.
"Requirement Date (Item delivery date)
SELECTION-SCREEN END OF BLOCK bl_1.
