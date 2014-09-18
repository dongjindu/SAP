*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM15E_6018TOP                                          *
*----------------------------------------------------------------------*
*/ Itab & WA for Creating Purchase Requisition from Planned Order
  DATA: BEGIN OF wa_plaf,
         plnum LIKE plaf-plnum, "Planned order number FOR INFO
         paart LIKE plaf-paart, "Order type
                        "Always 'NB'
         bnfpo LIKE eban-bnfpo, "Item number of purchase requisition
                                                            "Always '1'
         matnr LIKE plaf-matnr, "Material
         gsmng LIKE plaf-gsmng, "Total planned order quantity
         pedtr LIKE plaf-pedtr, "Order finish date in the planned order
         pwwrk LIKE plaf-pwwrk, "Production plant in planned order
         lgort LIKE plaf-lgort, "Storage location
         ekgrp LIKE marc-ekgrp, "Purchasing Group
         bednr LIKE eban-bednr, "Requirement Tracking Number
                        "Always 'LTP'
         flief LIKE plaf-flief, "Fixed Vendor
         ekorg LIKE plaf-ekorg, "Purchasing organization
                        "Always 'PU01'
        END OF wa_plaf.
  DATA: it_plaf LIKE TABLE OF wa_plaf.
  FIELD-SYMBOLS: <fs_plaf> LIKE LINE OF it_plaf.

*/ Itab & WA for BAPI_REQUISITION_CREATE
  DATA: it_requisition_items LIKE TABLE OF bapiebanc.
  "Transfer Structure: Create Requisition Item
  DATA: wa_requisition_items LIKE LINE OF it_requisition_items.

  DATA: it_bapireturn LIKE TABLE OF bapireturn.  "Return Parameter
  DATA: wa_bapireturn LIKE LINE OF it_bapireturn.
  FIELD-SYMBOLS: <fs_bapireturn> LIKE wa_bapireturn.

  DATA: it_bapiret2   LIKE TABLE OF bapiret2.
  DATA: wa_bapiret2   LIKE LINE OF it_bapiret2.

  DATA: wa_bapireturn1 LIKE bapireturn1.


* For BDC message
  DATA: it_bdcmsgcoll LIKE TABLE OF bdcmsgcoll.
  DATA: wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

* For Return code
  DATA: w_subrc LIKE sy-subrc.

* Returned Value from Search Help
  DATA BEGIN OF wa_ddshretval.
          INCLUDE STRUCTURE ddshretval.
  DATA END OF wa_ddshretval.
  DATA it_ddshretval LIKE TABLE OF wa_ddshretval.

* Itab & WA for Search Help of dispo(MRP Controller)
  DATA: BEGIN OF wa_f4_dispo,
*         werks LIKE t024d-werks,
         dispo LIKE t024d-dispo,
         dsnam LIKE t024d-dsnam,
        END OF wa_f4_dispo.
  DATA: it_f4_dispo LIKE TABLE OF wa_f4_dispo.

****/ Begin of Menu
* Itab & WA For FOR Dynamic Menu
  DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
  DATA:  wa_func LIKE LINE OF it_func.
* Title
  DATA: w_title(80).         " Title
****/ End of Menu

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
  DATA: wa_sort     TYPE lvc_s_sort.  "For sort
  DATA: it_sort     LIKE TABLE OF wa_sort. "For sort
  DATA: it_roid TYPE lvc_t_roid.         "For Selected Row ID
  DATA: wa_roid LIKE LINE OF it_roid.    "For Selected Row ID
  DATA: it_row TYPE lvc_t_row.           "For Selected Row ID:Before 620
  DATA: wa_row LIKE LINE OF it_row.      "For Selected Row ID:Before 620

**** Constants&Vars for Number range object ****************************
  CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

  DATA:      w_zdocno    TYPE num10.      "App. Doc. No.
*---------- Selection Screen ------------------------------------------*
  TABLES: t024d.   "MRP controllers
  TABLES: eban.    "Purchase Requisition

  SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
  SELECT-OPTIONS: s_lfdat FOR eban-lfdat   "Item delivery date
                         OBLIGATORY.
  PARAMETERS: p_dispo LIKE t024d-dispo     "MRP controller
                         OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK bl_1.
