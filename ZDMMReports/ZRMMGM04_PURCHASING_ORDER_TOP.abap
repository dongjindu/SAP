*----------------------------------------------------------------------*
*   INCLUDE ZRMMGM04_PURCHASING_ORDER_TOP                              *
*----------------------------------------------------------------------*

TYPE-POOLS: slis.


TABLES : ekko,
         ekpo,
         eket,
         ekes,
         resb,
         makt,
         lfa1,
         t163y, *t163y,
         ztmm_pur_order,
         ztmm_pro_info.


DATA :                             "basic po
       BEGIN  OF it_basic  OCCURS 0.
        INCLUDE STRUCTURE ztmm_pur_order.
DATA :   bsart    LIKE    ekko-bsart,
         pstyp    LIKE    ekpo-pstyp,
         mark,
       END    OF it_basic.

DATA :                              "basic process
       BEGIN  OF it_basic1  OCCURS 0.
        INCLUDE STRUCTURE ztmm_pro_info.
DATA :   bsart    LIKE    ekko-bsart,
         pstyp    LIKE    ekpo-pstyp,
         lgort    LIKE    ekpo-lgort,
         mark,
       END    OF it_basic1.


DATA : it_header LIKE TABLE OF it_basic "po all
                      WITH HEADER LINE,

       it_rfc    LIKE TABLE OF it_basic "po interface
                      WITH HEADER LINE,

       it_header1 LIKE TABLE OF it_basic1 "process all
                      WITH HEADER LINE,

       it_rfc1    LIKE TABLE OF it_basic1 "process interface
                      WITH HEADER LINE.

DATA : BEGIN  OF it_po  OCCURS 0,       "po
         ebeln  LIKE  ekko-ebeln,
         eindt  LIKE  eket-eindt,
         flag   LIKE  ztmm_pur_order-flag,
         mark,
       END    OF it_po,

       BEGIN  OF it_material OCCURS 0,  "material
         matnr  LIKE  ztmm_pur_order-matnr,
         maktx  LIKE  ztmm_pur_order-maktx,
         menge  LIKE  ztmm_pur_order-menge,
         meins  LIKE  ztmm_pur_order-meins,
         flag   LIKE  ztmm_pur_order-flag,
         mark,
       END    OF it_material,

       BEGIN  OF it_poitem  OCCURS 0,   "po item
         ebeln  LIKE  ztmm_pur_order-ebeln,
         ebelp  LIKE  ztmm_pur_order-ebelp,
         matnr  LIKE  ztmm_pur_order-matnr,
         maktx  LIKE  ztmm_pur_order-maktx,
         eindt  LIKE  ztmm_pur_order-eindt,
         menge  LIKE  ztmm_pur_order-menge,
         meins  LIKE  ztmm_pur_order-meins,
         flag   LIKE  ztmm_pur_order-flag,
         mark,
       END    OF it_poitem,

       BEGIN  OF it_comp  OCCURS 0,  "component
         matnr    LIKE    ekpo-matnr,
         nomng    LIKE    resb-nomng,
       END    OF it_comp.

DATA : it_ztmm_pur_order  LIKE  TABLE OF ztmm_pur_order
                                WITH HEADER LINE,
       it_ztmm_pro_info   LIKE  TABLE OF ztmm_pro_info
                                WITH HEADER LINE.

DATA : wa_basic       LIKE  it_basic,
       wa_basic1      LIKE  it_basic1,
       wa_zsmm_class  LIKE  zsmm_class.


DATA : w_total        LIKE ztca_if_log-total,  "total cnt in interface
       w_succ         LIKE ztca_if_log-zsucc,  "success cnt in interface
       wa_ztca_if_log LIKE ztca_if_log,

       w_loop_cnt     LIKE sy-loopc,
       w_tcname       LIKE feld-name,
       save_ok        LIKE sy-ucomm,
       ok_code        LIKE sy-ucomm.


CONSTANTS : c_dest(10)  VALUE 'WMRM01'.  "Outbound Destination


CONTROLS : tc0100     TYPE TABLEVIEW USING SCREEN 0100,
           tc0200     TYPE TABLEVIEW USING SCREEN 0200,
           tc0110     TYPE TABLEVIEW USING SCREEN 0110,
           tc0120     TYPE TABLEVIEW USING SCREEN 0120,
           tc0130     TYPE TABLEVIEW USING SCREEN 0130.


FIELD-SYMBOLS <tc>  TYPE cxtab_control.



** input condition
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-004.
SELECT-OPTIONS :    s_eindt  FOR   eket-eindt,  "delivery date
                    s_bedat  FOR   ekko-bedat.  "doc date
PARAMETERS     :    p_lifnr  LIKE  ekko-lifnr,  "vendor
                    p_matnr  LIKE  ekpo-matnr.  "material
SELECTION-SCREEN BEGIN OF LINE.             "info category
SELECTION-SCREEN COMMENT 1(20) text-008.
SELECTION-SCREEN POSITION 33.
PARAMETERS       rp1 RADIOBUTTON GROUP rd4 DEFAULT 'X'.
SELECTION-SCREEN POSITION 35.
SELECTION-SCREEN COMMENT 36(15) text-006 FOR FIELD rp1.
SELECTION-SCREEN POSITION 55.
PARAMETERS       rp2 RADIOBUTTON GROUP rd4.
SELECTION-SCREEN POSITION 58.
SELECTION-SCREEN COMMENT 59(25) text-007 FOR FIELD rp2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

** function
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-001.
PARAMETERS     : rf1 RADIOBUTTON GROUP rd1 DEFAULT 'X',
                 rf2 RADIOBUTTON GROUP rd1,
                 rf3 RADIOBUTTON GROUP rd1,
                 rf4 RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF BLOCK b5.

** interface
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       ri1 RADIOBUTTON GROUP rd3 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-009 FOR FIELD ri1.
SELECTION-SCREEN POSITION 40.
PARAMETERS       ri2 RADIOBUTTON GROUP rd3.
SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT 42(25) text-010 FOR FIELD ri2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b4.

** processing
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       rv1 RADIOBUTTON GROUP rd2 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-011 FOR FIELD rv1.
SELECTION-SCREEN POSITION 30.
PARAMETERS       rv2 RADIOBUTTON GROUP rd2.
SELECTION-SCREEN POSITION 32.
SELECTION-SCREEN COMMENT 33(10) text-012 FOR FIELD rv2.
SELECTION-SCREEN POSITION 55.
PARAMETERS       rv3 RADIOBUTTON GROUP rd2.
SELECTION-SCREEN POSITION 57.
SELECTION-SCREEN COMMENT 58(10) text-013 FOR FIELD rv3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
