*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM44E_6030TOP                                          *
*----------------------------------------------------------------------*
INCLUDE <icon>.
*----------------------------------------------------------------------*
* Begin of PF-STATUS, TITLEBAR, MENU, OK_CODE
*----------------------------------------------------------------------*
*/For OK code
DATA: ok_code TYPE sy-ucomm,  save_ok_code LIKE ok_code.

*/For PF-STATUS and Titlebar Class Reference Variable
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.

*/For Menu
* Itab & WA for Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
DATA:  wa_func LIKE LINE OF it_func.
* Title
DATA: w_title(80).         " Title

* Constants for Dynamic Function Code Text
DATA: dynftext(50). " VALUE C_dynftext1. "For dynamic function text

*----------------------------------------------------------------------*
* End of PF-STATUS, TITLEBAR, MENU, OK_CODE
*----------------------------------------------------------------------*

*/ Itab & wa for ZTMM_6030_01
*  DATA: BEGIN OF wa_ztmm_6030_01,
*         ebeln   LIKE ekko-ebeln,  "Purchasing Document Number
*         ebelp   LIKE ekpo-ebelp,  "Item Number of Purchasing Document
*         meins   LIKE ekpo-meins,  "Order unit
*         menge   LIKE ekbe-menge,  "Quantity
*         t001_waers  LIKE t001-waers,  "Company Code Currency Key
*         dmbtr   LIKE ekbe-dmbtr,  "Amount in local currency
*         mblnr   LIKE mkpf-mblnr,  "Number of material document
*         budat   LIKE mkpf-budat,  "Posting date in the document
*         retroactive TYPE wrbtr,   "RetroActive Price
*         ekko_waers  LIKE ekko-waers, "PO Currency Key
*         mark,
*        END OF wa_ztmm_6030_01.
TABLES: zsmm_6030_01.
DATA: wa_ztmm_6030_01 LIKE zsmm_6030_01.
DATA: it_ztmm_6030_01 LIKE TABLE OF wa_ztmm_6030_01.
FIELD-SYMBOLS: <fs_ztmm_6030_01> LIKE LINE OF it_ztmm_6030_01.

*/For Table Control
DATA: wa_ztmm_6030_01_st LIKE wa_ztmm_6030_01. "For TC(Table Control)

CONTROLS: tc_0100 TYPE TABLEVIEW USING SCREEN '0100'.
DATA:     wa_tc_0100cols LIKE LINE OF tc_0100-cols.
" Workarea for Table Control structure COLS

DATA w_visiblelines LIKE sy-loopc.
"Screens, number of lines visible in table at PBO

DATA w_top_line  TYPE i.
"Top Line of Table control or Steploop


*/For Custom Control (FOR ALV GRID)
DATA: cc_name TYPE scrfname. "VALUE 'CC_0100'.

*/For ALV Handler
CLASS lcl_h_alv DEFINITION DEFERRED.
DATA: crv_h_alv TYPE REF TO lcl_h_alv.  "Handler of ALV

*/For Class Reference Variable
DATA: crv_gui_custom_container TYPE REF TO cl_gui_custom_container,
      crv_gui_alv_grid         TYPE REF TO cl_gui_alv_grid.

*/For ALV Variables
*1. OOP
DATA: wa_layo    TYPE lvc_s_layo. "ALV control: Layout structure
DATA: it_fcat    TYPE lvc_t_fcat WITH HEADER LINE.
DATA: wa_toolbar TYPE stb_button.
DATA: wa_sort    TYPE lvc_s_sort.       "For sort
DATA: it_sort    LIKE TABLE OF wa_sort. "For sort
DATA: it_roid    TYPE lvc_t_roid.       "For Selected Row ID
DATA: wa_roid    LIKE LINE OF it_roid.  "For Selected Row ID
DATA: it_row     TYPE lvc_t_row.        "For Selected Row ID:Before 620
DATA: wa_row     LIKE LINE OF it_row.   "For Selected Row ID:Before 620

*2. For List
TYPE-POOLS: slis.
DATA:          it_fcat_list         TYPE slis_t_fieldcat_alv.
DATA:          wa_fcat_list         LIKE LINE OF it_fcat_list.
FIELD-SYMBOLS: <fs_fcat_list>       LIKE wa_fcat_list.
DATA:          it_slis_t_event      TYPE slis_t_event.
DATA:          it_slis_t_listheader TYPE slis_t_listheader.
DATA:          it_fm_sort           TYPE slis_t_sortinfo_alv.



**** Begin of Program, Screen
*DATA  repid LIKE sy-repid. "Field SYST-REPID does not exist in the newe
*DATA  cprog LIKE sy-cprog.
DATA  pgm   LIKE sy-cprog.   " LIKE trdirt-name.
DATA  scr   TYPE sy-dynnr.
DATA: ss_0110 LIKE sy-dynnr.   "For Subscreen of Screen 0110
**** End of Program, Screen

*/For IO Fields
DATA: io_maktx LIKE makt-maktx.   "Desc of matnr
DATA: io_desc_lifnr(40).

*/For Flag
DATA: w_retro_flg.   "'X': Not Filled, Space: Filled
*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
TABLES: mara,            "General Material Data
        lfa1,            "Vendor Master (General Section)
        ekpo,            "Purchasing Document Item
        mkpf,            "Header: Material Document
        ekbe.            "History per Purchasing Document
*/LP or KD Condition
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(31) text-li1.                    "Line 1
PARAMETERS rb1
           DEFAULT 'X'
           RADIOBUTTON GROUP rbg USER-COMMAND fcrb.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(31) text-li2.                    "Line 2
PARAMETERS rb2 RADIOBUTTON GROUP rbg.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK bl_1.

*/Selection Conditions
SELECTION-SCREEN BEGIN OF BLOCK bl_2 WITH FRAME TITLE text-bl2.
PARAMETERS:     p_matnr LIKE mara-matnr  "Material
                          VISIBLE LENGTH 18.
PARAMETERS:     p_lifnr LIKE lfa1-lifnr.  "Vendor
*                          OBLIGATORY.
PARAMETERS:     p_retro LIKE wa_ztmm_6030_01-retroactive.
"RetroActive price
SELECT-OPTIONS: s_budat FOR mkpf-budat   "GR Date
                           MODIF ID m01. "To modify screen.
*                          OBLIGATORY.
SELECT-OPTIONS: s_ivdat FOR ekbe-budat   "IV Date
                           MODIF ID m02. "To modify screen.
*                          OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl_2.
