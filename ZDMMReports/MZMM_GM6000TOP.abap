*&---------------------------------------------------------------------*
*& Include MZ_MM6000TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

* For OK code
DATA: ok_code LIKE sy-ucomm,  save_ok_code LIKE ok_code.

* For PF-STATUS and Titlebar
CLASS lcl_ps DEFINITION DEFERRED.
DATA: crv_ps TYPE REF TO lcl_ps.
DATA: w_title(80).   "Title
****(BEGIN) Itab & WA For FOR Dynamic Menu
DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
*DATA: BEGIN OF itabfunc OCCURS 0,
*         fcode LIKE rsmpe-func,
*       END OF itabfunc.
DATA:  wa_func LIKE LINE OF it_func.

* Constants for Dynamic Function Code Text
CONSTANTS: c_tree_on(30)   VALUE 'Tree On',
           c_tree_off(30)  VALUE 'Tree Off'.

DATA: dynftext(50). " VALUE c_tree_off. "For dynamic function text

****(END) Itab & WA For FOR Dynamic Menu

**** Begin of Tree
* Info Variable for Tree Event
DATA: io_event(30),
      io_node_key(30) TYPE c,
      io_item_name    TYPE tv_itmname,                      "Char 12
      io_header_name  TYPE tv_hdrname.                      "Char 72

CLASS lcl_h_tree DEFINITION DEFERRED.
*If the first access to a global class in a program is to its static
*components or in the definition of an event handler method, you must
*load it explicitly using the statement CLASS class DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

DATA: crv_h_tree    TYPE REF TO lcl_h_tree.
* Docking Container
DATA: crv_docking_container TYPE REF TO cl_gui_docking_container.
DATA: crv_tree_model        TYPE REF TO cl_column_tree_model.
*
DATA: event            TYPE cntl_simple_event,
      events           TYPE cntl_simple_events,
      hierarchy_header LIKE treemhhdr.
"Tree Model: Attributes of the Hierarchy Heading
* For tree node
DATA: item_table TYPE treemcitab,    "Column Tree Model: Item Table
      item       TYPE treemcitem,    "Column Tree Model: Item
      node_key   TYPE tm_nodekey,    "Tree Model: Key of a node
      node_desc(50).                 "Description
**** End of Tree
* Constants for Visible, Invisible
CONSTANTS: c_visible_true  VALUE '1',
           c_visible_false VALUE '0'.

**** Begin of Program, Screen
DATA  pgm     LIKE sy-cprog.   " LIKE trdirt-name.
DATA  scr     TYPE sy-dynnr.
DATA: ss_0110 LIKE sy-dynnr.   "For Subscreen of Screen 0110
**** End of Program, Screen


**** Itab & WA with External Data
* 9001
DATA: it_exdata_zsmm_6000_01 LIKE TABLE OF zsmm_6000_01.
DATA: wa_exdata_zsmm_6000_01 LIKE LINE OF it_exdata_zsmm_6000_01.
"Transfer Itab to/from External Screen

* For Deletion
DATA: it_exdata_zsmm_6000_01del LIKE it_exdata_zsmm_6000_01.
DATA: wa_exdata_zsmm_6000_01del LIKE LINE OF it_exdata_zsmm_6000_01del.

DATA: it_exdata_zsmm_6000_01ori LIKE it_exdata_zsmm_6000_01.
"For orginal entries.

* For Table Control
DATA: w_lines_tmp TYPE i.

* 9002
DATA: it_exdata_zsmm_6000_02 LIKE TABLE OF zsmm_6000_02.
DATA: wa_exdata_zsmm_6000_02 LIKE LINE OF it_exdata_zsmm_6000_02.
FIELD-SYMBOLS:
    <fs_exdata_zsmm_6000_02> LIKE LINE OF it_exdata_zsmm_6000_02.
"Transfer Itab to/from External Screen
* For Deletion
DATA: it_exdata_zsmm_6000_02del LIKE it_exdata_zsmm_6000_02.
DATA: wa_exdata_zsmm_6000_02del LIKE LINE OF it_exdata_zsmm_6000_02del.

DATA: it_exdata_zsmm_6000_02ori LIKE it_exdata_zsmm_6000_02.
"For original entries.

* 9003
DATA: it_exdata_zsmm_6000_03 LIKE TABLE OF zsmm_6000_03.
DATA: wa_exdata_zsmm_6000_03 LIKE LINE OF it_exdata_zsmm_6000_03.
FIELD-SYMBOLS:
    <fs_exdata_zsmm_6000_03> LIKE LINE OF it_exdata_zsmm_6000_03.
"Transfer Itab to/from External Screen
* For Deletion
DATA: it_exdata_zsmm_6000_03del LIKE it_exdata_zsmm_6000_03.
DATA: wa_exdata_zsmm_6000_03del LIKE LINE OF it_exdata_zsmm_6000_03del.

DATA: it_exdata_zsmm_6000_03ori LIKE it_exdata_zsmm_6000_03.
"For orginal entries.

* 9004
DATA: it_exdata_zsmm_6000_04 LIKE TABLE OF zsmm_6000_04.
DATA: wa_exdata_zsmm_6000_04 LIKE LINE OF it_exdata_zsmm_6000_04.
FIELD-SYMBOLS:
    <fs_exdata_zsmm_6000_04> LIKE LINE OF it_exdata_zsmm_6000_04.
"Transfer Itab to/from External Screen
* For Deletion
DATA: it_exdata_zsmm_6000_04del LIKE it_exdata_zsmm_6000_04.
DATA: wa_exdata_zsmm_6000_04del LIKE LINE OF it_exdata_zsmm_6000_04del.

DATA: it_exdata_zsmm_6000_04ori LIKE it_exdata_zsmm_6000_04.
"For orginal entries.

* 9005
DATA: it_exdata_zsmm_6000_05 LIKE TABLE OF zsmm_6000_05.
DATA: wa_exdata_zsmm_6000_05 LIKE LINE OF it_exdata_zsmm_6000_05.
FIELD-SYMBOLS:
    <fs_exdata_zsmm_6000_05> LIKE LINE OF it_exdata_zsmm_6000_05.
"Transfer Itab to/from External Screen
* For Deletion
DATA: it_exdata_zsmm_6000_05del LIKE it_exdata_zsmm_6000_05.
DATA: wa_exdata_zsmm_6000_05del LIKE LINE OF it_exdata_zsmm_6000_05del.

DATA: it_exdata_zsmm_6000_05ori LIKE it_exdata_zsmm_6000_05.
"For orginal entries.


* 9006
DATA: it_exdata_zsmm_6000_06 LIKE TABLE OF zsmm_6000_06.
DATA: wa_exdata_zsmm_6000_06 LIKE LINE OF it_exdata_zsmm_6000_06.
FIELD-SYMBOLS:
    <fs_exdata_zsmm_6000_06> LIKE LINE OF it_exdata_zsmm_6000_06.
"Transfer Itab to/from External Screen
* For Deletion
DATA: it_exdata_zsmm_6000_06del LIKE it_exdata_zsmm_6000_06.
DATA: wa_exdata_zsmm_6000_06del LIKE LINE OF it_exdata_zsmm_6000_06del.

DATA: it_exdata_zsmm_6000_06ori LIKE it_exdata_zsmm_6000_06.
"For orginal entries.


* 9001
DATA: w_bukrs    LIKE zsmm_6000_01-bukrs  VALUE 'H201',  "Company Code
      w_licode   LIKE zsmm_6000_01-licode, "Line Code
      w_opcode   LIKE zsmm_6000_01-opcode, "Operation Code
      cblicode,     "Check Box
      w_werks    LIKE zsmm_6000_01-werks.
DATA: w_visiblelines TYPE i,
      w_lines        TYPE i,
      w_top_line     TYPE i.


* 9002
DATA: w_matnr       LIKE zsmm_6000_02-matnr,    "Material Number
      w_ztconodisp  LIKE zsmm_6000_02-ztcono,   "TCO Number for Display
      w_ztcono      LIKE zsmm_6000_02-ztcono,   "TCO Number
      w_zapplied_date LIKE zsmm_6000_02-zapplied_date,"Applied Date
      w_erdat       LIKE zsmm_6000_02-erdat,         "Created On
      w_zch_desc_cd LIKE zsmm_6000_02-zch_desc_cd,   "Change Desc Code
      w_zch_reason_cd LIKE zsmm_6000_02-zch_reason_cd,"Change Reason Cd
      w_zinv_process_cd LIKE zsmm_6000_02-zinv_process_cd,
      "Stock Disposal
      w_zch_matnr1  LIKE zsmm_6000_02-zch_matnr1,  "Change Material1
      w_zch_matnr2  LIKE zsmm_6000_02-zch_matnr2,  "Change Material2
      w_zxfer_matnr LIKE zsmm_6000_02-zxfer_matnr, "Transfer Material

      w_trtyp           TYPE trtyp.                "Transaction Type
**** Indices
DATA w_check_idx TYPE i.
DATA w_tabix_idx LIKE sy-tabix. "Internal Table, Current Line Index
**** For duplicate check
DATA: BEGIN OF it_dup OCCURS 0,
        f1(30),
        f2(30),
        f3(30),
        f4(30),
        f5(30),
      END OF it_dup.

**** Dynamic Where Clause
DATA: w_where_condition(72).
DATA: it_where_condition LIKE TABLE OF w_where_condition.

**** Flag
DATA: w_once_flg. " SPACE: NOT EXECUTED, 'X': EXCUTED ONCE OR MORE
