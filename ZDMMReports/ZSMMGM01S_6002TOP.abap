*----------------------------------------------------------------------*
*   INCLUDE ZSMMGM01S_6002TOP                                          *
*----------------------------------------------------------------------*
INCLUDE <icon>.
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

* Constants for Dynamic Function Code Text
CONSTANTS: c_dynftext1(30) VALUE 'Dynamic Text 1',
           c_dynftext2(30) VALUE 'Dynamic Text 2'.
CONSTANTS: c_tree_on(30)   VALUE 'Tree On',
           c_tree_off(30)  VALUE 'Tree Off'.

DATA: dynftext(50). " VALUE C_dynftext1. "For dynamic function text
**** End of Menu

* Constants for Visible, Invisible
CONSTANTS: c_visible_true  VALUE '1',
           c_visible_false VALUE '0'.

* For search help
DATA: io_shlpname  TYPE shlpname.   "Search Help Name(Char 30)
DATA: io_shlpfield TYPE shlpfield.  "Search Help Parameter(Char 30)
"For Dynamic Search Help

* Returned Value from Search Help
DATA BEGIN OF wa_ddshretval.
        INCLUDE STRUCTURE ddshretval.
DATA END OF wa_ddshretval.
DATA it_ddshretval LIKE TABLE OF wa_ddshretval.

* For Dynamic screen field read/update
DATA BEGIN OF wa_dynpread.
        INCLUDE STRUCTURE dynpread.
DATA END OF wa_dynpread.
DATA it_dynpread LIKE TABLE OF wa_dynpread.
DATA: w_f4rc TYPE sy-subrc.

* Confirm message Related
DATA: w_answer.

* Sub Return Related
DATA: w_subrc LIKE sy-subrc."Return Value,Return Value After ABAP Statem


* Table Control
CONTROLS: tc_0100 TYPE TABLEVIEW USING SCREEN '0100'.
DATA:     wa_tc_0100cols LIKE LINE OF tc_0100-cols.
" Workarea for Table Control structure COLS

DATA w_visiblelines LIKE sy-loopc.    " sy-loopc at PBO

* Tabstrip Control
CONTROLS: ts_9000 TYPE TABSTRIP.


**** For Listbox
TYPE-POOLS: vrm.
DATA: id     TYPE vrm_id,
      values TYPE vrm_values,
      value  LIKE LINE OF values.

* Flag
DATA: w_once_flg.  " SPACE: NOT EXECUTED, 'X': EXCUTED ONCE OR MORE

* For sending Character Return (Ascii code 13)
DATA: BEGIN OF wa_cr,
       cr TYPE x VALUE '13',
      END OF wa_cr.      "(Hexadesimal: 0D)

*****
*If the first access to a global class in a program is to its static
*components or in the definition of an event handler method, you must
*load it explicitly using the statement CLASS class DEFINITION LOAD.
CLASS cl_gui_cfw DEFINITION LOAD.

*
DATA: event            TYPE cntl_simple_event,
      events           TYPE cntl_simple_events,
      hierarchy_header LIKE treemhhdr. "Tree Model: Attributes of the Hi


**** Begin of Program, Screen
*DATA  repid LIKE sy-repid. "Field SYST-REPID does not exist in the newe
*DATA  cprog LIKE sy-cprog.
DATA  pgm   LIKE sy-cprog.   " LIKE trdirt-name.
DATA  scr   TYPE sy-dynnr.
DATA: ss_0110 LIKE sy-dynnr.   "For Subscreen of Screen 0110
**** End of Program, Screen


**** For Cursor
DATA: w_cfield(20),
      w_cvalue(50),
      w_cline   TYPE i,
      w_coffset TYPE i.

**** Index for Internal table
DATA w_tabix LIKE sy-tabix. "Internal Table, Current Line Index

* General
DATA: enjoy_transaction_init,
      owner_repid TYPE sy-repid,
      owner_dynnr TYPE sy-dynnr.

* Transaction Type
DATA: w_trtyp TYPE trtyp.

*/For Custom Control (FOR ALV GRID)
DATA: cc_name TYPE scrfname VALUE 'CC_0100'.

*/For Class Reference Variable
DATA: crv_custom_container TYPE REF TO cl_gui_custom_container,
      crv_gui_alv_grid     TYPE REF TO cl_gui_alv_grid.

* Variables for ALV
DATA: wa_layout   TYPE lvc_s_layo.
DATA: it_fcat TYPE lvc_t_fcat WITH HEADER LINE.
DATA: wa_toolbar  TYPE stb_button.
DATA: wa_sort     TYPE lvc_s_sort.  "For sort
DATA: it_sort     LIKE TABLE OF wa_sort. "For sort
DATA: it_roid TYPE lvc_t_roid.         "For Selected Row ID
DATA: wa_roid LIKE LINE OF it_roid.    "For Selected Row ID
DATA: it_row TYPE lvc_t_row.           "For Selected Row ID:Before 620
DATA: wa_row LIKE LINE OF it_row.      "For Selected Row ID:Before 620

*/For WA & Itab for ALV Structure
DATA: wa_zsmm_6002_01 LIKE zsmm_6002_01.
DATA: it_zsmm_6002_01 LIKE TABLE OF wa_zsmm_6002_01.

*/For ALV Handler
CLASS lcl_h_alv DEFINITION DEFERRED.
DATA: crv_h_alv TYPE REF TO lcl_h_alv.  "Handler of ALV

*/For SmartForm
DATA: help_form           TYPE ssfscreen-fname,
      func_module_name    TYPE rs38l_fnam,
      control_parameters  TYPE ssfctrlop,
      output_options      TYPE ssfcompop,

      color(4),
      return_code         TYPE i.

***Selection Screen*****************************************************
TABLES: rkpf.            "Document Header: Reservation
TABLES: resb.            "Reservation/dependent requirements

SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS: s_rsnum FOR rkpf-rsnum.  "Material
SELECT-OPTIONS: s_matnr FOR resb-matnr.  "Material
SELECT-OPTIONS: s_kostl FOR rkpf-kostl.  "Cost center
SELECT-OPTIONS: s_aufnr FOR rkpf-aufnr.  "Order
SELECT-OPTIONS: s_anln1 FOR rkpf-anln1.  "Asset
SELECT-OPTIONS: s_saknr FOR resb-saknr.  "G/L account number
SELECT-OPTIONS: s_bwart FOR rkpf-bwart.  "Movement type
SELECT-OPTIONS: s_bdter FOR resb-bdter   "Requirements date
                        DEFAULT sy-datum
                        OBLIGATORY.
SELECT-OPTIONS: s_rsdat FOR rkpf-rsdat.  "Base date for reservation
SELECT-OPTIONS: s_werks FOR resb-werks.  "Issue plant
SELECT-OPTIONS: s_lgort FOR resb-lgort.  "Issue stor. loc.
SELECT-OPTIONS: s_umwrk FOR rkpf-umwrk.  "Receiving plant
SELECT-OPTIONS: s_umlgo FOR rkpf-umlgo.  "Receiving stor. loc.
SELECT-OPTIONS: s_usnam FOR rkpf-usnam.  "Created by
SELECTION-SCREEN END OF BLOCK bl_1.
