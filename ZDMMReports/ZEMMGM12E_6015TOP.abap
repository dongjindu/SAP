*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM15E_6018TOP                                          *
*----------------------------------------------------------------------*

**** Itab & WA for Data Manipulation
DATA: BEGIN OF wa_ztmm_6015_02_ext.
        INCLUDE STRUCTURE: ztmm_6015_02.
DATA:
*For Case A
       equnr      LIKE ztmm_6015_01-equnr,     "VIN no
       ftzoutdate LIKE ztmm_6015_01-ftzoutdate,"FTZ-Out Date
       atwre      LIKE ztmm_6015_01-atwre,     "Ext.Color
       atwri      LIKE ztmm_6015_01-atwri,     "Int.Color
*For Case B
       idnrkmblnr LIKE mkpf-mblnr, "Number of Material Doc
       idnrkmjahr LIKE mkpf-mjahr, "Material doc. year
*For Case c
       srcmblnr   LIKE mkpf-mblnr, "Number of Material Doc
       srcmjahr   LIKE mkpf-mjahr, "Material doc. year

      END OF wa_ztmm_6015_02_ext.
DATA: it_ztmm_6015_02_ext LIKE TABLE OF wa_ztmm_6015_02_ext.
FIELD-SYMBOLS: <fs_ztmm_6015_02_ext>
                    LIKE LINE OF it_ztmm_6015_02_ext.


*/ For Case A
DATA: it_stpox_alv LIKE TABLE OF stpox_alv.
"BOM Items (Extended for List Displays)
DATA: wa_stpox_alv LIKE LINE OF it_stpox_alv.

DATA: w_signoffdate TYPE d.   "Sign Off Date

*/ For Case B

*/ For Case C
DATA: it_stpox LIKE TABLE OF stpox.
DATA: wa_stpox LIKE LINE OF it_stpox.
FIELD-SYMBOLS: <fs_stpox> LIKE LINE OF it_stpox.




**** Itab & WA for Data Manipulation
DATA: it_ztmm_6015_02 LIKE TABLE OF ztmm_6015_02.
DATA: wa_ztmm_6015_02 LIKE LINE OF it_ztmm_6015_02.
FIELD-SYMBOLS: <fs_ztmm_6015_02> LIKE wa_ztmm_6015_02.


* For OK code
  DATA: ok_code TYPE sy-ucomm,  save_ok_code LIKE ok_code.

* For PF-STATUS and Titlebar
  CLASS lcl_ps DEFINITION DEFERRED.
  DATA: crv_ps TYPE REF TO lcl_ps.

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
CONSTANTS: w_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: w_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: w_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.



*---------- Selection Screen ------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
SELECT-OPTIONS: s_period FOR sy-datum.    "Period
*                      OBLIGATORY.

SELECTION-SCREEN END OF BLOCK bl_1.
