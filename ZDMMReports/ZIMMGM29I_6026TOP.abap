*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM29I_6026TOP                                          *
*----------------------------------------------------------------------*
*/ Itab & wa for ztmm_6026_01
  DATA: BEGIN OF wa_ztmm_6026_01.
          INCLUDE STRUCTURE ztmm_6026_01.
  DATA:  ebeln like mseg-ebeln,
*         "profl LIKE mara-profl,
*         werks LIKE mseg-werks,
*         BPUMZ LIKE eina-BPUMZ,
*             "Numerator for conversion of order unit to base unit
*         BPUMN LIKE eina-BPUMN,
*             "Denominator for conversion of order unit to base unit
        END OF wa_ztmm_6026_01.

  DATA: it_ztmm_6026_01 LIKE TABLE OF wa_ztmm_6026_01.
  DATA: it_ztmm_6026_01_tmp LIKE TABLE OF wa_ztmm_6026_01.
  FIELD-SYMBOLS: <fs_ztmm_6026_01> LIKE LINE OF it_ztmm_6026_01.
* Especially for PPIM
  DATA: it_ppim LIKE it_ztmm_6026_01.

*/ For Adjusting
  DATA: BEGIN OF wa_budat_udate_utime,
         budat LIKE ztmm_6026_01-budat,
         udate LIKE ztmm_6026_01-udate,
         utime LIKE ztmm_6026_01-utime,
        END OF wa_budat_udate_utime.
  DATA: it_budat_udate_utime LIKE TABLE OF wa_budat_udate_utime.

*&---Shiva
  data: begin of wa_marc,
          matnr like marc-matnr,
          werks like marc-werks,
        end of wa_marc.
  data: begin of wa_impreq,
          zfreqno like ztreqhd-zfreqno,
          ebeln   like ztreqhd-ebeln,
        end of wa_impreq.
  data: t_marc like table of wa_marc,
        t_impreq like table of wa_impreq.
*&---end shiva
**** Constants&Vars for Number range object ****************************
  CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

  DATA:      w_zdocno      TYPE num10.       "App. Doc. No.


* For interface log
  DATA: wa_ztca_if_log LIKE ztca_if_log.


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

*/Miscellaneous
  DATA: w_lines TYPE i.  "For the number of Itab Lines
  DATA: w_date  TYPE d.  "Present Date
  DATA: w_time  TYPE t.  "Present time


*.........................................................
  DATA: w_save,                          "for Parameter I_SAVE
        wa_disvariant TYPE disvariant.   "for parameter IS_VARIANT
*...........................................................


*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
  TABLES: mkpf.            "Header: Material Document
*/General or Adjusting Conditions
  SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 3(31) text-li1.                  "Line 1
  PARAMETERS rb1
             DEFAULT 'X'
             RADIOBUTTON GROUP rbg USER-COMMAND fcrb.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 3(31) text-li2.                  "Line 2
  PARAMETERS rb2 RADIOBUTTON GROUP rbg.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK bl_1.

*/Selection Conditions
  SELECTION-SCREEN BEGIN OF BLOCK bl_2 WITH FRAME TITLE text-bl2.
  PARAMETERS: p_budat LIKE mkpf-budat  "Material Doc Posting Date
                      MODIF ID m01     "To modify screen.
                      OBLIGATORY.
*/Begin of Added by Hakchin(20040426)
  SELECT-OPTIONS: s_budat FOR mkpf-budat  "Material Doc Posting Date
                          MODIF ID m02    "To modify screen.
                          OBLIGATORY.
*/End of Added by Hakchin(20040426)
  SELECTION-SCREEN END OF BLOCK bl_2.



*/Begin of For Test by Hakchin(20040512)

*/IPPC IPIM FSC Selection
  SELECTION-SCREEN BEGIN OF BLOCK bl_3 WITH FRAME TITLE text-bl3.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 3(31) text-li3                   "Line 3
                   MODIF ID m01.     "To modify screen.
  PARAMETERS rb3
             MODIF ID m01     "To modify screen.
             RADIOBUTTON GROUP rbgt. " USER-COMMAND fcrb.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 3(31) text-li4                   "Line 4
                   MODIF ID m01.     "To modify screen.
  PARAMETERS rb4
             MODIF ID m01     "To modify screen.
             RADIOBUTTON GROUP rbgt.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 3(31) text-li5                   "Line 5
                   MODIF ID m01.     "To modify screen.
  PARAMETERS rb5
             MODIF ID m01     "To modify screen.
             DEFAULT 'X'
             RADIOBUTTON GROUP rbgt.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK bl_3.
*/End of For Test by Hakchin(20040512)
