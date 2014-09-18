*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM24I_6017TOP                                          *
*----------------------------------------------------------------------*
*/ Itab & wa for ZTMM_6017_01
* ZTMM_6017_01: Material Master Interface Table
*  DATA: BEGIN OF wa_ztmm_6017_01,
*         matnr LIKE mara-matnr,
*         maktx LIKE makt-maktx, "Material description
*         meins LIKE mara-meins, "Base UoM
*         ntgew LIKE mara-ntgew, "Net weight
*         gewei LIKE mara-gewei, "Weight Unit
*         netpr LIKE eine-netpr, "Net price in purchasing info record
*         waers LIKE eine-waers, "Currency Key
*         date_time(15),         "Date Time [YYYYMMDDTHHMMSS]
*         udate LIKE cdhdr-udate,"Creation date of the change document
*         utime LIKE cdhdr-utime,"Time changed
*        END OF wa_ztmm_6017_01.
*  DATA: wa_ztmm_6017_01 LIKE ztmm_6017_01.
  DATA: BEGIN OF wa_ztmm_6017_01.
          INCLUDE STRUCTURE ztmm_6017_01.
  DATA: " profl LIKE mara-profl,
        END OF wa_ztmm_6017_01.


  DATA: it_ztmm_6017_01 LIKE TABLE OF wa_ztmm_6017_01.
  FIELD-SYMBOLS: <fs_ztmm_6017_01> LIKE LINE OF it_ztmm_6017_01.


**** Constants&Vars for Number range object ****************************
  CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
  DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

  DATA: w_zdocno TYPE num10.       "App. Doc. No.


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



*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
  TABLES: cdhdr.            "Change document header

  SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
  SELECT-OPTIONS: s_udate FOR cdhdr-udate
                               "Creation date of the change document
                          OBLIGATORY.
  SELECTION-SCREEN END OF BLOCK bl_1.

*/Additional Action(All Transfer)
  SELECTION-SCREEN BEGIN OF BLOCK bl_2 WITH FRAME TITLE text-bl2.
  PARAMETERS: p_all AS CHECKBOX DEFAULT space.
  SELECTION-SCREEN END OF BLOCK bl_2.
