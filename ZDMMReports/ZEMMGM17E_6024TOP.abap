*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM17E_6024TOP
*
*----------------------------------------------------------------------*

*/ Itab & wa for ZTMM_6024_01
* ZTMM_6024_01: EMMGM17 Characteristic Creation Log Table

  DATA: wa_ztmm_6024_01 LIKE ztmm_6024_01.
  DATA: it_ztmm_6024_01 LIKE TABLE OF wa_ztmm_6024_01.
  FIELD-SYMBOLS: <fs_ztmm_6024_01> LIKE LINE OF it_ztmm_6024_01.


*/ For File upload
  DATA: BEGIN OF wa_charact_data,
         classnumnew LIKE bapi_class_key-classnum,"Desc
         catchword   LIKE bapi1003_catch-catchword,
         classgroup  LIKE bapi1003_basic-classgroup,  "'SB'
         name_char   LIKE bapi1003_charact-name_char,
         zzret       TYPE zzret,
        END OF wa_charact_data.

  DATA: it_class_data LIKE TABLE OF wa_charact_data.
  FIELD-SYMBOLS: <fs_class_data> LIKE LINE OF it_class_data.

  DATA: w_filesize TYPE i,
        w_cancel,
        w_act_filename LIKE rlgrap-filename,
        w_act_filetype LIKE rlgrap-filetype,
        w_filelength TYPE i.

**** Constants&Vars for Number range object****************************
  CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
  CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
  CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
  DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                                 LIKE inri-object. "NRO for zbdcmsgcoll
* Number_Get_Next
  DATA:      w_nro_number  TYPE num10.      " Same type of nro_object

  DATA: w_zdocno TYPE num10.       "App. Doc. No.


* For interface log
  DATA: wa_ztca_if_log LIKE ztca_if_log.


* For OK code
  DATA: ok_code TYPE sy-ucomm,  save_ok_code LIKE
ok_code.

* For PF-STATUS and Titlebar
  CLASS lcl_ps DEFINITION DEFERRED.
  DATA: crv_ps TYPE REF TO lcl_ps.

*/ Begin of Menu
* Itab & WA For FOR Dynamic Menu
  DATA:  it_func TYPE STANDARD TABLE OF rsmpe-func.
  DATA:  wa_func LIKE LINE OF it_func.
* Title
  DATA: w_title(80).         " Title
*/ End of Menu

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
  DATA: it_roid TYPE lvc_t_roid.        "For Selected row id
  DATA: wa_roid LIKE LINE OF it_roid. "For Selected row id
  DATA: it_row TYPE lvc_t_row.        "For Selected row id:before 620
  DATA: wa_row LIKE LINE OF it_row.   "For Selected row id:before 620

* For BAPI
  DATA: it_classdescriptions LIKE TABLE OF bapi1003_catch.
  DATA: wa_classdescriptions LIKE LINE OF it_classdescriptions.

  DATA: it_classcharacteristics LIKE TABLE OF bapi1003_charact.
  DATA: wa_classcharacteristics LIKE LINE OF it_classcharacteristics.


  DATA: it_bapiret2 LIKE TABLE OF bapiret2.
  DATA: wa_bapiret2 LIKE LINE OF it_bapiret2.

  DATA: wa_classbasicdata LIKE bapi1003_basic.


**** Selection Screen **************************************************
  TABLES: cdhdr.            "Change document header

  SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
  PARAMETERS:
    p_file LIKE    rlgrap-filename
            DEFAULT 'C:\class_data.txt'
            OBLIGATORY.

  SELECTION-SCREEN END OF BLOCK bl_1.
