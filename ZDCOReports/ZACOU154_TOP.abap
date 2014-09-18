*&---------------------------------------------------------------------*
*&  Include           ZACOU154_TOP
*&---------------------------------------------------------------------*

TABLES: vbap.

DATA: g_sflag(1) TYPE c.

DATA: g_begda TYPE datum,
      g_endda TYPE datum.

*... Begin{ Data for ALV Screen
DATA: g_okcode   LIKE sy-ucomm,
      g_save_ok  LIKE sy-ucomm,
      g_html_init(1) TYPE c.

DATA: gr_matnr TYPE RANGE OF matnr,
      gs_matnr LIKE LINE OF gr_matnr.

DATA: BEGIN OF gs_s_matnr,
         matnr    TYPE matnr,
         quantity TYPE p,
         count    TYPE i,
      END OF gs_s_matnr.

DATA: gt_s_matnr LIKE TABLE OF gs_s_matnr.
*...
DATA: BEGIN OF gs_record,
        vbeln       TYPE vbeln_va,
        posnr       TYPE posnr_va,
        matnr       TYPE matnr,
        maktx       TYPE maktx,
        kwmeng      TYPE kwmeng,
        rfmng       TYPE rfmng,
        open_no     TYPE p,
        post_no     TYPE p,
        vrkme       TYPE vrkme,
        erdat       TYPE erdat,
        icon(4)     TYPE c,
        doc_num     TYPE vbeln,
        message     TYPE bapi_msg,
        tabcolor    TYPE lvc_t_scol,
        celltab     TYPE lvc_t_styl,
      END OF gs_record,
      gt_record LIKE TABLE OF gs_record.

DATA: gt_fieldcat         TYPE lvc_t_fcat,
      gt_exclude          TYPE ui_functions,
      gs_layout           TYPE lvc_s_layo,
      gs_variant          LIKE disvariant,
      g_repid             LIKE sy-repid,
      g_grid              TYPE REF TO cl_gui_alv_grid,
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      g_container         TYPE scrfname VALUE 'CNTL',
      g_event_receiver    TYPE REF TO zcl_alv_event_receiver.

DATA: gt_alv_sort TYPE lvc_t_sort,
      gs_alv_sort LIKE LINE OF gt_alv_sort.
*...}End

************************************************************************
* Selection Screen                                                     *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK f01 WITH FRAME TITLE text-b01.

*SELECT-OPTIONS: s_matnr FOR vbap-matnr NO INTERVALS. "  MATCHCODE OBJECT s_mat1 OBLIGATORY.
PARAMETERS: p_vstel TYPE vstel MATCHCODE OBJECT h_tvst OBLIGATORY,
            p_bdate TYPE datum OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31)   text-t01 FOR FIELD p_matnr1.
PARAMETERS :  p_matnr1           TYPE matnr MATCHCODE OBJECT s_mat1 OBLIGATORY.
SELECTION-SCREEN COMMENT 55(12)  text-t02 FOR FIELD  p_quan1.
PARAMETERS :  p_quan1(3)          TYPE c OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31)   text-t03 FOR FIELD p_matnr2.
PARAMETERS :  p_matnr2           TYPE matnr.
SELECTION-SCREEN COMMENT 55(12)  text-t02 FOR FIELD  p_quan2.
PARAMETERS :  p_quan2(3)          TYPE c.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31)   text-t04 FOR FIELD p_matnr3.
PARAMETERS :  p_matnr3           TYPE matnr.
SELECTION-SCREEN COMMENT 55(12)  text-t02 FOR FIELD  p_quan3.
PARAMETERS :  p_quan3(3)          TYPE c.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK f01.

SELECTION-SCREEN BEGIN OF BLOCK f02 WITH FRAME TITLE text-b02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(6)   text-t05 FOR FIELD p_radio1.
PARAMETERS :  p_radio1 TYPE c RADIOBUTTON GROUP rd.
SELECTION-SCREEN COMMENT 20(6)  text-t06 FOR FIELD  p_radio2.
PARAMETERS :   p_radio2 TYPE c RADIOBUTTON GROUP rd.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK f02.
