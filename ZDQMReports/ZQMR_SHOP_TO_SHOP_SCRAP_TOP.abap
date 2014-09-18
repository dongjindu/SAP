*&---------------------------------------------------------------------*
*&  Include           ZQM_SHOP_TO_SHOP_SCRAP_TOP
*&---------------------------------------------------------------------*

*... TABLES
TABLES: qmfe, qpct, qmel, zsqm_qmgrp.

DATA: gran_qmgrp TYPE RANGE OF qmgrp,
      gran_fecod TYPE RANGE OF fecod.
DATA: gs_fecod LIKE LINE OF gran_fecod.

DEFINE add_fecod.
  clear gs_fecod.

  gs_fecod-low    = &1.
  gs_fecod-sign   = 'E'.
  gs_fecod-option = 'EQ'.
  append gs_fecod to gran_fecod.

END-OF-DEFINITION.

DATA: BEGIN OF gs_record,
        qmnum       TYPE qmnum,
        mawerk      TYPE qmawerks,
        matnr       TYPE matnr,
        maktx       TYPE maktx,
        rkmng       TYPE rkmng,
        mgein       TYPE mgein,
        lifnum      TYPE lifnum,
        ernam       TYPE ernam,
        qmdat       TYPE qmdat,
        mzeit       TYPE mzeit,
        qmkat       TYPE qmkat,
        qmgrp       TYPE qmgrp,
        fekat       TYPE fekat,
        fever       TYPE qversnr,
        fegrp       TYPE fegrp,
        fecod       TYPE fecod,
        code        TYPE qcode,
        katalogart  TYPE qkatart,
        version	    TYPE qversnr,
        codegruppe  TYPE qcodegrp,
        kurztext1   TYPE qktextgr,
        kurztext2   TYPE qtxt_code,
*        tabcolor    TYPE lvc_t_scol,
*        celltab     TYPE lvc_t_styl,
      END OF gs_record.

DATA: gt_record LIKE TABLE OF gs_record.

DATA: g_okcode   LIKE sy-ucomm,
      g_save_ok  LIKE sy-ucomm,
      g_html_init(1) TYPE c.

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
************************************************************************
* Selection Screen                                                     *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK f01 WITH FRAME TITLE text-b01.

SELECT-OPTIONS s_qmart  FOR qmel-qmart MEMORY ID qmr.
SELECT-OPTIONS s_mawerk FOR qmel-mawerk MEMORY ID wrk.
SELECT-OPTIONS s_qmdat  FOR qmel-qmdat.
SELECT-OPTIONS s_qmkat  FOR qmel-qmkat.
SELECT-OPTIONS s_qmgrp  FOR zsqm_qmgrp-qmgrp NO INTERVALS.
SELECT-OPTIONS s_fegrp  FOR qmfe-fegrp.
*SELECT-OPTIONS s_fecod  FOR qmfe-fecod.
SELECT-OPTIONS s_matnr  FOR qmel-matnr MEMORY ID mat.
SELECT-OPTIONS s_lifnum FOR qmel-lifnum MEMORY ID lif.
SELECT-OPTIONS s_qmnum  FOR qmel-qmnum MEMORY ID iqm.
SELECT-OPTIONS s_ernam  FOR qmel-ernam.

SELECTION-SCREEN END OF BLOCK f01.
