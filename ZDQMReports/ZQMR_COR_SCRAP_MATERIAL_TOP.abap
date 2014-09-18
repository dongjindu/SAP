*&---------------------------------------------------------------------*
*&  Include           ZQMR_COR_SCRAP_MATERIAL_TOP
*&---------------------------------------------------------------------*

*... TABLES
TABLES: jcds, qmel.

DATA: BEGIN OF gs_record,
        qmnum       TYPE qmnum,
        matnr       TYPE matnr,
        maktx       TYPE maktx,
        rkmng       TYPE rkmng,
        mgein       TYPE mgein,
        lifnum      TYPE lifnum,
        ernam       TYPE ernam,
        qmdat       TYPE qmdat,
        mzeit       TYPE mzeit,
        mawerk      TYPE qmawerks,
        stat        TYPE j_status,
        udate       TYPE cddatum,
        utime       TYPE cduzeit,
        cdtcode     TYPE cdtcode,
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

SELECT-OPTIONS s_stat     FOR jcds-stat.
SELECT-OPTIONS s_chgnr    FOR jcds-chgnr.
SELECT-OPTIONS s_udate    FOR jcds-udate.
SELECT-OPTIONS s_utime    FOR jcds-utime.
SELECT-OPTIONS s_ccode    FOR jcds-cdtcode.
SELECT-OPTIONS s_inact    FOR jcds-inact.
SELECT-OPTIONS s_chind    FOR jcds-chind.

SELECTION-SCREEN END OF BLOCK f01.
