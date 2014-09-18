*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_PPC0_OCDTOP
*&---------------------------------------------------------------------*
TABLES: ppc_ord_inf, plaf.

TYPES:
  BEGIN OF gty_s_order,
*    headid     TYPE ppc_headid_int,
    orderid    TYPE ppc_orderid,
    materialnr TYPE matnr,
    plant      TYPE werks_d,
    ordernr    TYPE plnum, "ppc_ordernr,
  END   OF gty_s_order,
  gty_t_order  TYPE STANDARD TABLE OF gty_s_order,

  BEGIN OF gty_s_string,
    part1   TYPE symsgv,
    part2   TYPE symsgv,
    part3   TYPE symsgv,
    part4   TYPE symsgv,
  END OF gty_s_string,

  gtr_ordid    TYPE RANGE OF ppc_orderid,
  gtr_ordnr    TYPE RANGE OF plnum,
  gtr_matnr    TYPE RANGE OF matnr,
  gtr_werks    TYPE RANGE OF werks_d.

TYPES:
  BEGIN OF ts_ord_del,
    orderid      TYPE ppc_orderid,
    flg_synch    TYPE ppc_sync_posted,
    flg_asynch   TYPE ppc_async_posted,
    flg_asynch_a TYPE ppc_act_posted,
  END OF ts_ord_del.

* Constants ----------------------------------------------------------
CONSTANTS:
  gc_msgid          TYPE symsgid     VALUE 'ZSAPBF_PPC0',
  BEGIN OF gc_msgty,
    abend           TYPE symsgty     VALUE 'A',
    error           TYPE symsgty     VALUE 'E',
    warning         TYPE symsgty     VALUE 'W',
    info            TYPE symsgty     VALUE 'I',
    soft            TYPE symsgty     VALUE 'S',
    exit            TYPE symsgty     VALUE 'X',
  END OF gc_msgty,
*  gc_msgno_942      TYPE symsgno     VALUE '942',
  gc_msgno_943      TYPE symsgno     VALUE '943',
  gc_msgno_944      TYPE symsgno     VALUE '944',
  gc_true           TYPE xfeld       VALUE 'X',
*  gc_block_size       TYPE i         VALUE '25',
*  gc_time           TYPE syuzeit     VALUE '000000',
*  gc_al_extnumber_ocd TYPE balnrext  VALUE 'PPC0_OCD',
  gc_al_object      TYPE balobj_d    VALUE 'ZSAPBF_HKMC_DI',
  gc_al_subobj      TYPE balsubobj   VALUE 'ZSAPBF_DEL_CONF',
  gc_freetext_msgid TYPE symsgid     VALUE 'BL',
  gc_freetext_msgno TYPE symsgno     VALUE '001'.

* Global variables
DATA:
  gt_ordid         TYPE ppc_t_orderid,
*  gs_ordid         TYPE LINE OF ppc_t_orderid,

*  gt_block_ord TYPE gty_t_ord,
*  lv_ordid         TYPE ppc_orderid,
*  gv_selordtime    TYPE ppc_conftime,
  gv_selconftime   TYPE ppc_conftime,
  gv_log_handle    TYPE balloghndl,
*  gv_text(200)     TYPE c,
  gv_log_exist     TYPE xfeld.
*  gv_flag          TYPE char1.

* --------------------------------------------------------------------- *
* Selection-Screen
* --------------------------------------------------------------------- *
SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-001.
SELECT-OPTIONS:  so_ordid  FOR  ppc_ord_inf-orderid,
                 so_plord  FOR  plaf-plnum,
                 so_matnr  FOR  ppc_ord_inf-materialnr,
                 so_plant  FOR  ppc_ord_inf-plant.
PARAMETERS:      "p_ordday  TYPE zsapbf_ppc_order_days_in_past OBLIGATORY DEFAULT 0,
                 p_confds  TYPE zsapbf_ppc_conf_days_in_past OBLIGATORY DEFAULT 0.
SELECTION-SCREEN END OF BLOCK selection.
SELECTION-SCREEN BEGIN OF BLOCK control WITH FRAME TITLE text-002.
PARAMETERS:      "p_blsize  TYPE zsapbf_ppc_block_size,
                 p_protok  TYPE ppc_proto AS CHECKBOX DEFAULT 'X',
                 p_test    TYPE CFTEST AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK control.
