*&------------------------------------------------------------------*
*&  Include           ZLER0026_DEF                                  *
*&------------------------------------------------------------------*
*-------------------------------------------------------------------*
*   TABLE DECLARATION                                               *
*-------------------------------------------------------------------*
TABLES : pa0001, zspm0019.

*-------------------------------------------------------------------*
*   TYPE-POOLS                                                      *
*-------------------------------------------------------------------*
TYPE-POOLS: slis,
            sydes,
            kkblo.
*// ALV Global Field
TYPE-POOLS : f4typ.
*-------------------------------------------------------------------*
*   INCLUDE                                                         *
*-------------------------------------------------------------------*
INCLUDE <icon>.
INCLUDE <symbol>.

*-------------------------------------------------------------------*
*   DATA DECLARATION (INTERNAL TABLE DECLARE)                       *
*-------------------------------------------------------------------*

DATA : g_lminb LIKE mard-lminb,
       g_lbstf LIKE mard-lbstf,
       g_maabc like marc-maabc. "03.12.2014 Victor

*-> pop up open_order list
DATA : BEGIN OF it_poqty OCCURS 0,
        matnr   TYPE mara-matnr,
        werks   TYPE ekpo-werks,
        ebeln   TYPE ekpo-ebeln,
        ebelp   TYPE ekpo-ebelp,
        bstae   TYPE ekpo-bstae,
        meins   TYPE ekpo-meins,
        menge   TYPE eket-menge,
        wemng   TYPE eket-wemng,
        ameng   TYPE leshp_web_purord_itm-ameng,
        opnqt   TYPE ekpo-menge,   "open quantity
       END OF it_poqty.
DATA it_poqty_t LIKE TABLE OF it_poqty WITH HEADER LINE.

DATA : BEGIN OF it_preq OCCURS 0,
        banfn   TYPE eban-banfn,
        bnfpo   TYPE eban-bnfpo,
        matnr   TYPE mara-matnr,
        werks   TYPE ekpo-werks,
        meins   TYPE ekpo-meins,
        menge   TYPE eket-menge,
       END OF it_preq.

DATA : it_preq_t LIKE it_preq OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF g_cond OCCURS 0,
         text(72),
       END OF g_cond.


DATA: BEGIN OF g_itab_t OCCURS 0.
        INCLUDE STRUCTURE zspm0019.
DATA: END OF g_itab_t.

DATA : g_itab2_t LIKE g_itab_t OCCURS 0 WITH HEADER LINE.


DATA: BEGIN OF g_header_t OCCURS 0,
      box,
      icon(5),
      icon2(5).
        INCLUDE STRUCTURE g_itab_t.
DATA: END OF g_header_t.

DATA: BEGIN OF g_detail_t OCCURS 0,
        mark,
        icon1(5).
        INCLUDE STRUCTURE zspm0020.
DATA:  END OF g_detail_t.

DATA : error_in_data TYPE c.
DATA: ok_code  LIKE sy-ucomm.
DATA: gv_index LIKE sy-index,
      gv_index2 LIKE sy-index.

DATA: g_check2.

DATA : g_err(1)         VALUE 'N',   "에러없음
       g_index          LIKE  sy-tabix,
       g_ucomm          LIKE  sy-ucomm,
       g_cnt(5)         TYPE n,
       g_rc.

DATA : l_rundung LIKE pso39-psorz.

CONSTANTS : c_stat_a TYPE c VALUE 'A',
            c_stat_c TYPE c VALUE 'C'.

RANGES: r_date FOR sy-datum.

*--------------------------------------------------------------------*
*   CONSTANTS                                                        *
*--------------------------------------------------------------------*
DATA: custom_container TYPE REF TO cl_gui_custom_container,
      docking_container1 TYPE REF TO cl_gui_docking_container,
      docking_container2 TYPE REF TO cl_gui_docking_container,
      docking_container3 TYPE REF TO cl_gui_docking_container,
      docking_container4 TYPE REF TO cl_gui_docking_container,
      dialogbox_container TYPE REF TO cl_gui_dialogbox_container,
      splitter_container_1 TYPE REF TO cl_gui_easy_splitter_container.
DATA: item_tab  TYPE cntl_item,
      appl_init(1),
      item_tab_oo TYPE cntl_item_tab,  "// no header line
*      ok_code like sy-ucomm,
      width TYPE i, left TYPE  i,
      top   TYPE i, height TYPE i,
      this_repid LIKE sy-repid,
      this_dynnr LIKE sy-dynnr.

* BDC Detail Message.
DATA:
  BEGIN OF gt_bdcmsg OCCURS 0,
    updkz TYPE updkz_d,
    banfn TYPE eban-banfn.
        INCLUDE STRUCTURE bapiret2.
DATA:
  END OF gt_bdcmsg.
DATA:
  BEGIN OF gt_bdcmsg2 OCCURS 0,
    updkz TYPE updkz_d,
    vbeln TYPE likp-vbeln.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA:
  END OF gt_bdcmsg2.


DATA : c_dbeln LIKE lips-vbeln.
DATA : BEGIN OF gt_itab OCCURS 0,
       mark,
       icon   LIKE icon-name,
       new_pr LIKE eban-banfn,
       banfn  LIKE eban-banfn,
       bnfpo  LIKE eban-bnfpo,
       bsart  LIKE eban-bsart,
       knttp  LIKE eban-knttp,
       matnr  LIKE eban-matnr,
       menge  LIKE eban-menge,
       meins  LIKE eban-meins,
       lfdat  LIKE eban-lfdat,
       ekgrp  LIKE eban-ekgrp,
       afnam  LIKE eban-afnam,
       werks  LIKE eban-werks,
       preis  LIKE eban-preis,
       waers  LIKE eban-waers,
       peinh  LIKE eban-peinh,
       bednr  LIKE eban-bednr,
       sakto  LIKE ebkn-sakto,
       kostl  LIKE ebkn-kostl,
       anln1  LIKE ebkn-anln1,
       aufnr  LIKE ebkn-aufnr,
       fistl  LIKE ebkn-fistl,
       fipos  LIKE ebkn-fipos,
       account1(16) TYPE c,
       account2(16) TYPE c,
       type,
       message(100) TYPE c,
       END OF gt_itab.

DATA: it_eban LIKE bapiebanc OCCURS 0 WITH HEADER LINE.
DATA: it_ebkn LIKE bapiebkn OCCURS 0 WITH HEADER LINE.

DATA: v_return LIKE bapireturn OCCURS 0 WITH HEADER LINE,
      v_banfn  LIKE eban-banfn.

DATA: it_exten        LIKE TABLE OF bapiparex  WITH HEADER LINE.

DATA : g_afnam  LIKE  eban-ernam,
 gs_prheader    LIKE  bapimereqheader         ,
 gs_prheaderx   LIKE  bapimereqheaderx        ,
 g_banfn        LIKE  bapimereqheader-preq_no ,
 gt_return      LIKE  bapiret2           OCCURS 0 WITH HEADER LINE,
 gt_pritem      LIKE  bapimereqitemimp   OCCURS 0 WITH HEADER LINE,
 gt_pritemexp   LIKE  bapimereqitem      OCCURS 0 WITH HEADER LINE,
 gt_pritemx     LIKE  bapimereqitemx     OCCURS 0 WITH HEADER LINE,
 gt_pracct      LIKE  bapimereqaccount   OCCURS 0 WITH HEADER LINE,
 gt_pracctx     LIKE  bapimereqaccountx  OCCURS 0 WITH HEADER LINE,
 gt_praseg      LIKE  bapimereqaccountprofitseg  OCCURS 0 WITH HEADER
LINE.

*--------------------------------------------------------------------*
*   SELECTION SCREEN                                                 *
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-m05.

SELECT-OPTIONS : s_werks FOR marc-werks NO-EXTENSION NO INTERVALS
*                                          BLIGATORY
                                          DEFAULT 'P001' MEMORY ID wrk.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) h_shop.
SELECT-OPTIONS : s_lgort  FOR mard-lgort  NO-EXTENSION NO INTERVALS
                                          OBLIGATORY .
SELECTION-SCREEN COMMENT 50(15) t_fing.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS : s_matnr FOR mard-matnr.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) h_pernr.
PARAMETERS : p_pernr LIKE pa0001-pernr OBLIGATORY.
SELECTION-SCREEN COMMENT 50(15) t_pernr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-m01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(5) text-m02 FOR FIELD p_r10.
PARAMETERS p_r10 RADIOBUTTON GROUP rad2.
SELECTION-SCREEN COMMENT 15(20) text-m03 FOR FIELD p_r20.
PARAMETERS p_r20  RADIOBUTTON GROUP rad2.
SELECTION-SCREEN COMMENT 45(15) text-m04 FOR FIELD p_r30.
PARAMETERS p_r30  RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 70(12) text-m06 FOR FIELD p_r40.
PARAMETERS p_r40  RADIOBUTTON GROUP rad2.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b0.


*SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-004.
*PARAMETERS : p_vari    LIKE disvariant-variant.
*SELECTION-SCREEN END OF BLOCK bl4.
**----------------------------------------------------------------------
*
