*----------------------------------------------------------------------*
*   INCLUDE ZACO16L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : tka01, t134, aufk, cska, cobk, coep, mara, MSEG, ztco_abispost.

** Internal Table
* For I/O COEP
DATA : BEGIN OF it_io_coep OCCURS 1000.
DATA :  matnr  LIKE coep-matnr,
        stype  like ZTCO_ABISPOST-stype,
        werks  LIKE coep-werks,
        wkgbtr LIKE coep-wkgbtr,
        waers  LIKE tka01-waers,
        fevor  LIKE marc-fevor,
        objnr  LIKE coep-objnr,
        kstar  LIKE coep-kstar,
        REFBN  LIKE COVP-REFBN,
        GJAHR  LIKE COVP-GJAHR,
        BUZEI  LIKE COVP-BUZEI,
* Qty
        mefbtr LIKE coep-mefbtr,
        mbgbtr LIKE coep-mbgbtr,
        mbfbtr LIKE coep-mbfbtr,
        meinb  LIKE coep-meinb.
DATA : END OF   it_io_coep.
* For PCC order and material Information
DATA : BEGIN OF it_pcc_mat OCCURS 500,
        matnr  LIKE coep-matnr,
        werks  LIKE coep-werks,
        fevor  LIKE marc-fevor,
        aufnr  LIKE aufk-aufnr,
        objnr  LIKE coep-objnr,
       END OF   it_pcc_mat.
* For PCC B/F data
DATA : BEGIN OF it_pcc_coep OCCURS 1000.
        INCLUDE STRUCTURE it_io_coep.
*        INCLUDE STRUCTURE COEP.
DATA :  rate_child    LIKE ztco_abispost-rate_child.
DATA :  mbg_int       LIKE ztco_abispost-mbgbtr.
DATA : END OF   it_pcc_coep.
DATA : BEGIN OF it_tot_rate OCCURS 0.
DATA :
*       KSTAR  LIKE COEP-KSTAR,
        werks  LIKE coep-werks,
        objnr  LIKE coep-objnr,
        fevor  LIKE marc-fevor,
        kstar_rate    LIKE ztco_abispost-kstar_rate.
DATA : END OF  it_tot_rate.
* For POSTING
DATA : BEGIN OF it_post OCCURS 500.
        INCLUDE STRUCTURE ztco_abispost.
DATA : END OF it_post.
*DATA:  it_post_tmp LIKE it_post OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_post_fin OCCURS 500,
        kstar      LIKE ztco_abispost-kstar,
        matnr      like ztco_abispost-matnr,
        stype      like ztco_abispost-stype,
        io_aufnr   LIKE ztco_abispost-io_aufnr,
        pcc_aufnr  LIKE ztco_abispost-pcc_aufnr,
        chg_wkgbtr LIKE ztco_abispost-chg_wkgbtr,
        waers      LIKE ztco_abispost-waers,
        belnr      LIKE ztco_abispost-belnr,
        MBGBTR     LIKE ztco_abispost-MBGBTR,
        MEINB      LIKE ZTCO_ABISPOST-MEINB,
       END OF it_post_fin.
DATA : it_l_aufk LIKE STANDARD TABLE OF aufk
                 WITH HEADER LINE .

** Global Variables
RANGES : r_io_objnr FOR coep-objnr.
RANGES : r_fk_mtart FOR mara-mtart.

** Constants
CONSTANTS: c_blank  LIKE marc-fevor VALUE 'SPB',
           c_press  LIKE marc-fevor VALUE 'SPP',
           c_coil   LIKE marc-fevor VALUE 'AM',
           c_3c     LIKE marc-fevor VALUE 'SEC',
           c_engine LIKE marc-fevor VALUE 'SEA'.
*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
DEFINE cal_by_qty.
*  Distributed Qty / Unit
  if it_io_coep-mbgbtr < 0.
    it_post-mbgbtr
                = floor( it_io_coep-mbgbtr * it_post-&1 ).
  else.
    it_post-mbgbtr
                = ceil( it_io_coep-mbgbtr * it_post-&1 ) .
  endif.
*  SUM
  clear lv_res_qty.
  lv_res_qty = lv_sum_qty.
  lv_sum_qty = lv_sum_qty +  it_post-mbgbtr.
*
  if      it_io_coep-mbgbtr >= 0
     and  lv_sum_qty > it_io_coep-mbgbtr.
    it_post-mbgbtr = it_io_coep-mbgbtr - lv_res_qty.
    if it_post-mbgbtr < 0.
      it_post-mbgbtr = space.
    endif.
  endif.

  if      it_io_coep-mbgbtr < 0
     and  lv_sum_qty < it_io_coep-mbgbtr.
    it_post-mbgbtr = it_io_coep-mbgbtr - lv_res_qty.
    if it_post-mbgbtr > 0.
      it_post-mbgbtr = space.
    endif.
  endif.

*  Cal. Cost
  it_post-chg_wkgbtr  =
   ( it_io_coep-wkgbtr / it_io_coep-mbgbtr ) * it_post-mbgbtr .

END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
             p_gjahr LIKE coep-gjahr MEMORY ID gjr DEFAULT sy-datum(4)
                                                   OBLIGATORY,
             p_perio LIKE coep-perio MEMORY ID bpe OBLIGATORY,
             p_versn LIKE coep-versn  OBLIGATORY DEFAULT '000',
             p_wrttp LIKE coep-wrttp  OBLIGATORY DEFAULT '04' ,
             p_conf  AS CHECKBOX USER-COMMAND pchkbox.

SELECT-OPTIONS : s_mtart FOR t134-mtart MEMORY ID mta
                         OBLIGATORY
                         NO INTERVALS,
                 s_aufnr FOR aufk-aufnr MEMORY ID anr
                         OBLIGATORY
                         NO INTERVALS
                         MATCHCODE OBJECT zsh_co_io,
                 s_kstar FOR cska-kstar MEMORY ID kat
                         OBLIGATORY
                         NO INTERVALS,
                 s_matnr FOR coep-matnr.

SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS : p_report RADIOBUTTON GROUP ra01 MODIF ID zpa,
             p_frpost RADIOBUTTON GROUP ra01 MODIF ID zpa.
PARAMETERS : p_post   as checkbox  MODIF ID zpa.

SELECTION-SCREEN END OF BLOCK bl2.
