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
        verid  type VERID,
        categ(3) TYPE c,          "DI, REM(MTS), MTO
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

DATA : gt_return	      LIKE	STANDARD TABLE OF 	bapiret2
                              WITH  HEADER LINE.

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

DEFINE __CLS.                          " clear & refresh
  CLEAR &1.REFRESH &1.
END-OF-DEFINITION.

DEFINE __PROCESS.
  PERFORM SHOW_PROGRESS USING &1 &2.
END-OF-DEFINITION.

DEFINE __MESSAGE.
  CALL FUNCTION 'POPUP_TO_INFORM'
       EXPORTING
            TITEL = &1
            TXT1  = &2
            TXT2  = SY-SUBRC.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

DATA: G_ERROR(1),
      G_REPID  LIKE SY-REPID.
