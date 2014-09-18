*----------------------------------------------------------------------*
*   INCLUDE ZACO16L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES: tka01, t134, aufk, cska, cobk, coep, mara, mseg, ztco_abispost.

** Internal Table
* For I/O COEP
DATA : BEGIN OF it_io_coep OCCURS 1000.
DATA :  matnr  LIKE coep-matnr,
        stype  LIKE ztco_abispost-stype,
        werks  LIKE coep-werks,
        wkgbtr LIKE coep-wkgbtr,
        waers  LIKE tka01-waers,
        fevor  LIKE marc-fevor,
        objnr  LIKE coep-objnr,
        kstar  LIKE coep-kstar,
        refbn  LIKE covp-refbn,
        gjahr  LIKE covp-gjahr,
        buzei  LIKE covp-buzei,
* Qty
        mefbtr LIKE coep-mefbtr,
        mbgbtr LIKE coep-mbgbtr,
        mbfbtr LIKE coep-mbfbtr,
        meinb  LIKE coep-meinb.
DATA : END OF   it_io_coep.

DATA : BEGIN OF it_mard OCCURS 1000.
DATA :  matnr  LIKE mard-matnr,
        stype  LIKE ztco_abispost-stype,
        werks  LIKE mard-werks,
        lgort  LIKE mard-lgort,
        lfgja  LIKE mard-lfgja,
        lfmon  LIKE mard-lfmon,
        fevor  LIKE marc-fevor,
        kstar  TYPE kstar,
        kalnr  LIKE ckmlmv011-kalnr,
        meins  LIKE mara-meins,
* Qty
        labst  LIKE mard-labst,
        stprs  LIKE ckmlcr-stprs.
DATA : END OF   it_mard.


DATA : BEGIN OF it_ckmlmv011_temp OCCURS 0 ,
       kalnr  LIKE ckmlmv011-kalnr,
       matnr  LIKE ckmlmv011-matnr,
       werks  LIKE ckmlmv011-werks,
       END OF it_ckmlmv011_temp.

DATA : it_ckmlmv011 LIKE it_ckmlmv011_temp OCCURS 0 WITH HEADER LINE.


DATA : BEGIN OF it_ckmlcr OCCURS 0 ,
       kalnr  LIKE ckmlcr-kalnr,
       stprs  LIKE ckmlcr-stprs,
       END OF it_ckmlcr.

DATA : BEGIN OF it_mbew OCCURS 0,
         matnr LIKE mbew-matnr,
         bwkey LIKE mbew-bwkey,
         bklas LIKE mbew-bklas,
       END OF it_mbew.


DATA : BEGIN OF it_t030 OCCURS 0,
         bklas LIKE mbew-bklas,
         ktopl LIKE t030-ktopl,    " Chart of accounts
         konts TYPE t030-konts,
       END OF it_t030.

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
data : lgort type lgort_D.
DATA : END OF it_post.
*DATA:  it_post_tmp LIKE it_post OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_post_fin OCCURS 500,
        kstar      LIKE ztco_abispost-kstar,
        werks      LIKE ztco_abispost-werks,
        lgort      LIKE marD-lgort,
        matnr      LIKE ztco_abispost-matnr,
        stype      LIKE ztco_abispost-stype,
        io_aufnr   LIKE ztco_abispost-io_aufnr,
        pcc_aufnr  LIKE ztco_abispost-pcc_aufnr,
        chg_wkgbtr LIKE ztco_abispost-chg_wkgbtr,
        waers      LIKE ztco_abispost-waers,
        belnr      LIKE ztco_abispost-belnr,
        mbgbtr     LIKE ztco_abispost-mbgbtr,
        meinb      LIKE ztco_abispost-meinb,
       END OF it_post_fin.

DATA : BEGIN OF it_mb1a OCCURS 500,
        pcc_aufnr  LIKE ztco_abispost-pcc_aufnr,
        werks      LIKE ztco_abispost-werks,
        lgort      LIKE marD-lgort,
        bwart      like mseg-bwart,
        kstar      LIKE ztco_abispost-kstar,
        matnr      LIKE ztco_abispost-matnr,
        stype      LIKE ztco_abispost-stype,
        io_aufnr   LIKE ztco_abispost-io_aufnr,
        chg_wkgbtr LIKE ztco_abispost-chg_wkgbtr,
        waers      LIKE ztco_abispost-waers,
        belnr      LIKE ztco_abispost-belnr,
        mbgbtr     LIKE ztco_abispost-mbgbtr,
        meinb      LIKE ztco_abispost-meinb,
        reas(4).
DATA : END OF it_mb1a.

DATA : it_mfbf    LIKE it_post_fin OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_ppcvar OCCURS 500,
        flg_reversal LIKE BAPI_PPC_APOHEADS-FLG_REVERSAL,
        pcc_aufnr  LIKE ztco_abispost-pcc_aufnr,
        werks      LIKE ztco_abispost-werks,
        lgort      LIKE marD-lgort,
        kstar      LIKE ztco_abispost-kstar,
        matnr      LIKE ztco_abispost-matnr,
        stype      LIKE ztco_abispost-stype,
        io_aufnr   LIKE ztco_abispost-io_aufnr,
        chg_wkgbtr LIKE ztco_abispost-chg_wkgbtr,
        waers      LIKE ztco_abispost-waers,
        belnr      LIKE ztco_abispost-belnr,
        mbgbtr     LIKE ztco_abispost-mbgbtr,
        meinb      LIKE ztco_abispost-meinb.
DATA : END OF it_ppcvar.


DATA : it_l_aufk LIKE STANDARD TABLE OF aufk
                 WITH HEADER LINE .


*--- BAPI : posting(MB1A)
DATA : w_goodsmvt_header  LIKE bapi2017_gm_head_01,
       w_goodsmvt_code    LIKE bapi2017_gm_code,
       w_goodsmvt_headret LIKE bapi2017_gm_head_ret,
       w_materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
       w_matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
       it_goodsmvt_item
            LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE,
       it_goodsmvt_serialnumber
            LIKE TABLE OF bapi2017_gm_serialnumber WITH HEADER LINE,
       it_return
            LIKE TABLE OF bapiret2                 WITH HEADER LINE.


*--- BAPI : posting(MFBF)
** For Cerate Functional Location BAPI
DATA: external_number LIKE bapi_itob_parms-equipment,
      data_general    LIKE bapi_itob,
      data_specific   LIKE bapi_itob_eq_only,
      valid_date      LIKE bapi_itob_parms-inst_date,
      data_install    LIKE bapi_itob_eq_install,
** For Change Functional Location BAPI
      data_generalx   LIKE bapi_itobx,
      data_specificx  LIKE bapi_itob_eq_onlyx,
      return          LIKE bapiret2 .

DATA: bflushflags   LIKE bapi_rm_flg,
      bflushdatagen LIKE bapi_rm_datgen,
      bflushdatamts LIKE bapi_rm_datstock,
      wa_return LIKE bapiret2 ,
      it_serialnr LIKE bapi_rm_datserial OCCURS 0 WITH HEADER LINE,
      wa_confirmation TYPE bapi_rm_datkey-confirmation,
      wa_cancco       TYPE bapi_rm_datkey-cancconfirmation.

*--- BAPI : posting(PPCVAR)
** For resource data
DATA: IT_RESGUID16 TYPE KCR_GUID_16_TAB.
DATA: IF_MODEID16 TYPE PPC_MODE_GUID_INT.
DATA: IT_ACT_RAW TYPE PPC_T_ACT_RAW
                 WITH HEADER LINE.
DATA: IT_PPC_SHOW_EXT_ACT TYPE TABLE OF PPC_SHOW_EXT_ACT
                 WITH HEADER LINE.
* For the Combined PPC_Activity master data.
DATA : BEGIN OF IT_PPC_ACT_MOD OCCURS 0.
        INCLUDE STRUCTURE PPC_ACT.
DATA :  HEADID         LIKE PPC_SHOW_EXT_ACT-HEADID       ,
        RESOURCE_EXT   LIKE PPC_SHOW_EXT_ACT-RESOURCE_EXT ,
        ACTIVITY_NAME  LIKE PPC_SHOW_EXT_ACT-ACTIVITY_NAME,
        MODE_NO        LIKE PPC_SHOW_EXT_ACT-MODE_NO      ,
        COST_CENTER    LIKE PPC_SHOW_EXT_ACT-COST_CENTER  ,
        ACTTYPE        LIKE PPC_SHOW_EXT_ACT-ACTTYPE      ,
        CO_BUSPROC     LIKE PPC_SHOW_EXT_ACT-CO_BUSPROC   .
DATA : END OF  IT_PPC_ACT_MOD.
* For PPC DI B/F
DATA : WA_PPC_HEAD  TYPE PPC_VA_HEAD.
DATA:  IT_PPC_HEADS TYPE TABLE OF BAPI_PPC_APOHEADS
                    WITH HEADER LINE .
DATA : IT_APOHEADS  LIKE STANDARD TABLE OF  BAPI_PPC_APOHEADS
                    WITH HEADER LINE ,
       IT_APOCOMPLISTS
                    LIKE STANDARD TABLE OF  BAPI_PPC_APOCOMPLISTS
                    WITH HEADER LINE ,
       IT_APOACTLISTS
                    LIKE STANDARD TABLE OF  BAPI_PPC_APOACTLISTS
                    WITH HEADER LINE .



DATA: g_first_date TYPE datum.
DATA: g_last_date TYPE datum.
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
  if it_mard-labst < 0.
    it_post-mbgbtr
                = floor( it_mard-labst * it_post-&1 ).
  else.
    it_post-mbgbtr
                = ceil( it_mard-labst * it_post-&1 ) .
  endif.
*  SUM
  clear lv_res_qty.
  lv_res_qty = lv_sum_qty.
  lv_sum_qty = lv_sum_qty +  it_post-mbgbtr.
*
  if      it_mard-labst >= 0
     and  lv_sum_qty > it_mard-labst.
    it_post-mbgbtr = it_mard-labst - lv_res_qty.
    if it_post-mbgbtr < 0.
      it_post-mbgbtr = space.
    endif.
  endif.

  if      it_mard-labst < 0
     and  lv_sum_qty < it_mard-labst.
    it_post-mbgbtr = it_mard-labst - lv_res_qty.
    if it_post-mbgbtr > 0.
      it_post-mbgbtr = space.
    endif.
  endif.

*  Cal. Cost
*  it_post-amt  =
*   ( it_mard-stprs / it_mard-LABST) * it_post-LABST .
  it_post-chg_wkgbtr  = it_mard-stprs  * it_post-mbgbtr .


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
*                 s_aufnr FOR aufk-aufnr MEMORY ID anr
*                         OBLIGATORY
*                         NO INTERVALS
*                         MATCHCODE OBJECT zsh_co_io,
                 s_kstar FOR cska-kstar MEMORY ID kat
                         OBLIGATORY
                         NO INTERVALS,
                 s_matnr FOR coep-matnr.
PARAMETERS : P_os_Rs LIKE MSEG-GRUND  default '0001',    "X551
             P_key_Rs LIKE MSEG-GRUND default '0001'.   "X905


SELECTION-SCREEN END OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS : p_report RADIOBUTTON GROUP ra01 MODIF ID zpa,
             p_frpost RADIOBUTTON GROUP ra01 MODIF ID zpa.
PARAMETERS : p_post   AS CHECKBOX  MODIF ID zpa.

SELECTION-SCREEN END OF BLOCK bl2.
