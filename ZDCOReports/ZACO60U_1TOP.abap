*----------------------------------------------------------------------*
*   INCLUDE ZACO05U_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
REPORT zaco05u_mhcc MESSAGE-ID zmco.
* type-pools
TYPE-POOLS: slis.

** Tables
TABLES : ztco_nmhhrtrans, tka01, mara, marc, coss, ztco_nmhpcpost,
         aufk, blpk, blpp, mkal, caufv, afko, afpo, rmuser_tav,
         ckmlhd, mlcd.
TABLES : ppc_act, ppc_head, ppc_show_ext, ppc_show_ext_act.


** Internal table
*DATA : IT_ZTCO_NMHHRTRANS LIKE STANDARD TABLE OF ZTCO_NMHHRTRANS
*                         WITH HEADER LINE .
DATA : BEGIN OF it_ztco_nmhhrtrans  OCCURS 0,
        gjahr   LIKE ztco_nmhhrtrans-gjahr,
        perid   LIKE ztco_nmhhrtrans-perid,
        kostl   LIKE ztco_nmhhrtrans-kostl,
        lstar   LIKE ztco_nmhhrtrans-lstar,
        vaeqty  LIKE ztco_nmhhrtrans-vaeqty,
        unit    LIKE ztco_nmhhrtrans-unit,
       END OF  it_ztco_nmhhrtrans.
* For MATNR
DATA : BEGIN OF it_marc OCCURS 0,
        matnr   LIKE mara-matnr,
        mtart   LIKE mara-mtart,
        werks   LIKE marc-werks,
        sauft   LIKE marc-sauft,
        sfepr   LIKE marc-sfepr,
       END OF   it_marc.
* For Object Key
DATA : BEGIN OF it_ma_obj OCCURS 0.
        INCLUDE STRUCTURE it_marc.
DATA :  aufnr   LIKE afpo-aufnr,
        kostl   LIKE anlp-kostl,
        lstar   LIKE csla-lstar,
        objnr   LIKE coss-objnr,
        parob   LIKE coss-parob,
        uspob   LIKE coss-uspob,
        verid   LIKE mkal-verid,
       END OF   it_ma_obj.
* For Coss
DATA : BEGIN OF it_coss OCCURS 0.
DATA :  kostl   LIKE anlp-kostl,
        lstar   LIKE csla-lstar,
        aufnr   LIKE afpo-aufnr.
        INCLUDE STRUCTURE it_marc.
        INCLUDE STRUCTURE zsco_coss_key01.
        INCLUDE STRUCTURE zsco_coss_meg01.
DATA : END OF  it_coss.
DATA : BEGIN OF it_col_pcc OCCURS 0,
        perid   LIKE rku01g-perbi, "Period
        aufnr   LIKE afpo-aufnr,
        kostl   LIKE anlp-kostl,
        lstar   LIKE csla-lstar,
        megxxx  LIKE coss-meg001,
        meinh   LIKE coss-meinh,
        vaeqty  LIKE ztco_nmhhrtrans-vaeqty,
        unit    LIKE ztco_nmhhrtrans-unit,
        rate_%(16)  TYPE p DECIMALS 6,
        tomeg   LIKE coss-meg001,
        megxxx_rate_%
                LIKE coss-meg001.
DATA : END OF  it_col_pcc.
* For ZTCO_nMHPCPOST
DATA : it_ztco_nmhpcpost LIKE STANDARD TABLE OF ztco_nmhpcpost
                        WITH HEADER LINE .
* For DD data
DATA : it_et_fieldlist LIKE TABLE OF rfvicp_ddic_tabl_fieldname
                       WITH HEADER LINE.
* For POSTING (MTO)
DATA : BEGIN OF it_po_post OCCURS 500.
        INCLUDE STRUCTURE it_ztco_nmhpcpost.
DATA :    po_aufnr    LIKE  aufk-aufnr,
          plnty_exp   LIKE  caufvd-plnty,
          plnnr_exp   LIKE  caufvd-plnnr,
          plnal_exp   LIKE  caufvd-plnal,
          plnme_exp   LIKE  caufvd-plnme,
          arbid       LIKE  plpo-arbid,
          arbpl       LIKE  crhd-arbpl,
          vornr       LIKE  plpo-vornr.
DATA : END OF it_po_post.
* For POSTING (MTS-REM)
DATA : BEGIN OF it_rem_post OCCURS 500.
        INCLUDE STRUCTURE it_ztco_nmhpcpost.
DATA :
          pwerk       LIKE blpk-pwerk,
          plnty_exp   LIKE  caufvd-plnty,
          plnnr_exp   LIKE  caufvd-plnnr,
          plnal_exp   LIKE  caufvd-plnal,
          plnme_exp   LIKE  caufvd-plnme,
          arbid       LIKE  plpo-arbid,
          arbpl       LIKE  crhd-arbpl,
          vornr       LIKE  plpo-vornr.
DATA : END OF it_rem_post.
DATA  : wa_rem_post LIKE it_rem_post.
* For DI B/F
DATA : BEGIN OF it_di_post OCCURS 500.
DATA :  flg_reversal  LIKE bapi_ppc_apoheads-flg_reversal.
        INCLUDE STRUCTURE it_ztco_nmhpcpost.
DATA :  wrong_ppc.
DATA : END OF it_di_post.

* For BDC
*       Batchinputdata of single transaction
DATA:   it_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

** Variable
* For DD Table name
DATA : gv_ci_tabname     TYPE  ddobjname .
DATA : gv_percount       LIKE  cosp-perbl. "Period Counter
* Global Indicator (existence of records to be posted)
DATA : gv_new.
DATA : gv_postdate_bdc(10). "    LIKE  SY-DATUM.
DATA : gv_rev_date LIKE sy-datum.

** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
** REM Profile
DATA : gv_rempf_fsc      LIKE marc-sfepr VALUE 'VEHI'.
DATA : gv_rempf_eng      LIKE marc-sfepr VALUE 'ENGI'.
DATA : gv_rempf_blank    LIKE marc-sfepr VALUE space.

** For resource data
DATA: it_resguid16 TYPE kcr_guid_16_tab.
DATA: if_modeid16 TYPE ppc_mode_guid_int.
DATA: it_act_raw TYPE ppc_t_act_raw
                 WITH HEADER LINE.
DATA: it_ppc_show_ext_act TYPE TABLE OF ppc_show_ext_act
                 WITH HEADER LINE.
* For the Combined PPC_Activity master data.
DATA : BEGIN OF it_ppc_act_mod OCCURS 0.
        INCLUDE STRUCTURE ppc_act.
DATA :  headid         LIKE ppc_show_ext_act-headid       ,
        resource_ext   LIKE ppc_show_ext_act-resource_ext ,
        activity_name  LIKE ppc_show_ext_act-activity_name,
        mode_no        LIKE ppc_show_ext_act-mode_no      ,
        cost_center    LIKE ppc_show_ext_act-cost_center  ,
        acttype        LIKE ppc_show_ext_act-acttype      ,
        co_busproc     LIKE ppc_show_ext_act-co_busproc   .
DATA : END OF  it_ppc_act_mod.

* For PPC DI B/F
DATA : wa_ppc_head  TYPE ppc_va_head.
DATA:  it_ppc_heads TYPE TABLE OF bapi_ppc_apoheads
                    WITH HEADER LINE .
DATA : it_apoheads  LIKE STANDARD TABLE OF  bapi_ppc_apoheads
                    WITH HEADER LINE ,
       it_apocomplists
                    LIKE STANDARD TABLE OF  bapi_ppc_apocomplists
                    WITH HEADER LINE ,
       it_apoactlists
                    LIKE STANDARD TABLE OF  bapi_ppc_apoactlists
                    WITH HEADER LINE .

*- U1 Start
DATA: gt_coss TYPE TABLE OF coss WITH HEADER LINE,
      gt_afko TYPE TABLE OF afko WITH HEADER LINE.
*-U1 End

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY   ID cac OBLIGATORY,
             p_gjahr LIKE anlp-gjahr   MEMORY   ID gjr OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-002.
*   From Period.
PARAMETERS: p_frper LIKE rku01g-perab OBLIGATORY.
SELECTION-SCREEN COMMENT 52(05) text-003.
*   To Period.
PARAMETERS: p_toper LIKE rku01g-perbi  NO-DISPLAY. " OBLIGATORY.
SELECTION-SCREEN END OF LINE.
PARAMETERS : p_lstar LIKE csla-lstar            DEFAULT 'MAN_HR'
                                                       OBLIGATORY,
             p_ncoal LIKE grpdynp-name_coall    DEFAULT 'DIRECT'
                                                       OBLIGATORY.
*             p_mode(1)                  DEFAULT 'N'    OBLIGATORY.
* Reverse?
*             p_revs AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-004.
SELECT-OPTIONS : s_mtart FOR mara-mtart         OBLIGATORY.
PARAMETERS : p_versn LIKE coss-versn            DEFAULT '000'
                                                OBLIGATORY,
             p_wrttp LIKE coss-wrttp            DEFAULT '4'
                                                OBLIGATORY.
SELECT-OPTIONS : s_vrgng FOR coss-vrgng        .
SELECTION-SCREEN END OF BLOCK bl2.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End
