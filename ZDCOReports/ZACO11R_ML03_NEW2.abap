************************************************************************
**
** Program Name      : ZACO11R_ML03_NEW
** Author            : Manjunath Venkatesh
** Creation Date     : 02/03/2006
** Specifications By : Andy Choi
** Pattern           : Report 1-1
** Add documentation : Copy of ZACO11R_ML03_NEW
** Description       : ML report for detail
**                     ML data should be gathered as MVT Group and
**                     Process Category
** the BDC structures for BATCH INPUT processing
**
** Modifications Log
** Date      Developer   Request ID    Description
** 01/17/05  Manjunath   UD1K919229    Program changes to Add Ending STD
**                                     and VAR values from MBEWH & MBEW
************************************************************************
**
REPORT zaco11r_ml03_new MESSAGE-ID zmco.
*
** Top Include
**----------------------------------------------------------------------
**
**   INCLUDE ZACO11R_ML03_NEW_TOP
**
**----------------------------------------------------------------------
**
**----------------------------------------------------------------------
**
**   Macro
**----------------------------------------------------------------------
**
*DEFINE def_val.
*  data :
*          &1_lbkum like mlcd-lbkum,
*          &1_salk3 like mlcd-salk3,
*          &1_rd    like mlcd-salk3.
*END-OF-DEFINITION .
*
** To set Values from MLCD containers
*DEFINE mldr_set_val_proccat.
*  it_ztco_mlit-&1_&2_lbkum = &3-lbkum.
*  it_ztco_mlit-&1_&2_salk3 = &3-salk3.
*  it_ztco_mlit-&1_&2_rd    =
*                            &3-estprd
*                          + &3-estkdm
*                          + &3-mstprd
*                          + &3-mstkdm
*                          + &3-tpprd.
*END-OF-DEFINITION .
*
** By MVT GRP
*DEFINE mldr_set_val_mvtgrp.
*  it_ztco_mlit-&1_&2_lbkum = &3-lbkum * ( -1 ).
*  it_ztco_mlit-&1_&2_salk3 = &3-salk3 * ( -1 ).
*END-OF-DEFINITION .
** By Andy
*DEFINE mldr_set_val_mvtgrp_r.
*  it_ztco_mlit-&1_&2_lbkum = &3-lbkum.
*  it_ztco_mlit-&1_&2_salk3 = &3-salk3.
*END-OF-DEFINITION .
*
** For Cal. VKA
*DEFINE mldr_cal_vka.
** VKA.
**  it_ztco_mlit-vn_vka_&1
**   = it_ztco_mlit-vn_vka_&1
**  - ( it_ztco_mlit-vn_&2_&1 + it_ztco_mlit-vn_&3_&1 ).
** Begin of changes - UD1K918937
*  it_ztco_mlit-vn_vka_&1
*   =  ( it_ztco_mlit-vn_vka_&1 + it_ztco_mlit-vn_&2_&1 +
*     it_ztco_mlit-vn_&3_&1 )
*  - ( it_ztco_mlit-vn_&2_&1 + it_ztco_mlit-vn_&3_&1 ).
** End of changes - UD1K918937
*END-OF-DEFINITION .
*
*
** For Substraction from VN ETC
*DEFINE mldr_sub_vn_etc.
**  it_ztco_mlit-vn_etc_&1
**               = it_ztco_mlit-vn_etc_&1
**               - it_ztco_mlit-vn_6665_&1
**               - it_ztco_mlit-vn_5857_&1
**               - it_ztco_mlit-vn_5655_&1
**               - it_ztco_mlit-vn_6059_&1
**               - it_ztco_mlit-vn_6463_&1.
** Begin of changes - UD1K918937
*  it_ztco_mlit-vn_etc_&1
*               = ( it_ztco_mlit-vn_etc_&1
*               + it_ztco_mlit-vn_or66_&1
*               + it_ztco_mlit-vn_or58_&1
*               + it_ztco_mlit-vn_cc56_&1
*               + it_ztco_mlit-vn_cc60_&1
*               + it_ztco_mlit-vn_cc64_&1 )
*               -
*               (  it_ztco_mlit-vn_or66_&1
*                + it_ztco_mlit-vn_or58_&1
*                + it_ztco_mlit-vn_cc56_&1
*                + it_ztco_mlit-vn_cc60_&1
*                + it_ztco_mlit-vn_cc64_&1 ).
*
*
** End of changes - UD1K918937
*END-OF-DEFINITION .
*
*
**----------------------------------------------------------------------
**
**   Data Definition
**----------------------------------------------------------------------
**
*** Type-Pools
*TYPE-POOLS : ckmv0, slis.
*TYPE-POOLS : zcot1.
*
*** Tables
*TABLES : ckmlpp, ckmlcr, ckmlhd, ckmlct.
*TABLES : macku, marc, mara.
*TABLES : mlcd_key, mlcd, mlcr, mlpp,
*         mlhd, mlit,mbewh.
*TABLES : t001w, mbew.
*TABLES : ztco_mlit, ztco_mli2, ztco_mlii.
*TABLES : zvco_mlxxv.
*
***  Internal Tables
** For MLCD
*DATA : it_kalnr	  TYPE ckmv0_matobj_tbl WITH HEADER LINE.
*DATA : BEGIN OF it_mlcd OCCURS 1000,
**       INCLUDE STRUCTURE mlcd.
*        kalnr   LIKE mlcd-kalnr ,
*        bdatj   LIKE mlcd-bdatj ,
*        poper   LIKE mlcd-poper ,
**        UNTPER  like MLCD-UNTPER,
*        categ   LIKE mlcd-categ ,
*        ptyp    LIKE mlcd-ptyp  ,
*        bvalt   LIKE mlcd-bvalt ,
*        curtp   LIKE mlcd-curtp ,
*        object  LIKE mara-matnr,  "partner material
*        lbkum   LIKE mlcd-lbkum ,
*        meins   LIKE mlcd-meins ,
*        salk3   LIKE mlcd-salk3 ,
*        estprd  LIKE mlcd-estprd,
*        estkdm  LIKE mlcd-estkdm,
*        mstprd  LIKE mlcd-mstprd,
*        mstkdm  LIKE mlcd-mstkdm,
*        waers   LIKE mlcd-waers ,
*        tpprd   LIKE mlcd-tpprd ,
*       END OF   it_mlcd.
** For Display
*DATA : BEGIN OF it_ztco_mlit OCCURS 500,
*        bukrs LIKE ztco_mlit-bukrs,
*        bdatj LIKE mlcd-bdatj,
*        poper LIKE mlcd-poper,
*        mtart LIKE mara-mtart,
*        matnr LIKE ckmlhd-matnr,
*        bwkey LIKE ckmlhd-bwkey,
*        bwtar LIKE ckmlhd-bwtar,
*        meins LIKE mlcd-meins,
*        waers LIKE mlcd-waers.
**Value Part
*def_val ab.  "AB	Beginning inventory
*def_val ab_pc.  "PC	Price changes
*def_val zu_bb.  "PO GR
*def_val zu_bf.  "PP GR
*def_val zu_bl.  "OS GR
*def_val zu_zo.  "ZO	Other receipts
*def_val zu_bubs.  "BU  Stock trf
*def_val zu_bubm.  "BM  M2M
*def_val zu_vp.  "VP  debit/credit
*def_val zu_nd.  "ND	Not distributed
*def_val zu_etc. "ND	Not distributed
*def_val kb.  "KB	Cumulative inventory
*
*def_val vn_vka16.  "VN	Normal Consumption
*def_val vn_vka20.  "VN	Normal Consumption
*def_val vn_vka99.  "VN	Normal Consumption
*def_val vn_vf.     "B/F
*def_val vn_vl.     "OS GI
*def_val vn_cob.    "COB
*def_val vn_or66.   "COB
*def_val vn_or58.   "COB
*def_val vn_cc60.     "V+  Inv diff 711, B+ = 712
*def_val vn_cc99.     "VK  consumption cc
*def_val vn_vubs.  "VU  stock transfer
*def_val vn_vubm.  "VM  M2M
*def_val ni.  "NC	Not allocated/Included
*
*def_val eb.  "EB	Ending inventory
*
*DATA :  bklas LIKE mbew-bklas,
*        maktg LIKE makt-maktg,
*        kalnr LIKE mlcd-kalnr,
*       END OF it_ztco_mlit.
*
** For Detail Report
**DATA : it_ztco_mlit  LIKE STANDARD TABLE OF ztco_mlit
**                          WITH HEADER LINE
**                          INITIAL SIZE 3000.
** For ZVCO_MLXXV - Data by MVTGRP
**MLHD
**MLIT
**MLPP
**MLCR
**MLPPF
*DATA : BEGIN OF it_zvco_mlxxv OCCURS 1000,
*         belnr       LIKE mlcr-belnr     ,
*         kjahr       LIKE mlcr-kjahr     ,
*         posnr       LIKE mlcr-posnr     ,
*         bdatj       LIKE mlcr-bdatj     ,
*         poper       LIKE mlcr-poper     ,
*
*         awref       LIKE mlhd-awref     ,
*         aworg       LIKE mlhd-aworg     ,
*
*         matnr       LIKE mlit-matnr     ,
*         BWKEY       LIKE mlit-BWKEY     ,
*
*         kalnr       LIKE mlit-kalnr     ,
*         meins       LIKE mlit-meins     ,
*         waers       LIKE mlcr-waers     ,
*
*         psart       LIKE mlit-psart     , "Item type
*         mlast       LIKE mlit-mlast     , "2/3
*         storno      LIKE mlhd-storno    , "Ind:reversal
*         status      LIKE mlpp-status    , "period status
*         xabrerr     LIKE mlit-xabrerr   , "Ind:Err last time
*         bewartgrp   LIKE mlit-bewartgrp , "MvType Grp
*         kategorie   LIKE mlit-kategorie , "Category in ML
*         ptyp        LIKE mlit-ptyp      , "Original process cat.
*         ptyp_kat    LIKE mlit-ptyp_kat  , "Process cat.det.
*         ptyp_bvalt  LIKE mlit-ptyp_bvalt, "procurement alt.
*         ptyp_proc   LIKE mlit-ptyp_proc , "the process
*         feldg       LIKE mlppf-feldg    , "Field Group
*         lbkum       LIKE mlpp-lbkum     ,
*         salk3       LIKE mlcr-salk3     ,
*
*       END OF   it_zvco_mlxxv.
*
*
*DATA : BEGIN OF it_detail OCCURS 0,
*         kjahr       LIKE mlcr-kjahr     ,
*         poper       LIKE mlcr-poper     ,
*         belnr       LIKE mlcr-belnr     ,
*         posnr       LIKE mlcr-posnr     ,
*         kalnr       LIKE mlit-kalnr     ,
*         matnr       LIKE mlit-matnr     ,
*         awref       LIKE mlhd-awref     ,
*         aworg       LIKE mlhd-aworg     ,
*         mjahr       LIKE mseg-mjahr     ,
*       END OF it_detail.
*
*DATA : BEGIN OF it_mseg OCCURS 0,
*         mblnr       LIKE mseg-mblnr     ,
*         zeile       LIKE mseg-zeile     ,
*         matnr       LIKE mseg-matnr     ,
*         sakto       LIKE mseg-sakto     ,
*         kostl       LIKE mseg-kostl     ,
*         aufnr       LIKE mseg-aufnr     ,
*         dmbtr       LIKE mseg-dmbtr     ,
*         menge       LIKE mseg-menge     ,
*         meins       LIKE mseg-meins     ,
*       END OF it_mseg.
*
*
***** FOR Detail & clearing acc : Begin
*DATA : it_ztco_mlii LIKE ztco_mlii OCCURS 0 WITH HEADER LINE.
*DATA : it_ztco_mli2 LIKE ztco_mli2 OCCURS 0 WITH HEADER LINE.
*DATA : it_mli2_temp LIKE ztco_mli2 OCCURS 0 WITH HEADER LINE.
*
*
*DATA : BEGIN OF it_clear OCCURS 0,
*        awkey   LIKE bkpf-awkey,
*        mblnr   LIKE mseg-mblnr,
*        bdatj   LIKE mseg-mjahr.
*DATA : END OF   it_clear.
*
*DATA : BEGIN OF it_bkpf OCCURS 0,
*        gjahr   LIKE bkpf-gjahr,
*        awkey   LIKE bkpf-awkey,
*        blart   like bkpf-blart,
*        belnr   LIKE bkpf-belnr.
*DATA : END OF   it_bkpf.
*
*DATA : BEGIN OF it_bsas_temp OCCURS 0,
*        gjahr   like bsas-gjahr,
*        belnr   like bsas-belnr,
*        augdt   LIKE bsas-augdt,
*        augbl   LIKE bsas-augbl,
*        blart   like bsas-blart.
*data : END OF   it_bsas_temp.
*
*DATA : BEGIN OF it_bsas OCCURS 0,
*        bukrs   like bsas-bukrs,
*        gjahr   LIKE bsas-gjahr,
*        belnr   LIKE bsas-belnr,
*        augdt   like bsas-augdt,
*        augbl   like bsas-augbl,
*        bschl   LIKE bsas-bschl,
*        blart   LIKE bsas-blart,
*        buzei   LIKE bsas-buzei.
*DATA : END OF   it_bsas.
*
*DATA : BEGIN OF it_bseg OCCURS 0,
*        gjahr   LIKE bseg-gjahr,
*        belnr   LIKE bseg-belnr,
*        dmbtr   LIKE bseg-dmbtr,
*        hkont   LIKE bseg-hkont,
*        buzei   LIKE bseg-buzei,
*        kostl   like bseg-kostl,
*        aufnr   like bseg-aufnr.
*DATA : END OF   it_bseg.
*
***** FOR Detail & clearing acc : End
*
*DATA : BEGIN OF it_t001w OCCURS 0.
*        INCLUDE STRUCTURE t001w.
*DATA : END OF   it_t001w.
*
*DATA : p_poper LIKE mlcd_key-poper.
*
** Find BADY BOY
*TYPES:
*  BEGIN OF s_mats,
*     kalnr TYPE ckmlhd-kalnr,
*     matnr TYPE ckmlhd-matnr,
*     bwkey TYPE ckmlhd-bwkey,
*     bwtar TYPE ckmlhd-bwtar,
**     sobkz TYPE ckmlhd-sobkz,
**     vbeln TYPE ckmlhd-vbeln,
**     posnr TYPE ckmlhd-posnr,
**     pspnr TYPE ckmlhd-pspnr,
*     mtart TYPE mara-mtart,
*     matkl TYPE mara-matkl,
*     spart TYPE mara-spart,
**    prctr TYPE marc-prctr,
*     meins TYPE mara-meins,
*     bklas TYPE mbew-bklas,   "val class
*     lfgja TYPE mbew-lfgja,
*     lfmon TYPE mbew-lfmon,
*     maktg TYPE makt-maktg,
*     stprs TYPE mbew-stprs,
*     verpr TYPE mbew-verpr,
*
*     salk3 TYPE mbew-salk3,
**    lbkum TYPE mbew-lbkum,
*     status   LIKE ckmlpp-status,    "ML status
*     abkumo   LIKE ckmlpp-abkumo,    "Begin
*     umkumo   LIKE ckmlpp-umkumo,    "Prev Posting
*     zukumo   LIKE ckmlpp-zukumo,    "GR
*     vnkumo   LIKE ckmlpp-vnkumo,    "GI
*     lbkum    LIKE ckmlpp-lbkum ,    "End
*     ekkumo   LIKE ckmlpp-ekkumo,    "PO GR
*
*     absalk3  LIKE ckmlcr-absalk3,
*     abprd_o  LIKE ckmlcr-abprd_o,
*     abkdm_o  LIKE ckmlcr-abkdm_o,
*     abprd_mo LIKE ckmlcr-abprd_mo,
*     abkdm_mo LIKE ckmlcr-abkdm_mo,
*
*     vpprd_o  LIKE ckmlcr-vpprd_o,
*     zuprd_o  LIKE ckmlcr-zuprd_o,
*     zukdm_o  LIKE ckmlcr-zukdm_o,
*     vpkdm_o  LIKE ckmlcr-vpkdm_o,
*
*     zuprd_mo  LIKE ckmlcr-zuprd_mo,
*     zukdm_mo  LIKE ckmlcr-zukdm_mo,
*
*     vnprd_ea  LIKE ckmlcr-vnprd_ea,
*     vnkdm_ea  LIKE ckmlcr-vnkdm_ea,
*     ebprd_ea  LIKE ckmlcr-ebprd_ea,
*     ebkdm_ea  LIKE ckmlcr-ebkdm_ea,
*     vnprd_ma  LIKE ckmlcr-vnprd_ma,
*     vnkdm_ma  LIKE ckmlcr-vnkdm_ma,
*     ebprd_ma  LIKE ckmlcr-ebprd_ma,
*     ebkdm_ma  LIKE ckmlcr-ebkdm_ma,
*   END OF s_mats,
*
*  ty_mats TYPE STANDARD TABLE OF s_mats WITH KEY kalnr,
*
*    BEGIN OF s_ndi,
*       kalnr TYPE ckmlhd-kalnr,
*       bdatj TYPE ckmlpp-bdatj,
*       poper TYPE ckmlpp-poper,
*       untper TYPE ckmlpp-untper,
*       curtp TYPE ckmlcr-curtp,
*       matnr TYPE ckmlhd-matnr,
*       bwkey TYPE ckmlhd-bwkey,
*       bwtar TYPE ckmlhd-bwtar,
*       vbeln TYPE ckmlhd-vbeln,
*       posnr TYPE ckmlhd-posnr,
*       pspnr TYPE ckmlhd-pspnr,
*       pos_type(3),                 "NDI, NIN
*       bklas TYPE mbew-bklas,
*       mtart TYPE mara-mtart,
*       matkl TYPE mara-matkl,
*       spart TYPE mara-spart,
**       prctr TYPE marc-prctr,
*       meins TYPE ckmlpp-meins,
*       status TYPE ckmlpp-status,
*       lbkum TYPE ckmlpp-lbkum,
*       menge TYPE kkb_ml_menge,
*       pbpopo TYPE ckmlpp-pbpopo,
*       salk3 TYPE ckmlcr-salk3,
*       wert TYPE kkb_ml_bewer,
*       stprs TYPE ckmlcr-stprs,
*       pvprs TYPE ckmlcr-pvprs,
*       peinh TYPE ckmlcr-peinh,
*       waers TYPE ckmlcr-waers,
*       pbprd_o TYPE ckmlcr-pbprd_o,
*       pbkdm_o TYPE ckmlcr-pbkdm_o,
*       estprd TYPE ckml_estprd,
*       estkdm TYPE ckml_estkdm,
*       mstprd TYPE ckml_mstprd,
*       mstkdm TYPE ckml_mstkdm,
*       estdif TYPE ck_singlelevel_dif,
*       mstdif TYPE ck_multilevel_dif,
*       prdif TYPE ck_sum_prdif,
*       krdif TYPE ck_sum_krdif,
*       sumdif TYPE ck_sum_dif,
*       color(3) TYPE c,
*     END OF s_ndi,
*     ty_out TYPE STANDARD TABLE OF s_ndi WITH KEY kalnr.
*
*DATA: t_mats TYPE ty_mats  WITH HEADER LINE,
*
*      t_ckmlpp TYPE STANDARD TABLE OF ckmlpp
*               WITH KEY kalnr bdatj poper
*               WITH HEADER LINE,
*      t_ckmlcr TYPE STANDARD TABLE OF ckmlcr
*               WITH KEY kalnr bdatj poper curtp
*               WITH HEADER LINE,
*      t_mlcd TYPE STANDARD TABLE OF mlcd
*               WITH KEY kalnr bdatj poper untper categ ptyp bvalt curtp
*               WITH HEADER LINE,
*      t_mlcd_not_alloc TYPE STANDARD TABLE OF mlcd
*               WITH KEY kalnr bdatj poper untper categ ptyp bvalt curtp
*               WITH HEADER LINE,
*      t_bad  TYPE ty_out   WITH HEADER LINE.
*
*data : it_log like ztco_BATCH_log occurs 0 with header line.
*
*ranges: r_kalnr for ztco_mlit-kalnr.
** For Summary
**DATA : it_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.
**DATA : it_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.
**DATA : t_out TYPE zcot1_it_out WITH HEADER LINE .
*
**--- ALV
*TYPE-POOLS: slis.
*DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
*       w_eventcat TYPE slis_t_event WITH HEADER LINE,
*       w_selfield TYPE slis_selfield,
*       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
*       w_col_pos  TYPE i,
*       w_program  LIKE sy-repid,
*       w_top_of_page TYPE slis_t_listheader,
*       w_line1 TYPE slis_listheader.
*
*DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
*      gs_layout   TYPE slis_layout_alv,
*      gt_sp_group TYPE slis_t_sp_group_alv,
*      gt_events   TYPE slis_t_event,
*      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
*      gs_prnt     TYPE slis_print_alv,
*      g_repid     LIKE sy-repid,
*      g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
*
**---- ALV
*
*DATA: BEGIN OF i_proc_kalnr OCCURS 0,
*        werks LIKE ckmlmv001-werks,
*        matnr LIKE ckmlmv001-matnr,
*        bwtar LIKE ckmlmv001-bwtar,
*        prock LIKE ckmlmv001-proc_kalnr,
*        btyp  LIKE ckmlmv001-btyp,  "bf-production, bb-procurement
*        kalnr LIKE ckmlhd-kalnr,
*      END OF i_proc_kalnr.
*
**----------------------------------------------------------------------
**
**   Selection Condition
**
**----------------------------------------------------------------------
**
*SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
*PARAMETERS : p_kokrs LIKE csks-kokrs     MEMORY ID cac, "OBLIGATORY
**                                        VALUE CHECK,
*             p_bdatj LIKE mlcd_key-bdatj MEMORY ID bdtj. " OBLIGATORY.
*SELECT-OPTIONS :  s_poper FOR mlcd_key-poper MEMORY ID popr."OBLIGATORY
*.
*
**             p_poper like mlcd_key-poper memory id popr obligatory,
*PARAMETER:    p_curtp LIKE mlcd-curtp     DEFAULT '10'  . "OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK bl1.
*
*SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
*SELECT-OPTIONS : s_matnr FOR macku-matnr MEMORY ID mat,
*                 s_mtart FOR macku-mtart,
*                 s_bwkey FOR mbew-bwkey,
*                 s_bklas FOR mbew-bklas.
*SELECTION-SCREEN END OF BLOCK bl2.
*
*SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
*PARAMETERS : p_up AS CHECKBOX             MODIF ID dis,
*             p_db AS CHECKBOX DEFAULT 'X' MODIF ID dis.
*SELECTION-SCREEN END OF BLOCK bl3.
*PARAMETERS: p_detail AS CHECKBOX DEFAULT 'X',
*            p_batch(1) TYPE c  DEFAULT ' ' NO-DISPLAY.
*
*
**&---------------------------------------------------------------------
*-
**  Initialization
**&---------------------------------------------------------------------
*-
*INITIALIZATION.
*
*
**----------------------------------------------------------------------
**
** AT SELECTION-SCREEN
**----------------------------------------------------------------------
**
*AT SELECTION-SCREEN OUTPUT.
** Check For Update-Option
*  PERFORM chk_up_opt.
*
*AT SELECTION-SCREEN.
** Check Future Date
*  PERFORM chk_ft_date.
** Check Update Mode-> Delete with Period Key
*  PERFORM chk_par_up.
*
*
*
**----------------------------------------------------------------------
**
** Start-of-selection
**----------------------------------------------------------------------
**
*START-OF-SELECTION.
*  DATA :   p_perab TYPE co_perab.
** imported from parallel program
*  IMPORT  p_kokrs  FROM MEMORY ID 'ML03_KOKRS'.
*  IMPORT  p_bdatj  FROM MEMORY ID 'ML03_BDATJ'.
*  IMPORT  p_perab  FROM MEMORY ID 'ML03_PERAB'.
*  IMPORT  p_curtp  FROM MEMORY ID 'ML03_CURTP'.
*  IMPORT  p_batch  FROM MEMORY ID 'ML03_BATCH'.
*  IF p_batch = 'X'.
*    p_up = 'X'.
*    p_db = ' '.
*  ENDIF.
*
*  CLEAR: t_mats[].
*  IMPORT t_mats     = t_mats     FROM MEMORY ID 'ML03_MATS'.
*  IF NOT t_mats[] IS INITIAL.
*    CLEAR p_db.
*    s_poper-sign   = 'I'.    s_poper-option = 'EQ'.
*    s_poper-low    = p_perab.
*    APPEND s_poper.
*  ENDIF.
** imported from parallel program
*
*  PERFORM check_obligatory_field.
*
*  READ TABLE s_poper INDEX 1.
*  p_poper = s_poper-low.
*
*  IF p_db = 'X'.
*    PERFORM get_data_from_buffer.
*  ELSE.
*    IF t_mats[] IS INITIAL.
*      PERFORM get_materials_from_closing.
*    ELSE.
*      PERFORM get_materials_from_parallel.
*      LOOP AT t_mats.
*        PERFORM make_batch_log USING '2'.
*      ENDLOOP.
*    ENDIF.
*
*    SORT t_mats BY kalnr.
*    PERFORM get_proc_kalnr.
*
*    PERFORM read_mlcd.
*    PERFORM find_bad_boys.
*
*
*    PERFORM merge_mlcd_data.
*    PERFORM delete_zero_record.
*
*  ENDIF.
*
**----------------------------------------------------------------------
**
** END-of-selection
**----------------------------------------------------------------------
**
*END-OF-SELECTION.
*  IF p_db = space.
*
**parallel batch processing
*    IF p_batch = 'X'.
*      LOOP AT t_mats.
*        PERFORM make_batch_log USING '3'.
*      ENDLOOP.
*    ENDIF.
*
** Clear
*    CLEAR : it_ztco_mlit, it_ztco_mlit[].
*
*
** For Process Category ????????
*    PERFORM collect_mlcd.
*
** For  MVT Group
*    PERFORM mldr_read_data_by_mvtgrp.
**   PERFORM mldr_cal_vn_etc_vka.
*    PERFORM delete_blank_records.
*
** For detail : ztco_mli2, ztco_mlii.
*    IF p_detail = 'X'.
*      PERFORM get_from_mseg.
*    ENDIF.
*
** Read Material Data / Unit / Curr.
*    PERFORM fill_material_info.
*
** Update
*    PERFORM update_ztco_mlit.
*  ENDIF.
*
*  IF p_batch <> 'X'.
*    PERFORM call_alv_list.
*  ENDIF.
**----------------------------------------------------------------------
**
** For Sub-Routine
**----------------------------------------------------------------------
**
**----------------------------------------------------------------------
**
**   INCLUDE ZACO11L_F032
**
**----------------------------------------------------------------------
**
**&---------------------------------------------------------------------
**
**&      Form  collect_mlcd
**&---------------------------------------------------------------------
**
**       Read Data By Process Category
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM collect_mlcd.
*
** Normal Case (ZN,VN,PC,VP)
*  LOOP AT t_mlcd WHERE categ <> 'AB'.
**   Key Part
*    CLEAR it_ztco_mlit.
*    it_ztco_mlit-kalnr = t_mlcd-kalnr.
*
*
*    IF     t_mlcd-ptyp = 'VP'.                      "Debit/Credit
*      mldr_set_val_proccat zu vp  t_mlcd.
*    ELSEIF t_mlcd-ptyp = 'PC'.                      "Price Chg
**      mldr_set_val_proccat zu pc  t_mlcd.
*    ELSE.
**   Value
*      CASE t_mlcd-categ.
*
*** ZU (CATEG)
*        WHEN 'ZU'.
*          CASE t_mlcd-ptyp.
*            WHEN 'BB' OR 'BBK'.                   "Purchasing
*              mldr_set_val_proccat zu bb   t_mlcd.
*            WHEN 'BF'.                            "Production  Receipt
*              mldr_set_val_proccat zu bf   t_mlcd.
*            WHEN 'BL'.                            "Subcontract
*              mldr_set_val_proccat zu bl   t_mlcd.
*            WHEN 'BUBS' OR 'BU'.                  "Transfer    Receipt
*              mldr_set_val_proccat zu bubs t_mlcd.
*            WHEN 'BUBM'.                          "Change Mat. Receipt
*              mldr_set_val_proccat zu bubm t_mlcd.
*            WHEN  OTHERS.                         "Other Receipt
*              mldr_set_val_proccat zu etc  t_mlcd.
*          ENDCASE.
*
*
*** VN (CATEG)
*        WHEN 'VN'.
*          CASE t_mlcd-ptyp.
*            WHEN 'VKA'.                       "Sales
**            mldr_set_val_proccat vn vka  t_mlcd.
*            WHEN 'VF'.                 "Production
*              mldr_set_val_proccat vn vf   t_mlcd.
*            WHEN 'VL'.                 "Subcontract
*              mldr_set_val_proccat vn vl   t_mlcd.
*            WHEN 'VHP'.                       "Cost Object (4.7?)
*              mldr_set_val_proccat vn cob  t_mlcd.
*            WHEN 'VK'.                         "CC - Reason code + CC
**            mldr_set_val_proccat vn vk   t_mlcd.
*            WHEN 'VEAU'.                      "IO - Order Type
**            mldr_set_val_proccat vn or   t_mlcd.
*            WHEN 'VUBS' OR 'VU'.               "Transfer    Consumption
*              mldr_set_val_proccat vn vubs t_mlcd.
*            WHEN 'VUBM'.                       "Change Mat. Consumption
*              mldr_set_val_proccat vn vubm t_mlcd.
*            WHEN OTHERS.                     "ETC Consumption
**            BREAK-POINT.
**            mldr_set_val_proccat vn etc  t_mlcd.
*          ENDCASE.
*
*      ENDCASE.
*    ENDIF.
*
*    COLLECT it_ztco_mlit.
*  ENDLOOP.
*
*ENDFORM.                    " collect_mlcd
*
**&---------------------------------------------------------------------
**
**&      Form  MLDR_READ_DATA_BY_MVTGRP
**&---------------------------------------------------------------------
**
**       Read Data By MVTGRP
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM mldr_read_data_by_mvtgrp.
*  RANGES: r_ptyp      FOR mlit-ptyp.
*  RANGES: r_kjahr     FOR mlit-kjahr.
*  RANGES: r_bewartgrp FOR mlit-bewartgrp.
*
*  CHECK NOT t_mats[] IS INITIAL.
*
*  r_ptyp-sign   = 'E'.  r_ptyp-option = 'EQ'.
*  r_ptyp-low    = 'VF'.   APPEND r_ptyp.   "Production
*  r_ptyp-low    = 'VL'.   APPEND r_ptyp.   "Production
*  r_ptyp-low    = 'VU'.   APPEND r_ptyp.   "
*  r_ptyp-low    = 'VUBS'. APPEND r_ptyp.   "
*  r_ptyp-low    = 'VUBM'. APPEND r_ptyp.   "
*
*  r_kjahr-sign   = 'E'.   r_kjahr-option = 'EQ'.
*  r_kjahr-low    = p_bdatj.       APPEND r_kjahr.
*  r_kjahr-low    = p_bdatj - 1.   APPEND r_kjahr.
*
*  r_bewartgrp-sign   = 'E'.
*  r_bewartgrp-option = 'EQ'.
*  r_bewartgrp-low    = '16'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '15'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '20'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '19'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '66'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '65'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '58'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '57'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '60'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '59'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '64'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '63'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '56'.  APPEND r_bewartgrp.
*  r_bewartgrp-low    = '55'.  APPEND r_bewartgrp.
*
**ANDY
**ML document has system date,
**1 MLCR = 1 or 2 MLPP (cross period)
**need to check MLPPF (field group <> 'UMO')
*
*  CLEAR : it_zvco_mlxxv, it_zvco_mlxxv[].
*  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_zvco_mlxxv
*     FROM mlit
*              INNER JOIN mlcr
*                 ON mlcr~belnr =  mlit~belnr
*                AND mlcr~kjahr =  mlit~kjahr
*                AND mlcr~posnr =  mlit~posnr
*              INNER JOIN mlppf
*                 ON mlcr~belnr = mlppf~belnr
*                AND mlcr~kjahr = mlppf~kjahr
*                AND mlcr~posnr = mlppf~posnr
*                AND mlcr~bdatj = mlppf~bdatj
*                AND mlcr~poper = mlppf~poper
*              INNER JOIN mlpp
*                 ON mlcr~belnr =  mlpp~belnr
*                AND mlcr~kjahr =  mlpp~kjahr
*                AND mlcr~posnr =  mlpp~posnr
*                AND mlcr~bdatj =  mlpp~bdatj
*                AND mlcr~poper =  mlpp~poper
*              INNER JOIN mlhd
*                 ON mlcr~belnr =  mlhd~belnr
*                AND mlcr~kjahr =  mlhd~kjahr
*            FOR ALL entries IN t_mats
*            WHERE mlit~kalnr = t_mats-kalnr    "INDEX
*              AND mlit~psart = 'UP'            "ML update
**              AND mlit~KJAHR  in r_KJAHR       "current/previous year
*              AND mlit~kategorie = 'VN'         "Consumption Only
*              AND mlit~ptyp      IN r_ptyp
*              AND mlit~bewartgrp IN r_bewartgrp
*              AND mlcr~bdatj = p_bdatj
*              AND mlcr~poper = p_poper
*              AND mlcr~curtp = p_curtp
*              AND mlppf~feldg <> 'UMO'.         "No previous posting
*
*
*  LOOP AT it_zvco_mlxxv.
**   Key Part
*    CLEAR it_ztco_mlit.
**   MOVE-CORRESPONDING it_zvco_mlxxv TO it_ztco_mlit.
*    it_ztco_mlit-kalnr = it_zvco_mlxxv-kalnr.
**    it_ztco_mlit-meins = it_zvco_mlxxv-meins.
**    it_ztco_mlit-waers = it_zvco_mlxxv-waers.
*
** Set Value By MVTGRP.
*    PERFORM  mldr_set_val_by_mvtgrp.
** Collect
*    COLLECT it_ztco_mlit.
*    CLEAR   it_ztco_mlit.
*    CLEAR it_zvco_mlxxv.
*  ENDLOOP.
*
*ENDFORM.                    " MLDR_READ_DATA_BY_MVTGRP
*
**&---------------------------------------------------------------------
**
**&      Form  MLDR_SET_VAL_BY_MVTGRP
**&---------------------------------------------------------------------
**
**       Set Values By MVT Grp.
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM mldr_set_val_by_mvtgrp.
** There can NOT be PRD or ERD of data caused by Material Movement
** PRD/ERD is posted in different document without Material Movement
*  CASE it_zvco_mlxxv-bewartgrp.
** Domestic Revenue
*    WHEN '16' OR '15'.
*      mldr_set_val_mvtgrp  vn  vka16   it_zvco_mlxxv.
** Export   Revenue
*    WHEN '20' OR '19'.
*      mldr_set_val_mvtgrp  vn  vka20   it_zvco_mlxxv.
*
** Key-In   Consumption
*    WHEN '66' OR '65'.
*      IF it_zvco_mlxxv-ptyp = 'VEAU'.
*        mldr_set_val_mvtgrp  vn  or66      it_zvco_mlxxv.
*      ELSE.
*        mldr_set_val_mvtgrp  vn  cc99      it_zvco_mlxxv.
*      ENDIF.
** OS & D   Consumption
*    WHEN '58' OR '57'.
*      IF it_zvco_mlxxv-ptyp = 'VEAU'.
*        mldr_set_val_mvtgrp  vn  or58      it_zvco_mlxxv.
*      ELSE.
*        mldr_set_val_mvtgrp  vn  cc99      it_zvco_mlxxv.
*      ENDIF.
*
** Physical Inv. Consumption
*    WHEN '60' OR '59'.
*      mldr_set_val_mvtgrp  vn  cc60      it_zvco_mlxxv.
** Etc           Consumption
**    WHEN '64' OR '63'.
**      mldr_set_val_mvtgrp  vn  cc64      it_zvco_mlxxv.
**      PERFORM make_itab_detail.
** Other Usage   Consumption
*    WHEN '56' OR '55'.
*      IF     it_zvco_mlxxv-ptyp = 'VEAU'.  "ORDER
*        mldr_set_val_mvtgrp  vn  cc99    it_zvco_mlxxv.
**--fix sales order GI for other purpose
*      ELSEIF it_zvco_mlxxv-ptyp = 'VKA'.   "SALES DELIVERY w/o Billing
*        BREAK-POINT.
*        mldr_set_val_mvtgrp  vn  cc99    it_zvco_mlxxv.
*      ELSE.
*        mldr_set_val_mvtgrp  vn  cc99    it_zvco_mlxxv.
*      ENDIF.
*
*      PERFORM make_itab_detail.
*    WHEN OTHERS.
*      mldr_set_val_mvtgrp  vn  cc99    it_zvco_mlxxv.
*  ENDCASE.
*
*ENDFORM.                    " MLDR_SET_VAL_BY_MVTGRP
*
**&---------------------------------------------------------------------
**
**&      Form  MLDR_CAL_VN_ETC_VKA
**&---------------------------------------------------------------------
**
**       Cal. and Treat the remains
**----------------------------------------------------------------------
**
**FORM mldr_cal_vn_etc_vka.
**
**  LOOP AT it_ztco_mlit.
*** VKA.
**    mldr_cal_vka  lbkum  vka16 vka20.
**    mldr_cal_vka  salk3  vka16 vka20.
**    mldr_cal_vka  rd     vka16 vka20.
*** Sub. VN Etc
**    mldr_sub_vn_etc  lbkum.
**    mldr_sub_vn_etc  salk3.
**    mldr_sub_vn_etc  rd.
*** Modify
**    MODIFY  it_ztco_mlit.
**    CLEAR    it_ztco_mlit.
**  ENDLOOP.
**
**  CLEAR    it_ztco_mlit.
**
**ENDFORM.                    " MLDR_CAL_VN_ETC_VKA
*
**&---------------------------------------------------------------------
**
**&      Form  DEL_ZTCO_MLIT
**&---------------------------------------------------------------------
**
**       Delete data in table 'ZTCO_MLIT'
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM del_ztco_mlit.
*
*  CHECK p_up = 'X'.
*
** local Data definition
*  DATA : lv_answer .
*  DATA : lv_title(80).
*
** message
*  CLEAR : lv_answer,  lv_title.
*  CONCATENATE 'All data will be lost  ' p_bdatj '/' s_poper
*         INTO lv_title.
*
*  SELECT COUNT( * ) INTO sy-tabix
*    FROM ztco_mlit
*   WHERE bdatj = p_bdatj
*     AND poper = p_poper.
*
*  CHECK sy-tabix > 0.
*
*  IF p_batch <> 'X' .
*    AUTHORITY-CHECK OBJECT 'S_DEVELOP'
*       ID 'DEVCLASS' DUMMY
*       ID 'OBJTYPE'  FIELD 'DEBUG'
*       ID 'OBJNAME'  DUMMY
*       ID 'P_GROUP'  DUMMY
*       ID 'ACTVT'    FIELD '03'.
*
*
*    IF sy-subrc <> 0.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
**     DEFAULTOPTION        = 'Y'
*          textline1            = lv_title
*          textline2            = 'In Table - ZTCO_MLIT'
*          titel                = 'Delete DATA in Table'
**     START_COLUMN         = 25
**     START_ROW            = 6
**     CANCEL_DISPLAY       = 'X'
*        IMPORTING
*          answer               = lv_answer.
*
*      IF  lv_answer <> 'J'.
*        MESSAGE e043.
*      ENDIF.
*
*    ENDIF.
*  ENDIF.
*
** P_KOKRS  p_bdatj   P_POPER   P_CURTP
** whenever running this program
** All data - Period relative - should be deleted and replaced
** with new records
*  DELETE FROM ztco_mlit
*         WHERE bdatj = p_bdatj
*           AND poper = p_poper
*           AND matnr IN s_matnr
*           AND kalnr IN r_kalnr
*           AND mtart IN s_mtart
*           AND bwkey IN s_bwkey
*           AND bklas IN s_bklas.
*
*  DELETE FROM ztco_mlii
*         WHERE bdatj = p_bdatj
*           AND poper = p_poper
*           AND matnr IN s_matnr
*           AND bklas IN s_bklas.
*
*  DELETE FROM ztco_mli2
*         WHERE bdatj = p_bdatj
*           AND poper = p_poper
*           AND matnr IN s_matnr.
*
*
** No Check Subrc
*  COMMIT WORK.
*
*ENDFORM.                    " DEL_ZTCO_MLIT
*
**&---------------------------------------------------------------------
**
**&      Form  UPDATE_ZTCO_MLIT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM update_ztco_mlit.
*
*  CHECK p_up = 'X'.
*
** Delete data in table 'ZTCO_MLIT'.
*  PERFORM del_ztco_mlit.
*
*
*  LOOP AT it_ztco_mli2.
*    it_ztco_mli2-bukrs = p_kokrs.
*    it_ztco_mli2-bdatj = p_bdatj.
*    it_ztco_mli2-poper = s_poper-low.
*
*    it_ztco_mli2-erdat = sy-datum.
*    it_ztco_mli2-erzet = sy-uzeit.
*    it_ztco_mli2-ernam = sy-uname.
*
*    MODIFY it_ztco_mli2.  CLEAR it_ztco_mli2.
*  ENDLOOP.
*
*  LOOP AT it_ztco_mlii.
*    it_ztco_mlii-bukrs = p_kokrs.
*    it_ztco_mlii-bdatj = p_bdatj.
*    it_ztco_mlii-poper = s_poper-low.
*
*    it_ztco_mlii-erdat = sy-datum.
*    it_ztco_mlii-erzet = sy-uzeit.
*    it_ztco_mlii-ernam = sy-uname.
*
*    MODIFY it_ztco_mlii.  CLEAR it_ztco_mlii.
*  ENDLOOP.
*
*
** Success
**FIXME
**  insert ZTCO_MLIT from table it_ztco_mlit.
*  IF p_batch = 'X'.
*    IF NOT it_ztco_mlit[] IS INITIAL.
*      MODIFY ztco_mlit FROM TABLE it_ztco_mlit.
*    ENDIF.
*
*
*    IF NOT it_ztco_mli2[] IS INITIAL.
*      MODIFY ztco_mli2 FROM TABLE it_ztco_mli2.
*    ENDIF.
*
*    IF NOT it_ztco_mlii[] IS INITIAL.
*      MODIFY ztco_mlii FROM TABLE it_ztco_mlii.
*    ENDIF.
*
*  ELSE.
*
*    IF NOT it_ztco_mlit[] IS INITIAL.
*      MODIFY ztco_mlit FROM TABLE it_ztco_mlit.
*
*      IF sy-subrc <> 0.
*        WRITE:/ 'error during insert(ZTCO_MLIT)'.
*      ELSE.
*        WRITE:/ 'Data is saved(ZTCO_MLIT)'.
*      ENDIF.
*    ELSE.
*      WRITE:/ 'NO Data to  save(ZTCO_MLIT)'.
*    ENDIF.
*
*    IF NOT it_ztco_mli2[] IS INITIAL.
*      MODIFY ztco_mli2 FROM TABLE it_ztco_mli2.
*
*      IF sy-subrc <> 0.
*        WRITE:/ 'error during insert(ZTCO_MLI2)'.
*      ELSE.
*        WRITE:/ 'Data is saved(ZTCO_MLI2)'.
*      ENDIF.
*    ELSE.
*      WRITE:/ 'NO Data to  save(ZTCO_MLI2)'.
*    ENDIF.
*
*    IF NOT it_ztco_mlii[] IS INITIAL.
*      MODIFY ztco_mlii FROM TABLE it_ztco_mlii.
*      IF sy-subrc <> 0.
*        WRITE:/ 'error during insert(ZTCO_MLII)'.
*      ELSE.
*        WRITE:/ 'Data is saved(ZTCO_MLII)'.
*      ENDIF.
*    ELSE.
*      WRITE:/ 'NO Data to  save(ZTCO_MLII)'.
*    ENDIF.
*  ENDIF.
*
*
** for batch program.
*  IF p_batch = 'X'.
*    DATA : l_artnr(50),
*           l_temp_artnr TYPE matnr.
*    DATA : BEGIN OF it_temp_mlit OCCURS 0 ,
*           matnr LIKE ztco_mlit-matnr,
*           END OF it_temp_mlit.
*
*    SELECT  matnr INTO CORRESPONDING FIELDS OF TABLE  it_temp_mlit
*        FROM ztco_mlit
*        FOR ALL ENTRIES IN t_mats
*       WHERE bukrs = p_kokrs
*         AND bdatj = p_bdatj
*         AND poper = p_perab
*         AND matnr = t_mats-matnr.
*
*    LOOP AT t_mats.
*      CLEAR  it_ztco_mlit.
*      READ  TABLE it_ztco_mlit WITH KEY matnr = t_mats-matnr.
*      IF sy-subrc <> 0 .
*        PERFORM make_batch_log USING 'D'.
*      ELSE.
*        CLEAR it_temp_mlit.
*        READ TABLE it_temp_mlit WITH KEY matnr = t_mats-matnr.
*        IF sy-subrc = 0 .
*          PERFORM make_batch_log USING 'F'.
*        ELSE.
*          PERFORM make_batch_log USING 'E'.
*          CONCATENATE t_mats-matnr l_artnr INTO l_artnr
*          SEPARATED BY space.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF NOT l_artnr IS INITIAL.
*      EXPORT  l_artnr TO  MEMORY ID 'ML03_ARTNR'.
*    ENDIF.
*  ENDIF.
*
*
*
*ENDFORM.                    " UPDATE_ZTCO_MLIT
*
**&---------------------------------------------------------------------
**
**&      Form  CAL_NOT_DIS
**&---------------------------------------------------------------------
**
**       Not_Distributed
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
**form cal_not_dis.
**
**  data : it_l_rsparams like standard table of rsparams
**                       with header line .
**  clear : it_l_rsparams, it_l_rsparams[].
**  call function 'RS_REFRESH_FROM_SELECTOPTIONS'
**    exporting
**      curr_report           = 'ZACO11U_MLVA'
***   IMPORTING
***     SP                    =
**    tables
**      selection_table       = it_l_rsparams
**    exceptions
**      not_found             = 1
**      no_report             = 2
**      others                = 3.
**
**  if sy-subrc <> 0.
**    message id sy-msgid type sy-msgty number sy-msgno
**            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**  endif.
**
*** Clear
**  clear : t_out, t_out[].
**
**  submit zaco11u_mlva
***    VIA SELECTION-SCREEN
**    and return
**    with selection-table it_l_rsparams
**    with p_poper = p_poper
**    with p_bdatj = p_bdatj
**    with r_matnr in s_matnr
**    with p_notdis = 'X'.
***<data_tab>
**  import t_out   = t_out
**         from memory id 'HLV'.
**
**  free memory id 'HLV'.
**
**  clear : t_out.
**
***it_ztco_mlit
*** Cal. ND
**  loop at t_out.
**    loop at it_ztco_mlit
**      where kalnr  = t_out-kalnr
**        and bdatj  = t_out-bdatj
**        and poper  = t_out-poper
***       AND UNTPER = T_OUT-UNTPER
***       AND CURTP  = T_OUT-CURTP
**        and matnr  = t_out-matnr
**        and bwkey  = t_out-bwkey
**        and bwtar  = t_out-bwtar.
*** RD
**      it_ztco_mlit-eb_rd    = it_ztco_mlit-eb_rd - t_out-sumdif.
**      it_ztco_mlit-vn_etc_rd = it_ztco_mlit-vn_etc_rd - t_out-sumdif.
*** Not Distributed Price difference
**      it_ztco_mlit-NV_lbkum  = t_out-menge.
**      it_ztco_mlit-nv_rd    = it_ztco_mlit-nv_rd - t_out-sumdif.
**      modify it_ztco_mlit.
**      clear  it_ztco_mlit.
**    endloop.
**    clear t_out.
**  endloop.
**
**endform.                    " CAL_NOT_DIS
**----------------------------------------------------------------------
**
**   INCLUDE ZACO11L_F031
**
**----------------------------------------------------------------------
**
*
**&---------------------------------------------------------------------
**
**&      Form  READ_MLCD
**&---------------------------------------------------------------------
**
**       Read MLCD data
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM read_mlcd.
*
*  DATA: ls_mats  TYPE s_mats,
*        ls_kalnr TYPE ckmv0_matobj_str.
*
*  REFRESH: it_kalnr.
*  LOOP AT t_mats INTO ls_mats.
*    CLEAR: ls_kalnr.
*    ls_kalnr-kalnr = ls_mats-kalnr.
*    ls_kalnr-bwkey = ls_mats-bwkey.
*    APPEND ls_kalnr TO it_kalnr.
*  ENDLOOP.
*
** Read data
*  CALL FUNCTION 'CKMCD_MLCD_READ'
*    EXPORTING
*      i_from_bdatj            = p_bdatj
*      i_from_poper            = p_poper
**     I_TO_BDATJ              =
**     I_TO_POPER              =
**     I_UNTPER                =
**     I_RUN_ID                =
**     I_NO_BUFFER             =
*      i_refresh_buffer        = 'X'
*      i_online                = 'X'
**     I_NO_MLCD_CREATE        =
*    TABLES
*      it_kalnr                = it_kalnr
*      ot_mlcd                 = t_mlcd
*      ot_mlcd_not_alloc       = t_mlcd_not_alloc
*    EXCEPTIONS
*      data_error              = 1
*      OTHERS                  = 2.
*
*  IF sy-subrc <> 0.
*    MESSAGE ID   sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  SORT t_mlcd_not_alloc BY kalnr bdatj poper.
**
*ENDFORM.                    " READ_MLCD
*
**&---------------------------------------------------------------------
**
**&      Form  MERGE_MLCD_DATA
**&---------------------------------------------------------------------
**
**       Merge to IT_MLCD
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM merge_mlcd_data.
*
*  CLEAR : it_mlcd,    it_mlcd[].
*  CLEAR : it_ztco_mlit, it_ztco_mlit[].
*
** Ignore Process category for procurement alt. or consuption alt.
** Ignore Procurement alternative/process
** MLCD-PTYP MLCD-BVALT
*
**t_mlcd
**t_mlcd_not_alloc
*
** For Allocated data / Unallocated Data
*  PERFORM allocated_data_mlcd.
*
** Not distributed
*  PERFORM fill_ndi_nin.
*
** For Beginning Inv./Amt.
*  PERFORM beginning_inv_amt.
*  PERFORM cal_space_cat.
*
**After PC, goods movement -> use new STD price.
*  PERFORM read_prev_pst_var2.
*
** Calculate Cumulative / Ending
*  PERFORM cal_cumulative_ending.
*
*
*ENDFORM.                    " MERGE_MLCD_DATA
*
**&---------------------------------------------------------------------
**
**&      Form  TRANS_VAL_TO_DIS
**&---------------------------------------------------------------------
**
**       Transfering Value to Dis. Tab.
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM trans_val_to_dis.
*
**  DATA :  &1_LBKUM LIKE MLCD-LBKUM,
**          &1_SALK3 LIKE MLCD-SALK3,
**          &1_RD    LIKE MLCD-SALK3.
*
*  FIELD-SYMBOLS : <fsval> TYPE ANY.
*  DATA : lv_fname(60).
*
*  CLEAR : it_ztco_mlit.
*
** Key Part
*  MOVE-CORRESPONDING it_mlcd TO it_ztco_mlit.
*
** Qty.
*  CLEAR lv_fname.
*  CONCATENATE 'it_ztco_mlit' '-' it_mlcd-categ '_LBKUM'
*         INTO lv_fname.
*  ASSIGN (lv_fname) TO <fsval>.
*
*  <fsval> = it_mlcd-lbkum.
*
** Amt. / Valuated stock
*  CLEAR lv_fname.
*  CONCATENATE 'it_ztco_mlit' '-' it_mlcd-categ '_SALK3'
*         INTO lv_fname.
*  ASSIGN (lv_fname) TO <fsval>.
*
*  <fsval> = it_mlcd-salk3.
*
** Amt. / Price Difference (ERD+PRD In Category)
*  CLEAR lv_fname.
*  CONCATENATE 'it_ztco_mlit' '-' it_mlcd-categ '_RD'
*         INTO lv_fname.
*  ASSIGN (lv_fname) TO <fsval>.
*
*  <fsval> = it_mlcd-estprd
*          + it_mlcd-estkdm
*          + it_mlcd-mstprd
*          + it_mlcd-mstkdm
*          + it_mlcd-tpprd.
*
****ANDY
*  IF it_mlcd-categ = 'PC'.
*    it_ztco_mlit-ab_salk3 = - it_mlcd-salk3.
*    it_ztco_mlit-ab_rd    = + it_mlcd-salk3.
*    it_ztco_mlit-ab_pc_salk3 = + it_mlcd-salk3.
*    it_ztco_mlit-ab_pc_rd    = - it_mlcd-salk3.
*  ENDIF.
*
** Append
*  COLLECT it_ztco_mlit.
*  CLEAR  it_ztco_mlit.
*
*ENDFORM.                    " TRANS_VAL_TO_DIS
*
**&---------------------------------------------------------------------
**
**&      Form  ALLOCATED_DATA_MLCD
**&---------------------------------------------------------------------
**
**       For Allocated data / Unallocated Data
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM allocated_data_mlcd.
*
*  DATA: l_object LIKE mara-matnr.
*
*** For Allocated Data
** Ending/Beginning Inv/Amt will be calculated later part
*  LOOP AT t_mlcd WHERE categ NE 'AB'
*                   AND categ NE 'EB'.
*
** Find object for GI
*    CLEAR: it_mlcd, l_object.
*    IF ( t_mlcd-categ = 'VN' AND t_mlcd-ptyp = 'VF' )
*    OR t_mlcd-ptyp = 'BU' OR t_mlcd-ptyp = 'VO'.
*      READ TABLE i_proc_kalnr WITH KEY prock = t_mlcd-bvalt
*           BINARY SEARCH.
*      IF sy-subrc = 0.
*        MOVE: i_proc_kalnr-matnr TO it_mlcd-object,
*              i_proc_kalnr-matnr TO l_object.
*      ENDIF.
*    ENDIF.
*
**Allow Price Change
*    IF t_mlcd-categ = space AND t_mlcd-ptyp <> 'PC'.
*      CONTINUE.
*    ENDIF.
*    IF t_mlcd-ptyp = 'PC'.
*      t_mlcd-categ = 'PC'.
*    ENDIF.
*
**Transfer; change category
*    IF t_mlcd-ptyp(2) = 'VU' OR t_mlcd-ptyp(2) = 'BU'.
**     CONCATENATE t_mlcd-ptyp(1) 'S' INTO t_mlcd-categ.
*      t_mlcd-categ = t_mlcd-ptyp(2).
*
*      READ TABLE t_mats WITH KEY kalnr = t_mlcd-kalnr BINARY SEARCH.
*      IF l_object <> space AND t_mats-matnr <> l_object.  "M2M
*        IF t_mlcd-ptyp(2) = 'BU'.
*          t_mlcd-categ = 'BM'.
*        ELSE.
*          t_mlcd-categ = 'VM'.
*        ENDIF.
*      ELSEIF t_mlcd-ptyp+2(2) = 'BU'.
*        CONTINUE.  "SKIP (sales order stock transfer)
*      ENDIF.
*
**Debit/Credit
*    ELSEIF t_mlcd-categ = 'VP'.
*      t_mlcd-categ = 'VP'. "'BU'.
*    ELSEIF t_mlcd-categ = 'VN'.
*      CASE t_mlcd-ptyp.
*        WHEN 'VEAU'. t_mlcd-categ = 'VO'.   "GI-order
*        WHEN 'VK'.   t_mlcd-categ = 'VK'.   "GI-CC
*      ENDCASE.
*
*    ENDIF.
*
**physical inventory
*   if t_mlcd-categ = 'ZU' and t_mlcd-ptyp = 'B+'.
*      t_mlcd-categ = 'ZV'.
*   elseif t_mlcd-categ = 'VN' and t_mlcd-ptyp = 'V+'.
*      t_mlcd-categ = 'VV'.
*   endif.
*
** Transfer values
** Clear the values which are not useful.
*    CLEAR : t_mlcd-meins, t_mlcd-waers.
*    CLEAR : t_mlcd-bvalt, t_mlcd-ptyp.
*    MOVE-CORRESPONDING t_mlcd TO it_mlcd.
*
*
*    COLLECT it_mlcd.
*  ENDLOOP.
*
*** Trasfer data to Display Tab.
*  LOOP AT it_mlcd.
*    CASE it_mlcd-categ.
** AB	Beginning inventory
** EB	Ending inventory
** SPACE Value
*      WHEN 'AB' OR 'EB' OR space.
*
*      WHEN OTHERS .
*        PERFORM trans_val_to_dis.
*    ENDCASE.
*  ENDLOOP.
*
*  CLEAR : it_ztco_mlit, it_mlcd.
*
*
*ENDFORM.                    " ALLOCATED_DATA_MLCD
*
**&---------------------------------------------------------------------
**
**&      Form  fill_material_info
**&---------------------------------------------------------------------
**
**       Read Material Data / Unit / Curr.
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM fill_material_info.
** Read Material Information with Cost. Est. Number.
** read All data
**  IT_KALNR
*
*  CLEAR : macku, ckmlhd.
*
*  LOOP AT it_ztco_mlit.
*    CLEAR t_mats.
*    READ TABLE t_mats WITH KEY
*                      kalnr = it_ztco_mlit-kalnr
*                      BINARY SEARCH.
*    IF sy-subrc <> 0.
*      WRITE:/ '***Fatal error...', it_ztco_mlit-kalnr.
*    ELSE.
*      MOVE : t_mats-matnr TO it_ztco_mlit-matnr,
*             t_mats-bwkey TO it_ztco_mlit-bwkey,
*             t_mats-bwtar TO it_ztco_mlit-bwtar,
*             t_mats-mtart TO it_ztco_mlit-mtart,
*             t_mats-bklas TO it_ztco_mlit-bklas,
*             t_mats-meins TO it_ztco_mlit-meins.
*
*      it_ztco_mlit-bukrs = p_kokrs.
*      it_ztco_mlit-bdatj = p_bdatj.
*      it_ztco_mlit-poper = s_poper-low.
**      it_ztco_mlit-erdat = sy-datum.
**      it_ztco_mlit-erzet = sy-uzeit.
**      it_ztco_mlit-ernam = sy-uname.
*
*      MODIFY it_ztco_mlit.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " fill_material_info
*
**&---------------------------------------------------------------------
**
**&      Form  BEGINNING_INV_AMT
**&---------------------------------------------------------------------
**
**       For Beginning Inv./Amt.
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM beginning_inv_amt.
*
*  LOOP AT  t_mats.
*    CLEAR   it_ztco_mlit.
*    it_ztco_mlit-kalnr = t_mats-kalnr.
*
** Beginning Qty + Previous Posting (UMKUMO)
*    it_ztco_mlit-ab_lbkum = t_mats-abkumo + t_mats-umkumo.
*
** Beginning Amt.
*    it_ztco_mlit-ab_salk3 = t_mats-absalk3.
** Beginning RD.
*    it_ztco_mlit-ab_rd = t_mats-abprd_o  + t_mats-abkdm_o
*                   + t_mats-abprd_mo + t_mats-abkdm_mo.
*
*    COLLECT it_ztco_mlit.
*  ENDLOOP.
*
*ENDFORM.                    " BEGINNING_INV_AMT
*
**&---------------------------------------------------------------------
**
**&      Form  CAL_SPACE_CAT
**&---------------------------------------------------------------------
**
**       Calculate Values in the Category ' ' (SPACE)
**----------------------------------------------------------------------
**
*FORM cal_space_cat.
*
*  DATA : it_l_tmp_mlcd LIKE mlcd OCCURS 0 WITH HEADER LINE .
*
*  CLEAR : it_l_tmp_mlcd, it_l_tmp_mlcd[].
*
*** For Allocate (SAPCE)
*  LOOP AT t_mlcd WHERE categ EQ space  OR categ EQ 'AB'.
** Clear the values which are not useful.
*    CLEAR : t_mlcd-ptyp,  t_mlcd-bvalt.
*    CLEAR : t_mlcd-meins, t_mlcd-waers.
** --> AB
** Transfer values
*    CLEAR it_l_tmp_mlcd.
*    MOVE-CORRESPONDING t_mlcd TO it_l_tmp_mlcd.
*    it_l_tmp_mlcd-categ = 'AB'.
** Collect
*    COLLECT it_l_tmp_mlcd.
*    CLEAR t_mlcd.
*  ENDLOOP.
*
**** For Not Allocate (SAPCE)
**  LOOP AT t_mlcd_not_alloc
**                     WHERE categ EQ space.
*** Clear the values which are not useful.
**    CLEAR : t_mlcd_not_alloc-ptyp,  t_mlcd_not_alloc-bvalt.
**    CLEAR : t_mlcd_not_alloc-meins, t_mlcd_not_alloc-waers.
*** --> AB
*** Transfer values
**    CLEAR it_l_tmp_mlcd.
**    MOVE-CORRESPONDING t_mlcd_not_alloc TO it_l_tmp_mlcd.
**    it_l_tmp_mlcd-categ = 'AB'.
*** Collect
**    COLLECT it_l_tmp_mlcd.
**    CLEAR t_mlcd_not_alloc.
**  ENDLOOP.
*
*** Put data into Disp. Tab.
*  LOOP AT it_l_tmp_mlcd.
**
*    CLEAR it_ztco_mlit.
*    MOVE-CORRESPONDING it_l_tmp_mlcd TO it_ztco_mlit.
** Qty.
*    it_ztco_mlit-ab_lbkum  = it_l_tmp_mlcd-lbkum.
*
** Amt. / Valuated stock
*    it_ztco_mlit-ab_salk3  = it_l_tmp_mlcd-salk3.
*
** Amt. / Price Difference (ERD+PRD In Category)
*    it_ztco_mlit-ab_rd     = it_l_tmp_mlcd-estprd
*                         + it_l_tmp_mlcd-estkdm
*                         + it_l_tmp_mlcd-mstprd
*                         + it_l_tmp_mlcd-mstkdm
*                         + it_l_tmp_mlcd-tpprd.
** Append
*    COLLECT it_ztco_mlit.
*    CLEAR   it_ztco_mlit.
*    CLEAR   it_l_tmp_mlcd.
*  ENDLOOP.
*
*ENDFORM.                    " CAL_SPACE_CAT
*
**&---------------------------------------------------------------------
**
**&      Form  SET_BEGINNING_DATA_FOR_NO_TR
**&---------------------------------------------------------------------
**
** Data in MLCD could not be generated if any transaction
** is executied at specific period
** Set Beginning Inv. from Ending Inv. data of previous period.
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM set_beginning_data_for_no_tr.
*  DATA : it_l_dis LIKE STANDARD TABLE OF it_ztco_mlit
*                  WITH HEADER LINE .
*
**  MESSAGE e724 RAISING no_data_found.
*  CLEAR it_ztco_mlit.
*  CLEAR it_kalnr.
*  CLEAR mbew.
*
** IF PAST period and it_ztco_mlit[] is initial. Do not proceed futher.
*  CHECK  p_bdatj => sy-datum(4)
*    AND  p_poper => sy-datum+4(2)
*    AND  it_ztco_mlit[] IS INITIAL.
*
*
*  SORT it_ztco_mlit BY kalnr.
*
*  LOOP AT it_kalnr.
*    CLEAR it_l_dis.
*    CLEAR it_ztco_mlit.
*    READ TABLE  it_ztco_mlit WITH KEY kalnr = it_kalnr-kalnr
*                           BINARY SEARCH.
*    IF sy-subrc <> 0.
** MBEW
*      CLEAR mbew.
*      SELECT SINGLE * FROM mbew
*                     WHERE matnr = it_kalnr-matnr
*                       AND bwkey = it_kalnr-bwkey
*                       AND bwtar = it_kalnr-bwtar.
** Set Data
**
*      MOVE-CORRESPONDING mbew TO it_l_dis.
**  Values
*      it_l_dis-ab_lbkum = mbew-lbkum.
*      it_l_dis-ab_salk3 = mbew-salk3.
**
**      it_l_dis-bdatj = p_bdatj.
**      it_l_dis-poper = p_poper.
*
***FIXME
**      CLEAR mara .
**      SELECT SINGLE
**              mtart meins
**              INTO (it_l_dis-mtart, it_l_dis-meins)
**                    FROM mara
**                   WHERE matnr = it_kalnr-matnr.
**
*      it_l_dis-kalnr = it_kalnr-kalnr.
*
**FIXME
*** Currency types and valuation types in a valuation area
**      CLEAR ckmlct.
**      SELECT SINGLE
**              waers INTO it_l_dis-waers
**                    FROM ckmlct
**                   WHERE bwkey = it_l_dis-bwkey
**                     AND curtp = p_curtp.
**     Append
*      APPEND it_l_dis.
*      CLEAR  it_l_dis.
*    ENDIF.
*    CLEAR it_kalnr.
*  ENDLOOP.
*
** Add itab to Display-ITAB
*  IF NOT it_l_dis[] IS INITIAL.
*    APPEND LINES OF  it_l_dis  TO it_ztco_mlit.
*  ENDIF.
*
*ENDFORM.                    " SET_BEGINNING_DATA_FOR_NO_TR
*
**&---------------------------------------------------------------------
**
**&      Form  CHK_FT_DATE
**&---------------------------------------------------------------------
**
**       Check Future Period
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM chk_ft_date.
*  IF  p_bdatj > sy-datum(4).
*    MESSAGE e000 WITH text-301 p_bdatj p_poper.
*  ELSE.
**requested by andy
**changed by wskim,on 01/18/2005 : Program error
**-----Start
**    IF  P_POPER > SY-DATUM+4(2).
**      MESSAGE E000 WITH TEXT-301 p_bdatj P_POPER.
**    ENDIF.
**-----End
*  ENDIF.
*
*  IF p_db IS INITIAL AND NOT s_poper-high IS INITIAL.
*    MESSAGE e000 WITH text-302 s_poper-high.
*  ENDIF.
*
*ENDFORM.                    " CHK_FT_DATE
*
**&---------------------------------------------------------------------
**
**&      Form  CHK_UP_OPT
**&---------------------------------------------------------------------
**
**       Chk. Update Option
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM chk_up_opt.
*
** the authorization rule about CO is not defined yet.
** So, temporary, prevent running through update mode with T-code
*  IF sy-tcode = 'ZCOR11'.
*    CLEAR  p_up.
*    LOOP AT SCREEN.
*      IF screen-group1 = 'DIS'.
*        screen-invisible = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.
*ENDFORM.                    " CHK_UP_OPT
*
**&---------------------------------------------------------------------
**
**&      Form  CHK_PAR_UP
**&---------------------------------------------------------------------
**
**       Check Update Mode-> Delete with Period Key
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM chk_par_up.
**TEMP
**FIXME
*  EXIT.
*
*  CHECK p_up = 'X'.
*  IF    ( s_matnr[] IS INITIAL AND s_matnr IS INITIAL )
*    AND ( s_mtart[] IS INITIAL AND s_mtart IS INITIAL )
*    AND ( s_bklas[] IS INITIAL AND s_bklas IS INITIAL ).
*  ELSE.
*    MESSAGE e000 WITH text-401.
*  ENDIF.
*
*ENDFORM.                    " CHK_PAR_UP
**&---------------------------------------------------------------------
**
**&      Form  find_bad_boys
**&---------------------------------------------------------------------
**
*FORM find_bad_boys.
**refer MLHELP_VALUE_FLOW_ANALYZER program
*  DATA: ls_ndi_ndi TYPE s_ndi,
*        ls_ndi_cum TYPE s_ndi,
*        ls_ndi_nin TYPE s_ndi,
*        ls_mats TYPE s_mats,
*        ls_ckmlpp TYPE ckmlpp,
*        ls_ckmlcr TYPE ckmlcr,
*        ls_mlcd TYPE mlcd,
*        ls_mlcd_not_alloc TYPE mlcd,
*        l_color(3) TYPE c,
*        l_ab_menge LIKE mlcd-lbkum,
*        l_nin TYPE boole_d,
*        l_kalnr_old LIKE mlcd-kalnr.
*
*  CLEAR t_mats.
*  LOOP AT t_mats INTO ls_mats.
*    MOVE-CORRESPONDING ls_mats TO ls_ckmlpp.
*    MOVE-CORRESPONDING ls_mats TO ls_ckmlcr.
*
*    CLEAR: ls_ndi_ndi, ls_ndi_cum, ls_ndi_nin.
*    MOVE-CORRESPONDING ls_ckmlpp  TO ls_ndi_ndi.
*    MOVE-CORRESPONDING ls_ckmlpp  TO ls_ndi_cum.
*    MOVE-CORRESPONDING ls_ckmlpp  TO ls_ndi_nin.
*
*    IF ls_ckmlpp-status >= '40'.  "y_einstufig_abgerechnet.
*      MOVE-CORRESPONDING ls_ckmlcr  TO ls_ndi_ndi.
*      MOVE-CORRESPONDING ls_ckmlcr  TO ls_ndi_cum.
*      MOVE-CORRESPONDING ls_ckmlcr  TO ls_ndi_nin.
*
*      ls_ndi_cum-estprd = ls_mats-abprd_o + ls_mats-zuprd_o +
*                          ls_mats-vpprd_o.
*      ls_ndi_cum-estkdm = ls_mats-abkdm_o + ls_mats-zukdm_o +
*                          ls_mats-vpkdm_o.
**
*      IF ls_ckmlpp-status >= '50'.  "y_mehrstufig_abgerechnet.
*        ls_ndi_cum-mstprd = ls_mats-abprd_mo + ls_mats-zuprd_mo.
*        ls_ndi_cum-mstkdm = ls_mats-abkdm_mo + ls_mats-zukdm_mo.
*      ELSE.
*        ls_ndi_cum-mstprd = ls_mats-abprd_mo.
*        ls_ndi_cum-mstkdm = ls_mats-abkdm_mo.
*      ENDIF.
**
*      ls_ndi_ndi-estprd = ls_ndi_cum-estprd.
*      ls_ndi_ndi-estkdm = ls_ndi_cum-estkdm.
*      ls_ndi_ndi-mstprd = ls_ndi_cum-mstprd.
*      ls_ndi_ndi-mstkdm = ls_ndi_cum-mstkdm.
*      ls_ndi_ndi-estprd = ls_ndi_ndi-estprd -
*                          ( ls_mats-vnprd_ea + ls_mats-ebprd_ea ).
*      ls_ndi_ndi-estkdm = ls_ndi_ndi-estkdm -
*                          ( ls_mats-vnkdm_ea + ls_mats-ebkdm_ea ).
*      ls_ndi_ndi-mstprd = ls_ndi_ndi-mstprd -
*                          ( ls_mats-vnprd_ma + ls_mats-ebprd_ma ).
*      ls_ndi_ndi-mstkdm = ls_ndi_ndi-mstkdm -
*                          ( ls_mats-vnkdm_ma + ls_mats-ebkdm_ma ).
*      ls_ndi_ndi-sumdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm +
*                          ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
**       Gibt's eine 'Nicht verrechnet'-Zeile?
*      READ TABLE t_mlcd_not_alloc INTO ls_mlcd_not_alloc
*                                   WITH KEY kalnr = ls_mats-kalnr
*                                            bdatj = p_bdatj
*                                            poper = p_poper
**                                             untper = ls_ckmlcr-untper
**                                             curtp = ls_ckmlcr-curtp.
*                                           BINARY SEARCH.
*      IF sy-subrc = 0.
*        l_nin = 'X'.
*      ELSE.
*        CLEAR: l_nin.
*      ENDIF.
*      IF NOT ls_ndi_ndi-sumdif IS INITIAL OR
*         NOT l_nin IS INITIAL.
*        IF ls_ndi_ndi-kalnr <> l_kalnr_old.
*          l_kalnr_old = ls_ndi_ndi-kalnr.
*          IF l_color = 'C21'.
*            l_color = 'C20'.
*          ELSE.
*            l_color = 'C21'.
*          ENDIF.
*        ENDIF.
**        READ TABLE t_mats INTO ls_mats
**                           WITH KEY kalnr = ls_ndi_ndi-kalnr
**                           BINARY SEARCH.
**        IF sy-subrc = 0.
*        MOVE-CORRESPONDING ls_mats TO ls_ndi_cum.
*        MOVE-CORRESPONDING ls_mats TO ls_ndi_ndi.
*        MOVE-CORRESPONDING ls_mats TO ls_ndi_nin.
**        ENDIF.
**Not distributed
*        IF NOT ls_ndi_ndi-sumdif IS INITIAL.
*          ls_ndi_ndi-pos_type = 'NDI'.
*          ls_ndi_ndi-color = l_color.
**           ls_ndi_ndi-pos_type_text = text-006.
*          CLEAR: ls_ndi_ndi-menge, ls_ndi_ndi-wert.
*          ls_ndi_ndi-prdif = ls_ndi_ndi-estprd + ls_ndi_ndi-mstprd.
*          ls_ndi_ndi-krdif = ls_ndi_ndi-estkdm + ls_ndi_ndi-mstkdm.
*          ls_ndi_ndi-estdif = ls_ndi_ndi-estprd + ls_ndi_ndi-estkdm.
*          ls_ndi_ndi-mstdif = ls_ndi_ndi-mstprd + ls_ndi_ndi-mstkdm.
*          APPEND ls_ndi_ndi TO t_bad.
*        ENDIF.
**Not included
*        IF NOT l_nin IS INITIAL.
*          ls_ndi_nin-pos_type = 'NIN'.
*          ls_ndi_nin-color = l_color.
**           ls_ndi_nin-pos_type_text = text-007.
*          CLEAR: ls_ndi_nin-menge, ls_ndi_nin-wert.
*          ls_ndi_nin-estprd = ls_mlcd_not_alloc-estprd.
*          ls_ndi_nin-estkdm = ls_mlcd_not_alloc-estkdm.
*          ls_ndi_nin-mstprd = ls_mlcd_not_alloc-mstprd.
*          ls_ndi_nin-mstkdm = ls_mlcd_not_alloc-mstkdm.
*          ls_ndi_nin-prdif = ls_ndi_nin-estprd + ls_ndi_nin-mstprd.
*          ls_ndi_nin-krdif = ls_ndi_nin-estkdm + ls_ndi_nin-mstkdm.
*          ls_ndi_nin-estdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm.
*          ls_ndi_nin-mstdif = ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
*          ls_ndi_nin-sumdif = ls_ndi_nin-estprd + ls_ndi_nin-estkdm +
*                                ls_ndi_nin-mstprd + ls_ndi_nin-mstkdm.
*          APPEND ls_ndi_nin TO t_bad.
*        ENDIF.
*
*      ENDIF.
*    ELSE.
**   Da kommt noch was!
*    ENDIF.
*  ENDLOOP.
*
*
*ENDFORM.                    " get_material_periods
**&---------------------------------------------------------------------
**
**&      Form  fill_ndi_nin
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM fill_ndi_nin.
** Not Include --> Consumption Diff.
*  LOOP AT t_bad.
*    CLEAR it_mlcd.
*
*    it_mlcd-kalnr  = t_bad-kalnr.
*    it_mlcd-bdatj  = t_bad-bdatj.
*    it_mlcd-poper  = t_bad-poper.
**   it_mlcd-UNTPER = t_bad-UNTPER.
*    it_mlcd-curtp  = t_bad-curtp.
**    it_mlcd-MATNR  = t_bad-MATNR.
**    it_mlcd-BWKEY  = t_bad-BWKEY.
**    it_mlcd-BWTAR  = t_bad-BWTAR.
*
*    IF t_bad-pos_type = 'NDI'.
*      it_mlcd-categ = 'ND'.
*      it_mlcd-estprd = - t_bad-estprd.
*      it_mlcd-estkdm = - t_bad-estkdm.
*      it_mlcd-mstprd = - t_bad-mstprd.
*      it_mlcd-mstkdm = - t_bad-mstkdm.
*    ELSE.
*      it_mlcd-categ = 'NI'.
*      it_mlcd-estprd = + t_bad-estprd.
*      it_mlcd-estkdm = + t_bad-estkdm.
*      it_mlcd-mstprd = + t_bad-mstprd.
*      it_mlcd-mstkdm = + t_bad-mstkdm.
*    ENDIF.
*
*    PERFORM trans_val_to_dis.
*  ENDLOOP.
*
*ENDFORM.                    " fill_ndi_nin
**&---------------------------------------------------------------------
**
**&      Form  get_materials_from_closing
**&---------------------------------------------------------------------
**
*FORM get_materials_from_closing.
*  TABLES: t001k, marv.
*  RANGES: r_bwkey FOR t001k-bwkey.
*  r_bwkey-sign   = 'I'.
*  r_bwkey-option = 'EQ'.
*  REFRESH: t_mats.
*  SELECT * FROM t001k
*         WHERE bukrs = p_kokrs.
*    IF s_bwkey[] IS INITIAL.
*      r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
*    ELSE.
*      IF t001k-bwkey IN s_bwkey.
*        r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
*      ENDIF.
*    ENDIF.
*  ENDSELECT.
*
*  DATA: l_abrechdat LIKE ckmlhd-abrechdat.
*
*  SELECT SINGLE * FROM marv WHERE bukrs = p_kokrs.
*  IF p_bdatj = marv-lfgja AND p_poper = marv-lfmon.
*
*    SELECT mbew~bwkey mbew~matnr mbew~bwtar
*           makt~maktg
*           ckmlhd~kalnr
**        ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr ckmlhd~pspnr
*           mara~mtart mara~matkl mbew~bklas
*           ckmlcr~stprs
*           ckmlcr~pvprs AS verpr
*           mara~spart mara~meins
*           mbew~lfgja mbew~lfmon
*           ckmlpp~status
*           ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
*           ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
*           ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
*           ckmlcr~abprd_mo ckmlcr~abkdm_mo
*           ckmlcr~vpprd_o  ckmlcr~zuprd_o
*           ckmlcr~zukdm_o  ckmlcr~vpkdm_o
*           ckmlcr~zuprd_mo ckmlcr~zukdm_mo
*           ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
*           ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
*           ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
*           ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
*       INTO CORRESPONDING FIELDS OF TABLE t_mats
*             FROM ( mbew
*                    INNER JOIN mara
*                      ON  mbew~matnr = mara~matnr
*                    INNER JOIN makt
*                      ON  makt~matnr = mara~matnr
*                     AND  makt~spras = sy-langu
*                    INNER JOIN marc
*                      ON  mbew~matnr = marc~matnr
*                      AND mbew~bwkey = marc~werks
**                  INNER JOIN ckmlmv011
**                    ON  ckmlmv011~matnr = mbew~matnr
**                    AND ckmlmv011~bwkey = mbew~bwkey
**                  INNER JOIN ckmlrunperiod
**                    ON  ckmlmv011~laufid = ckmlrunperiod~run_id
*                    INNER JOIN ckmlcr
*                      ON  ckmlcr~kalnr = mbew~kaln1
*                     AND  ckmlcr~bdatj = p_bdatj
*                     AND  ckmlcr~poper = p_poper
*                     AND  ckmlcr~curtp = p_curtp
*                     AND  ckmlcr~untper = space
*                    INNER JOIN ckmlpp
*                      ON  ckmlpp~kalnr  = ckmlcr~kalnr
*                     AND  ckmlpp~bdatj  = p_bdatj
*                     AND  ckmlpp~poper  = p_poper
*                     AND  ckmlpp~untper = space
*                    INNER JOIN ckmlhd
*                      ON  ckmlhd~kalnr = mbew~kaln1 )
*             WHERE mbew~bwkey IN r_bwkey
*               AND mbew~bklas IN s_bklas
*               AND mara~mtart IN s_mtart
*               AND mbew~matnr IN s_matnr
*               AND ckmlhd~abrechdat <> l_abrechdat
*               AND (   ckmlpp~zukumo <> 0 OR ckmlpp~vnkumo <> 0
*                    OR ckmlpp~abkumo <> 0 OR ckmlpp~umkumo <> 0
*                    OR ckmlpp~lbkum <> 0 )
*      ORDER BY ckmlhd~kalnr.
*
*  ELSE.
*
*    SELECT mbew~bwkey mbew~matnr mbew~bwtar
*           makt~maktg
*           ckmlhd~kalnr
**        ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr ckmlhd~pspnr
*           ckmlmv011~mtart ckmlmv011~matkl ckmlmv011~bklas
*           ckmlcr~stprs
*           ckmlcr~pvprs AS verpr
*           mara~spart mara~meins
*           mbew~lfgja mbew~lfmon
*           ckmlpp~status
*           ckmlpp~abkumo   ckmlpp~umkumo   ckmlpp~zukumo
*           ckmlpp~vnkumo   ckmlpp~lbkum    ckmlpp~ekkumo
*           ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
*           ckmlcr~abprd_mo ckmlcr~abkdm_mo
*           ckmlcr~vpprd_o  ckmlcr~zuprd_o
*           ckmlcr~zukdm_o  ckmlcr~vpkdm_o
*           ckmlcr~zuprd_mo ckmlcr~zukdm_mo
*           ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
*           ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
*           ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
*           ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
*       INTO CORRESPONDING FIELDS OF TABLE t_mats
*             FROM ( mbew
*                    INNER JOIN mara
*                      ON  mbew~matnr = mara~matnr
*                    INNER JOIN makt
*                      ON  makt~matnr = mara~matnr
*                     AND  makt~spras = sy-langu
*                    INNER JOIN marc
*                      ON  mbew~matnr = marc~matnr
*                      AND mbew~bwkey = marc~werks
*                    INNER JOIN ckmlmv011
*                      ON  ckmlmv011~matnr = mbew~matnr
*                      AND ckmlmv011~bwkey = mbew~bwkey
*                    INNER JOIN ckmlrunperiod
*                      ON  ckmlmv011~laufid = ckmlrunperiod~run_id
*                    INNER JOIN ckmlcr
*                      ON  ckmlcr~kalnr  = ckmlmv011~kalnr
*                     AND  ckmlcr~bdatj  = ckmlrunperiod~gjahr
*                     AND  ckmlcr~poper  = ckmlrunperiod~poper
*                     AND  ckmlcr~curtp  = '10'
*                     AND  ckmlcr~untper = space
*                    INNER JOIN ckmlpp
*                      ON  ckmlpp~kalnr  = ckmlcr~kalnr
*                     AND  ckmlpp~bdatj  = p_bdatj
*                     AND  ckmlpp~poper  = p_poper
*                     AND  ckmlpp~untper = space
*                    INNER JOIN ckmlhd
*                      ON  ckmlmv011~kalnr = ckmlhd~kalnr )
*             WHERE ckmlrunperiod~gjahr = p_bdatj
*               AND ckmlrunperiod~poper = p_poper
*               AND ckmlmv011~bwkey IN r_bwkey
*               AND ckmlmv011~bklas IN s_bklas
*               AND ckmlmv011~mtart IN s_mtart
*               AND ckmlmv011~matnr IN s_matnr
*               AND ckmlhd~abrechdat <> l_abrechdat
*               AND (   ckmlpp~zukumo <> 0 OR ckmlpp~vnkumo <> 0
*                    OR ckmlpp~abkumo <> 0 OR ckmlpp~umkumo <> 0
*                    OR ckmlpp~lbkum <> 0 )
*      ORDER BY ckmlhd~kalnr.
*
*    PERFORM get_extra_data_mbew.
*    DELETE ADJACENT DUPLICATES FROM t_mats COMPARING kalnr.
*  ENDIF.
*
*  SORT t_mats BY kalnr.
*
*ENDFORM.                    " get_materials_from_closing
**&---------------------------------------------------------------------
**
**&      Form  get_data_from_buffer
**&---------------------------------------------------------------------
**
*FORM get_data_from_buffer.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_mlit
*      FROM ztco_mlit
*     WHERE bukrs = p_kokrs
*       AND bdatj = p_bdatj
**         and poper = p_poper
*       AND poper IN  s_poper
*       AND matnr IN s_matnr
*       AND bklas IN s_bklas
*       AND mtart IN s_mtart.
*
*ENDFORM.                    " get_data_from_buffer
**&---------------------------------------------------------------------
**
**&      Form  get_mat_info
**&---------------------------------------------------------------------
**
**FORM get_mat_info.
**  exit.
**
**  DATA: l_idx LIKE sy-index.
**
**  LOOP AT it_ztco_mlit.
**    l_idx = sy-tabix.
**    CLEAR t_mats.
**    READ TABLE t_mats WITH KEY
**                      kalnr = it_ztco_mlit-kalnr
**                      BINARY SEARCH.
***    MOVE : t_mats-meins TO it_ztco_mlit-meins.
***           t_mats-matnr TO it_ztco_mlit-matnr,
***           t_mats-bwkey TO it_ztco_mlit-bwkey,
***           t_mats-bwtar TO it_ztco_mlit-bwtar,
***           t_mats-mtart TO it_ztco_mlit-mtart,
***           t_mats-bklas TO it_ztco_mlit-bklas.
**
**    MODIFY it_ztco_mlit INDEX l_idx.
**  ENDLOOP.
**
**ENDFORM.                    " get_mat_info
**&---------------------------------------------------------------------
**
**&      Form  call_alv_list
**&---------------------------------------------------------------------
**
*FORM call_alv_list.
*
*  PERFORM field_setting(zcogsrev) TABLES gt_fieldcat USING :
* 'MTART'            'Mtyp'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'BKLAS'            'V.Cl'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'MATNR'            'Material'    '18' 'X' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'BWKEY'            'Plant'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'BWTAR'            'V.Typ'       '06' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'KALNR'            'CostNo'      '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
* 'MEINS'            'UoM'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
*
*'AB_LBKUM'       ' BIG   '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS' ' ',
*'AB_SALK3'       '$BIG  S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',
*'AB_RD'          '$BIG  V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  ' ',
*'ZU_PC_LBKUM'    ' PChg'      '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_PC_SALK3'    '$Pchg S'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_PC_RD'       '$Pchg V'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*
*'ZU_BB_LBKUM'    ' GR MM '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_BB_SALK3'    '$GR MMS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BB_RD'       '$GR MMV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BF_LBKUM'    ' GR PP '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_BF_SALK3'    '$GR PPS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BF_RD'       '$GR PPV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BL_LBKUM'    ' GR OS '    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_BL_SALK3'    '$GR OSS'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BL_RD'       '$GR OSV'    '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BUBS_LBKUM'  ' GR TRF '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_BUBS_SALK3'  '$GR TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BUBS_RD'     '$GR TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BUBM_LBKUM'  ' GR M2M'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_BUBM_SALK3'  '$GR M2MS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_BUBM_RD'     '$GR M2MV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_VP_LBKUM'    ' GR D/C'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_VP_SALK3'    '$GR D/CS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_VP_RD'       '$GR D/CV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_ETC_LBKUM'   ' GR ETC'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'ZU_ETC_SALK3'   '$GR ETC S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_ETC_RD'      '$GR ETC V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'ZU_ND_RD'       '$GR NoDst'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*
*'VN_NI_RD'       '$GI NoInc'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA16_LBKUM' ' GI SD  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VKA16_SALK3' '$GI SD S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA16_RD'    '$GI SD V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA20_LBKUM' ' GI SE  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VKA20_SALK3' '$GI SE S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA20_RD'    '$GI SE V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA99_LBKUM' ' GI SX  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VKA99_SALK3' '$GI SX S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VKA99_RD'    '$GI SX V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VF_LBKUM'    ' GI PP  '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VF_SALK3'    '$GI PP S'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VF_RD'       '$GI PP V'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_COB_LBKUM'   ' GI COB  '  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_COB_SALK3'   '$GI COB S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_COB_RD'      '$GI COB V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_OR66_LBKUM'  ' GI Key '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_OR66_SALK3'  '$GI Key S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_OR66_RD'     '$GI Key V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_OR58_LBKUM'  ' GI OSD  '  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_OR58_SALK3'  '$GI OSD S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_OR58_RD'     '$GI OSD V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_CC60_LBKUM'  ' GI PhyIn ' '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_CC60_SALK3'  '$GI PhyInS' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_CC60_RD'     '$GI PhyInV' '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_CC99_LBKUM'  ' GI Mis Q'  '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_CC99_SALK3'  '$GI Mis S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_CC99_RD'     '$GI Mis V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VUBS_LBKUM'  ' GI TRF'    '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VUBS_SALK3'  '$GI TRFS'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VUBS_RD'     '$GI TRFV'   '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VUBM_LBKUM'  ' GI M2M '   '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'VN_VUBM_SALK3'  '$GI M2M S'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'VN_VUBM_RD'     '$GI M2M V'  '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*
*'EB_LBKUM'       ' End Q'     '16' ' ' 'R' ' ' ' ' ' ' 'MEINS'  'X',
*'EB_SALK3'       '$End S'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X',
*'EB_RD'          '$End V'     '16' ' ' 'R' ' ' ' ' ' ' '     '  'X'.
*
*
*
*  g_repid = sy-repid.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*       EXPORTING
*            i_callback_program = g_repid
*            it_fieldcat        = gt_fieldcat
**i_callback_user_command           = g_user_command
*            i_save             = 'A'
*       TABLES
*            t_outtab           = it_ztco_mlit
*       EXCEPTIONS
*            program_error      = 1
*            OTHERS             = 2.
*
*
*ENDFORM.                    " call_alv_list
**&---------------------------------------------------------------------
**
**&      Form  make_itab_kalnr
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM make_itab_detail.
*
*  MOVE-CORRESPONDING it_zvco_mlxxv TO it_detail.
*  it_detail-mjahr = it_detail-aworg.
*  COLLECT it_detail. CLEAR it_detail.
*
*ENDFORM.                    " make_itab_kalnr
**&---------------------------------------------------------------------
**
**&      Form  get_from_mseg
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_from_mseg.
*
*  DATA : l_fdate TYPE datum,
*         l_tdate TYPE datum.
*
*  CHECK NOT it_detail[] IS INITIAL.
*
*  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_mseg
*    FROM mseg
*     FOR ALL ENTRIES IN it_detail
*   WHERE mblnr = it_detail-awref
*     AND mjahr = it_detail-mjahr
*     AND matnr = it_detail-matnr.
*
*
*  LOOP AT it_mseg.
*    CLEAR it_detail.
*    READ TABLE it_detail WITH KEY awref = it_mseg-mblnr.
*
*    CLEAR t_mats.
*    READ TABLE t_mats WITH KEY matnr = it_mseg-matnr.
*
*    PERFORM move_it_ztco_mli2.
*
*    IF it_ztco_mli2-saknr CS '137090'.
*      PERFORM make_tab_for_bkpf USING it_ztco_mli2-mblnr
*                                      it_ztco_mli2-bdatj.
*    ENDIF.
*    APPEND it_ztco_mli2. CLEAR it_ztco_mli2.
*  ENDLOOP.
*
** for clearing acc : '137090'.
** previous data (not cleared : 137090)
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mli2_temp
*    FROM ztco_mli2
*   WHERE bdatj = p_bdatj
*     AND poper < p_poper
*     AND saknr = '000137090'.
*
*  LOOP AT it_mli2_temp.
*    MOVE-CORRESPONDING it_mli2_temp TO it_ztco_mli2.
*    PERFORM make_tab_for_bkpf USING it_ztco_mli2-mblnr
*                                    it_ztco_mli2-bdatj.
*    APPEND it_ztco_mli2. CLEAR it_ztco_mli2.
*  ENDLOOP.
*
*  IF NOT it_clear[] IS INITIAL.
*    PERFORM modify_clear_acc.
*  ENDIF.
*
*
*  LOOP AT it_ztco_mli2 .
*    MOVE-CORRESPONDING it_ztco_mli2 TO it_ztco_mlii.
*    COLLECT it_ztco_mlii. CLEAR it_ztco_mlii.
*  ENDLOOP.
*
*ENDFORM.       " get_from_mseg
**&---------------------------------------------------------------------
**
**&      Form  move_it_ztco_mli2
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM move_it_ztco_mli2.
*  it_ztco_mli2-bdatj     = it_detail-kjahr.
*  it_ztco_mli2-poper     = it_detail-poper.
*  it_ztco_mli2-kalnr     = it_detail-kalnr.
*  it_ztco_mli2-mblnr     = it_mseg-mblnr.
*  it_ztco_mli2-zeile     = it_mseg-zeile.
*  it_ztco_mli2-matnr     = it_mseg-matnr.
*  it_ztco_mli2-bklas     = t_mats-bklas.
*  it_ztco_mli2-saknr     = it_mseg-sakto.
*  it_ztco_mli2-kostl     = it_mseg-kostl.
*  it_ztco_mli2-aufnr     = it_mseg-aufnr.
*  it_ztco_mli2-dmbtr     = it_mseg-dmbtr.
*  it_ztco_mli2-menge     = it_mseg-menge.
*  it_ztco_mli2-meins     = it_mseg-meins.
*
*ENDFORM.                    " move_it_ztco_mli2
**&---------------------------------------------------------------------
**
**&      Form  get_clearing_doc
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_clearing_doc.
*  it_ztco_mli2-bdatj     = it_detail-kjahr.
*  it_ztco_mli2-poper     = it_detail-poper.
*  it_ztco_mli2-kalnr     = it_detail-kalnr.
*  it_ztco_mli2-mblnr     = it_mseg-mblnr.
**  CONCATENATE
*ENDFORM.                    " get_clearing_doc
**&---------------------------------------------------------------------
**
**&      Form  make_tab_for_bkpf
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM make_tab_for_bkpf USING p_mblnr
*                             p_bdatj.
*
*  CONCATENATE p_mblnr p_bdatj INTO it_clear-awkey .
*  it_clear-mblnr = p_mblnr.
*  it_clear-bdatj = p_bdatj.
*  APPEND it_clear. CLEAR it_clear.
*
*ENDFORM.                    " make_tab_for_bkpf
**&---------------------------------------------------------------------
**
**&      Form  MODIFY_CLEAR_ACC
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM modify_clear_acc.
*
*  DATA : l_fdate TYPE datum,
*         l_tdate TYPE datum.
*
*  DATA : l_belnr LIKE bseg-belnr,
*         l_buzei LIKE bseg-buzei.
*
*  CHECK NOT it_clear[] IS INITIAL.
*
*  SELECT gjahr belnr awkey blart
*    INTO CORRESPONDING FIELDS OF TABLE it_bkpf
*    FROM bkpf
*     FOR ALL ENTRIES IN it_clear
*   WHERE awtyp = 'MKPF'
*     AND awkey = it_clear-awkey.
*
*  CHECK NOT it_bkpf[] IS INITIAL.
*
*  CONCATENATE p_bdatj '0101' INTO l_fdate.
*  CONCATENATE p_bdatj '1231' INTO l_tdate.
*
*  SELECT gjahr belnr augdt augbl blart
*    INTO CORRESPONDING FIELDS OF TABLE it_bsas_temp
*    FROM bsas
*     FOR ALL ENTRIES IN it_bkpf
*   WHERE gjahr = it_bkpf-gjahr
*     AND belnr = it_bkpf-belnr
**    AND buzei = it_bkpf-buzei
*     AND augdt BETWEEN l_fdate AND l_tdate.
*
*
*  CHECK NOT it_bsas_temp[] IS INITIAL.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bsas
*    FROM bsas
*     FOR ALL ENTRIES IN it_bsas_temp
*   WHERE augdt  =  it_bsas_temp-augdt
*     AND augbl  =  it_bsas_temp-augbl
*     AND blart  <> it_bsas_temp-blart.
*
*  CHECK NOT it_bsas[] IS INITIAL.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bseg
*    FROM bseg
*     FOR ALL ENTRIES IN it_bsas
*   WHERE bukrs  = it_bsas-bukrs
*     AND belnr  = it_bsas-belnr
*     AND gjahr  = it_bsas-gjahr.
*
*  LOOP AT it_ztco_mli2 WHERE saknr = '0000137090'.
*    CLEAR it_clear.
*    READ TABLE it_clear WITH KEY  mblnr = it_ztco_mli2-mblnr
*                                  bdatj = it_ztco_mli2-bdatj.
*    CHECK sy-subrc = 0 .
*    CLEAR it_bkpf.
*    READ TABLE it_bkpf WITH KEY awkey = it_clear-awkey.
*    CHECK sy-subrc = 0 .
*    CLEAR it_bsas_temp.
*    READ TABLE it_bsas_temp WITH KEY gjahr = it_bkpf-gjahr
*                                     belnr = it_bkpf-belnr.
*    CHECK sy-subrc = 0 .
*    CLEAR it_bsas.
*    READ TABLE it_bsas WITH KEY augdt  = it_bsas_temp-augdt
*                                augbl  = it_bsas_temp-augbl.
*
*    CHECK sy-subrc = 0 .
*    LOOP AT it_bseg WHERE belnr = it_bsas-belnr
*                      AND dmbtr = it_ztco_mli2-dmbtr.
*      CLEAR : l_belnr, l_buzei.
*      SELECT SINGLE belnr buzei INTO (l_belnr, l_buzei)
*         FROM ztco_mli2
*        WHERE belnr = it_bseg-belnr
*          AND buzei = it_bseg-buzei
*          AND bdatj = it_ztco_mli2-bdatj
*          AND mblnr <> it_ztco_mli2-mblnr
*          AND zeile <> it_ztco_mli2-zeile.
*      IF sy-subrc <> 0 .
*        it_ztco_mli2-saknr = it_bseg-hkont.
*        it_ztco_mli2-belnr = it_bseg-belnr.
*        it_ztco_mli2-buzei = it_bseg-buzei.
*        it_ztco_mli2-kostl = it_bseg-kostl.
*        it_ztco_mli2-aufnr = it_bseg-aufnr.
*        MODIFY it_ztco_mli2. CLEAR it_ztco_mli2.
*        DELETE it_bseg.
*        EXIT.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
*ENDFORM.                    " MODIFY_CLEAR_ACC
**&---------------------------------------------------------------------
**
**&      Form  get_materials_from_PARALLEL
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_materials_from_parallel.
**  PERFORM get_mats_form_mbew.
*  LOOP AT t_mats.
*    s_matnr-sign       = 'I' .
*    s_matnr-option     = 'EQ'.
*    s_matnr-low        = t_mats-matnr.
*    APPEND s_matnr.
*
*    r_kalnr-sign       = 'I' .
*    r_kalnr-option     = 'EQ'.
*    r_kalnr-low        = t_mats-kalnr.
*    APPEND r_kalnr.
*  ENDLOOP.
*
*ENDFORM.                    " get_materials_from_PARALLEL
**&---------------------------------------------------------------------
**
**&      Form  GET_MATS_FORM_MBEW
**&---------------------------------------------------------------------
**
**       Obsolete function
**----------------------------------------------------------------------
**
*FORM get_mats_form_mbew.
**MBEW - current
**MBEWH - past... but blank period...
*  DATA: l_mbewh LIKE mbewh.
*  LOOP AT t_mats.
**old data... get from history
*    IF t_mats-lfgja = p_bdatj AND t_mats-lfmon > p_poper
*    OR t_mats-lfgja > p_bdatj.
*
*      SELECT * INTO l_mbewh
*       FROM mbewh
*          WHERE matnr = t_mats-matnr
*            AND bwkey = t_mats-bwkey
*            AND bwtar = t_mats-bwtar
*           AND lfgja  = p_bdatj
*           AND lfmon <= p_poper
*       ORDER BY lfgja DESCENDING
*                lfmon DESCENDING.
*        EXIT.
*      ENDSELECT.
*      IF sy-subrc <> 0.
*        SELECT * INTO l_mbewh
*         FROM mbewh
*            WHERE matnr = t_mats-matnr
*              AND bwkey = t_mats-bwkey
*              AND bwtar = t_mats-bwtar
*             AND lfgja < p_bdatj
*         ORDER BY lfgja DESCENDING
*                  lfmon DESCENDING.
*          EXIT.
*        ENDSELECT.
*      ENDIF.
*
*      t_mats-bklas = l_mbewh-bklas.
*      t_mats-stprs = l_mbewh-stprs.
*      t_mats-verpr = l_mbewh-verpr.
*      MODIFY t_mats.
*    ENDIF.
*
**delete
*    IF NOT t_mats-bklas IN s_bklas.
*      DELETE t_mats.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " GET_MATS_FORM_MBEW
**&---------------------------------------------------------------------
**
**&      Form  make_batch_log
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_1361   text
**----------------------------------------------------------------------
**
*FORM make_batch_log USING p_flag.
*  CHECK p_batch = 'X'.
*
*  IF p_flag = 'D'.
*    DELETE FROM ztco_batch_log WHERE repid =  'ML03'
*                                 AND kokrs = p_kokrs
*                                 AND bdatj = p_bdatj
*                                 AND poper = p_perab
*                                 AND matnr = t_mats-matnr.
*  ELSE.
*    CLEAR : it_log, it_log[].
*    it_log-kokrs = p_kokrs.
*    it_log-bdatj = p_bdatj.
*    it_log-poper = p_perab.
*    it_log-matnr = t_mats-matnr.
*    it_log-repid = 'ML03'.
*    it_log-flag  = p_flag.
*    APPEND it_log. CLEAR it_log.
*    MODIFY ztco_batch_log FROM TABLE it_log.
*  ENDIF.
*  COMMIT WORK.
*ENDFORM.                    " make_batch_log
**&---------------------------------------------------------------------
**
**&      Form  check_obligatory_field
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM check_obligatory_field.
*  IF p_kokrs IS INITIAL.
*    MESSAGE s000 WITH 'Please input Controlling area. '.
*  ENDIF.
*
*  IF p_bdatj IS INITIAL.
*    MESSAGE s000 WITH 'Please input Year. '.
*  ENDIF.
*
*  IF s_poper[] IS INITIAL.
*    MESSAGE s000 WITH 'Please input Period. '.
*  ENDIF.
*
*  IF p_curtp IS INITIAL.
*    MESSAGE s000 WITH 'Please input Crcy type/val.view. '.
*  ENDIF.
*
*ENDFORM.                    " check_obligatory_field
**&---------------------------------------------------------------------
**
**&      Form  get_extra_data_mbew
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_extra_data_mbew.
*  TABLES: t134, t025.
*  DATA: i_t134 LIKE t134 OCCURS 0 WITH HEADER LINE.
*
*  IF s_bklas[] IS INITIAL.
*    SELECT * INTO TABLE i_t134 FROM t134
*       WHERE mtart IN s_mtart
*         AND kkref <> space.
*
*    s_bklas-option = 'EQ'. s_bklas-sign = 'I'.
*    SELECT * FROM t025
*       FOR ALL ENTRIES IN i_t134
*       WHERE bklas IN s_bklas
*         AND kkref = i_t134-kkref.
*      s_bklas-low = t025-bklas. APPEND s_bklas.
*    ENDSELECT.
*  ENDIF.
*
*  SELECT mbewh~bwkey mbewh~matnr mbewh~bwtar
*         makt~maktg
*         ckmlhd~kalnr
**        ckmlhd~sobkz ckmlhd~vbeln ckmlhd~posnr ckmlhd~pspnr
*         mara~mtart mara~matkl mbewh~bklas
*         mbewh~stprs
*         mbewh~verpr
*         mara~spart mara~meins
*         mbewh~lfgja mbewh~lfmon
*         ckmlpp~status
*         ckmlpp~abkumo ckmlpp~umkumo ckmlpp~zukumo
*         ckmlpp~vnkumo ckmlpp~lbkum  ckmlpp~ekkumo
*         ckmlcr~absalk3  ckmlcr~abprd_o  ckmlcr~abkdm_o
*         ckmlcr~abprd_mo ckmlcr~abkdm_mo
*         ckmlcr~vpprd_o  ckmlcr~zuprd_o
*         ckmlcr~zukdm_o  ckmlcr~vpkdm_o
*         ckmlcr~zuprd_mo ckmlcr~zukdm_mo
*         ckmlcr~vnprd_ea ckmlcr~vnkdm_ea
*         ckmlcr~ebprd_ea ckmlcr~ebkdm_ea
*         ckmlcr~vnprd_ma ckmlcr~vnkdm_ma
*         ckmlcr~ebprd_ma ckmlcr~ebkdm_ma
*     APPENDING CORRESPONDING FIELDS OF TABLE t_mats
*           FROM ( mbewh
*                  INNER JOIN mara
*                    ON  mbewh~matnr = mara~matnr
*                  INNER JOIN makt
*                    ON  makt~matnr = mara~matnr
*                   AND  makt~spras = sy-langu
*                  INNER JOIN ckmlhd
*                    ON  mbewh~matnr = ckmlhd~matnr
*                   AND  mbewh~bwkey = ckmlhd~bwkey
*                  INNER JOIN ckmlcr
*                    ON  ckmlcr~kalnr  = ckmlhd~kalnr
*                   AND  ckmlcr~bdatj  = p_bdatj
*                   AND  ckmlcr~poper  = p_poper
*                   AND  ckmlcr~curtp  = '10'
*                   AND  ckmlcr~untper = space
*                  INNER JOIN ckmlpp
*                    ON  ckmlpp~kalnr  = ckmlhd~kalnr
*                   AND  ckmlpp~bdatj  = p_bdatj
*                   AND  ckmlpp~poper  = p_poper
*                   AND  ckmlpp~untper = space )
*             WHERE mbewh~matnr IN s_matnr
*               AND mbewh~bwkey IN s_bwkey
*               AND mbewh~lfgja = p_bdatj
*               AND mbewh~lfmon = p_poper
*               AND mbewh~bklas IN s_bklas
*               AND mbewh~vprsv = 'V'
*               AND (   ckmlpp~zukumo <> 0 OR ckmlpp~vnkumo <> 0
*                    OR ckmlpp~abkumo <> 0 OR ckmlpp~umkumo <> 0
*                    OR ckmlpp~lbkum <> 0 )
*    ORDER BY ckmlhd~kalnr.
*
**   DELETE T_MATS WHERE NOT MATNR IN S_BKLAS.
*  DELETE t_mats WHERE NOT mtart IN s_mtart.
*  DELETE t_mats WHERE NOT matnr IN s_matnr.
*ENDFORM.                    " get_extra_data_mbew
**&---------------------------------------------------------------------
**
**&      Form  delete_blank_records
**&---------------------------------------------------------------------
**
*FORM delete_blank_records.
*  DELETE it_ztco_mlit
*    WHERE ab_lbkum          = 0
*      AND ab_salk3          = 0
*      AND ab_rd             = 0
*      AND zu_bb_lbkum       = 0
*      AND zu_bb_salk3       = 0
*      AND zu_bb_rd          = 0
*      AND zu_bf_lbkum       = 0
*      AND zu_bf_salk3       = 0
*      AND zu_bf_rd          = 0
*      AND zu_bubs_lbkum     = 0
*      AND zu_bubs_salk3     = 0
*      AND zu_bubs_rd        = 0
*      AND zu_bubm_lbkum     = 0
*      AND zu_bubm_salk3     = 0
*      AND zu_bubm_rd        = 0
*      AND zu_vp_lbkum       = 0
*      AND zu_vp_salk3       = 0
*      AND zu_vp_rd          = 0
*      AND ab_pc_lbkum       = 0
*      AND ab_pc_salk3       = 0
*      AND ab_pc_rd          = 0
*      AND zu_etc_lbkum      = 0
*      AND zu_etc_salk3      = 0
*      AND zu_etc_rd         = 0
*      AND vn_vka16_lbkum    = 0
*      AND vn_vka16_salk3    = 0
*      AND vn_vka16_rd       = 0
*      AND vn_vka20_lbkum    = 0
*      AND vn_vka20_salk3    = 0
*      AND vn_vka20_rd       = 0
*      AND vn_vka99_lbkum    = 0
*      AND vn_vka99_salk3    = 0
*      AND vn_vka99_rd       = 0
*      AND vn_vf_lbkum       = 0
*      AND vn_vf_salk3       = 0
*      AND vn_vf_rd          = 0
*      AND vn_or66_lbkum     = 0
*      AND vn_or66_salk3     = 0
*      AND vn_or66_rd        = 0
*      AND vn_or58_lbkum     = 0
*      AND vn_or58_salk3     = 0
*      AND vn_or58_rd        = 0
*      AND vn_cc60_lbkum     = 0
*      AND vn_cc60_salk3     = 0
*      AND vn_cc60_rd        = 0
*      AND vn_cc99_lbkum     = 0
*      AND vn_cc99_salk3     = 0
*      AND vn_cc99_rd        = 0
*      AND vn_vubs_lbkum     = 0
*      AND vn_vubs_salk3     = 0
*      AND vn_vubs_rd        = 0
*      AND vn_vubm_lbkum     = 0
*      AND vn_vubm_salk3     = 0
*      AND vn_vubm_rd        = 0
*      AND eb_lbkum          = 0
*      AND eb_lbkum          = 0
*      AND eb_salk3          = 0
*      AND eb_rd             = 0.
*ENDFORM.                    " delete_blank_records
**&---------------------------------------------------------------------
**
**&      Form  cal_cumulative_ending
**&---------------------------------------------------------------------
**
*FORM cal_cumulative_ending.
** Cumulative Inventory = Sum of Begining inventory + Receipts +
**                        not distributed.
*  LOOP AT it_ztco_mlit.
*
**Cumulative Qty
*    it_ztco_mlit-kb_lbkum = it_ztco_mlit-ab_lbkum
*                        + it_ztco_mlit-zu_lbkum + it_ztco_mlit-zo_lbkum
*                        + it_ztco_mlit-bu_lbkum + it_ztco_mlit-bm_lbkum
*                        + it_ztco_mlit-zv_lbkum
*                        + it_ztco_mlit-nd_lbkum.
**Cumulative Value (PC -> AB)
*    it_ztco_mlit-kb_salk3 =  it_ztco_mlit-ab_salk3
*                        + it_ztco_mlit-pc_salk3
*                        + it_ztco_mlit-zu_salk3 + it_ztco_mlit-zo_salk3
*                        + it_ztco_mlit-zv_salk3
*                        + it_ztco_mlit-bu_salk3 + it_ztco_mlit-bm_salk3
*.
*
*
*
**Cumulative Price Difference (PC -> AB)
*    it_ztco_mlit-kb_rd =   it_ztco_mlit-ab_rd  + it_ztco_mlit-pc_rd
*                     + it_ztco_mlit-zu_rd  + it_ztco_mlit-zo_rd
*                     + it_ztco_mlit-zv_rd
*                     + it_ztco_mlit-bu_rd  + it_ztco_mlit-bm_rd
*                     + it_ztco_mlit-vp_rd
*                     + it_ztco_mlit-nd_rd.
*
** Ending.
*    it_ztco_mlit-eb_lbkum = it_ztco_mlit-kb_lbkum
*                        - it_ztco_mlit-vn_lbkum
*                        - it_ztco_mlit-vo_lbkum - it_ztco_mlit-vk_lbkum
*                        - it_ztco_mlit-vv_lbkum
*                        - it_ztco_mlit-vu_lbkum - it_ztco_mlit-vm_lbkum
*.
** Ending Amt.
*    it_ztco_mlit-eb_salk3 =  it_ztco_mlit-kb_salk3
*                        - it_ztco_mlit-vn_salk3
*                        - it_ztco_mlit-vo_salk3 - it_ztco_mlit-vk_salk3
*                        - it_ztco_mlit-vv_salk3
*                        - it_ztco_mlit-vu_salk3 - it_ztco_mlit-vm_salk3
*.
** Ending - RD
*    it_ztco_mlit-eb_rd    = it_ztco_mlit-kb_rd
*                        - it_ztco_mlit-vn_rd
*                        - it_ztco_mlit-vo_rd - it_ztco_mlit-vk_rd
*                        - it_ztco_mlit-vv_rd
*                        - it_ztco_mlit-vu_rd - it_ztco_mlit-vm_rd
*                        - it_ztco_mlit-ni_rd.
*
*    MODIFY it_ztco_mlit.
*  ENDLOOP.
*
*ENDFORM.                    " cal_cumulative_ending
**&---------------------------------------------------------------------
**
**&      Form  read_prev_pst_var2
**&---------------------------------------------------------------------
**
*FORM read_prev_pst_var2.
*
*  DATA: BEGIN OF lt_mats_prev OCCURS 0,
*          kalnr LIKE ckmlcr-kalnr,
*          salk3 LIKE ckmlcr-salk3,
*        END OF lt_mats_prev,
*        l_pryr LIKE mlcd_key-bdatj,
*        l_prmn LIKE mlcd_key-poper.
*
*  l_prmn = p_poper - 1.
*  IF l_prmn = 0.
*    l_prmn = 12.
*    l_pryr = p_bdatj - 1.
*  ELSE.
*    l_pryr = p_bdatj.
*  ENDIF.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_mats_prev
*      FROM ckmlcr
*      FOR ALL ENTRIES IN t_mats
*      WHERE kalnr = t_mats-kalnr
*        AND bdatj = l_pryr
*        AND poper = l_prmn.
*  SORT lt_mats_prev BY kalnr.
*
*  DATA: l_diff  LIKE ckmlcr-salk3,
*        w_mlsum LIKE it_ztco_mlit.
*  LOOP AT it_ztco_mlit INTO w_mlsum.
*    READ TABLE lt_mats_prev WITH KEY kalnr = w_mlsum-kalnr
*         BINARY SEARCH.
*    IF sy-subrc = 0.
*      l_diff = lt_mats_prev-salk3 - w_mlsum-ab_salk3.
**     - w_mlsum-pc_salk3.
*
*      it_ztco_mlit-kalnr = w_mlsum-kalnr.
*      it_ztco_mlit-ab_salk3 = + l_diff.
*      it_ztco_mlit-ab_rd    = - l_diff.
*      it_ztco_mlit-pc_salk3 = - l_diff.
*      it_ztco_mlit-pc_rd    = + l_diff.
*      COLLECT it_ztco_mlit.
*    ENDIF.
*  ENDLOOP.
*
**zero inventory... clear (simplified method)
*  LOOP AT it_ztco_mlit WHERE ab_lbkum = 0.
*    it_ztco_mlit-ab_salk3 = it_ztco_mlit-ab_salk3 +
*it_ztco_mlit-pc_salk3.
*    it_ztco_mlit-pc_salk3 = 0.
*    MODIFY it_ztco_mlit INDEX sy-tabix.
*  ENDLOOP.
*
*ENDFORM.                    " read_prev_pst_var2
**&---------------------------------------------------------------------
**
**&      Form  delete_zero_record
**&---------------------------------------------------------------------
**
*FORM delete_zero_record.
** Exclude records which have zero values
*  DELETE it_ztco_mlit WHERE ab_lbkum EQ 0 AND
*                          ab_salk3 EQ 0 AND
*                          ab_rd EQ 0    AND
*                          zu_lbkum EQ 0 AND
*                          zu_salk3 EQ 0 AND
*                          zu_rd EQ 0    AND
*                          vn_lbkum EQ 0 AND
*                          vn_salk3 EQ 0 AND
*                          vn_rd EQ 0    AND
*                          eb_lbkum EQ 0 AND
*                          eb_salk3 EQ 0 AND
*                          eb_rd EQ 0    AND
*                          bu_lbkum EQ 0 AND
*                          bu_salk3 EQ 0 AND
*                          bu_rd EQ 0    AND
*                          bm_lbkum EQ 0 AND
*                          bm_salk3 EQ 0 AND
*                          bm_rd EQ 0    AND
*                          vu_lbkum EQ 0 AND
*                          vu_salk3 EQ 0 AND
*                          vu_rd EQ 0    AND
*                          vm_lbkum EQ 0 AND
*                          vm_salk3 EQ 0 AND
*                          vm_rd EQ 0    AND
*                          pc_lbkum EQ 0 AND
*                          pc_salk3 EQ 0 AND
*                          pc_rd EQ 0    AND
*                          nd_lbkum EQ 0 AND
*                          nd_salk3 EQ 0 AND
*                          nd_rd EQ 0    AND
*                          kb_lbkum EQ 0 AND
*                          kb_salk3 EQ 0 AND
*                          kb_rd EQ 0    AND
*                          ni_lbkum EQ 0 AND
*                          ni_salk3 EQ 0 AND
*                          ni_rd EQ 0    .
*ENDFORM.                    " delete_zero_record
**&---------------------------------------------------------------------
**
**&      Form  get_proc_kalnr
**&---------------------------------------------------------------------
**
*form get_proc_kalnr.
**for all entries???
*
*  SELECT werks matnr bwtar proc_kalnr btyp kalnr
*     INTO TABLE i_proc_kalnr
*     FROM ckmlmv001
*     FOR ALL ENTRIES IN t_mats
*     WHERE bwkey = t_mats-bwkey
*       AND matnr = t_mats-matnr
*       AND bwtar = t_mats-bwtar.
*
*  SORT i_proc_kalnr BY prock. "matnr bwkey bwtar.
*
*
*endform.                    " get_proc_kalnr
