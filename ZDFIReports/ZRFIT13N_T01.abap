*&---------------------------------------------------------------------*
*&  Include           ZRFIT13N_T01
*&---------------------------------------------------------------------*


************************************************************************
*     DATA DECLARATION
************************************************************************
TABLES: ztfi_map3, knb1, pbim, pbed, a005, konp, t052, knvv, mara, mbew,
        t025, a004, a307, ekpo, ekkn,
        plaf, eina, eine, lfb1, lfa1, lfm2, eban.
TABLES: fdes.

DATA : BEGIN OF it_kun OCCURS 0.
        INCLUDE STRUCTURE ztfi_map3.
DATA : zterm LIKE knb1-zterm,
       matnr LIKE pbim-matnr,
       pdatu LIKE pbed-pdatu,  "finish date
       plnmg LIKE pbed-plnmg,  "planned qty.
       netpr LIKE konp-kbetr,  "net price
       kbetr LIKE konp-kbetr,  "sales amount
       waers LIKE eine-waers,  "currency
       actyp(1),               "account type

       END OF it_kun.

DATA : BEGIN OF it_mat OCCURS 0,
       matnr LIKE pbim-matnr,
       kunnr LIKE a005-kunnr,
       pdatu LIKE pbed-pdatu,  "finish date
       plnmg LIKE pbed-plnmg,  "planned qty.
       kbetr LIKE konp-kbetr,  "sales price
       waers LIKE konp-konwa,  "currency
       pbdnr LIKE pbim-pbdnr,
       entlu LIKE pbed-entlu,  "day/week
       END OF it_mat.

DATA : BEGIN OF it_matpl OCCURS 0,
         matnr LIKE mbew-matnr,
         bwkey LIKE mbew-bwkey,
       END OF it_matpl.

DATA : BEGIN OF it_plaf OCCURS 0,
       matnr LIKE pbim-matnr,
       waers LIKE konp-konwa,  "currency
       flief LIKE plaf-flief,  "fixed vendor
       konnr LIKE plaf-konnr,  "agreement
*      psttr LIKE plaf-psttr,  "start date
       pedtr LIKE plaf-pedtr,  "finish date
       gsmng LIKE plaf-gsmng,  "order qty
       kbetr LIKE konp-kbetr,  "price (info)
*      plnum LIKE plaf-plnum,  "planned sales order
       END OF it_plaf.

DATA : BEGIN OF wa052,
        amon LIKE t052-zmona,
        fday LIKE t052-zfael,
       END OF wa052.

DATA : BEGIN OF it_alv OCCURS 0,
       matnr LIKE pbim-matnr,
       actyp(1),               "account type
       kunnr LIKE a005-kunnr,  "customer/vendor
       grupp LIKE fdes-grupp,  "planning group
       pdatu LIKE pbed-pdatu,  "origin date
       datum LIKE fdes-datum,  "plan date
       plnmg LIKE pbed-plnmg,  "qty
       netpr LIKE konp-kbetr,  "net price
       kbetr LIKE konp-kbetr,  "amount
       waers LIKE eine-waers,  "currency
       dmshb LIKE fdes-dmshb,
       dsart LIKE fdes-dsart,
* error => 0: ok, 1: Plan Group missing, 2: amount is zero, 9: other
       stts(1),
       END OF it_alv.

*DATA : BEGIN OF it_ylist OCCURS 0,
*       perbl(7) TYPE c,                  "year/month
*       kndnr    LIKE ce2h201-kndnr,      "customer
*       absmg001 LIKE ce2h201-absmg001,   "sales qty
*       erlos001 LIKE ce2h201-erlos001,   "amount(revenue)
*       END OF it_ylist.

DATA: it_dele LIKE fdes        OCCURS 0 WITH HEADER LINE.
DATA: wk_dele LIKE fdes        OCCURS 0 WITH HEADER LINE,
      wk_fdes LIKE fdes_import OCCURS 0 WITH HEADER LINE.
DATA: i_t025 LIKE t025 OCCURS 0 WITH HEADER LINE.

DATA: sv_waers LIKE t001-waers,
      sv_land1 LIKE t001-land1.

RANGES: rv_vkorg FOR tvko-vkorg.

CONSTANTS : c_sftyp(2) VALUE 'SF',
            c_mftyp(2) VALUE 'MF',    "Planning Type for CM
            c_refer(4) VALUE 'COPA'.

DATA:    g_seqfr LIKE sy-datum,   "seq from date
         g_seqto LIKE sy-datum,   "seq to date
         g_mrpfr LIKE sy-datum,   "mrp to date
         g_mrpto LIKE sy-datum.   "mrp to date

RANGES : r_datum FOR sy-datum,     "total
         r_ltpdt FOR sy-datum,     "ltp
         r_seqdt FOR sy-datum,     "ltp
         r_matnr FOR plaf-matnr,   "LP or space
         r_bwkey FOR mbew-bwkey,   "Plant
         r_kdmat FOR plaf-matnr.   "KD

DATA: BEGIN OF i_matnr OCCURS 0,
        matnr  LIKE mara-matnr,
        kkref  LIKE t025-kkref,
        bklas  LIKE t025-bklas,
      END OF i_matnr.

*MM Info Record
DATA: BEGIN OF xeina OCCURS 0.
        INCLUDE STRUCTURE eina.
DATA: END OF xeina.
DATA: BEGIN OF xeine OCCURS 0.
        INCLUDE STRUCTURE eine.
DATA: END OF xeine.
DATA: BEGIN OF xeipa OCCURS 0.
        INCLUDE STRUCTURE eipa.
DATA: END OF xeipa.


* USING ALV REPORTING..
TYPE-POOLS : slis.

INCLUDE rvreuse_global_data.
INCLUDE rvreuse_local_data.
INCLUDE rvreuse_forms.

DATA : gs_layout    TYPE slis_layout_alv,
       gt_fieldcat  TYPE slis_t_fieldcat_alv,
       gt_field     TYPE slis_t_fieldcat_alv,
       g_fieldcat_s TYPE slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    TYPE slis_t_event,
       ls_sort      TYPE slis_sortinfo_alv,
       gt_sort      TYPE slis_t_sortinfo_alv,
       g_save(1)    TYPE c,
       g_exit(1)    TYPE c,
       gx_variant   LIKE disvariant,
       g_variant    LIKE disvariant,
       g_repid      LIKE sy-repid,
       g_cnt(2)     TYPE n.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.

************************************************************************
* SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  MEMORY ID buk
                                       default 'H201'.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_gjahr(4) TYPE n DEFAULT sy-datum(4)   NO-DISPLAY,
            p_month(2) TYPE n DEFAULT sy-datum+4(2) NO-DISPLAY,
            p_perid(2) TYPE n DEFAULT '04'          OBLIGATORY,
            p_plscn LIKE rm61r-plscn  DEFAULT '900' NO-DISPLAY,
            p_versb LIKE pbim-versb   DEFAULT '99'  NO-DISPLAY,
            p_veras LIKE pbim-versb   DEFAULT 'AS'  NO-DISPLAY.

*           p_versi LIKE ce3h201-versi DEFAULT '311' OBLIGATORY.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : p_mhly RADIOBUTTON GROUP rd DEFAULT 'X' USER-COMMAND exe.
*SELECTION-SCREEN COMMENT 5(26) text-002.
*PARAMETERS : p_yrly RADIOBUTTON GROUP rd.
*SELECTION-SCREEN COMMENT 36(36) text-003.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_cust AS CHECKBOX DEFAULT ' ',  "sales plan
            p_vend AS CHECKBOX DEFAULT 'X'.  "procurement plan

PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',
            p_detail AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN skip 1.
SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-004.
PARAMETER :      p_profl LIKE mara-profl DEFAULT 'L' NO-DISPLAY.
SELECT-OPTIONS : s_matnr FOR plaf-matnr.
SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN skip 1.
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-010.
PARAMETERS: p_dayc LIKE v_t052-ztagg DEFAULT '07' OBLIGATORY,
            p_dayv LIKE v_t052-ztagg DEFAULT '30' OBLIGATORY.


*SELECTION-SCREEN COMMENT /2(50) text-100 MODIF ID txt. "Status
*SELECTION-SCREEN COMMENT /2(50) text-101.
*SELECTION-SCREEN COMMENT /2(50) text-102.
*SELECTION-SCREEN COMMENT /2(50) text-103.
*SELECTION-SCREEN COMMENT /2(50) text-104.
SELECTION-SCREEN COMMENT /2(50) text-110 MODIF ID txt.  "Plan Version
SELECTION-SCREEN COMMENT /2(50) text-111.
SELECTION-SCREEN COMMENT /2(50) text-112.
*SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl1.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN OUTPUT.
*  IF p_yrly EQ 'X'.
*    LOOP AT SCREEN.
*      IF screen-name EQ 'P_MONTH'.
*        p_month = '12'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  LOOP AT SCREEN.
    IF screen-name EQ 'P_VERSI'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*  LOOP AT SCREEN.
*    IF screen-group1 = 'TXT'.
*      screen-intensified = '1'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
