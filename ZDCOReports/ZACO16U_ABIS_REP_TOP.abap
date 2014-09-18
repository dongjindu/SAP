*----------------------------------------------------------------------*
*   INCLUDE ZACO16U_ABIS_REP_TOP                                       *
*----------------------------------------------------------------------*
TABLES : mkpf, mara, mseg, aufk.


DATA : BEGIN OF it_ckmlhd OCCURS 0,
         kalnr   LIKE ckmlhd-kalnr,
         matnr   LIKE ckmlhd-matnr,
         f_objnr LIKE cpzp-f_objnr,
         bwkey   LIKE ckmlhd-bwkey,
       END OF it_ckmlhd.

DATA : BEGIN OF it_cpzp OCCURS 0,
         objnr   LIKE cpzp-objnr,
         f_objnr LIKE cpzp-f_objnr,
         varmn   LIKE cpzp-varmn,
         budat   type datum,
       END OF it_cpzp.

DATA: BEGIN OF lt_pcc OCCURS 0,
        objnr       LIKE aufk-objnr,
        aufnr       LIKE aufk-aufnr,
        prwrk       LIKE ckmlmv013-prwrk,
        pmatn       LIKE ckmlmv013-pmatn,
        verid       LIKE ckmlmv013-verid,
      END OF lt_pcc.

DATA : BEGIN OF it_fsc_mat OCCURS 100,
         aufnr     LIKE lt_pcc-aufnr,
         objnr     LIKE lt_pcc-objnr,
         verid     LIKE lt_pcc-verid,
         werks     LIKE lt_pcc-prwrk,
         matnr     LIKE lt_pcc-pmatn,
       END OF it_fsc_mat.


DATA : BEGIN OF it_covp OCCURS 1000,
       objnr	 LIKE covp-objnr,
       kstar   like covp-kstar,
       werks	 LIKE covp-werks,
       matnr	 LIKE covp-matnr,
       budat   LIKE covp-budat,
       vrgng	 LIKE covp-vrgng,    "CO business transaction
       meinb	 LIKE covp-meinb,    "Unit of measure
       mbgbtr	 LIKE covp-mbgbtr,   "Qty
       sgtxt   LIKE coep-sgtxt,    "To get material info
      END OF  it_covp.

DATA : BEGIN OF it_coep OCCURS 1000,
       objnr	 LIKE covp-objnr,
       matnr	 LIKE covp-matnr,
       budat   LIKE covp-budat,
       meinb	 LIKE covp-meinb,    "Unit of measure
       mbgbtr	 LIKE covp-mbgbtr,   "Qty
      END OF  it_coep.


*DATA : it_coep LIKE it_covp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_categ OCCURS 0,
         objnr LIKE covp-objnr,
         kstar LIKE covp-kstar,
         vrgng LIKE covp-vrgng,
         beweg LIKE kkbcs_out-beweg,
       END OF it_categ.

DATA : BEGIN OF it_abispost OCCURS 0,
         werks     LIKE ztco_abispost-werks,
         matnr     LIKE ztco_abispost-matnr,
         stype     LIKE ztco_abispost-stype,
         mbgbtr    LIKE ztco_abispost-mbgbtr,
         meinb     LIKE ztco_abispost-meinb,
         fsc_matnr LIKE ztco_abispost-fsc_matnr,
         budat     type datum,
       END OF it_abispost.


DATA : BEGIN OF it_mseg OCCURS 0,
         matnr LIKE mseg-matnr,
         werks LIKE mseg-werks,
         lgort LIKE mseg-lgort,
         bwart LIKE mseg-bwart,
         menge LIKE mseg-menge,
         budat like mkpf-budat,
         usnam LIKE mkpf-usnam,
       END OF it_mseg.


DATA : BEGIN OF it_pcc OCCURS 0,
         llv_matnr LIKE mseg-matnr,
         budat     TYPE datum,
         aufnr     LIKE aufk-aufnr,
         artnr     LIKE mara-matnr,
         verid     LIKE ckmlmv013-verid,
         mbgbtr    LIKE coep-mbgbtr,
         mbgbtr2   LIKE coep-mbgbtr,
         osnd      LIKE coep-mbgbtr,
         keyin     LIKE coep-mbgbtr,
         usnam     LIKE covp-usnam,
       END OF it_pcc.

DATA : BEGIN OF it_goodsmv OCCURS 0,
         llv_matnr LIKE mseg-matnr,
         budat     TYPE datum,
         scrap     LIKE mseg-menge,
         x551      LIKE mseg-menge,
         x905      LIKE mseg-menge,
         trf       LIKE mseg-menge,
         usnam     LIKE mkpf-usnam,
       END OF it_goodsmv.

DATA : BEGIN OF IT_MARA OCCURS 0,
         MATNR     LIKE mARA-matnr,
         maktx     LIKE makt-maktx,
       END OF IT_MARA.

DATA : BEGIN OF it_display OCCURS 0,
         type(1),
         llv_matnr LIKE mseg-matnr,
         maktx     LIKE makt-maktx,
         budat     TYPE datum,
         aufnr     LIKE aufk-aufnr,
         artnr     LIKE mara-matnr,
         verid     LIKE ckmlmv013-verid,
         mbgbtr    LIKE COEP-MBGBTR,
         mbgbtr2   LIKE COEP-MBGBTR,
         osnd      LIKE COEP-MBGBTR,
         keyin     LIKE COEP-MBGBTR,
         scrap     LIKE COEP-MBGBTR,
         x551      LIKE COEP-MBGBTR,
         x905      LIKE COEP-MBGBTR,
         trf       LIKE COEP-MBGBTR,
      END OF it_display.


DATA : g_versn(3) TYPE n,
       g_last_date type datum.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV
