*&---------------------------------------------------------------------*
*&  Include           ZCFIT03_T01
*&---------------------------------------------------------------------*


**********************************************************************
* Data Declaration
**********************************************************************
TABLES : fdsr, fdsb, fdes, fdt1, t036, t038, t001,
         ztfi_cmap, ztfi_cmaf, ztfi_des, ztfi_dss.
TABLES : t038v, t035z.

*DATA: BEGIN OF it_src OCCURS 0.
*        INCLUDE STRUCTURE fdes.
*DATA: orign LIKE t036-orign,         "..Original Source
*      o_ebe LIKE fdes-ebene.         "..Original plan level
*
*DATA: END OF it_src.
*DATA : itmp02 LIKE it_src OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_plan OCCURS 0, "target data
       torgn    LIKE t036-orign,  "data source
       tgrpp    LIKE fdes-grupp,  "group
       teben    LIKE fdes-ebene,  "level
       tdate    LIKE sy-datum,    "date
       tbukr    LIKE fdes-bukrs,  "company
       tdspw    LIKE fdes-dispw,  "currency
       tdamt    LIKE glt0-hslvt,  "document amount
       tlamt    LIKE glt0-hslvt,  "local amount
       gsber    LIKE fdes-gsber,  "b/a
       dsart    LIKE fdes-dsart,  "planning type
       tweek(6) TYPE n,           "week
       END OF it_plan.
DATA : BEGIN OF it_src OCCURS 0, "source list
       sorgn    LIKE t036-orign,  "data source
       sgrpp    LIKE fdes-grupp,  "group
       seben    LIKE fdes-ebene,  "level
       sdate    LIKE sy-datum,    "date
       sbukr    LIKE fdes-bukrs,  "company
       sdspw    LIKE fdes-dispw,  "currency
       sdamt    LIKE glt0-hslvt,  "document amount
       slamt    LIKE glt0-hslvt,  "local amount
       gsber    LIKE fdes-gsber,  "b/a
       dsart    LIKE fdes-dsart,  "planning type
       bnkko    LIKE fdes-bnkko,  "dup. check
       END OF it_src.
DATA : it_dst LIKE it_src  OCCURS 0 WITH HEADER LINE.  "day distribution
DATA : it_tgt LIKE it_plan OCCURS 0 WITH HEADER LINE.  "target log list
DATA : *isrc  LIKE it_src  OCCURS 0 WITH HEADER LINE,  "fdes dup. check
       *ifdt1 LIKE it_src  OCCURS 0 WITH HEADER LINE.  "fdt1 dup. check

DATA : itmp01 LIKE it_plan OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF i_data OCCURS 0,               "..Month Table
      mark,                                   "mark january
      count(02) TYPE n,
      gjahr     LIKE ztfi_cmal-gjahr,
      datum     LIKE sy-datum,
      END OF i_data.

DATA: BEGIN OF i_vtb OCCURS 0,   "flow type
      bukrs   LIKE vtbfhapo-bukrs,
      rfha    LIKE vtbfhapo-rfha,
      rfhazu  LIKE vtbfhapo-rfhazu,
      dcrdat  LIKE vtbfhapo-dcrdat,
      tcrtim  LIKE vtbfhapo-tcrtim,
      rfhazb  LIKE vtbfhapo-rfhazb,
      sfhazba LIKE vtbfhapo-sfhazba,
      rantyp  LIKE vtbfhapo-rantyp,
      ssign   LIKE vtbfhapo-ssign,
      wzbetr  LIKE vtbfhapo-wzbetr,
      END OF i_vtb.
DATA : iztfi_cmap LIKE ztfi_cmap OCCURS 0 WITH HEADER LINE.
DATA : i_cmaf LIKE ztfi_cmaf OCCURS 0 WITH HEADER LINE.
DATA : iztfi_cmad LIKE ztfi_cmad OCCURS 0 WITH HEADER LINE.
DATA : i_map2 LIKE ztfi_map2 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF isum,
         asum1 LIKE glt0-hslvt,
         bsum1 LIKE glt0-hslvt,
         asum2 LIKE glt0-hslvt,
         bsum2 LIKE glt0-hslvt,
         asum3 LIKE glt0-hslvt,
         bsum3 LIKE glt0-hslvt,
       END OF isum.
DATA : BEGIN OF isum2,
         hslvt_r  LIKE glt0-hslvt,
         hslvt_rr LIKE glt0-hslvt,
       END OF isum2.
DATA : isum3 LIKE isum2.

DATA: BEGIN OF it_zdes OCCURS 0.
        INCLUDE STRUCTURE ztfi_des.
DATA: gubun.                           "create mode(Y/N)
DATA: END OF it_zdes.
DATA: dt_zdes LIKE it_zdes OCCURS 0 WITH HEADER LINE.
DATA: it_zdss LIKE ztfi_dss OCCURS 0 WITH HEADER LINE.
DATA: ut_zdss LIKE ztfi_dss OCCURS 0 WITH HEADER LINE.

*..FOR WORK
DATA: it_dele LIKE fdes        OCCURS 0 WITH HEADER LINE.
DATA: wk_dele LIKE fdes        OCCURS 0 WITH HEADER LINE,
      wk_fdes LIKE fdes_import OCCURS 0 WITH HEADER LINE.

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
       it_sort      TYPE slis_t_sortinfo_alv,
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

DATA : sv_gubun(20) TYPE c,
       sv_gubun2(20) TYPE c,
       sv_textl  LIKE t035t-textl,
       sv_hslvt  LIKE glt0-hslvt,
       sv_hslvt1 LIKE glt0-hslvt,
       sv_hslvt2 LIKE glt0-hslvt,
       sv_hslvt3 LIKE glt0-hslvt,
       sv_spras  LIKE t001-spras,
       sv_%hslvt(10), " TYPE p DECIMALS 2,
       sv_trunctxt(15),
       sv_rollym(6).

DATA : gv_fdsbsum LIKE glt0-hslvt,
       l_cycle_date LIKE sy-datum.

FIELD-SYMBOLS: <fs_wtp>, <fs_wlp>.
DATA : fs_wtp(20),
       fs_wlp(20),
       idx(2) TYPE n.

*..Working Info.
RANGES: r_wrk_fdsb  FOR  t036-ebene,     "Planning Level
        r_wrk_fdsr  FOR  t036-ebene,

        r_ftype     FOR  vtbfhapo-sfhazba,
        r_perid     FOR  sy-datum.

RANGES: $grupp  FOR ztfi_cmal-grupp,
        $group  FOR ztfi_cmal-ebene,            "..Grouping.Level
        $group2 FOR ztfi_cmal-grupp,            "..Grouping.Group
        $datum FOR sy-datum.

CONSTANTS: c_levl1 TYPE fdes-ebene VALUE 'BT'.
CONSTANTS: h_gubun(20)   VALUE  '                    ',
           h_gubun2(20)  VALUE  '                    ',
           h_group(10)   VALUE  'Pln. Grp',
           h_textl(30)   VALUE  'Description',
           h_dmshb(21)   VALUE  'Amount',
           h_dmshb1(21)  VALUE  'Accu. Amt.',
           h_%dmshb(10)  VALUE  '%',
           h_head(118)   VALUE   'Cash Flow Report',
           h_headline(118) VALUE '==========================='.

CONSTANTS: c_cate1(2) VALUE 'A',  "Available cash bal.
           c_cate2(2) VALUE 'A1', "Beg. Bal.
           c_cate3(2) VALUE 'A2', "End. Book
           c_cate4(2) VALUE 'A3', "Bank clg.
           c_cate5(2) VALUE 'A4', "End. Bank
           c_fdsr1 TYPE t036-orign VALUE 'PSK',
           c_fdsr2 TYPE t036-orign VALUE 'SDF',
           c_fdsr3 TYPE t036-orign VALUE 'MMF',
           c_fdsb1 TYPE t036-orign VALUE 'BNK',
           c_fdes1 TYPE t036-orign VALUE 'V1',
           c_fdes2 TYPE t036-orign VALUE 'V2',
           c_fdes3 TYPE t036-orign VALUE 'V3',

           c_levlb(2) VALUE 'B+',
           c_levlc(2) VALUE 'C+',
           c_levlw(2) VALUE 'W+',
           c_levld(2) VALUE 'D+',
           c_f0type(2) VALUE 'F0',
           c_f1type(2) VALUE 'F1',
           c_f2type(2) VALUE 'F2'.

**********************************************************************
*  SELECTION-SCREEN
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_test  AS CHECKBOX DEFAULT 'X',
             p_log   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  MEMORY ID buk.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.


PARAMETERS : p_gjahr LIKE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4),
             p_month(2) TYPE n OBLIGATORY DEFAULT sy-datum+4(2),
             p_perid(2) TYPE n OBLIGATORY DEFAULT '04',
             p_glied LIKE t038-glied DEFAULT 'P1' OBLIGATORY,
             p_kurst LIKE tcurv-kurst OBLIGATORY DEFAULT 'M'.

SELECTION-SCREEN skip 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_freez RADIOBUTTON GROUP rd DEFAULT 'X'.
SELECTION-SCREEN COMMENT 5(25) text-004.
PARAMETERS : p_final RADIOBUTTON GROUP rd.
SELECTION-SCREEN COMMENT 35(36) text-005.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b2.
***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
AT SELECTION-SCREEN.
  IF p_month < 1 OR p_month > 12.
    MESSAGE e002 WITH 'Month' p_month.
  ENDIF.

  IF p_test EQ 'X' AND p_log EQ space.
    MESSAGE w008. STOP.
  ELSEIF p_test EQ space AND p_log EQ 'X'.

  ENDIF.

AT SELECTION-SCREEN ON p_kurst.
  PERFORM select_data_tcurv.
