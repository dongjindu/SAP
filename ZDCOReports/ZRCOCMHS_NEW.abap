*&--------------------------------------------------------------------&*
*&    Program: ZRCOCMHS.                                              &*
*&    Author : Andy Choi                                              &*
*&    Specification: Calculate Overhead cost per Man hour per shop.   &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 09/22/2006  HS Jung  UD1K914019      initial program.
*& 11/39/2007  IG MOON  UD1K942291      Add period column at ALV
*  06/18/2013  T00303   UD1K957405      U1: Apply Archiving
*&--------------------------------------------------------------------&*
REPORT zrcocmhs_new LINE-SIZE 230.

*----------------------------------------------------------------------*
*   INCLUDE ZRCOCMHS_NEW_TOP                                           *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES : csks, cokl,keph, cosp, coss.

DATA : it_coss_temp LIKE coss OCCURS 0 WITH HEADER LINE.
DATA : st_coss      LIKE it_coss_temp.

RANGES: r_semi    FOR  csks-kostl,
        r_indi    FOR  csks-kostl.

DATA : BEGIN OF it_coss OCCURS 0,
       type(1),
       lstar TYPE lstar,
       kostl TYPE kostl,
       par_kostl TYPE kostl.
        INCLUDE STRUCTURE it_coss_temp.
DATA : END OF it_coss.

DATA : it_coss_semi  LIKE it_coss OCCURS 0 WITH HEADER LINE.
DATA : it_coss_psemi LIKE it_coss OCCURS 0 WITH HEADER LINE. "PLAN???

DATA : BEGIN OF it_sum_psemi OCCURS 0,
        objnr LIKE it_coss-objnr,
        kstar LIKE it_coss-kstar,
       END OF it_sum_psemi.

DATA : it_cosp      LIKE cosp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF wa_result,
        wrttp       LIKE cosp-wrttp,
        wrtxt(6)    TYPE c,
        shop        TYPE zzshop,
        kostl       LIKE cssl-kostl,
*       lstar       LIKE cssl-lstar,
        kstar       TYPE kstar,
        par_kostl   TYPE kostl,
        meinh       TYPE meinh,
* ig.moon 11/30 {
        perio	      TYPE co_perio,
* }
        elemt       LIKE plobjcom-elemt,
        txele       LIKE tckh1-txele,
        incst       LIKE coss-wkg001,
        smcst       LIKE coss-wkg001,
        dicst       LIKE coss-wkg001,
        tocst       LIKE coss-wkg001,
        manhr_qty   TYPE coss-meg001,

        incst_hr    LIKE coss-wkg001,
        smcst_hr    LIKE coss-wkg001,
        dicst_hr    LIKE coss-wkg001,
        tocst_hr    LIKE coss-wkg001,

        incst_f     LIKE coss-wkg001,
        smcst_f     LIKE coss-wkg001,
        dicst_f     LIKE coss-wkg001,
        tocst_f     LIKE coss-wkg001,

        incst_f_hr  LIKE coss-wkg001,
        smcst_f_hr  LIKE coss-wkg001,
        dicst_f_hr  LIKE coss-wkg001,
        tocst_f_hr  LIKE coss-wkg001,

      END OF wa_result.

DATA : it_coss_cc LIKE wa_result OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_coss_cc_semi OCCURS 0 ,
        wrttp LIKE cosp-wrttp,
        wrtxt(6) TYPE c,
        perbl(3) TYPE n,
        kostl LIKE cssl-kostl,
        lstar LIKE cssl-lstar,
        kstar LIKE coss-kstar,
        uspob LIKE it_coss_semi-objnr,
        objnr LIKE it_coss_semi-objnr,
        txele LIKE tckh1-txele,
        smqty   LIKE coss-meg001,
        smqty_f LIKE coss-meg001,

        wkg00   LIKE coss-wkg001,
        wkg00_f LIKE coss-wkg001,
      END OF it_coss_cc_semi.


DATA : it_coss_cc_psemi LIKE it_coss_cc_semi OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_cskb  OCCURS 0,
         kstar LIKE cskb-kstar,
         katyp LIKE cskb-katyp,
       END OF it_cskb.

DATA : BEGIN OF it_ztco_mha OCCURS 0,
         kostl LIKE ztco_mha-kostl,
         anzhl LIKE ztco_mha-anzhl,
       END OF it_ztco_mha.

DATA : BEGIN OF itab OCCURS 0,
        kostl    LIKE csks-kostl,
        lstar    LIKE csla-lstar,
        act_unit TYPE co_meinh_l,
        from_per LIKE cobk-perab,
        to_per   LIKE cobk-perab,
        w000     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w010     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w020     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w030     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w040     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w050     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w060     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w070     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w080     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w090     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w100     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w110     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w120     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w130     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w140     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w150     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w160     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w170     TYPE kstbmt, "LIKE kkb_split-w000 ,
        w000_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w010_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w020_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w030_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w040_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w050_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w060_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w070_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w080_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w090_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w100_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w110_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w120_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w130_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w140_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w150_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w160_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
        w170_f   TYPE kstbmt, "LIKE kkb_split-w000 ,
END OF itab.
DATA: i_cosl LIKE cosl OCCURS 0 WITH HEADER LINE.


DATA : gt_result LIKE wa_result OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_kstar OCCURS 0,
       kstar LIKE coss-kstar,
       elemt LIKE ztco_shop_cc-elemt,
       END OF it_kstar.

DATA: BEGIN OF it_tckh2 OCCURS 0,
        ktopl LIKE tckh2-ktopl,
        elehk LIKE tckh2-elehk,
        kstav LIKE tckh2-kstav,
        kstab LIKE tckh2-kstab,
        elemt LIKE tckh2-elemt,
        txele LIKE tckh1-txele,
      END OF it_tckh2.


DATA : BEGIN OF it_coomco OCCURS 0.
        INCLUDE STRUCTURE coomco.
DATA :  poper    LIKE ccss-buper,
        from_per LIKE cobk-perab,
        to_per   LIKE cobk-perab,
        kostl    LIKE csks-kostl,
        lstar    LIKE csla-lstar.
DATA : END OF it_coomco.

DATA : BEGIN OF it_koat_p OCCURS 0.
DATA :  gjahr    LIKE ccss-gjahr,
        poper    LIKE ccss-buper,
        kostl    LIKE csks-kostl,
        lstar    LIKE csla-lstar,
        elemt    LIKE kkb_split-elemt,
        w000     LIKE kkb_split-w000 ,      "Over all
        w001     LIKE kkb_split-w001 ,      "Fixed
        waers    LIKE kkb_split-waers,
        total    LIKE kkb_split-w000 ,
        cp_%(16) TYPE p DECIMALS 7.
DATA : END OF it_koat_p.

DATA : BEGIN OF it_kostl_lstar_pct OCCURS 0.
DATA :  from_per LIKE cobk-perab,
        to_per   LIKE cobk-perab,
        objnr    LIKE coomco-objnr,
        kadky    LIKE sy-datum ,
        bidat    LIKE sy-datum ,
        wrttp    TYPE wrttp.
        INCLUDE STRUCTURE it_koat_p.
DATA : END OF it_kostl_lstar_pct.


RANGES: r_objnr FOR cosl-objnr.
RANGES: r_kostl FOR csks-kostl.
RANGES: r_lstar FOR csla-lstar.



*&----------- Work area
DATA: BEGIN OF wa_selfld,
        fname(20) TYPE c,
      END OF wa_selfld.
DATA:wa_plobjcom LIKE plobjcom,
     wa_return1  LIKE bapiret2,
     wa_hnodes   LIKE bapiset_hier,
     wa_hvalues  LIKE bapi1112_values.
DATA: BEGIN OF wa_group_detail,
         gname   LIKE wa_hnodes-groupname,
         valfrom LIKE wa_hvalues-valfrom,
         valto   LIKE wa_hvalues-valto,
      END OF wa_group_detail.
DATA: wa_direct_cegrp LIKE wa_group_detail,
      wa_indir_cegrp LIKE wa_group_detail,
      wa_semidir_cegrp LIKE wa_group_detail.

DATA : BEGIN OF it_actqty OCCURS 0,
        objnr  LIKE cosl-objnr,
        lst001 LIKE cosl-lst001,
        lst002 LIKE cosl-lst002,
        lst003 LIKE cosl-lst003,
        lst004 LIKE cosl-lst004,
        lst005 LIKE cosl-lst005,
        lst006 LIKE cosl-lst006,
        lst007 LIKE cosl-lst007,
        lst008 LIKE cosl-lst008,
        lst009 LIKE cosl-lst009,
        lst010 LIKE cosl-lst010,
        lst011 LIKE cosl-lst011,
        lst012 LIKE cosl-lst012,
       END OF it_actqty.

DATA: BEGIN OF it_cssl OCCURS 0,
        kostl LIKE cssl-kostl,
        lstar LIKE cssl-lstar,
        objnr LIKE cssl-objnr,
        objnr1 LIKE csks-objnr,
      END OF it_cssl.
DATA : it_cssl1 LIKE it_cssl OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_csks OCCURS 0 ,
        kokrs  LIKE csks-kokrs,
        kostl  LIKE csks-kostl,
        objnr  LIKE csks-objnr,
        objnr2 LIKE cssl-objnr,  "CC + AT
      END OF it_csks.

DATA : it_csks1 LIKE it_csks OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF wa_cosp,
       objnr LIKE cosp-objnr,
       wrttp LIKE cosp-wrttp,
       vrgng LIKE cosp-vrgng,
       kstar LIKE cosp-kstar,
       wkg001 LIKE cosp-wkg001,
       wkg002 LIKE cosp-wkg002,
       wkg003 LIKE cosp-wkg003,
       wkg004 LIKE cosp-wkg004,
       wkg005 LIKE cosp-wkg005,
       wkg006 LIKE cosp-wkg006,
       wkg007 LIKE cosp-wkg007,
       wkg008 LIKE cosp-wkg008,
       wkg009 LIKE cosp-wkg009,
       wkg010 LIKE cosp-wkg010,
       wkg011 LIKE cosp-wkg011,
       wkg012 LIKE cosp-wkg012,
       parob  LIKE coss-parob,
      END OF wa_cosp.
DATA: wa_coss LIKE wa_cosp.
DATA: BEGIN OF wa_sel_coss,
       objnr LIKE cosp-objnr,
       wrttp LIKE cosp-wrttp,
       vrgng LIKE cosp-vrgng,
       kstar LIKE cosp-kstar,
       parob  LIKE coss-parob,
       wkg001 LIKE cosp-wkg001,
       wkg002 LIKE cosp-wkg002,
       wkg003 LIKE cosp-wkg003,
       wkg004 LIKE cosp-wkg004,
       wkg005 LIKE cosp-wkg005,
       wkg006 LIKE cosp-wkg006,
       wkg007 LIKE cosp-wkg007,
       wkg008 LIKE cosp-wkg008,
       wkg009 LIKE cosp-wkg009,
       wkg010 LIKE cosp-wkg010,
       wkg011 LIKE cosp-wkg011,
       wkg012 LIKE cosp-wkg012,
      END OF wa_sel_coss.
DATA: BEGIN OF it_cosl OCCURS 0,
         objnr  LIKE cosl-objnr,
         wrttp  LIKE cosl-wrttp,
         kostl  TYPE kostl,
         lstar  TYPE lstar,
         meinh  TYPE meinh,
         lst001 LIKE cosl-lst001,
         lst002 LIKE cosl-lst002,
         lst003 LIKE cosl-lst003,
         lst004 LIKE cosl-lst004,
         lst005 LIKE cosl-lst005,
         lst006 LIKE cosl-lst006,
         lst007 LIKE cosl-lst007,
         lst008 LIKE cosl-lst008,
         lst009 LIKE cosl-lst009,
         lst010 LIKE cosl-lst010,
         lst011 LIKE cosl-lst011,
         lst012 LIKE cosl-lst012,
      END OF it_cosl.

DATA: BEGIN OF wa_coss_qty,
       objnr LIKE cosp-objnr,
       wrttp LIKE cosp-wrttp,
       vrgng LIKE cosp-vrgng,
       kstar LIKE cosp-kstar,
       parob  LIKE coss-parob,
       uspob  LIKE coss-uspob,
       meg001 LIKE coss-meg001,
       meg002 LIKE cosp-meg002,
       meg003 LIKE cosp-meg003,
       meg004 LIKE cosp-meg004,
       meg005 LIKE cosp-meg005,
       meg006 LIKE cosp-meg006,
       meg007 LIKE cosp-meg007,
       meg008 LIKE cosp-meg008,
       meg009 LIKE cosp-meg009,
       meg010 LIKE cosp-meg010,
       meg011 LIKE cosp-meg011,
       meg012 LIKE cosp-meg012,
      END OF wa_coss_qty.

DATA: BEGIN OF wa_cosl_qty,
       objnr LIKE cosp-objnr,
       wrttp LIKE cosp-wrttp,
       vrgng LIKE cosp-vrgng,
       lst001 LIKE cosl-lst001,
       lst002 LIKE cosl-lst002,
       lst003 LIKE cosl-lst003,
       lst004 LIKE cosl-lst004,
       lst005 LIKE cosl-lst005,
       lst006 LIKE cosl-lst006,
       lst007 LIKE cosl-lst007,
       lst008 LIKE cosl-lst008,
       lst009 LIKE cosl-lst009,
       lst010 LIKE cosl-lst010,
       lst011 LIKE cosl-lst011,
       lst012 LIKE cosl-lst012,
      END OF wa_cosl_qty.

DATA: BEGIN OF it_manhr_qty OCCURS 0 ,
         wrttp  LIKE cosl-wrttp,
         kostl  TYPE kostl,
         lstar  TYPE lstar,
         meinh  TYPE meinh,
         lst001 LIKE cosl-lst001,
         objnr  TYPE j_objnr,
* ig.moon 11/30 {
        perio	      TYPE co_perio,
* }
      END OF it_manhr_qty.

DATA: BEGIN OF wa_coks,
        objnr LIKE coks-objnr,
        kstar LIKE coks-kstar,
        parob LIKE coks-parob,
        kostl LIKE cssl-kostl,
       END OF wa_coks.

DATA: BEGIN OF wa_tckh2,
        ktopl LIKE tckh2-ktopl,
        elehk LIKE tckh2-elehk,
        kstav LIKE tckh2-kstav,
        kstab LIKE tckh2-kstab,
        elemt LIKE tckh2-elemt,
        txele LIKE tckh1-txele,
      END OF wa_tckh2.

DATA: BEGIN OF wa_cokl,
       kokrs LIKE cskb-kokrs,
       kostl LIKE cssl-kostl,
       kstar LIKE cskb-kstar,
       objnr LIKE cokl-objnr,
      END OF wa_cokl.

DATA: BEGIN OF wa_cost_com,
         elemt     LIKE tckh2-elemt,
         kstar     LIKE cosp-kstar,
         wrttp     LIKE cosp-wrttp,
         perbl     LIKE cosp-perbl,
         wkg001    LIKE cosp-wkg001,
         wkg001_f  LIKE cosp-wkg001,
         smqty     LIKE coss-meg001,
         kostl     LIKE cssl-kostl,     " Manju
         par_kostl TYPE kostl,
         lstar     LIKE cssl-lstar,     " Manju
      END OF wa_cost_com.
DATA: BEGIN OF wa_result1,
        gname LIKE wa_hnodes-groupname,
        elemt LIKE plobjcom-elemt,
        wrttp LIKE wa_cosp-wrttp,
        skstg LIKE plobjcom-skstg,
        kostl LIKE cssl-kostl,     " Manju
        lstar LIKE cssl-lstar,     " Manju
      END OF wa_result1.

DATA : it_cocom1   LIKE wa_plobjcom OCCURS 0 WITH HEADER LINE,
       it_cal_com  LIKE wa_plobjcom OCCURS 0 WITH HEADER LINE,
       it_semi_cc_pln LIKE wa_cost_com OCCURS 0 WITH HEADER LINE.

RANGES : r_perbl FOR cosp-perbl.

*&----------------Internal table
DATA:
      it_cocom LIKE TABLE OF wa_plobjcom,

      it_hnodes LIKE TABLE OF wa_hnodes,
      it_hvalues LIKE TABLE OF wa_hvalues WITH HEADER LINE,

      it_ceg_detail LIKE TABLE OF wa_group_detail,
      it_direct_cegrp LIKE TABLE OF wa_group_detail,
      it_indir_cegrp LIKE TABLE OF wa_group_detail,
      it_semidir_cegrp LIKE TABLE OF wa_group_detail,

      it_coks LIKE TABLE OF wa_coks,
      it_cokl  LIKE TABLE OF wa_cokl.

CONSTANTS: c_cs012 LIKE cosr-stagr VALUE 'CS012'.  "Temp MH
DATA  :BEGIN OF it_mha_temp OCCURS 0,
        kostl    LIKE ztco_mha-kostl,
        objnr    LIKE cosr-objnr,
        wrttp    TYPE wrttp,
        meinh    LIKE cosr-meinh,
        menge    LIKE ztco_mha-netmh,       "Out sourcing
* ig.moon 11/30 {
        perio	      TYPE co_perio,
* }
       END OF it_mha_temp.


*&---------------------Variables
DATA: w_objnr LIKE plobjcom-objnr,
      w_cnt1 TYPE i,
      w_cnt2 TYPE i,
      w_idx TYPE i,
      w_flag TYPE c,
      w_perbl LIKE cosp-perbl,
      w_prd_fld LIKE cosp-perbl,
      w_field1(14) TYPE c,
      w_time(3) TYPE n,
      w_groupname LIKE bapico_group-groupname,
      c_versn LIKE coss-versn VALUE '000'.

FIELD-SYMBOLS: <fs>, <fs1>.
CONSTANTS: c_cc_grp TYPE bapiset_groupname VALUE 'OVERHEAD1'.


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

DATA : g_ktopl LIKE t001-ktopl.

*- U1 Start
DATA: gt_coss_a TYPE TABLE OF coss WITH HEADER LINE,
      gt_cosr_a TYPE TABLE OF cosr WITH HEADER LINE,
      gt_cosp_a TYPE TABLE OF cosp WITH HEADER LINE.
*-U1 End


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_kokrs   LIKE csks-kokrs OBLIGATORY MEMORY ID cac,
            p_gjahr   LIKE cosp-gjahr OBLIGATORY MEMORY ID bdtj,
            p_fperbl  LIKE cosp-perbl OBLIGATORY MEMORY ID vpe,
            p_tperbl  LIKE cosp-perbl ,
            p_versn   LIKE cosp-versn OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS: p_ccgrp LIKE bapico_group-groupname DEFAULT 'DIRECT'.
SELECT-OPTIONS s_kostl FOR csks-kostl.
PARAMETERS: p_cegrp LIKE bapico_group-groupname." default 'OVERHEAD1'.
SELECT-OPTIONS s_kstar FOR coss-kstar.
SELECTION-SCREEN END OF BLOCK blk2.

PARAMETERS: p_semic TYPE grpname DEFAULT 'SEMIDIRECT'.
PARAMETERS: p_indic TYPE grpname DEFAULT 'INDIRECT'.

* Costing Type
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-022.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(13)  text-020. "
SELECTION-SCREEN POSITION 15.
PARAMETERS : p_cca RADIOBUTTON GROUP ra01
             USER-COMMAND  cty.
SELECTION-SCREEN COMMENT  25(13) text-021.
SELECTION-SCREEN POSITION 39.
PARAMETERS : p_mh  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
PARAMETERS : p_os AS CHECKBOX.
* ig.moon 11/30 {
PARAMETERS: p_acum AS CHECKBOX DEFAULT 'X'.
* }
SELECTION-SCREEN END OF BLOCK bl3.

PARAMETERS: p_act(1) TYPE c DEFAULT 'X' NO-DISPLAY.
PARAMETERS: p_pln(1) TYPE c DEFAULT 'X' NO-DISPLAY.
PARAMETERS: p_detail AS CHECKBOX.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

DATA: gv_level(5).

* ig.moon 12/02 {
RANGES r_kstar FOR cosp-kstar.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
* }

START-OF-SELECTION.

  GET PARAMETER ID 'ZCOLV1' FIELD gv_level.
*  clear p_detail.
*  if gv_level = 'ADM'. p_detail = 'X'. endif.

  PERFORM set_r_perbl.

  PERFORM get_master_info.

  PERFORM get_cc_groups.

* ig.moon 12/02 {
  PERFORM read_cegrp.
* }

*  perform get_master_info.

  PERFORM get_cskb.
* Get Actual & Plan
  PERFORM select_coss.
* Get other plan
  PERFORM select_cosp.

* Get Cost component usign cost elemt
* (Except Semi)
  PERFORM get_elemt.
  PERFORM make_cost_component.
  PERFORM collect_semi_qty.

* Man Hour
  IF p_os = 'X'.
    PERFORM get_temp_mh.
  ENDIF.
  PERFORM calc_mh_total.
  IF p_mh =  'X'.
    PERFORM get_man_hr.
  ENDIF.

* Process for Semi , but define type using par_kostl
*  if p_act = 'X'.
**    perform actual_semi.
*  endif.
*  if p_pln = 'X'.
  PERFORM plan_semi.
*  endif.


  PERFORM get_result.

  PERFORM display_output.

*&---------------------------------------------------------------------*
*&      Form  select_coss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_coss.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_coss_temp
   FROM coss
   FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND ( ( ( wrttp = '01' OR wrttp = '02' ) AND versn = p_versn ) OR
           ( ( wrttp = '03' OR wrttp = '04' ) AND versn = c_versn ) )
     AND vrgng IN ('KSI1', 'KSI2', 'KSI3', 'KSP1', 'KSP2', 'RKP7' )
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr2.

*actual activity posting
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE  it_coss_temp
   FROM coss
   FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND wrttp = '04'
     AND versn = c_versn
     AND vrgng IN ('KSII', 'RKL' )
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_coss.
  ENDIF.
*- U1 End

  SORT it_coss_temp BY objnr versn vrgng parob uspob.

  LOOP AT it_coss_temp.
    CLEAR st_coss.
    MOVE-CORRESPONDING it_coss_temp TO st_coss.

    CLEAR it_cskb.
    READ TABLE it_cskb WITH KEY kstar = st_coss-kstar
                       BINARY SEARCH.

    CASE st_coss-wrttp.
*      Actual
      WHEN '03' OR '04' .
        IF p_act = 'X'.
          PERFORM make_actual_data.
        ENDIF.
*      Plan
      WHEN '01' OR '02' .
        IF p_pln = 'X'.
          PERFORM make_plan_data.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.                    " select_coss
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_master_info.

* by ig.moon 12/02 {

*  if p_ccgrp <> space.
*    refresh s_kostl.
*    s_kostl[] = r_kostl[].
*  endif.

  IF p_ccgrp <> space.
    PERFORM get_cc_group TABLES r_kostl
                         USING  p_ccgrp.
    REFRESH s_kostl.
    s_kostl[] = r_kostl[].
  ELSE.
    IF s_kostl IS INITIAL.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-006.
      EXIT.
    ENDIF.
  ENDIF.
* }

*&---------Get cost center information.
  SELECT b~kokrs b~kostl b~objnr a~objnr
    INTO TABLE it_csks
    FROM cssl AS a
   INNER JOIN csks AS b
      ON a~kokrs = b~kokrs
     AND a~kostl = b~kostl
   WHERE b~kokrs = p_kokrs
     AND a~gjahr = p_gjahr
     AND b~kostl IN s_kostl.

  IF sy-subrc NE 0.
    MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-006.
    EXIT.
  ENDIF.


ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  make_cost_component
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_cost_component.

  DATA : l_amt     TYPE p DECIMALS 4,
         l_amt_f   TYPE p DECIMALS 4,
         l_qty     TYPE p DECIMALS 4,
         l_qty_f   TYPE p DECIMALS 4,
         l_type(1).

  DATA : l_cnt(3)  TYPE n,
         l_field(50).


  LOOP AT it_coss .

* by ig.moon 11/30 {
*    clear :l_amt, l_amt_f.
*    perform move_it_coss_to_coss_cc changing l_amt
*                                             l_amt_f.
*
*    check sy-subrc = 0.
* }

*   Cost center type define : using par_kostl
    CLEAR : l_type.
    PERFORM get_cc_type  USING    it_coss-par_kostl
                         CHANGING l_type.

    IF it_coss-par_kostl(2) EQ 'MX'.
      IF it_coss-kostl EQ it_coss-par_kostl.
        l_type = 'D'.
      ENDIF.
    ENDIF.

* by ig.moon 11/30 {

    CLEAR it_kstar.
    READ TABLE it_kstar WITH KEY kstar = it_coss-kstar BINARY SEARCH.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING it_coss TO it_coss_cc.

    IF p_detail = space.
      CLEAR it_coss_cc-kstar.
    ENDIF.

    it_coss_cc-elemt = it_kstar-elemt.

    CLEAR :l_cnt.
    l_cnt = p_fperbl.
    DO w_time TIMES.

      CLEAR :l_amt, l_amt_f.

      CONCATENATE 'IT_COSS-WKG' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_amt = <fs> + l_amt.
      CONCATENATE 'IT_COSS-WKF' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_amt_f = <fs> + l_amt_f.
      CLEAR : l_field.
      it_coss_cc-perio = l_cnt.

      IF l_type = 'D' .
        it_coss_cc-dicst   = l_amt.
        it_coss_cc-dicst_f = l_amt_f.
      ELSEIF l_type = 'S'.
        it_coss_cc-smcst   = l_amt.
        it_coss_cc-smcst_f = l_amt_f.
      ELSE.
        it_coss_cc-incst   = l_amt.
        it_coss_cc-incst_f = l_amt_f.
      ENDIF.

      COLLECT it_coss_cc ..
      l_cnt = l_cnt + 1.
    ENDDO.
    CLEAR it_coss_cc.

* }
  ENDLOOP.


ENDFORM.                    " make_cost_component
*&---------------------------------------------------------------------*
*&      Form  GET_MAN_HR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_man_hr.
  CHECK NOT it_csks[] IS INITIAL.
  DATA : it_ztco_mha_temp LIKE ztco_mha OCCURS 0 WITH HEADER LINE.

*  select *
*    into corresponding fields of table it_ztco_mha_temp
*    from ztco_mha
*     for all entries in it_csks
*   where gjahr = p_gjahr
*     and perid in r_perbl
*     and kostl = it_csks-kostl
*     and lgart in ('1', '2', '3') .
*
*  loop at it_ztco_mha_temp.
*    move-corresponding it_ztco_mha_temp to it_ztco_mha.
*
**-- add up temp MH
*    read table it_mha_temp with key kostl = it_ztco_mha-kostl
*                                    wrttp = '04'.  "actual
*    if sy-subrc = 0.
*      it_ztco_mha-anzhl = it_ztco_mha-anzhl + it_mha_temp-menge.
*    endif.
*
*    collect it_ztco_mha. clear it_ztco_mha.
*  endloop.

** Furong on 02/29/12

  DATA: lt_cosr LIKE TABLE OF cosr WITH HEADER LINE.
  DATA: l_from(3) TYPE n,
         l_to(3) TYPE n,
         l_text(40).

  FIELD-SYMBOLS: <fs>.

  SELECT *
       INTO CORRESPONDING FIELDS OF TABLE lt_cosr
       FROM cosr
        FOR ALL ENTRIES IN it_csks
      WHERE gjahr = p_gjahr
*     and perid in r_perbl
        AND stagr = 'CS010'
        AND objnr = it_csks-objnr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_cosr TABLES lt_cosr USING 'X'.
  ENDIF.
*- U1 End

  LOOP AT lt_cosr.
    it_ztco_mha-kostl = lt_cosr-objnr+6(12).
    l_from = p_fperbl.
    l_to = p_tperbl.

    WHILE l_from =< l_to.

      CONCATENATE 'LT_COSR-SME' l_from INTO l_text.
      ASSIGN (l_text) TO <fs>.

      it_ztco_mha-anzhl = <fs>.

*-- add up temp MH
      READ TABLE it_mha_temp WITH KEY kostl = it_ztco_mha-kostl
                                      wrttp = '04'.  "actual
      IF sy-subrc = 0.
        it_ztco_mha-anzhl = it_ztco_mha-anzhl + it_mha_temp-menge.
      ENDIF.

      COLLECT it_ztco_mha. CLEAR it_ztco_mha.
      l_from  = l_from + 1.
    ENDWHILE.
  ENDLOOP.
** End on 02/29/12

ENDFORM.                    " GET_MAN_HR
*&---------------------------------------------------------------------*
*&      Form  calc_mh_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_mh_total.
  DATA : lt_cosl  LIKE it_cosl OCCURS 0 WITH HEADER LINE.

  LOOP AT it_csks1.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_cosl
       FROM cosl AS a
       INNER JOIN cssl AS b
          ON a~gjahr = b~gjahr
         AND a~objnr = b~objnr
       WHERE a~gjahr = p_gjahr
         AND ( ( wrttp = '04' AND versn = c_versn )
          OR   ( wrttp = '01' AND versn = p_versn ) )
         AND b~kostl = it_csks1-kostl.
*- U1 Start
*  IF p_arch EQ 'X'.
*    PERFORM archive_read_cosl.
*  ENDIF.
*- U1 End
  ENDLOOP.

  LOOP AT lt_cosl.
    MOVE-CORRESPONDING lt_cosl TO it_cosl.
    it_cosl-kostl = lt_cosl-objnr+6(10).
    it_cosl-lstar = lt_cosl-objnr+16(6).
    COLLECT it_cosl. CLEAR it_cosl.
  ENDLOOP.


  DATA: w_manhr_qty LIKE cosl-lst001,
        w_round_qty TYPE p DECIMALS 1.

  LOOP AT it_cosl.
    w_prd_fld =  p_fperbl.
    it_manhr_qty-wrttp = it_cosl-wrttp.
    it_manhr_qty-objnr = it_cosl-objnr.
    DO w_time TIMES.
      CONCATENATE 'IT_COSL-LST' w_prd_fld INTO w_field1.
      ASSIGN (w_field1) TO <fs1>.
      w_manhr_qty = <fs1>.
      it_manhr_qty-lst001 = w_manhr_qty .
      it_manhr_qty-lstar  = it_cosl-lstar.
      it_manhr_qty-kostl  = it_cosl-kostl.
      it_manhr_qty-meinh  = it_cosl-meinh.
* ig.moon 11/30 {
      it_manhr_qty-perio  = w_prd_fld.
* }
      COLLECT it_manhr_qty.
      w_prd_fld = w_prd_fld + 1.
    ENDDO.
    CLEAR it_manhr_qty.
  ENDLOOP.

  LOOP AT it_manhr_qty .
*   Change unit(Mch_hr)
    IF it_manhr_qty-meinh <> 'STD'.
      PERFORM unit_converion USING it_manhr_qty-lst001
                                   it_manhr_qty-meinh
                                   'STD'
                    CHANGING it_manhr_qty-lst001.
    ENDIF.
    w_round_qty = it_manhr_qty-lst001.
    it_manhr_qty-lst001 = w_round_qty.
    IF it_manhr_qty-wrttp = '02' .
      it_manhr_qty-wrttp = '01' .
    ENDIF.
    IF it_manhr_qty-wrttp = '04' .
      it_manhr_qty-wrttp = '04' .
    ENDIF.

*-- add up temp MH
    READ TABLE it_mha_temp WITH KEY kostl = it_manhr_qty-kostl
                                    wrttp = it_manhr_qty-wrttp
* ig.moon 11/30 {
                                    perio = it_manhr_qty-perio
* }
                                    .
    IF sy-subrc = 0.
      it_manhr_qty-lst001 = it_manhr_qty-lst001 + it_mha_temp-menge.
    ENDIF.

    MODIFY it_manhr_qty. CLEAR it_manhr_qty.
  ENDLOOP.

* TEMP MH

ENDFORM.                    " calc_mh_total
*&---------------------------------------------------------------------*
*&      Form  GET_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_result.
  PERFORM get_elemt_name.

  LOOP AT it_coss_cc.
    MOVE-CORRESPONDING it_coss_cc TO gt_result.
    gt_result-shop = it_coss_cc-kostl(4).
    CASE it_coss_cc-wrttp.
      WHEN '01' OR '02'.
        it_coss_cc-wrttp = '01'.
        gt_result-wrtxt = 'Plan'.
      WHEN '03' OR '04'.
        it_coss_cc-wrttp = '04'.
        gt_result-wrtxt = 'Actual'.
    ENDCASE.

    CLEAR it_tckh2.
    READ TABLE it_tckh2 WITH KEY elemt = it_coss_cc-elemt
                                         BINARY SEARCH.
    gt_result-txele = it_tckh2-txele.

    IF p_detail = space.
      CLEAR: gt_result-par_kostl, gt_result-kstar.
    ENDIF.
    COLLECT gt_result. CLEAR gt_result.
  ENDLOOP.

  DATA: l_idx LIKE sy-tabix.
  LOOP AT gt_result.
    l_idx = sy-tabix.

*actual only
    IF gt_result-wrttp = '04' AND p_mh = 'X'.
      CLEAR it_ztco_mha.
      READ TABLE it_ztco_mha WITH KEY kostl = gt_result-kostl.
      gt_result-manhr_qty = it_ztco_mha-anzhl.

*plan/actual
    ELSE.
      CLEAR it_manhr_qty.
      READ TABLE it_manhr_qty WITH KEY wrttp = gt_result-wrttp
                                       kostl = gt_result-kostl
                                       lstar = 'MAN_HR'
* ig.moon 11/30 {
                                       perio = gt_result-perio.
* }
      gt_result-manhr_qty = it_manhr_qty-lst001.
    ENDIF.

    PERFORM calculate_value.
    gt_result-meinh = 'STD'.  "HR

    MODIFY gt_result INDEX l_idx.
  ENDLOOP.

* by ig.moon 12/03 {
  IF p_acum EQ 'X'.
    DATA $gt_result LIKE gt_result OCCURS 0 WITH HEADER LINE.

    LOOP AT gt_result.
      $gt_result = gt_result.
      CLEAR $gt_result-perio.
      COLLECT $gt_result.
    ENDLOOP.
    __cls gt_result.
    gt_result[] = $gt_result[].
  ENDIF.
* }
ENDFORM.                    " GET_RESULT
*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output.

  PERFORM field_setting TABLES gt_fieldcat USING :
'WRTXT'     'Cat'            '06' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
* ig.moon 11/30 {
  IF p_acum EQ 'X'.
  ELSE.
    PERFORM field_setting TABLES gt_fieldcat USING :
    'PERIO'     'Per'            '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
  ENDIF.
* }

  PERFORM field_setting TABLES gt_fieldcat USING :
  'SHOP'      'Shop'           '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'KOSTL'     'CostCtr'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'ELEMT'     'CC'             '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'TXELE'     'CC desc'        '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'INCST'     'Indirect'       '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'SMCST'     'SemiDirect'     '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'DICST'     'Direct'         '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'TOCST'     'Total'          '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'MANHR_QTY' 'MH'             '12' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
  'INCST_HR'  'Indirect/HR'    '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'SMCST_HR'  'Semi/HR'        '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'DICST_HR'  'Direct/HR'      '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
  'TOCST_HR'  'Total/HR'       '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X'.

  IF p_detail = 'X'.
    PERFORM field_setting TABLES gt_fieldcat USING :
  'KSTAR'     'CostElm'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
  'PAR_KOSTL' 'PartnerCC'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.
  ENDIF.

  g_repid = sy-repid.
  SORT gt_result BY wrttp shop kostl.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = gt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " display_output
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  read_data_from_coomco
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_from_coomco.
* KSBT dose not have data by period in case of (BP/Qarter) planning
* Select data with date range

* Local Data Definition
  DATA : f_kadky LIKE coomco-kadky,
         t_kadky LIKE coomco-kadky,
         l_kostl   TYPE kostl,
         l_lstar   TYPE lstar,
         l_tarkz LIKE coomco-tarkz.

* From period - To period
  PERFORM get_kadky CHANGING f_kadky
                             t_kadky.


  l_tarkz = '001'.
  CHECK NOT it_coss_semi[] IS INITIAL.
* This logic copy from ZCOCCS10
  DATA: l_coomco LIKE it_coomco.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_coomco
          FROM coomco
          FOR ALL ENTRIES IN it_coss_semi
         WHERE lednr = '00'
           AND objnr = it_coss_semi-uspob
           AND gjahr = p_gjahr
           AND versn = c_versn
           AND tvers = '04'         "01-plan,04-actual
           AND ( kadky >= f_kadky AND kadky <= t_kadky )
           AND tarkz = l_tarkz      " gv_tarkz    "price indicator
           AND kkzma = space .      "Additive - > X



  SORT it_coomco BY lednr objnr gjahr versn bzobj tvers kadky kalnr
                    kalka kkzma dipa  bwvar keart kkzmm kkzst losfx
                    patnr.

  DELETE ADJACENT DUPLICATES FROM it_coomco.

  CLEAR : it_coomco.

ENDFORM.                    " read_data_from_coomco
*&---------------------------------------------------------------------*
*&      Form  get_kadky
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_F_KADKY  text
*      <--P_T_KADKY  text
*----------------------------------------------------------------------*
FORM get_kadky CHANGING p_fdate
                        p_tdate.
  DATA : l_date(8).
  CONCATENATE p_gjahr p_fperbl+1(2) '01' INTO l_date.
  p_fdate = l_date.


  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_gjahr
*     I_MONMIT       = 00
      i_periv        = 'K4'
      i_poper        = p_tperbl
    IMPORTING
      e_date         = p_tdate
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.

ENDFORM.                    " get_kadky
*&---------------------------------------------------------------------*
*&      Form  read_comp_value_ksbt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_comp_value_ksbt.

  DATA : it_l_keph    LIKE STANDARD TABLE OF keph
                      WITH HEADER LINE .
  DATA : it_l_kkb_split
                      LIKE STANDARD TABLE OF  kkb_split
                      WITH HEADER LINE .
  DATA : wa_l_ckikekokey LIKE ckikekokey .


* Clear
  CLEAR : it_kostl_lstar_pct, it_kostl_lstar_pct[].
  SORT it_coomco.
  LOOP AT it_coomco.
* Move Keys
    MOVE-CORRESPONDING it_coomco TO wa_l_ckikekokey  .
* Read KEPH
    CLEAR : it_l_keph, it_l_keph[].
    CALL FUNCTION 'CK_F_KEKO_KEPH_DIRECT_READ'
      EXPORTING
        f_kekokey              = wa_l_ckikekokey
        read_keko              = space
        read_keph              = 'X'
*       READ_ONLY_BUFFER       = ' '
*       READ_ONLY_DB           = ' '
*       CACHED_READ            = ' '
*       KEPH_MANDATORY         = 'X'
*     IMPORTING
*       F_KEKO                 =
      TABLES
        i_keph                 = it_l_keph
      EXCEPTIONS
        data_not_found         = 1
        wrong_call             = 2
        OTHERS                 = 3.

* Read Keph (1 line)
    CLEAR it_l_keph.
    CLEAR keph.
    READ TABLE it_l_keph INTO keph INDEX 1.
* Read Costs by Cost Comp.
    CLEAR : it_l_kkb_split, it_l_kkb_split[].
    CALL FUNCTION 'K_KKB_SPLITTING_CONVERT'
      EXPORTING
        i_elehk     = 'H1'
        i_sicht     = '01'
        i_keart     = keph-keart
        i_losfx     = keph-losfx
        i_waers     = 'USD'
      TABLES
        t_keph      = it_l_keph
        t_kkb_split = it_l_kkb_split.
*ELEMT      -> Cost Component Number
*ELEMT_TEXT
*W000       -> PLAN
    LOOP AT it_l_kkb_split.
      MOVE-CORRESPONDING it_coomco      TO it_kostl_lstar_pct.
      MOVE-CORRESPONDING it_l_kkb_split TO it_kostl_lstar_pct.
      it_kostl_lstar_pct-wrttp = it_coomco-tvers.
      COLLECT it_kostl_lstar_pct.
      CLEAR  it_kostl_lstar_pct.
    ENDLOOP.
  ENDLOOP.

* Get CCTR / AT
  LOOP AT it_kostl_lstar_pct.
*   From Period
    PERFORM set_from_to_period
      USING it_kostl_lstar_pct-kadky
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-from_per.
*   TO Period
    PERFORM set_from_to_period
      USING it_kostl_lstar_pct-bidat
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-to_per.

    MODIFY it_kostl_lstar_pct.
    CLEAR it_kostl_lstar_pct.
  ENDLOOP.

  CLEAR it_kostl_lstar_pct.


ENDFORM.                    " read_comp_value_ksbt
*&---------------------------------------------------------------------*
*&      Form  set_from_to_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_KOSTL_LSTAR_PCT_BIDAT  text
*      -->P_P_KOKRS  text
*      -->P_IT_KOSTL_LSTAR_PCT_GJAHR  text
*      -->P_IT_KOSTL_LSTAR_PCT_TO_PER  text
*----------------------------------------------------------------------*
FORM set_from_to_period USING p_date
                              p_kokrs
                              p_bdatj
                              p_per.
* period (From/To)
  CALL FUNCTION 'K_DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date             = p_date
      i_kokrs            = p_kokrs
    IMPORTING
      e_gjahr            = p_bdatj
      e_perio            = p_per
    EXCEPTIONS
      no_period_determin = 1
      t009b_notfound     = 2
      t009_notfound      = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
  ENDIF.
ENDFORM.                    " set_from_to_period
*&---------------------------------------------------------------------*
*&      Form  get_activity_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_QTY  text
*      <--P_ITAB_ACT_UNIT  text
*----------------------------------------------------------------------*
FORM get_activity_qty CHANGING f_qty  f_unit.

  CLEAR f_qty.

*  IF p_wrttp = '04'.
  READ TABLE i_cosl WITH KEY objnr = it_kostl_lstar_pct-objnr
                             wrttp = it_kostl_lstar_pct-wrttp.
  f_unit = i_cosl-meinh.

  CASE it_coss_cc_semi-perbl.
    WHEN  01.  f_qty = i_cosl-lst001.
    WHEN  02.  f_qty = i_cosl-lst002.
    WHEN  03.  f_qty = i_cosl-lst003.
    WHEN  04.  f_qty = i_cosl-lst004.
    WHEN  05.  f_qty = i_cosl-lst005.
    WHEN  06.  f_qty = i_cosl-lst006.
    WHEN  07.  f_qty = i_cosl-lst007.
    WHEN  08.  f_qty = i_cosl-lst008.
    WHEN  09.  f_qty = i_cosl-lst009.
    WHEN  10.  f_qty = i_cosl-lst010.
    WHEN  11.  f_qty = i_cosl-lst011.
    WHEN  12.  f_qty = i_cosl-lst012.
  ENDCASE.

*  ELSE.
** plan...
*    REFRESH i_plan_act.
*    DATA: l_fr  LIKE  bapi0012_6-period,
*          l_to  LIKE  bapi0012_6-period.
*    l_fr = it_kostl_lstar_pct-kadky+4(2).
*    l_to = it_kostl_lstar_pct-bidat+4(2).
*    CALL FUNCTION 'BAPI_CTR_GETACTIVITYQUANTITIES'
*         EXPORTING
*              coarea         = p_kokrs
*              fiscyear       = p_bdatj
*              version        = p_versn
*              costcenterfrom = it_kostl_lstar_pct-kostl
*              acttypefrom    = it_kostl_lstar_pct-lstar
*              periodfrom     = l_fr
*              periodto       = l_to
*         TABLES
*              return         = it_return
*              actquantities  = i_plan_act.
*
*
** plan exist in multi-periods
*    LOOP AT i_plan_act.
*      f_qty  = f_qty + i_plan_act-act_quantity.
*      f_unit = i_plan_act-ACT_UNIT.
*    ENDLOOP.
*
*  ENDIF.

ENDFORM.                    " get_activity_qty
*&---------------------------------------------------------------------*
*&      Form  read_activity_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_activity_data.

  SELECT * INTO TABLE i_cosl FROM cosl
    WHERE lednr = '00'
      AND gjahr = p_gjahr.
*      AND versn = p_versn.

*- U1 Start
*  IF p_arch EQ 'X'.
*    PERFORM archive_read_cosl.
*  ENDIF.
*- U1 End

ENDFORM.                    " read_activity_data
*&---------------------------------------------------------------------*
*&      Form  calculate_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_value.

  gt_result-tocst    =  gt_result-incst + gt_result-smcst +
                        gt_result-dicst .

  gt_result-tocst_f    =  gt_result-incst_f + gt_result-smcst_f +
                          gt_result-dicst_f .

  IF   gt_result-manhr_qty <> 0 .
    gt_result-incst_hr =  gt_result-incst / gt_result-manhr_qty.
    gt_result-smcst_hr =  gt_result-smcst / gt_result-manhr_qty.
    gt_result-dicst_hr =  gt_result-dicst / gt_result-manhr_qty.
    gt_result-tocst_hr =  gt_result-tocst / gt_result-manhr_qty.

    gt_result-incst_f_hr =  gt_result-incst_f / gt_result-manhr_qty.
    gt_result-smcst_f_hr =  gt_result-smcst_f / gt_result-manhr_qty.
    gt_result-dicst_f_hr =  gt_result-dicst_f / gt_result-manhr_qty.
    gt_result-tocst_f_hr =  gt_result-tocst_f / gt_result-manhr_qty.
  ENDIF.

ENDFORM.                    " calculate_value
*&---------------------------------------------------------------------*
*&      Form  get_elemt_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_elemt_name.
*--------Get Cost component
  SELECT t1~ktopl t1~elehk kstav kstab t1~elemt txele
                       INTO TABLE it_tckh2
                       FROM tckh2 AS t1
                       INNER JOIN tka01 AS t2
                       ON t1~ktopl = t2~ktopl
                       INNER JOIN tckh1 AS t3
                       ON t3~elehk = t1~elehk
                       AND t3~elemt = t1~elemt
                       WHERE t2~kokrs = p_kokrs
                       AND   t1~elehk = 'H1'
                       AND   spras = sy-langu.

ENDFORM.                    " get_elemt_name
*&---------------------------------------------------------------------*
*&      Form  make_actual_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_actual_data.
* caution:   collect & calcualte as like semi/indirect/direct
*            but, finally define CC TYPE using par_kostl

*  SEMI
  IF ( st_coss-vrgng  = 'KSII' OR st_coss-vrgng  = 'RKL' )
    AND it_cskb-katyp = '43'.

    READ TABLE it_csks WITH KEY objnr = st_coss-objnr.
    st_coss-objnr = it_csks-objnr2.
    PERFORM collect_it_semi TABLES it_coss_semi.
*    PERFORM collect_it_coss.
*    indirect
  ELSEIF st_coss-vrgng = 'KSI2'  AND it_cskb-katyp = '42'.
    PERFORM collect_it_coss.
    PERFORM collect_it_kstar.
*    direct
  ELSEIF ( st_coss-vrgng  = 'KSI1'  OR st_coss-vrgng  = 'KSI2' )
           AND it_cskb-katyp BETWEEN '01' AND '12'.
    PERFORM collect_it_coss.
    PERFORM collect_it_kstar.
  ENDIF.


ENDFORM.                    " make_actual_data
*&---------------------------------------------------------------------*
*&      Form  make_plan_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_plan_data.
* caution:   collect & calcualte as like semi/indirect/direct
*            but, finally define using par_kostl

*  SEMI
  IF st_coss-vrgng  = 'RKP7' AND it_cskb-katyp = '43'.
    PERFORM collect_it_semi TABLES it_coss_semi. "it_coss_semi.
    PERFORM collect_it_coss.

* indirect
  ELSEIF st_coss-vrgng = 'KSP2' AND it_cskb-katyp = '42'.
    PERFORM collect_it_coss.
    PERFORM collect_it_kstar.

*   direct
  ELSEIF st_coss-vrgng  = 'KSP1' OR  st_coss-vrgng = 'KSP2'.
    IF it_cskb-katyp BETWEEN '01' AND '12'.
      PERFORM collect_it_coss.
      PERFORM collect_it_kstar.
    ENDIF.
  ENDIF.

ENDFORM.                    " make_plan_data
*&---------------------------------------------------------------------*
*&      Form  GET_CSKB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cskb.
  SELECT kstar katyp INTO TABLE it_cskb
     FROM cskb
    WHERE kokrs = p_kokrs
      AND datbi >= sy-datum
      AND datab <= sy-datum.

  SORT it_cskb BY kstar.

ENDFORM.                    " GET_CSKB
*&---------------------------------------------------------------------*
*&      Form  select_cosp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cosp.

  CHECK p_pln = 'X'.

* Plan
  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_cosp
   FROM cosp
   FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND ( wrttp = '01' OR wrttp = '02' )
     AND versn = p_versn
     AND vrgng = 'RKP6'
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr2 .

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_cosp.
  ENDIF.
*- U1 End

  LOOP AT it_cosp.
*   it_csop => move to it_coss_temp and then
*   and then make it_coss(ref. perform select_coss).
    CLEAR st_coss.
    MOVE-CORRESPONDING it_cosp TO st_coss.
    PERFORM collect_it_coss.
    PERFORM collect_it_kstar.

  ENDLOOP.

ENDFORM.                    " select_cosp
*&---------------------------------------------------------------------*
*&      Form  get_elemt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_elemt.
  SORT it_kstar BY kstar.
  DELETE ADJACENT DUPLICATES FROM it_kstar.

  LOOP AT it_kstar .
    CALL FUNCTION 'KKEK_COST_COMPONENT_ELEMENT'
      EXPORTING
        elehk_imp         = 'H1'
        ktopl_imp         = 'HNA1'
        kstar_imp         = it_kstar-kstar
        message_on_screen = space
      IMPORTING
        elemt_exp         = it_kstar-elemt
      EXCEPTIONS
        calling_error     = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.

    ELSE.
      MODIFY it_kstar. CLEAR it_kstar.
    ENDIF.
  ENDLOOP.

  SORT it_kstar BY elemt.
  DELETE it_kstar WHERE elemt = space.

  SORT it_kstar BY kstar.

ENDFORM.                    " get_elemt
*&---------------------------------------------------------------------*
*&      Form  actual_Semi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actual_semi.
* Read DATA From COOMCO
  PERFORM read_data_from_coomco.
* Read Activity Data
  PERFORM read_activity_data.
* Read Component Values - KSBT
  PERFORM read_comp_value_ksbt.

  PERFORM get_semi_act_qty.

  PERFORM make_semi_it_coss_act.

ENDFORM.                    " actual_Semi
*&---------------------------------------------------------------------*
*&      Form  plan_Semi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plan_semi.
  DATA: lt_cocom LIKE it_cocom1 OCCURS 0.

  LOOP AT it_cokl INTO wa_cokl.
    REFRESH: it_cocom1.
    CLEAR: wa_cost_com,w_perbl.

    REFRESH: lt_cocom.
    PERFORM get_components TABLES lt_cocom USING p_gjahr
                                                 p_versn
                                                 '01'        "PLAN
                                                 wa_cokl-objnr.
    APPEND LINES OF lt_cocom TO it_cocom1.
    REFRESH: lt_cocom.
    PERFORM get_components TABLES lt_cocom USING p_gjahr
                                                 '00'
                                                 '04'        "Act
                                                 wa_cokl-objnr.
    APPEND LINES OF lt_cocom TO it_cocom1.


    SORT it_cocom1.
    LOOP AT it_cocom1.
      CHECK it_cocom1-perio IN r_perbl.
      MOVE-CORRESPONDING it_cocom1 TO it_cal_com.
      COLLECT it_cal_com. CLEAR it_cal_com.
    ENDLOOP.

  ENDLOOP.

  SORT it_cal_com BY perio objnr wrttp elemt.

  PERFORM calc_semi_plan TABLES it_cal_com USING '01'.
  PERFORM calc_semi_plan TABLES it_cal_com USING '04'.
  PERFORM make_semi_it_coss_act_pln.

ENDFORM.                    " plan_Semi
*&---------------------------------------------------------------------*
*&      Form  get_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_components TABLES   p_it_cocom STRUCTURE plobjcom
                    USING    f_gjahr
                             f_versn
                             f_wrttp
                             f_objnr.


  DATA: wa_plobjcom LIKE plobjcom.
  DATA  it_plobjcom LIKE TABLE OF wa_plobjcom.

  REFRESH: it_plobjcom.

  CALL FUNCTION 'K_OBJECT_COMPONENTS_READ'
    EXPORTING
      i_gjahr = f_gjahr
      i_versn = f_versn
      i_wrttp = f_wrttp
      i_objnr = f_objnr
      i_lednr = '00'
    TABLES
      t_cocom = it_plobjcom.

  p_it_cocom[] = it_plobjcom[].

ENDFORM.                    " get_components
*&---------------------------------------------------------------------*
*&      Form  calc_semi_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CAL_COM  text
*      -->f_type ; 01 - plan, 04 - actual
*----------------------------------------------------------------------*
FORM calc_semi_plan TABLES p_it_cocom STRUCTURE plobjcom
                    USING  f_wrttp.

  DATA: wa_objcom LIKE plobjcom,
        wa_objcom2 LIKE plobjcom.
  DATA: BEGIN OF wa_com_tot,
          objnr LIKE wa_objcom-objnr,
          par_kostl TYPE kostl,
          skstg LIKE wa_objcom-skstg,
          skstf LIKE wa_objcom-skstf,
* by ig.moon 11/30 {
          perio TYPE  co_perio,
* }
        END OF wa_com_tot,
        it_com_tot LIKE TABLE OF wa_com_tot.

  DATA: gt_cc_pln LIKE wa_objcom OCCURS 0 WITH HEADER LINE,
        gt_cc_act LIKE TABLE OF wa_objcom.


  DATA: w_org_amt            TYPE p DECIMALS 3,
        w_org_amt_f          TYPE p DECIMALS 3,
        w_cecom_value        TYPE p DECIMALS 3,
        w_cecom_value_f      TYPE p DECIMALS 3,

        w_cc_amt             TYPE p DECIMALS 2,
        w_cc_amt_f           TYPE p DECIMALS 2,
        w_cc_amt_tot         TYPE p DECIMALS 3,
        w_cc_amt_tot_f       TYPE p DECIMALS 3.

  w_perbl = p_fperbl.
  w_prd_fld = '001'.

*PLAN
  REFRESH: gt_cc_pln.
* by ig.moon 11/30 {
  LOOP AT p_it_cocom WHERE wrttp = f_wrttp
                       AND perio IN r_perbl.
    MOVE-CORRESPONDING p_it_cocom TO gt_cc_pln.
    APPEND gt_cc_pln. CLEAR  gt_cc_pln.
  ENDLOOP.
*}

* sum of CC
  REFRESH it_com_tot.
  CLEAR: wa_objcom.
  LOOP AT gt_cc_pln INTO wa_objcom.
    wa_com_tot-objnr = wa_objcom-objnr.

* by ig.moon 11/30 {
*    at end of objnr.
    AT END OF  perio.
* }
      SUM.
* by ig.moon 11/30 {
      wa_com_tot-perio     = wa_objcom-perio.
* }
      wa_com_tot-skstg     = wa_objcom-skstg.
      wa_com_tot-par_kostl = wa_com_tot-objnr+6(10).
      APPEND wa_com_tot TO it_com_tot.
    ENDAT.
  ENDLOOP.

*  concatenate 'wa_cosp-wkg' w_prd_fld into w_field.

*  loop at it_coks into wa_coks .
  LOOP AT  it_coss_cc_semi WHERE wrttp = f_wrttp.

    w_org_amt    = it_coss_cc_semi-wkg00.
    w_org_amt_f  = it_coss_cc_semi-wkg00_f.

    CHECK NOT  w_org_amt IS INITIAL.
    READ TABLE it_com_tot INTO wa_com_tot
                          WITH KEY objnr = it_coss_cc_semi-uspob
* by ig.moon 11/30 {
                                   perio  = it_coss_cc_semi-perbl.
* }
    IF sy-subrc EQ 0.
      w_cc_amt_tot   = wa_com_tot-skstg.
      w_cc_amt_tot_f = wa_com_tot-skstf.

      LOOP AT gt_cc_pln INTO wa_objcom
                          WHERE objnr = it_coss_cc_semi-uspob
* by ig.moon 11/30 {
                          AND   perio EQ it_coss_cc_semi-perbl.
* }
        w_cc_amt   = wa_objcom-skstg.
        w_cc_amt_f = wa_objcom-skstf.
        IF w_cc_amt_tot <> 0 .
          w_cecom_value   = w_org_amt *
                            w_cc_amt   / w_cc_amt_tot.
        ENDIF.
        IF w_cc_amt_tot_f <> 0 .
          w_cecom_value_f = w_org_amt *
                            w_cc_amt_f / w_cc_amt_tot_f.
        ENDIF.

        wa_cost_com-elemt      = wa_objcom-elemt.
        wa_cost_com-wrttp      = wa_objcom-wrttp.
        wa_cost_com-perbl      = wa_objcom-perio.
        wa_cost_com-wkg001     = w_cecom_value.
        wa_cost_com-wkg001_f   = w_cecom_value_f.
        wa_cost_com-kstar      = it_coss_cc_semi-kstar.

        PERFORM get_cc_fr_obj USING    it_coss_cc_semi-objnr
                          CHANGING wa_cost_com-kostl.
        wa_cost_com-lstar      = it_coss_cc_semi-objnr+16(6).

        PERFORM get_cc_fr_obj USING    it_coss_cc_semi-uspob
                              CHANGING wa_cost_com-par_kostl.

        APPEND wa_cost_com TO it_semi_cc_pln.
        CLEAR: w_cc_amt, w_cecom_value,wa_cost_com.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " calc_semi_plan
*&---------------------------------------------------------------------*
*&      Form  get_semidirect_costcenters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_semidirect_costcenters.
  DATA: w_nlin TYPE i.

  SELECT DISTINCT t1~kokrs t3~kostl kstar t2~objnr
    INTO TABLE it_cokl
    FROM cskb AS t1
   INNER JOIN cokl AS t2
      ON t2~vksta = t1~kstar
   INNER JOIN cssl AS t3
      ON t3~objnr = t2~objnr
     AND t3~gjahr = t2~gjahr
   WHERE t1~kokrs = p_kokrs
     AND t2~gjahr = p_gjahr
     AND versn = p_versn
     AND katyp = '43'.

  CLEAR wa_cokl.

*&-------To get the relevant direct cost center.
  DESCRIBE TABLE it_cokl LINES w_nlin.
  IF w_nlin EQ 0.
  ELSE.
*...secondary planning (SAP include LTP)
    SELECT DISTINCT objnr kstar parob FROM coks
                             INTO TABLE it_coks
                             FOR ALL ENTRIES IN it_cokl "it_csks
                             WHERE lednr = '00'
                             AND   objnr = it_cokl-objnr "it_csks-objnr2
                             AND   gjahr = p_gjahr
                             AND   wrttp = '01'
                             AND   versn = p_versn
                             AND   vrgng = 'RKP7'.
*                             and   kstar = it_cokl-kstar
*                             and   parob = it_cokl-objnr

    SORT it_coks BY objnr.
  ENDIF.

ENDFORM.                    " get_semidirect_costcenters
*&---------------------------------------------------------------------*
*&      Form  make_semi_it_coss_act_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_semi_it_coss_act_pln.
  DATA : l_type(1).

  LOOP AT it_semi_cc_pln .
    IF it_semi_cc_pln-wkg001 =  0 AND it_semi_cc_pln-wkg001_f =  0 .
    ELSE.
      PERFORM get_cc_type USING    it_semi_cc_pln-par_kostl
                          CHANGING l_type.
      CASE l_type.
        WHEN 'I' .
          PERFORM make_vlaue_pln CHANGING it_coss_cc-incst
                                     it_coss_cc-incst_f.
        WHEN 'S'.
          PERFORM make_vlaue_pln CHANGING it_coss_cc-smcst
                                     it_coss_cc-smcst_f.
        WHEN 'D'.
          PERFORM make_vlaue_pln CHANGING it_coss_cc-dicst
                                      it_coss_cc-dicst_f.
      ENDCASE.
* by ig.moon 11/30 {
      it_coss_cc-perio =  it_semi_cc_pln-perbl.
* }
      COLLECT it_coss_cc. CLEAR it_coss_cc.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " make_semi_it_coss_act_pln
*&---------------------------------------------------------------------*
*&      Form  unit_converion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MANHR_QTY_LST001  text
*      -->P_IT_MANHR_QTY_MEINH  text
*      -->P_0940   text
*      <--P_IT_MANHR_QTY_LST001  text
*----------------------------------------------------------------------*
FORM unit_converion USING    p_input
                             p_unit_in
                             p_unit_out
                    CHANGING p_output.

  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = p_input
        unit_in              = p_unit_in
        unit_out             = p_unit_out
      IMPORTING
        output               = p_output
      EXCEPTIONS
        conversion_not_found = 1
        division_by_zero     = 2
        input_invalid        = 3
        output_invalid       = 4
        overflow             = 5
        type_invalid         = 6
        units_missing        = 7
        unit_in_not_found    = 8
        unit_out_not_found   = 9
        OTHERS               = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " unit_converion
*&---------------------------------------------------------------------*
*&      Form  make_semi_it_coss_act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_semi_it_coss_act.
  DATA: l_type(1).

  SORT  it_coss_cc_semi.
  LOOP AT it_coss_cc_semi .

    CLEAR it_coss_semi.
    READ TABLE it_coss_semi WITH KEY uspob = it_coss_cc_semi-uspob
                                     kostl = it_coss_cc_semi-kostl
                                     lstar = it_coss_cc_semi-lstar
                                     wrttp = '04'.
    CHECK sy-subrc = 0 .

    CLEAR it_kostl_lstar_pct.
    LOOP AT it_kostl_lstar_pct  WHERE objnr    = it_coss_cc_semi-uspob.
*                                  and from_per = it_coss_cc_semi-perbl.
      IF it_kostl_lstar_pct-w000 = 0 AND
         it_kostl_lstar_pct-w001 = 0 .
      ELSE.
        MOVE-CORRESPONDING it_coss_semi TO it_coss_cc.
        it_coss_cc-elemt =  it_kostl_lstar_pct-elemt.
        PERFORM get_cc_type USING it_coss_semi-par_kostl
                            CHANGING l_type.
        CASE l_type.
          WHEN 'I'.
            PERFORM make_vlaue CHANGING it_coss_cc-incst
                                        it_coss_cc-incst_f.
          WHEN 'S'.
            PERFORM make_vlaue CHANGING it_coss_cc-smcst
                                        it_coss_cc-smcst_f.
          WHEN 'D'.
            PERFORM make_vlaue CHANGING it_coss_cc-dicst
                                        it_coss_cc-dicst_f.
        ENDCASE.
        COLLECT it_coss_cc. CLEAR it_coss_cc.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " make_semi_it_coss_act
*&---------------------------------------------------------------------*
*&      Form  set_r_perbl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_r_perbl.
  IF p_tperbl = space OR p_tperbl < p_fperbl.
    p_tperbl = p_fperbl.
  ENDIF.

  r_perbl-sign     = 'I'.
  r_perbl-option   = 'BT'.
  r_perbl-low      = p_fperbl.
  r_perbl-high     = p_tperbl.
  APPEND r_perbl.

  READ TABLE r_perbl INDEX 1.

  w_time =  p_tperbl - p_fperbl + 1.

ENDFORM.                    " set_r_perbl
*&---------------------------------------------------------------------*
*&      Form  get_group_details
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CEG_DETAIL  text
*      -->P_C_CC_GRP  text
*----------------------------------------------------------------------*
FORM get_group_details TABLES p_itab STRUCTURE wa_group_detail
                       USING p_grpnam.

  REFRESH: it_hnodes, it_hvalues.
  CLEAR: w_idx,w_cnt1,w_cnt2,wa_hnodes,wa_hvalues,wa_return1.

  CALL FUNCTION 'BAPI_COSTELEMENTGRP_GETDETAIL'
    EXPORTING
      chartofaccounts = 'HNA1'
      groupname       = p_grpnam
    IMPORTING
      return          = wa_return1
    TABLES
      hierarchynodes  = it_hnodes
      hierarchyvalues = it_hvalues.
  IF wa_return1-type = 'E'.
    IF p_grpnam = c_cc_grp. "'OVERHEAD1'.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-009.
    ELSE.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-012.
    ENDIF.
    EXIT.
  ENDIF.

  w_idx = 1.
  LOOP AT it_hnodes INTO wa_hnodes.
    IF wa_hnodes-valcount NE 0.
      w_cnt1 = wa_hnodes-valcount.
    ELSE.
      CONTINUE.
    ENDIF.
    LOOP AT it_hvalues INTO wa_hvalues FROM w_idx.
      w_cnt2 = w_cnt2 + 1.
      IF w_cnt2 > w_cnt1.
        w_idx  = sy-tabix.
        CLEAR w_cnt2.
        EXIT.
      ENDIF.
      IF p_cegrp IS INITIAL OR p_cegrp = c_cc_grp. "'OVERHEAD1'.
        wa_group_detail-gname = wa_hnodes-groupname.
        wa_group_detail-valfrom = wa_hvalues-valfrom.
        wa_group_detail-valto = wa_hvalues-valto.
        APPEND wa_group_detail TO p_itab.
      ELSE.
        IF wa_hnodes-groupname = p_cegrp.
          wa_group_detail-gname = wa_hnodes-groupname.
          wa_group_detail-valfrom = wa_hvalues-valfrom.
          wa_group_detail-valto = wa_hvalues-valto.
          APPEND wa_group_detail TO p_itab.
        ENDIF.
      ENDIF.
      CASE wa_hnodes-groupname.
        WHEN 'DIRECT'.
          wa_direct_cegrp-gname = wa_hnodes-groupname.
          wa_direct_cegrp-valfrom = wa_hvalues-valfrom.
          wa_direct_cegrp-valto = wa_hvalues-valto.
          APPEND wa_direct_cegrp TO it_direct_cegrp.
        WHEN 'INDIR'.
          wa_indir_cegrp-gname = wa_hnodes-groupname.
          wa_indir_cegrp-valfrom = wa_hvalues-valfrom.
          wa_indir_cegrp-valto = wa_hvalues-valto.
          APPEND wa_indir_cegrp TO it_indir_cegrp.
        WHEN 'SEMIDIR'.
          wa_semidir_cegrp-gname = wa_hnodes-groupname.
          wa_semidir_cegrp-valfrom = wa_hvalues-valfrom.
          wa_semidir_cegrp-valto = wa_hvalues-valto.
          APPEND wa_semidir_cegrp TO it_semidir_cegrp.
      ENDCASE.
      CLEAR: wa_direct_cegrp, wa_indir_cegrp, wa_semidir_cegrp.
    ENDLOOP.
  ENDLOOP.
  CLEAR:w_idx, wa_hnodes, wa_hvalues.

ENDFORM.                    " get_group_details
*&---------------------------------------------------------------------*
*&      Form  collect_it_kstar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_it_kstar.

* For get cost component using cost elemt
* (Except Semi)
  it_kstar-kstar = st_coss-kstar.
  APPEND it_kstar.
  CLEAR it_kstar.

ENDFORM.                    " collect_it_kstar
*&---------------------------------------------------------------------*
*&      Form  collect_it_coss
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_it_coss.
  MOVE-CORRESPONDING st_coss TO it_coss.

* Plan
  IF it_coss-wrttp = '01' OR it_coss-wrttp = '02'.
    it_coss-wrttp = '01' .
  ENDIF.
* Actual
  IF it_coss-wrttp = '03' OR it_coss-wrttp = '04'.
    it_coss-wrttp = '04' .
  ENDIF.

  CLEAR it_coss-uspob.

  PERFORM get_cc_fr_obj USING    st_coss-objnr
                        CHANGING it_coss-kostl.

  it_coss-lstar = st_coss-objnr+16(6).

  PERFORM get_cc_fr_obj USING    st_coss-uspob
                        CHANGING it_coss-par_kostl.

  COLLECT it_coss. CLEAR it_coss.

ENDFORM.                    " collect_it_coss
*&---------------------------------------------------------------------*
*&      Form  collect_it_semi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_it_semi TABLES p_coss_semi STRUCTURE it_coss.
  MOVE-CORRESPONDING st_coss TO it_coss.

  PERFORM get_cc_fr_obj USING    st_coss-objnr
                        CHANGING it_coss-kostl.
  IF it_coss-kostl = space.
    IF 1 = 1.
    ENDIF.
  ENDIF.


* Plan
  IF it_coss-wrttp = '01' OR it_coss-wrttp = '02'.
    it_coss-wrttp = '01' .
  ENDIF.
* Actual
  IF it_coss-wrttp = '03' OR it_coss-wrttp = '04'.
    it_coss-wrttp = '04' .
  ENDIF.


  MOVE-CORRESPONDING it_coss TO p_coss_semi.

  p_coss_semi-uspob     = st_coss-uspob.
  p_coss_semi-par_kostl = st_coss-uspob+6(10).
  COLLECT p_coss_semi.  CLEAR p_coss_semi.

ENDFORM.                    " collect_it_semi
*&---------------------------------------------------------------------*
*&      Form  collect_it_coss2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_it_coss2.
  MOVE-CORRESPONDING it_cosp TO it_coss.
  CLEAR it_coss-uspob.
  READ TABLE it_csks WITH KEY objnr2 = it_cosp-objnr.
  IF sy-subrc = 0 .
    it_coss-kostl = it_csks-kostl.
  ENDIF.

  it_coss-lstar = it_cosp-objnr+16(6).
  it_coss-type = 'D'.
  it_kstar-kstar = it_coss-kstar.
  COLLECT it_kstar. CLEAR it_kstar.
  IF  it_coss-wrttp = '01' OR it_coss-wrttp = '02'.
    it_coss-wrttp = '01' .
  ENDIF.
  COLLECT it_coss. CLEAR it_coss.

ENDFORM.                    " collect_it_coss2
*&---------------------------------------------------------------------*
*&      Form  move_it_coss_to_coss_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_AMT  text
*      <--P_L_AMT_F  text
*----------------------------------------------------------------------*
FORM move_it_coss_to_coss_cc CHANGING p_amt
                                      p_amt_f.

  DATA : l_cnt(3)  TYPE n,
         l_field(50).

*it_kstar has no '43' category.!!! - activity ce.
  CLEAR it_kstar.
  READ TABLE it_kstar WITH KEY kstar = it_coss-kstar BINARY SEARCH.
  CHECK sy-subrc = 0.

  MOVE-CORRESPONDING it_coss TO it_coss_cc.
  it_coss_cc-elemt = it_kstar-elemt.

  CLEAR :l_cnt.
  l_cnt = p_fperbl.
  DO w_time TIMES.
    CONCATENATE 'IT_COSS-WKG' l_cnt INTO l_field.
    ASSIGN (l_field) TO <fs>.
    p_amt = <fs> + p_amt.
    CONCATENATE 'IT_COSS-WKF' l_cnt INTO l_field.
    ASSIGN (l_field) TO <fs>.
    p_amt_f = <fs> + p_amt_f.
    CLEAR : l_field.
    l_cnt = l_cnt + 1.
  ENDDO.

ENDFORM.                    " move_it_coss_to_coss_cc
*&---------------------------------------------------------------------*
*&      Form  get_cc_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_TYPE  text
*----------------------------------------------------------------------*
FORM get_cc_type USING    p_par_kostl
                 CHANGING p_type.


  IF p_par_kostl = ''.
    p_type = 'D'.
  ELSEIF p_par_kostl IN r_semi.
    p_type = 'S'.
  ELSEIF p_par_kostl IN r_indi.
    p_type = 'I'.
  ELSE.
    p_type = 'I'.
  ENDIF.


ENDFORM.                    " get_cc_type
*&---------------------------------------------------------------------*
*&      Form  make_vlaue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_COSS_CC_INCST  text
*      <--P_IT_COSS_CC_INCST_F  text
*----------------------------------------------------------------------*
FORM make_vlaue CHANGING p_value
                         p_value_f.
  DATA : l_qty TYPE p DECIMALS 4,
         l_unit TYPE meins.

  CLEAR : l_qty, l_unit.
  PERFORM get_activity_qty CHANGING l_qty l_unit.

  IF it_coss_cc_semi-smqty <> 0 AND l_qty <> 0 .
    p_value   = it_kostl_lstar_pct-w000 *
                it_coss_cc_semi-smqty   / l_qty .
  ENDIF.
  IF  it_coss_cc_semi-smqty_f <> 0 AND l_qty <> 0 .
    p_value_f = it_kostl_lstar_pct-w001 *
                it_coss_cc_semi-smqty_f / l_qty .
  ENDIF.

ENDFORM.                    " make_vlaue
*&---------------------------------------------------------------------*
*&      Form  make_vlaue_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_COSS_CC_INCST  text
*      <--P_IT_COSS_CC_INCST_F  text
*----------------------------------------------------------------------*
FORM make_vlaue_pln CHANGING p_value
                             p_value_f.

  MOVE-CORRESPONDING it_semi_cc_pln TO it_coss_cc.

  p_value     = it_semi_cc_pln-wkg001.
  p_value_f   = it_semi_cc_pln-wkg001_f.

ENDFORM.                    " make_vlaue_pln
*&---------------------------------------------------------------------*
*&      Form  get_semi_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_semi_act_qty.
  DATA : l_cnt(3)  TYPE n,
         l_qty     TYPE p DECIMALS 4,
         l_qty_f   TYPE p DECIMALS 4,
         l_field(50).

  LOOP AT it_coss_semi.
    MOVE-CORRESPONDING it_coss_semi TO it_coss_cc_semi.
    CLEAR :l_cnt, l_qty, l_qty_f.
    l_cnt = p_fperbl.
    DO w_time TIMES.
      CLEAR :l_qty, l_qty_f.
      CONCATENATE 'IT_COSS_SEMI-MEG' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_qty = <fs> .
      CONCATENATE 'IT_COSS_SEMI-MEF' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_qty_f = <fs> .
      MOVE-CORRESPONDING it_coss_semi TO it_coss_cc_semi.
      it_coss_cc_semi-smqty   = l_qty.
      it_coss_cc_semi-smqty_f = l_qty_f.
      it_coss_cc_semi-perbl   = l_cnt.
      COLLECT it_coss_cc_semi . CLEAR it_coss_cc_semi.

      l_cnt = l_cnt + 1.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " get_semi_qty
*&---------------------------------------------------------------------*
*&      Form  get_cc_groups
*&---------------------------------------------------------------------*
FORM get_cc_groups.
  CLEAR g_ktopl.
  SELECT SINGLE ktopl INTO g_ktopl FROM t001
    WHERE bukrs = p_kokrs.

*&------Get Semi-Direct cost center information.
  PERFORM get_semidirect_costcenters.


  PERFORM get_cc_group TABLES r_semi
                       USING  p_semic.
  PERFORM get_cc_group TABLES r_indi
                       USING  p_indic.

* commented by ig.moon 12/02 {
*  if p_ccgrp <> space.
*    perform get_cc_group tables r_kostl
*                         using  p_ccgrp.
*  else.
*    if s_kostl is initial.
*      message id 'ZFI' type 'E' number '999' with text-006.
*      exit.
*    endif.
*  endif.
* }

  REFRESH: it_csks1, it_cssl.
  SELECT DISTINCT kokrs kostl objnr INTO TABLE it_csks1
    FROM csks
    WHERE kokrs = p_kokrs.
  SORT it_csks1 BY objnr.

  SELECT DISTINCT kostl lstar objnr INTO TABLE it_cssl
    FROM cssl
    WHERE kokrs = p_kokrs.
  SORT it_cssl BY objnr.

ENDFORM.                    " get_cc_groups
*&---------------------------------------------------------------------*
*&      Form  GET_CC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cc_group  TABLES p_cc STRUCTURE r_semi
                   USING  l_ksgru.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.

*BAPI is not working..
*    call function 'BAPI_COSTCENTERGROUP_GETDETAIL'
*         exporting
*              controllingarea = p_kokrs
*              groupname       = p_ccgrp
*         importing
*              return          = wa_return1
*         tables
*              hierarchynodes  = it_hnodes
*              hierarchyvalues = it_hvalues.

  CLEAR : t_setlist.
  CHECK NOT l_ksgru IS INITIAL.

  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      setclass      = '0101'
      shortname     = l_ksgru
      kokrs         = p_kokrs
      ktopl         = g_ktopl
    TABLES
      matching_sets = t_setlist.
  IF t_setlist[] IS INITIAL.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ELSE.
    READ TABLE t_setlist INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      setid                     = t_setlist-setname
    TABLES
      set_hierarchy             = t_sethier
      set_values                = t_setvalues
    EXCEPTIONS
      set_not_found             = 1
      illegal_field_replacement = 2
      illegal_table_replacement = 3
      OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE e002(sy) WITH 'Cost Center group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CC RANGE.
  p_cc-sign = 'I'.
  p_cc-option = 'BT'.
  LOOP AT t_setvalues.
    p_cc-low  = t_setvalues-from.
    p_cc-high = t_setvalues-to.
    APPEND p_cc.
  ENDLOOP.
  CLEAR p_cc.

ENDFORM.                    " GET_CC_GROUP
*&---------------------------------------------------------------------*
*&      Form  get_cc_fr_obj
*&---------------------------------------------------------------------*
FORM get_cc_fr_obj USING    f_objnr
                   CHANGING f_kostl.

  CLEAR f_kostl.

  CASE f_objnr(2).
    WHEN 'KS'.
      READ TABLE it_csks1 WITH KEY objnr = f_objnr.
      IF sy-subrc = 0 .
        f_kostl = it_csks1-kostl.
      ENDIF.

    WHEN 'KL'.
      READ TABLE it_cssl WITH KEY objnr = f_objnr.
      IF sy-subrc = 0 .
        f_kostl = it_cssl-kostl.
      ENDIF.

  ENDCASE.

ENDFORM.                    " get_cc_fr_obj
*&---------------------------------------------------------------------*
*&      Form  collect_semi_qty
*&---------------------------------------------------------------------*
FORM collect_semi_qty.

  DATA : l_cnt(3)  TYPE n,
         l_amt     LIKE coss-wkg001,
         l_qty     LIKE coss-meg001,
         l_field(50).


  LOOP AT it_coss_semi.
    MOVE-CORRESPONDING it_coss_semi TO it_coss_cc_semi.
    CLEAR :l_cnt, l_qty.

    l_cnt = p_fperbl.
    DO w_time TIMES.
      MOVE-CORRESPONDING it_coss_semi TO it_coss_cc_semi.

      CLEAR :l_qty, l_amt.
      CONCATENATE 'IT_COSS_SEMI-MEG' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_qty = <fs> .
      it_coss_cc_semi-smqty   = l_qty.

      CONCATENATE 'IT_COSS_SEMI-MEF' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_qty = <fs> .
      it_coss_cc_semi-smqty_f = l_qty.

      CONCATENATE 'IT_COSS_SEMI-WKG' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_amt = <fs> .
      it_coss_cc_semi-wkg00 = l_amt.

      CONCATENATE 'IT_COSS_SEMI-WKF' l_cnt INTO l_field.
      ASSIGN (l_field) TO <fs>.
      l_amt = <fs> .
      it_coss_cc_semi-wkg00_f = l_amt.

* by ig.moon 11/30 {
      it_coss_cc_semi-perbl = l_cnt.
* }
      COLLECT it_coss_cc_semi . CLEAR it_coss_cc_semi.

      l_cnt = l_cnt + 1.
    ENDDO.
  ENDLOOP.

  SORT it_coss_cc_semi BY objnr.

ENDFORM.                    " collect_semi_qty
*&---------------------------------------------------------------------*
*&      Form  get_temp_mh
*&---------------------------------------------------------------------*
FORM get_temp_mh.
  DATA : it_cosr_os  LIKE cosr OCCURS 0 WITH HEADER LINE.

  SELECT  * INTO CORRESPONDING FIELDS OF TABLE  it_cosr_os
      FROM cosr
    WHERE gjahr = p_gjahr
      AND wrttp = '04'     "actual
      AND versn = '000'
      AND stagr = c_cs012.
  SELECT  * APPENDING CORRESPONDING FIELDS OF TABLE  it_cosr_os
      FROM cosr
    WHERE gjahr = p_gjahr
      AND wrttp = '01'     "plan
      AND versn = p_versn
      AND stagr = c_cs012.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_cosr TABLES it_cosr_os USING 'Y'.
  ENDIF.
*- U1 End

  DATA : l_cnt(3)  TYPE n,
         l_field(50).
  FIELD-SYMBOLS : <f_field>.

  LOOP AT it_cosr_os.
    CLEAR :l_cnt.
    MOVE-CORRESPONDING it_cosr_os TO it_mha_temp.
    l_cnt = p_fperbl.
    DO w_time TIMES.

      it_mha_temp-kostl = it_cosr_os-objnr+6(10).
      CONCATENATE 'IT_COSR_OS-SME' l_cnt INTO l_field.
      ASSIGN (l_field) TO <f_field>.
      it_mha_temp-menge = <f_field>.
* ig.moon 11/30 {
      it_mha_temp-perio = l_cnt.
* }

      COLLECT it_mha_temp. CLEAR it_mha_temp.
    ENDDO.

  ENDLOOP.

  LOOP AT it_mha_temp.
*   Change unit(Mch_hr)
    IF it_mha_temp-meinh <> 'STD'.
      PERFORM unit_converion USING it_mha_temp-menge
                                   it_mha_temp-meinh
                                   'STD'
                    CHANGING it_mha_temp-menge.
    ENDIF.
    IF it_mha_temp-wrttp = '02' .
      it_mha_temp-wrttp = '01' .
    ENDIF.
    IF it_mha_temp-wrttp = '04' .
      it_mha_temp-wrttp = '04' .
    ENDIF.

    MODIFY it_mha_temp. CLEAR it_mha_temp.
  ENDLOOP.

ENDFORM.                    " get_temp_mh
*&---------------------------------------------------------------------*
*&      Form  get_costcenter_control_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_costcenter_control_data.
  SELECT t1~kostl lstar t1~objnr t2~objnr
                           INTO TABLE it_cssl
                           FROM cssl AS t1
                           INNER JOIN csks AS t2
                           ON t1~kokrs = t2~kokrs
                           AND t1~kostl = t2~kostl
                           WHERE t2~kokrs = p_kokrs
                           AND   t1~gjahr = p_gjahr.

ENDFORM.                    " get_costcenter_control_data
*&---------------------------------------------------------------------*
*&      Form  read_cegrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cegrp.

  IF p_cegrp EQ space AND s_kstar[] IS INITIAL.
    EXIT.
  ENDIF.
  __cls r_kstar.

  IF NOT s_kstar[] IS INITIAL.
    r_kstar[] = s_kstar[].
    EXIT.
  ENDIF.


  REFRESH: it_hnodes, it_hvalues.
  CLEAR: wa_hnodes,wa_hvalues,wa_return1.

  CALL FUNCTION 'BAPI_COSTELEMENTGRP_GETDETAIL'
    EXPORTING
      chartofaccounts = 'HNA1'
      groupname       = p_cegrp
    IMPORTING
      return          = wa_return1
    TABLES
      hierarchynodes  = it_hnodes
      hierarchyvalues = it_hvalues.
  IF wa_return1-type = 'E'.
    MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH
               'Not a Valid cost element group'.
    EXIT.
  ENDIF.

  w_idx = 1.
  LOOP AT it_hnodes INTO wa_hnodes.
    IF wa_hnodes-valcount NE 0.
      w_cnt1 = wa_hnodes-valcount.
    ELSE.
      CONTINUE.
    ENDIF.
    LOOP AT it_hvalues INTO wa_hvalues FROM w_idx.
      w_cnt2 = w_cnt2 + 1.
      IF w_cnt2 > w_cnt1.
        w_idx  = sy-tabix.
        CLEAR w_cnt2.
        EXIT.
      ENDIF.
*      IF wa_hnodes-groupname = p_cegrp.
      wa_group_detail-gname = wa_hnodes-groupname.
      wa_group_detail-valfrom = wa_hvalues-valfrom.
      wa_group_detail-valto = wa_hvalues-valto.
      APPEND wa_group_detail TO it_ceg_detail.
*      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CLEAR : w_idx,wa_hnodes, wa_hvalues.

  LOOP AT it_ceg_detail INTO wa_group_detail.
    r_kstar-sign    = 'I'.
    r_kstar-option  = 'BT'.
    r_kstar-low     = wa_group_detail-valfrom.
    r_kstar-high    = wa_group_detail-valto.
    APPEND r_kstar. CLEAR r_kstar.
  ENDLOOP.


ENDFORM.                    " read_cegrp
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_COSS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_coss .

  TYPES: BEGIN OF ty_coss,
         objnr TYPE j_objnr,
         gjahr TYPE gjahr,
         wrttp TYPE co_wrttp,
         kstar TYPE kstar,
         versn TYPE versn,
         vrgng TYPE co_vorgang,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_coss.

  DATA: l_handle    TYPE sytabix,
        lt_coss     TYPE TABLE OF coss WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_coss TYPE TABLE OF ty_coss,
        ls_inx_coss TYPE ty_coss.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZCOSS_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_coss[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_coss
    FROM (l_gentab)
     FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND ( ( ( wrttp = '01' OR wrttp = '02' ) AND versn = p_versn ) OR
           ( ( wrttp = '03' OR wrttp = '04' ) AND versn = c_versn ) )
     AND vrgng IN ('KSI1', 'KSI2', 'KSI3', 'KSP1', 'KSP2', 'RKP7' )
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr2.

*actual activity posting
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_inx_coss
   FROM (l_gentab)
   FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND wrttp = '04'
     AND versn = c_versn
     AND vrgng IN ('KSII', 'RKL' )
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr.

  CHECK NOT lt_inx_coss[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_coss_a, gt_coss_a[].
  LOOP AT lt_inx_coss INTO ls_inx_coss.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_coss-archivekey
        offset                    = ls_inx_coss-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_coss, lt_coss[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'COSS'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_coss
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_coss[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_coss INTO TABLE gt_coss_a.
  ENDLOOP.

  SORT gt_coss_a.
  DELETE ADJACENT DUPLICATES FROM gt_coss_a COMPARING ALL FIELDS.

  CHECK NOT gt_coss_a[] IS INITIAL.

  INSERT LINES OF gt_coss_a INTO TABLE it_coss_temp.

ENDFORM.                    " ARCHIVE_READ_COSS
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_COSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_COSR  text
*----------------------------------------------------------------------*
FORM archive_read_cosr  TABLES  pt_cosr  STRUCTURE  cosr
                         USING  p_true   TYPE char1.

  TYPES: BEGIN OF ty_cosr,
         objnr TYPE j_objnr,
         gjahr TYPE gjahr,
         wrttp TYPE co_wrttp,
         versn TYPE versn,
         stagr TYPE stagr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_cosr.

  DATA: l_handle    TYPE sytabix,
        lt_cosr     TYPE TABLE OF cosr WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_cosr TYPE TABLE OF ty_cosr,
        ls_inx_cosr TYPE ty_cosr.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZCOSR_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  IF p_true = 'X'.
    CLEAR lt_inx_cosr[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_cosr
      FROM (l_gentab)
      FOR ALL ENTRIES IN it_csks
     WHERE gjahr = p_gjahr
*     and perid in r_perbl
       AND stagr = 'CS010'
       AND objnr = it_csks-objnr.
  ELSEIF p_true = 'Y'.
    CLEAR lt_inx_cosr[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_cosr
      FROM (l_gentab)
     WHERE gjahr = p_gjahr
       AND wrttp = '04'     "actual
       AND versn = '000'
       AND stagr = c_cs012.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE lt_inx_cosr
      FROM (l_gentab)
     WHERE gjahr = p_gjahr
       AND wrttp = '01'     "plan
       AND versn = p_versn
       AND stagr = c_cs012.
  ENDIF.

  CHECK NOT lt_inx_cosr[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_cosr_a, gt_cosr_a[].
  LOOP AT lt_inx_cosr INTO ls_inx_cosr.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_cosr-archivekey
        offset                    = ls_inx_cosr-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_cosr, lt_cosr[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'COSR'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_cosr
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_cosr[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_cosr INTO TABLE gt_cosr_a.
  ENDLOOP.

  SORT gt_cosr_a.
  DELETE ADJACENT DUPLICATES FROM gt_cosr_a COMPARING ALL FIELDS.

  INSERT LINES OF gt_cosr_a INTO TABLE pt_cosr.

ENDFORM.                    " ARCHIVE_READ_COSR
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_COSP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_cosp .

  TYPES: BEGIN OF ty_cosp,
         gjahr TYPE gjahr,
         wrttp TYPE co_wrttp,
         versn TYPE versn,
         vrgng TYPE co_vorgang,
         kstar TYPE kstar,
         objnr TYPE j_objnr,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_cosp.

  DATA: l_handle    TYPE sytabix,
        lt_cosp     TYPE TABLE OF cosp WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_cosp TYPE TABLE OF ty_cosp,
        ls_inx_cosp TYPE ty_cosp.

  CONSTANTS: c_zcosp_001(9) VALUE 'ZCOSP_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zcosp_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_cosp[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_cosp
    FROM (l_gentab)
    FOR ALL ENTRIES IN it_csks
   WHERE gjahr = p_gjahr
     AND ( wrttp = '01' OR wrttp = '02' )
     AND versn = p_versn
     AND vrgng = 'RKP6'
     AND kstar IN r_kstar
     AND objnr = it_csks-objnr2.

  CHECK NOT lt_inx_cosp[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_cosp_a, gt_cosp_a[].
  LOOP AT lt_inx_cosp INTO ls_inx_cosp.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_cosp-archivekey
        offset                    = ls_inx_cosp-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_cosp, lt_cosp[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'COSP'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_cosp
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_cosp[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_cosp INTO TABLE gt_cosp_a.
  ENDLOOP.

  SORT gt_cosp_a.
  DELETE ADJACENT DUPLICATES FROM gt_cosp_a COMPARING ALL FIELDS.

  INSERT LINES OF gt_cosp_a INTO TABLE it_cosp.

ENDFORM.                    " ARCHIVE_READ_COSP
