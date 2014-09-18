REPORT zrcocmhs LINE-SIZE 230.
*&--------------------------------------------------------------------&*
*&    Program: ZRCOCMHS.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Calculate Overhead cost per Man hour per shop.   &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 01/06/2005  Shiva    UD1K914019      initial program.
*& 02/17/2005  Shiva    UD1K914462      Remove test data.(year = '2004')
*& 02/17/2005  Shiva    UD1K914505      Pass parameter plan as '01' and
*&                                      actual as 04' instead '1' & '4'.
*& 04/11/2005  Shiva    UD1K915243      Logic to include the calculation
*&                                     of reverse postings.
*&--------------------------------------------------------------------&*
*nclude zcoomtop.



CONSTANTS: c_cc_grp TYPE bapiset_groupname VALUE 'OVERHEAD1'.

*----------------------------------------------------------------------*
*   INCLUDE ZCOOMTOP                                                   *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*&----------- Work area
DATA: BEGIN OF wa_selfld,
        fname(20) TYPE c,
      END OF wa_selfld.
DATA:wa_plobjcom LIKE plobjcom,
     wa_return1 LIKE bapiret2,
     wa_hnodes  LIKE bapiset_hier,
     wa_hvalues  LIKE bapi1112_values.
DATA: BEGIN OF wa_group_detail,
         gname   LIKE wa_hnodes-groupname,
         valfrom LIKE wa_hvalues-valfrom,
         valto   LIKE wa_hvalues-valto,
      END OF wa_group_detail.
DATA: wa_direct_cegrp LIKE wa_group_detail,
      wa_indir_cegrp LIKE wa_group_detail,
      wa_semidir_cegrp LIKE wa_group_detail.
DATA: BEGIN OF wa_cssl,
        kostl LIKE cssl-kostl,
        lstar LIKE cssl-lstar,
        objnr LIKE cssl-objnr,
        objnr1 LIKE csks-objnr,
      END OF wa_cssl.
DATA: BEGIN OF wa_csks,
        kokrs  LIKE csks-kokrs,
        kostl  LIKE csks-kostl,
        objnr  LIKE csks-objnr,
        objnr2 LIKE cssl-objnr, "CC+AT
      END OF wa_csks.
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
DATA: BEGIN OF wa_cosl,
         objnr  LIKE cosl-objnr,
         wrttp  LIKE cosl-wrttp,
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
      END OF wa_cosl.
DATA: BEGIN OF wa_manhr_qty,
         wrttp  LIKE cosl-wrttp,
         lst001 LIKE cosl-lst001,
      END OF wa_manhr_qty.
DATA: BEGIN OF wa_coks,
        objnr LIKE coks-objnr,
        kstar LIKE coks-kstar,
        parob LIKE coks-parob,
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
         elemt LIKE tckh2-elemt,
         kstar LIKE cosp-kstar,
         wrttp LIKE cosp-wrttp,
         perbl LIKE cosp-perbl,
         wkg001 LIKE cosp-wkg001,
      END OF wa_cost_com.
DATA: BEGIN OF wa_result1,
        gname LIKE wa_hnodes-groupname,
        elemt LIKE plobjcom-elemt,
        wrttp LIKE wa_cosp-wrttp,
        skstg LIKE plobjcom-skstg,
      END OF wa_result1.
DATA: BEGIN OF wa_result,
        wrttp LIKE wa_cosp-wrttp,
        wrtxt(6) TYPE c,
        elemt LIKE plobjcom-elemt,
        txele LIKE tckh1-txele,
        incst LIKE plobjcom-skstg,
        smcst LIKE plobjcom-skstg,
        dicst LIKE plobjcom-skstg,
        tocst LIKE plobjcom-skstg,
        manhr_qty TYPE p DECIMALS 1,
        in_manhr  LIKE plobjcom-skstg,
        sm_manhr  LIKE plobjcom-skstg,
        di_manhr  LIKE plobjcom-skstg,
        to_manhr  LIKE plobjcom-skstg,
      END OF wa_result.
DATA: wa_fieldcat1 TYPE LINE OF slis_t_fieldcat_alv,
       wa_layout1 TYPE slis_layout_alv,
       wa_sortinfo TYPE slis_sortinfo_alv,
       wa_group    TYPE slis_sp_group_alv,
       wa_print    TYPE slis_print_alv.
*&----------------Internal table
DATA: it_selfld LIKE TABLE OF wa_selfld,
      it_selfld1 LIKE TABLE OF wa_selfld,
      it_selfld2 LIKE TABLE OF wa_selfld,
      it_cssl LIKE TABLE OF wa_cssl,
      it_cssl1 LIKE TABLE OF wa_cssl,
      it_manhr_qty LIKE TABLE OF wa_manhr_qty,
      it_cocom LIKE TABLE OF wa_plobjcom,
      it_cocom1 LIKE TABLE OF wa_plobjcom,
      it_cocom2 LIKE TABLE OF wa_plobjcom,
      it_cal_com LIKE TABLE OF wa_plobjcom,
      it_hnodes LIKE TABLE OF wa_hnodes,
      it_hvalues LIKE TABLE OF wa_hvalues WITH HEADER LINE,
      it_ceg_detail LIKE TABLE OF wa_group_detail,
      it_direct_cegrp LIKE TABLE OF wa_group_detail,
      it_indir_cegrp LIKE TABLE OF wa_group_detail,
      it_semidir_cegrp LIKE TABLE OF wa_group_detail,
      it_csks LIKE TABLE OF wa_csks,
      it_csks1 LIKE TABLE OF wa_csks,
      it_cosl LIKE TABLE OF wa_cosl,
      it_cosl1 LIKE TABLE OF wa_cosl,
      it_cosp LIKE TABLE OF wa_cosp,
      it_cosp1 LIKE TABLE OF wa_cosp,
      it_coss LIKE TABLE OF wa_coss,
      it_coss1 LIKE TABLE OF wa_coss,
      it_sel_coss LIKE TABLE OF wa_sel_coss,
      it_coks LIKE TABLE OF wa_coks,
      it_actplan_values LIKE TABLE OF wa_cosp,
      it_tckh2 LIKE TABLE OF wa_tckh2,
      it_cokl  LIKE TABLE OF wa_cokl,
      it_cokl1  LIKE TABLE OF wa_cokl,
      it_cost_com LIKE TABLE OF wa_cost_com,
      it_result1 LIKE TABLE OF wa_result1,
      it_result LIKE TABLE OF wa_result,
      it_fieldcat  TYPE slis_t_fieldcat_alv,
      it_sortinfo LIKE TABLE OF wa_sortinfo,
      it_group    LIKE TABLE OF wa_group.
*&---------------------Variables
DATA: w_objnr LIKE plobjcom-objnr,
      w_cnt1 TYPE i,
      w_cnt2 TYPE i,
      w_idx TYPE i,
      w_flag TYPE c,
      w_perbl LIKE cosp-perbl,
      w_prd_fld LIKE cosp-perbl,
      w_field(14) TYPE c,
      w_field1(14) TYPE c,
      w_groupname LIKE bapico_group-groupname.
FIELD-SYMBOLS: <fs>, <fs1>.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS: p_kokrs  LIKE csks-kokrs OBLIGATORY,
            p_gjahr  LIKE cosp-gjahr OBLIGATORY,
            p_fperbl  LIKE cosp-perbl OBLIGATORY,
            p_tperbl  LIKE cosp-perbl OBLIGATORY,
            p_versn  LIKE cosp-versn OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS: p_ccgrp LIKE bapico_group-groupname.
SELECT-OPTIONS s_kostl FOR wa_csks-kostl.
PARAMETERS: p_cegrp LIKE bapico_group-groupname.
SELECT-OPTIONS s_kstar FOR wa_cokl-kostl.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN.
  IF p_tperbl LT p_fperbl.
    MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-003.
    EXIT.
  ENDIF.
  IF p_ccgrp IS INITIAL.
    IF s_kostl IS INITIAL.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-004.
      EXIT.
    ENDIF.
  ENDIF.
  IF NOT p_cegrp IS INITIAL.
    IF NOT s_kstar IS INITIAL.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-010.
      EXIT.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM create_selection_field.
  PERFORM get_master_info.
  PERFORM filter_cssl.

  PERFORM get_plan_actual_values.
*&Filter for only relevant values.(using cost elements)
  PERFORM filter_selections.

  SORT: it_csks BY objnr,
        it_cosp BY objnr wrttp kstar,
        it_coss BY objnr wrttp kstar,
        it_cssl BY objnr objnr1.
*&---------Get relevant plans. (Direct)
  PERFORM get_plan_value TABLES it_cosp it_cosp1.
*&---------Get relevant actuals. (Direct)
  PERFORM get_actuals_value TABLES it_cosp it_cosp1.
*&--------Get relevant actuals. (Indirect & Semi direct)
  PERFORM get_actuals_value TABLES it_coss it_coss1.

*&--------Get relevant plans. (Indirect & Semi direct)
  PERFORM get_plan_value1 TABLES it_coss it_coss1.

*&--------Get relevant plans. (Indirect & Semi direct)
*  PERFORM get_plan_value TABLES it_coss it_coss1.

*&-----Delete direct cost element info from COSS.
  PERFORM delete_ce_fr_coss.

sort it_cosp1 by objnr wrttp
       vrgng
       kstar
       wkg001.

delete adjacent duplicates from it_cosp1 comparing objnr wrttp
       vrgng
       kstar
       wkg001.
  APPEND LINES OF it_cosp1 TO it_actplan_values.
  APPEND LINES OF it_coss1 TO it_actplan_values.
  FREE: it_cosp, it_cosp1, it_coss, it_coss1, wa_coss.
  CLEAR: wa_cosp.
  SORT: it_actplan_values BY objnr kstar.

*&----Get relavent Activity qauntity.
  PERFORM filter_cosl.

  delete it_actplan_values where wrttp = '01' and vrgng = 'KSP2'
                  and WKG001  < 0.

* Cost component
  PERFORM build_cc_info.  "direct,indirect from CE.
  PERFORM build_cc_semi.  "semi-direct

  FREE: it_actplan_values, it_cssl, it_coks.
  CLEAR: wa_cost_com.

*&----Assign group name for the result.
  PERFORM assign_grp_name.

*&----Group the information as(Direct,Semi-direct,In-direct)
  PERFORM build_group_info.
  FREE:it_result1.
  SORT it_result BY wrttp elemt.
  CLEAR w_cnt1.
  DESCRIBE TABLE it_result LINES w_cnt1.
  IF w_cnt1 EQ 0.
    MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-011.
    EXIT.
  ENDIF.

*---------------------------------------------------------------------*
  PERFORM display_output.

*---------------------------------------------------------------------*
*       FORM TOP-OF-PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA: wa_list_comm TYPE slis_listheader.
  DATA: it_list_comm LIKE TABLE OF wa_list_comm.

  CLEAR: wa_list_comm.
  REFRESH it_list_comm.

  wa_list_comm-typ = 'S'.
  IF p_ccgrp IS INITIAL.
    IF s_kostl-high IS INITIAL.
      wa_list_comm-key = 'Cost Center:'.
      wa_list_comm-info = s_kostl-low.
      APPEND wa_list_comm TO it_list_comm.
    ELSE.
      wa_list_comm-key = 'Cost Center:'.
      CONCATENATE s_kostl-low '~' s_kostl-high INTO wa_list_comm-info.
      APPEND wa_list_comm TO it_list_comm.
    ENDIF.
  ELSE.
    wa_list_comm-key = 'Cost Center group:'.
    wa_list_comm-info = p_ccgrp.
    APPEND wa_list_comm TO it_list_comm.
  ENDIF.
  wa_list_comm-key = 'Fiscal Year:'.
  wa_list_comm-info = p_gjahr.
  APPEND wa_list_comm TO it_list_comm.
  wa_list_comm-key = 'Period:'.
  TRANSLATE p_fperbl USING '0 '.
  TRANSLATE p_tperbl USING '0 '.
  CONCATENATE p_fperbl 'to' p_tperbl INTO wa_list_comm-info
                                     SEPARATED BY space.
  APPEND wa_list_comm TO it_list_comm.
  CLEAR wa_list_comm.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary       = it_list_comm
*   I_LOGO                   =
*   I_END_OF_LIST_GRID       =
            .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COCOM  text
*      -->P_W_GJAHR  text
*      -->P_W_VERSN  text
*      -->P_W_WRTTP  text
*      -->P_W_OBJNR  text
*----------------------------------------------------------------------*
FORM get_components TABLES   p_it_cocom STRUCTURE plobjcom
                    USING    p_gjahr
                             p_versn
                             p_wrttp
                             p_objnr.

  DATA: wa_plobjcom LIKE plobjcom.
  DATA  it_plobjcom LIKE TABLE OF wa_plobjcom.

  CALL FUNCTION 'K_OBJECT_COMPONENTS_READ'
    EXPORTING
      i_gjahr       = p_gjahr
      i_versn       = p_versn
      i_wrttp       = p_wrttp
      i_objnr       = p_objnr
*      I_LEDNR       = '00'
    TABLES
      t_cocom       = it_plobjcom.

  p_it_cocom[] = it_plobjcom[].

ENDFORM.                    " get_components
*&---------------------------------------------------------------------*
*&      Form  get_actuals_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COSP  text
*----------------------------------------------------------------------*
FORM get_actuals_value TABLES p_itab1 STRUCTURE wa_cosp
                              p_itab2 STRUCTURE wa_cosp.
  DATA w_nline TYPE i.
  DATA: it_filter_ce LIKE TABLE OF wa_cosp.
  FIELD-SYMBOLS: <fs_ceg> LIKE LINE OF it_ceg_detail.

  DESCRIBE TABLE p_itab1 LINES w_nline.
  w_idx = 1.
  CLEAR w_flag.
  LOOP AT it_csks INTO wa_csks.
*    clear: wa_cosp, w_flag.
*    if w_idx eq 1.
*      read table p_itab1 with key objnr = wa_csks-objnr binary search
*                                          transporting no fields.
*      if sy-subrc eq 0.
*        w_idx = sy-tabix.
*      endif.
*    endif.
*    loop at p_itab1 into wa_cosp from w_idx.
*      if wa_cosp-objnr ne wa_csks-objnr or wa_cosp-wrttp ne '04'.
*        w_idx = sy-tabix.
*        if w_idx eq w_nline.
*          w_idx = 1.
*        endif.
*        if w_flag eq 'X'.
*          exit.
*        endif.
*        continue.
*      endif.
    LOOP AT p_itab1 INTO wa_cosp
                    WHERE ( objnr EQ wa_csks-objnr AND wrttp = '04' )
                    OR    ( parob EQ wa_csks-objnr AND wrttp = '04' ).
      APPEND wa_cosp TO p_itab2.
*      w_flag = 'X'.
    ENDLOOP.
  ENDLOOP.
*&-----------Filter based on Cost element group or Cost element.
  IF p_cegrp IS INITIAL.
    IF NOT s_kstar IS INITIAL.
      LOOP AT p_itab2 INTO wa_cosp
                      WHERE kstar IN s_kstar.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
      REFRESH p_itab2.
      p_itab2[] = it_filter_ce[].
    ENDIF.
  ELSE.
    LOOP AT it_ceg_detail ASSIGNING <fs_ceg>.
      LOOP AT p_itab2 INTO wa_cosp
              WHERE kstar BETWEEN <fs_ceg>-valfrom
                          AND     <fs_ceg>-valto.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
    ENDLOOP.
    REFRESH p_itab2.
    p_itab2[] = it_filter_ce[].
  ENDIF.

ENDFORM.                    " get_actuals_value
*&---------------------------------------------------------------------*
*&      Form  get_plan_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COSP  text
*      -->P_IT_COSP1  text
*----------------------------------------------------------------------*
FORM get_plan_value TABLES p_itab1 STRUCTURE wa_cosp
                           p_itab2 STRUCTURE wa_cosp.
  DATA w_nline TYPE i.
  DATA: it_filter_ce LIKE TABLE OF wa_cosp.
  FIELD-SYMBOLS: <fs_ceg> LIKE LINE OF it_ceg_detail.

  DESCRIBE TABLE p_itab1 LINES w_nline.
  w_idx = 1.
  CLEAR w_flag.
* LOOP AT it_cssl INTO wa_cssl.
*    clear: wa_cosp, w_flag.
*    if w_idx eq 1 .
*      read table p_itab1 with key objnr = wa_cssl-objnr binary search
*                                          transporting no fields.
*      if sy-subrc eq 0.
*        w_idx = sy-tabix.
*      endif.
*    endif.
**Include also the reverse posting.
*    loop at p_itab1 into wa_cosp from w_idx.
*      if ( wa_cosp-objnr ne wa_cssl-objnr
*                 and wa_cosp-parob ne wa_cssl-objnr )
*         or wa_cosp-wrttp ne '01'.
*        w_idx = sy-tabix.
*        if w_idx eq w_nline.
*          w_idx = 1.
*        endif.
*        if w_flag = 'X' .
*          exit.
*        endif.
*        continue.
*      endif.
  LOOP AT it_cssl INTO wa_cssl.
    LOOP AT p_itab1 INTO wa_cosp
                    WHERE ( objnr = wa_cssl-objnr AND wrttp = '01' )
                    OR    ( parob = wa_cssl-objnr AND wrttp = '01' ).
*                    or    ( objnr = wa_cssl-objnr1 AND wrttp = '01').
*.... exclude CC -> CC+AT
      IF NOT wa_cosp-parob IS INITIAL.
        IF wa_cosp-parob = wa_cssl-objnr AND
                 wa_cosp-objnr NE wa_cssl-objnr1.
          CONTINUE.
        ENDIF.
      ENDIF.
      APPEND wa_cosp TO p_itab2.
      w_flag = 'X'.
    ENDLOOP.
  ENDLOOP.

*&-----------Filter based on Cost element group or Cost element.
*Why???
  IF p_cegrp IS INITIAL.
    IF NOT s_kstar IS INITIAL.
      LOOP AT p_itab2 INTO wa_cosp
                      WHERE kstar IN s_kstar.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
      REFRESH p_itab2.
      p_itab2[] = it_filter_ce[].
    ENDIF.
  ELSE.
    LOOP AT it_ceg_detail ASSIGNING <fs_ceg>.
      LOOP AT p_itab2 INTO wa_cosp
              WHERE kstar BETWEEN <fs_ceg>-valfrom
                          AND     <fs_ceg>-valto.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
    ENDLOOP.
    REFRESH p_itab2.
    p_itab2[] = it_filter_ce[].
  ENDIF.

ENDFORM.                    " get_plan_value
*&---------------------------------------------------------------------*
*&      Form  built_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RESULT  text
*      -->P_W_IND  text
*      -->P_0950   text
*      -->P_0951   text
*      -->P_0952   text
*      -->P_0953   text
*----------------------------------------------------------------------*
FORM built_result TABLES p_it_result STRUCTURE wa_result
                  USING  p_elemt p_wrttp
                         p_dval p_ival p_sval.

  DATA: w_total LIKE wa_cosp-wkg001.

  READ TABLE it_tckh2 INTO wa_tckh2 WITH KEY elemt = p_elemt
                                                     BINARY SEARCH
                                                     TRANSPORTING txele.
  IF sy-subrc NE 0.
    wa_result-txele = space.
  ENDIF.
  wa_result-elemt = p_elemt.
  wa_result-txele = wa_tckh2-txele.
  wa_result-wrttp = p_wrttp.
  CASE p_wrttp.
    WHEN '01'.
      wa_result-wrtxt = 'Plan'.
    WHEN '04'.
      wa_result-wrtxt = 'Actual'.
  ENDCASE.
  wa_result-dicst = p_dval.
  wa_result-incst = p_ival.
  wa_result-smcst = p_sval.
  w_total = p_dval + p_ival + p_sval.
  wa_result-tocst = w_total.
  READ TABLE it_manhr_qty INTO wa_manhr_qty WITH KEY wrttp = p_wrttp.
  IF sy-subrc EQ 0.
    wa_result-manhr_qty = wa_manhr_qty-lst001.
    IF  wa_result-manhr_qty EQ 0.
      wa_result-in_manhr  = 0.
      wa_result-sm_manhr  = 0.
      wa_result-di_manhr  = 0.
    ELSE.
      wa_result-in_manhr = p_ival / wa_manhr_qty-lst001.
      wa_result-sm_manhr = p_sval / wa_manhr_qty-lst001.
      wa_result-di_manhr = p_dval / wa_manhr_qty-lst001  .
    ENDIF.
    wa_result-to_manhr  = wa_result-in_manhr + wa_result-sm_manhr +
                          wa_result-di_manhr.
  ENDIF.
  APPEND wa_result TO p_it_result.

ENDFORM.                    " built_result
*&---------------------------------------------------------------------*
*&      Form  get_semidir_com
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COCOM1  text
*      -->P_W_PERBL  text
*----------------------------------------------------------------------*
FORM get_semidir_com TABLES p_it_cocom STRUCTURE plobjcom
                     USING p_wrttp.

  DATA: wa_objcom LIKE plobjcom,
        wa_objcom2 LIKE plobjcom.
  DATA: BEGIN OF wa_com_tot,
          objnr LIKE wa_objcom-objnr,
          skstg LIKE wa_objcom-skstg,
        END OF wa_com_tot.
  DATA: it_cal_com1 LIKE TABLE OF wa_objcom,
        it_cal_com2 LIKE TABLE OF wa_objcom,
        it_com_tot LIKE TABLE OF wa_com_tot.

  DATA: w_costcenter_value TYPE p DECIMALS 2,
        w_costcom_value    TYPE p DECIMALS 2,
        w_costcom_tot      TYPE p DECIMALS 2,
        w_cecom_value      TYPE p DECIMALS 2.

  CLEAR: w_field.

  w_perbl = p_fperbl.
  w_prd_fld = '001'.
  WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
    REFRESH: it_cal_com1,it_com_tot.
    CLEAR: w_field.
    LOOP AT p_it_cocom INTO wa_objcom
                      WHERE perio = w_perbl
                      AND   wrttp = p_wrttp.
      APPEND wa_objcom TO it_cal_com1.
    ENDLOOP.
    CLEAR: wa_objcom.
    LOOP AT it_cal_com1 INTO wa_objcom.
      wa_com_tot-objnr = wa_objcom-objnr.
      AT END OF objnr.
        SUM.
        wa_com_tot-skstg = wa_objcom-skstg.
        APPEND wa_com_tot TO it_com_tot.
      ENDAT.
    ENDLOOP.

    CONCATENATE 'wa_cosp-wkg' w_prd_fld INTO w_field.

    LOOP AT it_coks INTO wa_coks.
      CLEAR: w_costcenter_value,w_costcom_tot.
      READ TABLE it_actplan_values INTO wa_cosp
                                   WITH KEY objnr = wa_coks-objnr
                                            kstar = wa_coks-kstar
                                            wrttp = p_wrttp.
      IF sy-subrc NE 0.
        READ TABLE it_cssl INTO wa_cssl WITH KEY objnr = wa_coks-objnr
                                                      BINARY SEARCH
                                                    TRANSPORTING objnr1.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
        READ TABLE it_actplan_values INTO wa_cosp
                                     WITH KEY objnr = wa_cssl-objnr1
                                              kstar = wa_coks-kstar
                                              wrttp = p_wrttp.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
      ENDIF.
      ASSIGN (w_field) TO <fs>.
      w_costcenter_value = <fs>.
      READ TABLE it_com_tot INTO wa_com_tot
                            WITH KEY objnr = wa_coks-parob.
      IF sy-subrc EQ 0.
        w_costcom_tot = wa_com_tot-skstg.
        LOOP AT it_cal_com1 INTO wa_objcom
                            WHERE objnr = wa_coks-parob
                            AND   perio = w_perbl.
          w_costcom_value = wa_objcom-skstg.
          w_cecom_value = ( w_costcom_value / w_costcom_tot ) *
                                                   w_costcenter_value.
          wa_cost_com-elemt = wa_objcom-elemt.
          wa_cost_com-kstar = wa_cosp-kstar.
          wa_cost_com-wrttp = wa_objcom-wrttp.
          wa_cost_com-perbl = wa_objcom-perio.
          wa_cost_com-wkg001 = w_cecom_value.
          APPEND wa_cost_com TO it_cost_com.
          CLEAR: w_costcom_value, w_cecom_value,wa_cost_com.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
    w_perbl = w_perbl + 1.
    w_prd_fld = w_prd_fld + 1.
  ENDWHILE.

ENDFORM.                    " get_semidir_com
*&---------------------------------------------------------------------*
*&      Form  filter_cosl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_cosl.
  DATA: w_manhr_qty LIKE cosl-lst001,
        w_round_qty TYPE p DECIMALS 1.
  FIELD-SYMBOLS <fs_mqty> LIKE LINE OF it_manhr_qty.

  w_idx = 1.
  CLEAR w_flag.
  LOOP AT it_cssl INTO wa_cssl
                  WHERE lstar = 'MAN_HR'.
    CLEAR w_flag.
    LOOP AT it_cosl INTO wa_cosl FROM w_idx.
      IF wa_cosl-objnr <> wa_cssl-objnr.
        w_idx = sy-tabix.
        IF w_flag EQ 'X'.
          EXIT.
        ENDIF.
      ELSE.
        APPEND wa_cosl TO it_cosl1.
        w_flag = 'X'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  LOOP AT it_cosl1 INTO wa_cosl.
    w_perbl = p_fperbl.
    w_prd_fld = '001'.
    CLEAR w_manhr_qty.
    wa_manhr_qty-wrttp = wa_cosl-wrttp.
    WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
      CONCATENATE 'wa_cosl-lst' w_prd_fld INTO w_field1.
      ASSIGN (w_field1) TO <fs1>.
      w_manhr_qty = <fs1>.
      wa_manhr_qty-lst001 = w_manhr_qty.
      COLLECT wa_manhr_qty INTO it_manhr_qty.
      w_perbl = w_perbl + 1.
      w_prd_fld = w_prd_fld + 1.
    ENDWHILE.
  ENDLOOP.
  LOOP AT it_manhr_qty ASSIGNING <fs_mqty>.
    w_round_qty = <fs_mqty>-lst001.
    <fs_mqty>-lst001 = w_round_qty.
  ENDLOOP.

ENDFORM.                    " filter_cosl
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
*&      Form  get_semidirect_costcenters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_semidirect_costcenters.
  DATA: w_nlin TYPE i.

  SELECT t1~kokrs t3~kostl kstar t2~objnr
                     INTO TABLE it_cokl
                     FROM cskb AS t1
                     INNER JOIN cokl AS t2
                     ON t2~vksta = t1~kstar
                     INNER JOIN cssl AS t3
                     ON t3~objnr = t2~objnr
                     AND t3~gjahr = t2~gjahr
                     WHERE t1~kokrs = p_kokrs
                     AND   t2~gjahr = p_gjahr
                     AND   versn = p_versn
                     AND   katyp = '43'.

  CLEAR wa_cokl.

  IF NOT s_kstar IS INITIAL.
    LOOP AT it_cokl INTO wa_cokl
                     WHERE kstar IN s_kstar.
      APPEND wa_cokl TO it_cokl1.
    ENDLOOP.
  ELSE.
    LOOP AT it_ceg_detail INTO wa_group_detail.
      LOOP AT it_cokl INTO wa_cokl
               WHERE kstar
              BETWEEN wa_group_detail-valfrom AND wa_group_detail-valto.
        APPEND wa_cokl TO it_cokl1.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
  REFRESH it_cokl.
  it_cokl[] = it_cokl1.
  FREE: it_cokl1.
*&-------To get the relevant direct cost center.
  DESCRIBE TABLE it_cokl LINES w_nlin.
  IF w_nlin EQ 0.
  ELSE.
*...secondary planning (SAP include LTP)
    SELECT objnr kstar parob FROM coks
                             INTO TABLE it_coks
                             FOR ALL ENTRIES IN it_cokl
                             WHERE gjahr = p_gjahr
                             AND   wrttp = '01'
                             AND   versn = p_versn
                             AND   kstar = it_cokl-kstar
                             AND   parob = it_cokl-objnr
                             AND NOT objnr LIKE 'A%'.
  ENDIF.
ENDFORM.                    " get_semidirect_costcenters
*&---------------------------------------------------------------------*
*&      Form  get_group_details
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_group_details TABLES p_itab STRUCTURE wa_group_detail
                       USING  p_grpnam.

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
*&      Form  format_fieldcatlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM format_layout_fieldcatlog.

*&------------Sort and Subtotal.
  wa_sortinfo-fieldname = 'WRTXT'.
  wa_sortinfo-tabname   = 'WA_RESULT'.
  wa_sortinfo-down       = 'X'.
  wa_sortinfo-subtot    = 'X'.
  APPEND wa_sortinfo TO it_sortinfo.
*&----------Group.
  wa_group-sp_group = 'OHC'.
  wa_group-text     = 'Overhead Cost'.
  APPEND wa_group TO it_group.
  wa_group-sp_group = 'OHCR'.
  wa_group-text     = 'Overhead Cost Rate'.
  APPEND wa_group TO it_group.
*&-------Layout
  wa_layout1-group_buttons = 'X'.
  wa_layout1-colwidth_optimize = 'X'.
  wa_layout1-no_totalline   = 'X'.
*&-------Print.
  wa_print-prnt_title  = 'X'.
  wa_print-prnt_info   = 'X'.

*&--------------Modify Field catalog information.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-key = 'X'.
  wa_fieldcat1-no_out = 'X'.
  MODIFY it_fieldcat FROM wa_fieldcat1 TRANSPORTING key no_out
                     WHERE fieldname = 'WRTTP'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-key = 'X'.
  wa_fieldcat1-seltext_m = 'Plan/Actual'.
  wa_fieldcat1-ddictxt   = 'M'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING key seltext_m ddictxt
                     WHERE fieldname = 'WRTXT'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'In-Direct'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHC'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'INCST'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_m = 'Semi-Direct'.
  wa_fieldcat1-ddictxt   = 'M'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHC'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_m ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'SMCST'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'Direct'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHC'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'DICST'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'Total'.
  wa_fieldcat1-ddictxt   = 'S'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                     WHERE fieldname = 'TOCST'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-seltext_s = 'MAN_HR'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 1.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING seltext_s ddictxt decimals_out
                                  quantity qfieldname qtabname datatype
                     WHERE fieldname = 'MANHR_QTY'
                     AND   tabname    = 'WA_RESULT'.

  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'In-Direct'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHCR'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'IN_MANHR'
                     AND   tabname    = 'WA_RESULT'.

  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_m = 'Semi-Direct'.
  wa_fieldcat1-ddictxt   = 'M'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHCR'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_m ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'SM_MANHR'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'Direct'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 2.
  wa_fieldcat1-sp_group = 'OHCR'.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                                  sp_group
                     WHERE fieldname = 'DI_MANHR'
                     AND   tabname    = 'WA_RESULT'.
  CLEAR wa_fieldcat1.
  wa_fieldcat1-do_sum    = 'X'.
  wa_fieldcat1-seltext_s = 'Total'.
  wa_fieldcat1-ddictxt   = 'S'.
  wa_fieldcat1-decimals_out = 2.
  MODIFY it_fieldcat FROM wa_fieldcat1
                     TRANSPORTING do_sum seltext_s ddictxt decimals_out
                     WHERE fieldname = 'TO_MANHR'
                     AND   tabname    = 'WA_RESULT'.


ENDFORM.                    " format_fieldcatlog
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
     i_bypassing_buffer                = 'X'
*   I_BUFFER_ACTIVE                   = ' '
     i_callback_program                = 'ZRCOCMHS'
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
    i_callback_top_of_page            = 'TOP_OF_PAGE'
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
     i_structure_name                  = 'WA_RESULT'
*   I_BACKGROUND_ID                   = ' '
*    I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
     is_layout                         = wa_layout1
     it_fieldcat                       = it_fieldcat[]
*   IT_EXCLUDING                      =
   it_special_groups                 = it_group
    it_sort                           = it_sortinfo
*   IT_FILTER                         =
*    IS_SEL_HIDE                       = wa_selhide
*   I_DEFAULT                         = 'X'
     i_save                            = 'A'
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
    is_print                          = wa_print
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
*   IT_EXCEPT_QINFO                   =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_result[]
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  filter_selections
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_selections.

*&----Direct values from COSP (601xxx - 609xxx)
  LOOP AT it_direct_cegrp INTO wa_direct_cegrp.
    LOOP AT it_cosp INTO wa_cosp
          WHERE kstar BETWEEN wa_direct_cegrp-valfrom
                      AND     wa_direct_cegrp-valto.
      COLLECT wa_cosp INTO it_cosp1.
    ENDLOOP.
  ENDLOOP.
  REFRESH: it_cosp.
  it_cosp[] = it_cosp1[].
  REFRESH: it_cosp1.

*Indirect / Semidirect from COSS
  LOOP AT it_sel_coss INTO wa_sel_coss.
    MOVE-CORRESPONDING wa_sel_coss TO wa_coss.
    COLLECT wa_coss INTO it_coss.
  ENDLOOP.


*&-----Change '02' -> '01' for uniformity.
  wa_coss-wrttp = '01'.
  MODIFY it_coss FROM wa_coss TRANSPORTING wrttp WHERE wrttp = '02'.
  CLEAR wa_coss.

ENDFORM.                  " filter_selections
*&---------------------------------------------------------------------*
*&      Form  read_ce_group
*&---------------------------------------------------------------------*
FORM read_ce_group.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.
  DATA: l_indh LIKE sy-tabix,
        l_indv LIKE sy-tabix.

  IF p_cegrp = space.

  ELSE.
    CALL FUNCTION 'G_SET_LIST_SELECT'
         EXPORTING
              setclass      = '0102'
              shortname     = p_cegrp
              kokrs         = 'H201'
              ktopl         = 'HNA1'
         TABLES
              matching_sets = t_setlist.
    IF t_setlist[] IS INITIAL.
      MESSAGE e002(sy) WITH 'Cost element group does not exist'.
      EXIT.
    ELSE.
      READ TABLE t_setlist INDEX 1.
    ENDIF.

* TRANSFER THE VALUE TO CE RANGE.
    s_kstar-sign = 'I'.
    s_kstar-option = 'BT'.
    LOOP AT t_setvalues.
      s_kstar-low  = t_setvalues-from.
      s_kstar-high = t_setvalues-to.
      APPEND s_kstar.
    ENDLOOP.
    CLEAR s_kstar.
  ENDIF.

ENDFORM.                    " read_ce_group
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
FORM get_master_info.

  IF p_ccgrp IS INITIAL.
*&---------Get cost center information.
*    SELECT kokrs kostl objnr INTO TABLE it_csks
*                             FROM csks
*                             WHERE kokrs = p_kokrs
*                             AND   kostl IN s_kostl.

    SELECT t2~kokrs t2~kostl t2~objnr t1~objnr
                             INTO TABLE it_csks
                             FROM cssl AS t1
                             INNER JOIN csks AS t2
                                ON t1~kokrs = t2~kokrs
                               AND t1~kostl = t2~kostl
                             WHERE t2~kokrs = p_kokrs
                               AND t1~gjahr = p_gjahr
                               AND t2~kostl IN s_kostl.

    IF sy-subrc NE 0.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-006.
      EXIT.
    ENDIF.
    PERFORM get_costcenter_control_data.
  ELSE.
*&----------Get cost center group information.
    CALL FUNCTION 'BAPI_COSTCENTERGROUP_GETDETAIL'
         EXPORTING
              controllingarea = p_kokrs
              groupname       = p_ccgrp
         IMPORTING
              return          = wa_return1
         TABLES
              hierarchynodes  = it_hnodes
              hierarchyvalues = it_hvalues.
    IF wa_return1-type = 'E'.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-005.
      EXIT.
    ENDIF.

    RANGES: r_kostl FOR csks-kostl.
    DATA: w_hvalues LIKE it_hvalues.
    REFRESH r_kostl.
    r_kostl-option = 'BT'. r_kostl-sign = 'I'.
    LOOP AT it_hvalues INTO w_hvalues.
      r_kostl-low  = w_hvalues-valfrom.
      r_kostl-high = w_hvalues-valto.
      APPEND r_kostl.
    ENDLOOP.

*&---------Get cost center information.(FIXME)
*    SELECT kokrs kostl objnr INTO TABLE it_csks
*                             FROM csks
*                             WHERE kokrs = p_kokrs.
    SELECT t2~kokrs t2~kostl t2~objnr t1~objnr
                             INTO TABLE it_csks
                             FROM cssl AS t1
                             INNER JOIN csks AS t2
                                ON t1~kokrs = t2~kokrs
                               AND t1~kostl = t2~kostl
                             WHERE t2~kokrs = p_kokrs
                               AND t1~gjahr = p_gjahr
                               AND t2~kostl IN r_kostl.


    IF sy-subrc NE 0.
      MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-007.
      EXIT.
    ENDIF.
    PERFORM get_costcenter_control_data.
*&-----Get the relavent cost center only.
    LOOP AT it_hvalues INTO wa_hvalues.
      LOOP AT it_csks INTO wa_csks
            WHERE kostl BETWEEN wa_hvalues-valfrom AND wa_hvalues-valto.
        APPEND wa_csks TO it_csks1.
      ENDLOOP.
      CLEAR wa_csks.
    ENDLOOP.
    REFRESH: it_csks,it_hnodes, it_hvalues.
    CLEAR: wa_hnodes, wa_hvalues.
    it_csks[] = it_csks1[].
    FREE it_csks1.
  ENDIF.
*&---------Get cost element group information.
  CLEAR: wa_return1.
  CLEAR: w_idx,w_cnt1,w_cnt2,wa_hnodes,wa_hvalues.
*  if p_cegrp is initial.
  PERFORM get_group_details TABLES it_ceg_detail USING c_cc_grp.
*  else.
*    perform get_group_details tables it_ceg_detail using p_cegrp.
*  endif.
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
  IF sy-subrc NE 0.
    MESSAGE ID 'ZFI' TYPE 'E' NUMBER '999' WITH text-008.
    EXIT.
  ENDIF.
*&------Get Semi-Direct cost center information.
  PERFORM get_semidirect_costcenters.

ENDFORM.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  get_plan_actual_values
*&---------------------------------------------------------------------*
FORM get_plan_actual_values.
  RANGES: r_vrgng FOR coss-vrgng.
  REFRESH r_vrgng.
  r_vrgng-option = 'EQ'. r_vrgng-sign = 'I'.

  r_vrgng-low = 'RKP6'. APPEND r_vrgng. "Plan - Direct
  r_vrgng-low = 'KSP1'. APPEND r_vrgng. "COSS
  r_vrgng-low = 'KSP2'. APPEND r_vrgng. "COSS
  r_vrgng-low = 'RKP7'. APPEND r_vrgng. "Semi-direct




*&-----------Get plan and actual values ->for direct
  SELECT (it_selfld)
     INTO TABLE it_cosp
     FROM cosp
     FOR ALL ENTRIES IN it_csks
     WHERE gjahr = p_gjahr
       AND ( wrttp = '04' OR wrttp = '03'
        OR ( versn = p_versn AND vrgng IN r_vrgng ) )
       AND kstar IN s_kstar
       AND ( objnr = it_csks-objnr OR objnr = it_csks-objnr2 )
     GROUP by objnr wrttp vrgng kstar.
*&----------Get plan and actual values -> for Indirect & Semi direct.
  SELECT (it_selfld2)
     INTO TABLE it_sel_coss
     FROM coss
     FOR ALL ENTRIES IN it_csks
     WHERE gjahr = p_gjahr
       AND ( wrttp = '04' OR wrttp = '03'
        OR ( versn = p_versn AND vrgng IN r_vrgng ) )
       AND kstar IN s_kstar
       AND ( objnr = it_csks-objnr OR objnr = it_csks-objnr2 )
     GROUP BY objnr wrttp vrgng kstar parob.
*&-----------Get Activity Quantity -> for cost centers.
  SELECT (it_selfld1)
     INTO TABLE it_cosl
     FROM cosl
     FOR ALL ENTRIES IN it_csks
     WHERE gjahr = p_gjahr
       AND ( wrttp = '04'
        OR ( wrttp = '01' AND versn = p_versn ) )
       AND ( objnr = it_csks-objnr OR objnr = it_csks-objnr2 )
     GROUP by objnr wrttp.

ENDFORM.                    " get_plan_actual_values
*&---------------------------------------------------------------------*
*&      Form  filter_cssl
*&---------------------------------------------------------------------*
FORM filter_cssl.
*&----------Filter CSSL.
  SORT: it_csks BY kostl,
        it_cssl BY kostl,
        it_cosl BY objnr.
  w_idx = 1.
  CLEAR w_flag.
  LOOP AT it_csks INTO wa_csks.
    CLEAR wa_cssl.
    LOOP AT it_cssl INTO wa_cssl FROM w_idx.
      IF wa_cssl-kostl <> wa_csks-kostl.
        w_idx = sy-tabix.
        IF w_flag EQ 'X'.
          EXIT.
        ENDIF.
      ELSE.
        APPEND wa_cssl TO it_cssl1.
        w_flag = 'X'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  REFRESH it_cssl.
  it_cssl[] = it_cssl1[].
  FREE it_cssl1.

ENDFORM.                    " filter_cssl
*&---------------------------------------------------------------------*
*&      Form  display_output
*&---------------------------------------------------------------------*
FORM display_output.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
     i_program_name               = 'ZRCOCMHS'
     i_internal_tabname           = 'WA_RESULT'
*     I_STRUCTURE_NAME             =
*     I_CLIENT_NEVER_DISPLAY       = 'X'
     i_inclname                   = 'ZRCOCMHS'
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE              =
    CHANGING
      ct_fieldcat                  = it_fieldcat
   EXCEPTIONS
     inconsistent_interface       = 1
     program_error                = 2
     OTHERS                       = 3 .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  PERFORM format_layout_fieldcatlog.
  PERFORM display_alv.

ENDFORM.                    " display_output
*&---------------------------------------------------------------------*
*&      Form  build_cc_semi
*&---------------------------------------------------------------------*
FORM build_cc_semi.
*&-----Get cost components for Semi-Direct.
  SORT it_coks BY objnr kstar parob.

*&--------------------------------------------------------------------&*
*&                           IMPORTANT                                &*
*&                                                                    &*
* Since in '2004', costcenter '55001' is used for both 'MXBX' and 'MXSX'
* cost center group and then later on changed to use only for 'MXSX' we
* need to disregard '55001' for 'MXBX' in calculation. Also we have to
* use only half the cost element amount for component calculation.
* The same applies for '55005' with respect to 'MXTX' and 'MXEX'.
*&--------------------------------------------------------------------&*
  IF p_gjahr = '2004'.
    DELETE it_coks WHERE objnr+6(4) = 'MXBX' AND parob+11(5) = '55001'
                   OR    objnr+6(4) = 'MXTX' AND parob+11(5) = '55005'.
  ENDIF.


  LOOP AT it_cokl INTO wa_cokl.
    REFRESH: it_cocom1, it_cocom2.
    CLEAR: wa_cost_com,w_perbl.
    w_perbl = p_fperbl.
    PERFORM get_components TABLES it_cocom1 USING p_gjahr
                                                  p_versn
                                                  '01'
                                                 wa_cokl-objnr.

    PERFORM get_components TABLES it_cocom2 USING p_gjahr
                                                  p_versn
                                                  '04'
                                                  wa_cokl-objnr.
    CLEAR wa_plobjcom.
    WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
      LOOP AT it_cocom1 INTO wa_plobjcom
                        WHERE perio = w_perbl.
        APPEND wa_plobjcom TO it_cal_com.
      ENDLOOP.
      CLEAR wa_plobjcom.
      LOOP AT it_cocom2 INTO wa_plobjcom
                        WHERE perio = w_perbl.
        APPEND wa_plobjcom TO it_cal_com.
      ENDLOOP.
      w_perbl = w_perbl + 1.
    ENDWHILE.
  ENDLOOP.
  SORT it_cal_com BY objnr elemt wrttp perio.
  PERFORM get_semidir_com TABLES it_cal_com USING '01'.
  PERFORM get_semidir_com TABLES it_cal_com USING '04'.


ENDFORM.                    " build_cc_semi
*&---------------------------------------------------------------------*
*&      Form  build_cc_info
*&---------------------------------------------------------------------*
FORM build_cc_info.

*&-----Build information based on cost component(-> Direct & In-Direct)
  LOOP AT it_tckh2 INTO wa_tckh2.
    LOOP AT it_actplan_values INTO wa_cosp
            WHERE kstar BETWEEN wa_tckh2-kstav AND wa_tckh2-kstab.
      CLEAR: w_prd_fld, w_field,w_perbl,wa_cost_com.
      wa_cost_com-elemt = wa_tckh2-elemt.
      wa_cost_com-kstar = wa_cosp-kstar.
      wa_cost_com-wrttp = wa_cosp-wrttp.
      w_perbl = p_fperbl.
      WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
        w_prd_fld  = w_prd_fld  + 1.
        CONCATENATE 'wa_cosp-wkg' w_prd_fld INTO w_field.
        ASSIGN (w_field) TO <fs>.
        wa_cost_com-perbl = w_perbl.
        wa_cost_com-wkg001 = <fs>.
        COLLECT wa_cost_com INTO it_cost_com.
        CLEAR w_field.
        w_perbl = w_perbl + 1.
      ENDWHILE.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " build_cc_info
*&---------------------------------------------------------------------*
*&      Form  delete_ce_fr_coss
*&---------------------------------------------------------------------*
* Actual
FORM delete_ce_fr_coss.
  CLEAR: wa_cosp.
  LOOP AT it_cosp1 INTO wa_cosp.
    DELETE it_coss1 WHERE objnr = wa_cosp-objnr
                      AND kstar = wa_cosp-kstar.
  ENDLOOP.

ENDFORM.                    " delete_ce_fr_coss
*&---------------------------------------------------------------------*
*&      Form  assign_grp_name
*&---------------------------------------------------------------------*
FORM assign_grp_name.
*&----Assign group name for the result.
  LOOP AT it_ceg_detail INTO wa_group_detail.
    LOOP AT it_cost_com INTO wa_cost_com
        WHERE kstar
              BETWEEN wa_group_detail-valfrom AND wa_group_detail-valto.
      wa_result1-gname = wa_group_detail-gname.
      wa_result1-elemt = wa_cost_com-elemt.
      wa_result1-wrttp = wa_cost_com-wrttp.
      wa_result1-skstg = wa_cost_com-wkg001.
      COLLECT wa_result1 INTO it_result1.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " assign_grp_name
*&---------------------------------------------------------------------*
*&      Form  build_group_info
*&---------------------------------------------------------------------*
FORM build_group_info.

*&----Group the information as(Direct,Semi-direct,In-direct)
  SORT : it_result1 BY gname elemt wrttp,
         it_tckh2 BY elemt.
  LOOP AT it_result1 INTO wa_result1.
    CLEAR: wa_result.
    CASE wa_result1-gname.
      WHEN 'DIRECT'.
        PERFORM built_result TABLES it_result
                             USING wa_result1-elemt
                              wa_result1-wrttp wa_result1-skstg '0' '0'.
      WHEN 'INDIR'.
  READ TABLE it_result INTO wa_result WITH KEY elemt = wa_result1-elemt
                                               wrttp = wa_result1-wrttp.
        IF sy-subrc NE 0.
          PERFORM built_result TABLES it_result
                               USING wa_result1-elemt
                              wa_result1-wrttp '0' wa_result1-skstg '0'.
        ELSE.
          wa_result-incst = wa_result1-skstg.
          wa_result-tocst = wa_result-tocst + wa_result1-skstg.
          READ TABLE it_manhr_qty INTO wa_manhr_qty
                                  WITH KEY wrttp = wa_result1-wrttp.
          IF sy-subrc NE 0.
            wa_result-manhr_qty = 0.
            wa_result-in_manhr  = 0.
          ELSE.
            wa_result-manhr_qty = wa_manhr_qty-lst001.
            IF wa_manhr_qty-lst001 = 0.
              wa_result-in_manhr  = 0.
            ELSE.
          wa_result-in_manhr =  wa_result1-skstg / wa_manhr_qty-lst001 .
            ENDIF.
          ENDIF.
          wa_result-to_manhr = wa_result-to_manhr + wa_result-in_manhr.
          MODIFY it_result FROM wa_result
                   TRANSPORTING incst tocst manhr_qty in_manhr to_manhr
                           WHERE elemt = wa_result1-elemt
                           AND   wrttp = wa_result1-wrttp.
        ENDIF.
      WHEN 'SEMIDIR'.
  READ TABLE it_result INTO wa_result WITH KEY elemt = wa_result1-elemt
                                               wrttp = wa_result1-wrttp.
        IF sy-subrc NE 0.
          PERFORM built_result TABLES it_result
                               USING wa_result1-elemt
                              wa_result1-wrttp '0' '0' wa_result1-skstg.
        ELSE.
          wa_result-smcst = wa_result1-skstg.
          wa_result-tocst = wa_result-tocst + wa_result1-skstg.
          READ TABLE it_manhr_qty INTO wa_manhr_qty
                                  WITH KEY wrttp = wa_result1-wrttp.
          IF sy-subrc NE 0.
            wa_result-manhr_qty = 0.
            wa_result-sm_manhr =  0.
          ELSE.
            wa_result-manhr_qty = wa_manhr_qty-lst001.
            IF wa_manhr_qty-lst001 = 0.
              wa_result-sm_manhr  = 0.
            ELSE.
            wa_result-sm_manhr = wa_result1-skstg / wa_manhr_qty-lst001.
            ENDIF.
          ENDIF.
          wa_result-to_manhr = wa_result-to_manhr + wa_result-sm_manhr.
          MODIFY it_result FROM wa_result
                   TRANSPORTING smcst tocst manhr_qty sm_manhr to_manhr
                           WHERE elemt = wa_result1-elemt
                           AND   wrttp = wa_result1-wrttp.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " build_group_info
*&---------------------------------------------------------------------*
*&      Form  get_plan_value1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COSS  text
*      -->P_IT_COSS1  text
*----------------------------------------------------------------------*
FORM get_plan_value1 TABLES   p_itab1 STRUCTURE wa_cosp
                              p_itab2 STRUCTURE wa_cosp.
  DATA w_nline TYPE i.
  DATA: it_filter_ce LIKE TABLE OF wa_cosp.
  FIELD-SYMBOLS: <fs_ceg> LIKE LINE OF it_ceg_detail.

  DESCRIBE TABLE p_itab1 LINES w_nline.
  w_idx = 1.
  CLEAR w_flag.
  LOOP AT it_cssl INTO wa_cssl.
    LOOP AT p_itab1 INTO wa_cosp
                    WHERE ( objnr = wa_cssl-objnr AND wrttp = '01' )
                    OR    ( parob = wa_cssl-objnr AND wrttp = '01' ).
*                    or    ( objnr = wa_cssl-objnr1 AND wrttp = '01' ).
*.... exclude CC -> CC+AT
      IF NOT wa_cosp-parob IS INITIAL.
        IF wa_cosp-parob = wa_cssl-objnr AND
                 wa_cosp-objnr NE wa_cssl-objnr1.
          CONTINUE.
        ENDIF.
      ENDIF.
      APPEND wa_cosp TO p_itab2.
      w_flag = 'X'.
    ENDLOOP.
  ENDLOOP.

*&-----------Filter based on Cost element group or Cost element.
*Why???
  IF p_cegrp IS INITIAL.
    IF NOT s_kstar IS INITIAL.
      LOOP AT p_itab2 INTO wa_cosp
                      WHERE kstar IN s_kstar.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
      REFRESH p_itab2.
      p_itab2[] = it_filter_ce[].
    ENDIF.
  ELSE.
    LOOP AT it_ceg_detail ASSIGNING <fs_ceg>.
      LOOP AT p_itab2 INTO wa_cosp
              WHERE kstar BETWEEN <fs_ceg>-valfrom
                          AND     <fs_ceg>-valto.
        APPEND wa_cosp TO it_filter_ce.
      ENDLOOP.
    ENDLOOP.
    REFRESH p_itab2.
    p_itab2[] = it_filter_ce[].
  ENDIF.

ENDFORM.                    " get_plan_value
*&---------------------------------------------------------------------*
*&      Form  create_selection_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_selection_field.

  w_perbl = p_fperbl.

  wa_selfld = 'objnr'.
  APPEND: wa_selfld TO it_selfld, wa_selfld TO it_selfld2.

  wa_selfld = 'wrttp'.
  APPEND: wa_selfld TO it_selfld, wa_selfld TO it_selfld2.

  wa_selfld = 'VRGNG'.
  APPEND: wa_selfld TO it_selfld, wa_selfld TO it_selfld2.

  wa_selfld = 'kstar'.
  APPEND: wa_selfld TO it_selfld, wa_selfld TO it_selfld2.

  wa_selfld = 'parob'. APPEND wa_selfld TO it_selfld2.
*  wa_selfld = 'vrgng'. APPEND wa_selfld TO it_selfld2.

  WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
    CONCATENATE 'sum( wkg' w_perbl ' )' INTO wa_selfld.
    APPEND wa_selfld TO it_selfld.
    APPEND wa_selfld TO it_selfld2.
    CLEAR wa_selfld.
    w_perbl = w_perbl + 1 .
  ENDWHILE.

  CLEAR wa_selfld.
  w_perbl = p_fperbl.
  wa_selfld = 'objnr'. APPEND wa_selfld TO it_selfld1.
  wa_selfld = 'wrttp'. APPEND wa_selfld TO it_selfld1.
  WHILE w_perbl BETWEEN p_fperbl AND p_tperbl.
    CONCATENATE 'sum( lst' w_perbl ' )' INTO wa_selfld.
    APPEND wa_selfld TO it_selfld1.
    CLEAR wa_selfld.
    w_perbl = w_perbl + 1 .
  ENDWHILE.

  PERFORM read_ce_group.


ENDFORM.                    " get_plan_value1
