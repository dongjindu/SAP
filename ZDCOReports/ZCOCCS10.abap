*
* developed by Andy Choi
*
* RKPLNC34    Two COSL records exist: Correction report
*
report zcoccs10 message-id zmco.

tables: ionra, covja .

parameters: p_kokrs like csks-kokrs  memory id cac  obligatory.
parameters: p_bdatj like keko-bdatj  memory id bdtj obligatory.

*PARAMETERS: p_perab LIKE covja-perab MEMORY ID vpe
*                                     MODIF ID per OBLIGATORY.

select-options: s_perab for covja-perab memory id vpe
                                     modif id per obligatory.


parameters : p_wrttp like cosl-wrttp   default '04' obligatory,
             p_versn like coomco-versn.
*             p_tvers LIKE keko-tvers   DEFAULT '01'   MODIF ID div
*                                       NO-DISPLAY.
parameters : p_elehk like tckh4-elehk  default 'H1'   modif id div
                                       no-display.
parameters : p_unit as checkbox.

select-options: s_kostl for ionra-kostl,
                s_lstar for ionra-lstar.


tables: tka01, tckh4, keph.

* For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.
*BAPI plan activity
data: i_plan_act         like standard table of bapi0012_actquantities
                         with header line.

data : begin  of it_cctr  occurs 0,
        kostl like csks-kostl.
data : end    of it_cctr.

data : begin of it_coomco occurs 0.
        include structure coomco.
data :  poper    like ccss-buper,
        from_per like cobk-perab,
        to_per   like cobk-perab,
        kostl    like csks-kostl,
        lstar    like csla-lstar.
data : end of it_coomco.

data: i_cosl like cosl occurs 0 with header line.


data : begin of it_koat_p occurs 0.
data :  gjahr    like ccss-gjahr,
        poper    like ccss-buper,
        kostl    like csks-kostl,
        lstar    like csla-lstar,
        elemt    like kkb_split-elemt,
        w000     like kkb_split-w000 ,
        waers    like kkb_split-waers,
        total    like kkb_split-w000 ,
        cp_%(16) type p decimals 7.
data : end of it_koat_p.

data : begin of it_kostl_lstar_pct occurs 0.
data :  from_per like cobk-perab,
        to_per   like cobk-perab,
        objnr    like coomco-objnr,
        kadky    like sy-datum ,
        bidat    like sy-datum .
        include structure it_koat_p.
data : end of it_kostl_lstar_pct.

* Temp. Table for Main ITAB - Press / Engine
data : begin of it_tmp_pe occurs 0,
        llv_matnr like mara-matnr ,
        werks     like t001w-werks,
        bwkey     like mbew-bwkey ,
        bwtar     like mbew-bwtar .
data : end of   it_tmp_pe .

ranges: r_objnr for cosl-objnr.

constants : c_gp_kostl_e(15)    value 'HMMA-SHOP',
            c_gp_direct(15)     value 'DIRECT'.

* type Pools for Variable Ratio Table
type-pools gseth .
data : it_nodes  type gseth_node_tab
                 with header line ,
       it_values type gseth_val_tab
                 with header line .

** Global Vriables
data : gv_tarkz like cokl-tarkz.  "price indicator

data : begin of itab occurs 0,
        kostl    like csks-kostl,
        lstar    like csla-lstar,
        act_unit type co_meinh_l,
        from_per like cobk-perab,
        to_per   like cobk-perab,
        w000     type kstbmt, "LIKE kkb_split-w000 ,
        w010     type kstbmt, "LIKE kkb_split-w000 ,
        w020     type kstbmt, "LIKE kkb_split-w000 ,
        w030     type kstbmt, "LIKE kkb_split-w000 ,
        w040     type kstbmt, "LIKE kkb_split-w000 ,
        w050     type kstbmt, "LIKE kkb_split-w000 ,
        w060     type kstbmt, "LIKE kkb_split-w000 ,
        w070     type kstbmt, "LIKE kkb_split-w000 ,
        w080     type kstbmt, "LIKE kkb_split-w000 ,
        w090     type kstbmt, "LIKE kkb_split-w000 ,
        w100     type kstbmt, "LIKE kkb_split-w000 ,
        w110     type kstbmt, "LIKE kkb_split-w000 ,
        w120     type kstbmt, "LIKE kkb_split-w000 ,
        w130     type kstbmt, "LIKE kkb_split-w000 ,
        w140     type kstbmt, "LIKE kkb_split-w000 ,
        w150     type kstbmt, "LIKE kkb_split-w000 ,
        w160     type kstbmt, "LIKE kkb_split-w000 ,
        w170     type kstbmt, "LIKE kkb_split-w000 ,
* by ig.moon {
        perab    like cobk-perab,
* }
end of itab.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.
*---- ALV

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

* by ig.moon {
  data p_perab like covja-perab.
* }

  do 12 times.
    p_perab = sy-index.

    check p_perab in s_perab.

* Controlling Area Information
    perform read_tka01.
* Read CCTRs in SHOP
    perform read_cctr_in_shop.
* Read Active Cost Component Structure
    perform read_ccs_cust.
* Read Activity Data(plan/actual)
    perform read_activity_data.
* Read DATA From COOMCO
    perform read_data_from_coomco.
* Read Component Values - KSBT
    perform read_comp_value_ksbt.
** Cal. Rate
    perform cal_percent_ksbt.
* CALC UNIT PRICE
    perform calc_unit_rate.

  enddo.

*---------------------------------------------------------------
end-of-selection.


  perform display_out.

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
form read_tka01.
  clear tka01.
  select single * from tka01
                 where kokrs = p_kokrs.
  if sy-subrc <> 0.
    message e038 with p_kokrs.
  endif.
endform.                    " Read_TKA01
*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*       Read CCtrs linked to SHOP
*----------------------------------------------------------------------*
form read_cctr_in_shop.


* Set Validity Date (Start)
  data : lv_datum like sy-datum.
* Get First Date (From-Period)
  call function 'FIRST_DAY_IN_PERIOD_GET'
    exporting
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_perab
    importing
      e_date               = lv_datum
    exceptions
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      others               = 4.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* Read CCtrs
  clear : it_costcenterlist, it_costcenterlist[].
  clear : it_return, it_return[].

  call function 'BAPI_COSTCENTER_GETLIST1'
       exporting
            controllingarea = p_kokrs
            date_from       = lv_datum
            costcentergroup = c_gp_kostl_e
       tables
            costcenterlist  = it_costcenterlist
            return          = it_return.

  if it_costcenterlist[] is initial.
    message e080 with c_gp_kostl_e p_kokrs lv_datum.
  endif.

* Read SHOP (Linkage bwtween CCtrs and Shops)
* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA-SHOP'
  perform read_hi_fr_setid(saplzgco_global_form)
                            tables it_nodes
                                   it_values
                            using  p_kokrs
                                   '0101'
                                   c_gp_kostl_e.
  clear : it_cctr , it_cctr[].

  loop at it_costcenterlist.
    clear it_values.
    loop at it_values where vfrom =< it_costcenterlist-costcenter
                        and vto   => it_costcenterlist-costcenter.
    endloop.
    if sy-subrc = 0.
      loop at it_nodes where setid  = it_values-setid.
        it_cctr-kostl =  it_costcenterlist-costcenter.
        append  it_cctr.
        clear   it_cctr.
      endloop.
    endif.
    clear it_costcenterlist.
  endloop.

endform.                    " READ_CCTR_IN_SHOP
*&---------------------------------------------------------------------*
*&      Form  READ_CCS_CUST
*&---------------------------------------------------------------------*
form read_ccs_cust.
  clear tckh4.
  select single * into corresponding fields of tckh4
                  from tckh4
                 where elehk = p_elehk
                   and aktiv = 'X'.
  if tckh4 is initial.
    message e000 with text-250.
  endif.


  gv_tarkz = '001'.   "price indicator
  if p_wrttp = '04'.
    p_versn = '000'.
  endif.

  if p_versn is initial.
    p_versn = '000'.
  endif.

endform.                    " READ_CCS_CUST
*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FROM_COOMCO
*&---------------------------------------------------------------------*
*       Read DATA From COOMCO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data_from_coomco.

* KSBT dose not have data by period in case of (BP/Qarter) planning
* Select data with date range

* Local Data Definition
  data : lv_percount like cosp-perbl. "Period Counter
  data : lv_cnt  like  cosp-perbl.
  data : lv_kadky like coomco-kadky.
  lv_percount = 1.

* Period Counter : Set From-Period .
  clear lv_cnt.
  lv_cnt = p_perab .

* Clear
  clear : it_coomco, it_coomco[].

* From period - To period
  do lv_percount times.

* KEPH -> 'C' COKL/KEKO/KEPH
* K_KKB_SPLITTING_CONVERT
    perform get_kadky using    lv_cnt
                      changing lv_kadky.

* Comment : Functional Team could NOT find the reason
* why the Costing type and Costing Variant for specific
* version are not matched to the values on IMG
*  Cost Components for Cost of Goods Mfd is now created
*  only on PPC1/01 .

    data: l_obj(10) type c,
          l_kostl   type kostl,
          l_lstar   type lstar.
    concatenate 'KL' p_kokrs '%' into l_obj.
    refresh r_objnr.
    r_objnr-sign = 'I'.  r_objnr-option = 'EQ'.

*Plan - can overlap, but plan unit price is not changed
*with new plan price calculation.
*so, just use first data.!!! (by Andy)
*Verify it with KSBT.
*01/01/2005         02/28/2005
*03/01/2005         03/31/2005
*04/01/2005         06/30/2005 << correct
*05/01/2005         05/31/2005 << do not use it
*06/01/2005         06/30/2005 << do not use it
*07/01/2005         09/30/2005

*Actual - every month
*04    01/31/2005   01/31/2005
*04    02/28/2005   02/28/2005
*04    03/31/2005   03/31/2005
*04    04/30/2005   04/30/2005
*04    05/31/2005   05/31/2005
*04    06/30/2005   06/30/2005
    data: l_coomco like it_coomco.
    select * into corresponding fields of it_coomco
            from coomco
           where lednr = '00'
             and objnr like l_obj
             and gjahr = p_bdatj
             and versn = p_versn     "actual-000, plan-...
             and tvers = p_wrttp     "01-plan,04-actual
*            AND KLVAR = P_KLVAR
*            AND KALKA = TCK03-KALKA
*            AND BWVAR = TCK03-BWVAR
*            AND LEDNR = '00'
             and ( kadky <= lv_kadky and bidat >= lv_kadky )
             and tarkz = gv_tarkz    "price indicator
             and kkzma = space       "Additive - > X
           order by kadky ascending.

*   Object -> Kostl Key
      call function 'OBJECT_KEY_GET_KL'
           exporting
                objnr = it_coomco-objnr
           importing
                kokrs = p_kokrs
                kostl = l_kostl
                lstar = l_lstar.

      check l_kostl in s_kostl and l_lstar in s_lstar.

* check existing record...(for plan)
*      read table it_coomco into l_coomco
*          with key kostl = l_kostl
*                   lstar = l_lstar.
*      if sy-subrc = 0.
*        continue.
*      endif.

      it_coomco-kostl = l_kostl.
      it_coomco-lstar = l_lstar.
      append it_coomco.

      r_objnr-low  = it_coomco-objnr.
      collect r_objnr.

    endselect.


* Period Counter
    lv_cnt = lv_cnt + 1.
  enddo.

  sort it_coomco by
                    lednr
                    objnr
                    gjahr
                    versn
                    bzobj
                    tvers
                    kadky
                    kalnr
                    kalka
                    kkzma
                    dipa
                    bwvar
                    keart
                    kkzmm
                    kkzst
                    losfx
                    patnr.

  delete adjacent duplicates from it_coomco.

  clear : it_coomco.

*  IF it_coomco[] IS INITIAL .
*    MESSAGE e081.
*  ENDIF.
endform.                    " READ_DATA_FROM_COOMCO
*&---------------------------------------------------------------------*
*&      Form  READ_COMP_VALUE_KSBT
*&---------------------------------------------------------------------*
*       Read Coponent Values - KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_comp_value_ksbt.

  data : it_l_keph    like standard table of keph
                      with header line .
  data : it_l_kkb_split
                      like standard table of  kkb_split
                      with header line .
  data : wa_l_ckikekokey like ckikekokey .


* Clear
  clear : it_kostl_lstar_pct, it_kostl_lstar_pct[].

  loop at it_coomco.
* Move Keys
    move-corresponding it_coomco to wa_l_ckikekokey  .
* Read KEPH
    clear : it_l_keph, it_l_keph[].
    call function 'CK_F_KEKO_KEPH_DIRECT_READ'
      exporting
        f_kekokey              = wa_l_ckikekokey
        read_keko              = space
        read_keph              = 'X'
*       READ_ONLY_BUFFER       = ' '
*       READ_ONLY_DB           = ' '
*       CACHED_READ            = ' '
*       KEPH_MANDATORY         = 'X'
*     IMPORTING
*       F_KEKO                 =
      tables
        i_keph                 = it_l_keph
      exceptions
        data_not_found         = 1
        wrong_call             = 2
        others                 = 3.

    if sy-subrc <> 0.
      message e079 with it_tmp_pe-llv_matnr
                        it_tmp_pe-werks.
    endif.

* Read Keph (1 line)
    clear it_l_keph.
    clear keph.
    read table it_l_keph into keph index 1.
* Read Costs by Cost Comp.
    clear : it_l_kkb_split, it_l_kkb_split[].
    call function 'K_KKB_SPLITTING_CONVERT'
         exporting
              i_elehk     = tckh4-elehk
              i_sicht     = '01'
              i_keart     = keph-keart
              i_losfx     = keph-losfx
              i_waers     = tka01-waers
         tables
              t_keph      = it_l_keph
              t_kkb_split = it_l_kkb_split.
*ELEMT      -> Cost Component Number
*ELEMT_TEXT
*W000       -> PLAN
    loop at it_l_kkb_split.
      move-corresponding it_coomco      to it_kostl_lstar_pct.
      move-corresponding it_l_kkb_split to it_kostl_lstar_pct.
      append it_kostl_lstar_pct.
      clear  it_kostl_lstar_pct.
    endloop.
  endloop.

* Get CCTR / AT
  loop at it_kostl_lstar_pct.
*   From Period
    perform set_from_to_period
      using it_kostl_lstar_pct-kadky
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-from_per.
*   TO Period
    perform set_from_to_period
      using it_kostl_lstar_pct-bidat
            p_kokrs
            it_kostl_lstar_pct-gjahr
            it_kostl_lstar_pct-to_per.

    modify it_kostl_lstar_pct.
    clear it_kostl_lstar_pct.
  endloop.

  clear it_kostl_lstar_pct.

endform.                    " READ_COMP_VALUE_KSBT
*&---------------------------------------------------------------------*
*&      Form  SET_FROM_TO_PERIOD
*&---------------------------------------------------------------------*
form set_from_to_period using p_date
                              p_kokrs
                              p_bdatj
                              p_per.
* period (From/To)
  call function 'K_DATE_TO_PERIOD_CONVERT'
       exporting
            i_date             = p_date
            i_kokrs            = p_kokrs
       importing
            e_gjahr            = p_bdatj
            e_perio            = p_per
       exceptions
            no_period_determin = 1
            t009b_notfound     = 2
            t009_notfound      = 3
            others             = 4.
  if sy-subrc <> 0.
  endif.
endform.                    " SET_FROM_TO_PERIOD
*&---------------------------------------------------------------------*
*&      Form  CAL_PERCENT_KSBT
*&---------------------------------------------------------------------*
*       Cal. Rate KSBT
*----------------------------------------------------------------------*
form cal_percent_ksbt.

  clear : it_kostl_lstar_pct.
  clear : it_koat_p, it_koat_p[].

  data :  it_tmp_% like standard table of it_koat_p
                   with header line .

  loop at it_kostl_lstar_pct.
    move-corresponding it_kostl_lstar_pct to it_koat_p.
    move-corresponding it_kostl_lstar_pct to it_tmp_%.
    collect it_koat_p.
    clear   it_koat_p.
* Cal SUM
    clear it_tmp_%-elemt.
    collect it_tmp_%.
    clear   it_tmp_%.
    clear   it_kostl_lstar_pct.
  endloop.

  sort it_koat_p by gjahr poper kostl lstar elemt.
  sort it_tmp_%  by gjahr poper kostl lstar elemt.

  loop at it_koat_p.
    clear it_tmp_%.
    read table  it_tmp_% with key  gjahr = it_koat_p-gjahr
                                   poper = it_koat_p-poper
                                   kostl = it_koat_p-kostl
                                   lstar = it_koat_p-lstar.
* Total
    it_koat_p-total = it_tmp_%-w000.
* %
    if it_koat_p-total <> 0.
      it_koat_p-cp_% = it_koat_p-w000  /  it_koat_p-total.
    endif.
* Modify
    modify it_koat_p.
    clear it_koat_p.
  endloop.

  sort it_koat_p by gjahr poper kostl lstar elemt.

endform.                    " CAL_PERCENT_KSBT
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
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

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
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

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
form display_out.
* ==> build field category
  perform field_setting tables gt_fieldcat using :

    'PERAB'  'perd          '   '03' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',

    'KOSTL'  'CCtr          '   '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
    'LSTAR'  'AT            '   '06' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
    'ACT_UNIT' 'Unit'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
    'FROM_PER' 'From'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
    'TO_PER'   'To  '           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
    'W000'   'Total'            '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W010'   'Mat-Part-Imp  '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W020'   'Mat-Part-Dom  '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W030'   'Mat-Coil      '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W040'   'Mat-Paint     '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W050'   'Other-Material'   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W060'   'Salary & Wage '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W070'   'Supp.Sly/Wage '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W080'   'Emp Benefit   '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W090'   'Emp Exp       '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W100'   'Logistics     '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W110'   'Repair & Maint'   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W120'   'Utility       '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W130'   'Royalty       '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W140'   'Outsourcing   '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W150'   'Submat.cost   '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W160'   'Other-General '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
    'W170'   'Depreciation  '   '13' ' ' 'R'  ' '  ' '  ' '  ' '  'X'.

  g_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = itab
       exceptions
            program_error      = 1
            others             = 2.
endform.                    " display_out
*&---------------------------------------------------------------------*
*&      Form  calc_unit_rate
*&---------------------------------------------------------------------*
form calc_unit_rate.
  data: l_qty  like cosl-lst001.

  loop at it_kostl_lstar_pct.
    clear itab.
    itab-kostl    = it_kostl_lstar_pct-kostl.
    itab-lstar    = it_kostl_lstar_pct-lstar.
    itab-from_per = it_kostl_lstar_pct-from_per.
    itab-to_per   = it_kostl_lstar_pct-to_per.

    if p_unit = 'X'.
      perform get_activity_qty  changing l_qty  itab-act_unit.
    endif.

    if l_qty = 0. l_qty = 1. endif.

    case it_kostl_lstar_pct-elemt.
      when '010'.  itab-w010  = it_kostl_lstar_pct-w000 / l_qty.
      when '020'.  itab-w020  = it_kostl_lstar_pct-w000 / l_qty.
      when '030'.  itab-w030  = it_kostl_lstar_pct-w000 / l_qty.
      when '040'.  itab-w040  = it_kostl_lstar_pct-w000 / l_qty.
      when '050'.  itab-w050  = it_kostl_lstar_pct-w000 / l_qty.
      when '060'.  itab-w060  = it_kostl_lstar_pct-w000 / l_qty.
      when '070'.  itab-w070  = it_kostl_lstar_pct-w000 / l_qty.
      when '080'.  itab-w080  = it_kostl_lstar_pct-w000 / l_qty.
      when '090'.  itab-w090  = it_kostl_lstar_pct-w000 / l_qty.
      when '100'.  itab-w100  = it_kostl_lstar_pct-w000 / l_qty.
      when '110'.  itab-w110  = it_kostl_lstar_pct-w000 / l_qty.
      when '120'.  itab-w120  = it_kostl_lstar_pct-w000 / l_qty.
      when '130'.  itab-w130  = it_kostl_lstar_pct-w000 / l_qty.
      when '140'.  itab-w140  = it_kostl_lstar_pct-w000 / l_qty.
      when '150'.  itab-w150  = it_kostl_lstar_pct-w000 / l_qty.
      when '160'.  itab-w160  = it_kostl_lstar_pct-w000 / l_qty.
      when '170'.  itab-w170  = it_kostl_lstar_pct-w000 / l_qty.
    endcase.

    itab-w000  = it_kostl_lstar_pct-w000 / l_qty.
    itab-perab = p_perab.
    collect itab.
  endloop.

endform.                    " calc_unit_rate
*&---------------------------------------------------------------------*
*&      Form  get_kadky
*&---------------------------------------------------------------------*
form get_kadky using    f_cnt
               changing f_kadky.

* Actual: End date
* Plan  : First Date of Period
  clear f_kadky.
  if p_wrttp = '04'.
    call function 'LAST_DAY_IN_PERIOD_GET'
      exporting
        i_gjahr              = p_bdatj
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
        i_poper              = f_cnt
      importing
        e_date               = f_kadky
      exceptions
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        others               = 4.
  else.
    call function 'FIRST_DAY_IN_PERIOD_GET'
      exporting
        i_gjahr              = p_bdatj
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
        i_poper              = f_cnt
      importing
        e_date               = f_kadky
      exceptions
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        others               = 4.

  endif.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " get_kadky
*&---------------------------------------------------------------------*
*&      Form  get_activity_qty
*&---------------------------------------------------------------------*
form get_activity_qty changing f_qty  f_unit.
  clear f_qty.

  if p_wrttp = '04'.
    read table i_cosl with key objnr = it_kostl_lstar_pct-objnr.
    f_unit = i_cosl-meinh.

    case p_perab.
      when  01.  f_qty = i_cosl-lst001.
      when  02.  f_qty = i_cosl-lst002.
      when  03.  f_qty = i_cosl-lst003.
      when  04.  f_qty = i_cosl-lst004.
      when  05.  f_qty = i_cosl-lst005.
      when  06.  f_qty = i_cosl-lst006.
      when  07.  f_qty = i_cosl-lst007.
      when  08.  f_qty = i_cosl-lst008.
      when  09.  f_qty = i_cosl-lst009.
      when  10.  f_qty = i_cosl-lst010.
      when  11.  f_qty = i_cosl-lst011.
      when  12.  f_qty = i_cosl-lst012.
    endcase.

  else.
* plan...
    refresh i_plan_act.
    data: l_fr  like  bapi0012_6-period,
          l_to  like  bapi0012_6-period.
    l_fr = it_kostl_lstar_pct-kadky+4(2).
    l_to = it_kostl_lstar_pct-bidat+4(2).
    call function 'BAPI_CTR_GETACTIVITYQUANTITIES'
         exporting
              coarea         = p_kokrs
              fiscyear       = p_bdatj
              version        = p_versn
              costcenterfrom = it_kostl_lstar_pct-kostl
              acttypefrom    = it_kostl_lstar_pct-lstar
              periodfrom     = l_fr
              periodto       = l_to
         tables
              return         = it_return
              actquantities  = i_plan_act.


* plan exist in multi-periods
    loop at i_plan_act.
      f_qty  = f_qty + i_plan_act-act_quantity.
      f_unit = i_plan_act-act_unit.
    endloop.

  endif.
endform.                    " get_activity_qty
*&---------------------------------------------------------------------*
*&      Form  read_activity_data
*&---------------------------------------------------------------------*
form read_activity_data.
  if p_wrttp = '04'.

    refresh i_cosl.

    select * into table i_cosl from cosl
      where lednr = '00'
        and objnr in r_objnr
        and gjahr = p_bdatj
        and wrttp = p_wrttp
        and versn = p_versn.

  endif.

endform.                    " read_activity_data
