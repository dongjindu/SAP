*----------------------------------------------------------------------*
*   INCLUDE ZACO19L_F001                                               *
*----------------------------------------------------------------------*
*
*&---------------------------------------------------------------------*
*&      Form  MOD_SCREEN_ATT
*&---------------------------------------------------------------------*
*       Modfiy Screen Attribute
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mod_screen_att.
* Modify Screen
  loop at screen.
    check screen-group1 = 'DIV'.
    screen-input = '0'.
    modify screen.
  endloop.
endform.                    " MOD_SCREEN_ATT

*&---------------------------------------------------------------------*
*&      Form  SET_REC_TYPE_R_USAGE
*&---------------------------------------------------------------------*
*       Set Record Type / Routing Usage
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_rec_type_r_usage.

* Set Cost Variant
  case 'X'.
*    WHEN P_BPL.
*      P_KLVAR = C_BPL.
*      GV_RECORD_TYPE = 'B'. GV_VERWE = '10'.
*      GV_TARKZ = '001'.     CLEAR GV_FREIG .
*    WHEN P_STD.
*      P_KLVAR = C_STD.
*      GV_RECORD_TYPE = 'S'. GV_VERWE = '1'.
*      GV_TARKZ       = '001'. GV_FREIG = 'X'.
    when p_act.
*      P_KLVAR = C_ACT.
      gv_record_type = 'A'. gv_verwe = '1'.
      gv_tarkz       = '001'.
  endcase.

* Period
  if p_perab > w_perbi.
    message e031.
  endif.

  if p_perab > 12 or p_perab < 1.
    message e007.
  endif.

  if w_perbi > 12 or w_perbi < 1.
    message e007.
  endif.

endform.                    " SET_REC_TYPE_R_USAGE

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*  -->  p1        text
*  <--  p2        text
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
        it_cctr-shop  =  it_nodes-shortname.
        it_cctr-kostl =  it_costcenterlist-costcenter.
        append  it_cctr.
        clear   it_cctr.
      endloop.
    endif.
    clear it_costcenterlist.
  endloop.

endform.                    " READ_CCTR_IN_SHOP

*&---------------------------------------------------------------------*
*&      Form  READ_BASE_INFO
*&---------------------------------------------------------------------*
*       Read Base Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_base_info.

* -  Progress Ind.
  perform progress_ind using '10'
                             text-210.

* Read Material Information (ALL)
  clear : it_mat, it_mat[].
  select * into corresponding fields of table  it_mat
           from ( macku inner join marc
             on macku~matnr = marc~matnr )
                        inner join t001w
             on marc~werks  = t001w~werks
            and macku~bwkey = t001w~bwkey.
  clear it_mat.


* Read CKMLHD
  loop at it_mat.
    select single kalnr into it_mat-kalnr
      from ckmlhd
     where matnr = it_mat-matnr
       and bwkey = it_mat-bwkey
       and bwtar = it_mat-bwtar.
    modify it_mat.
    clear  it_mat.
  endloop.


  sort it_mat by werks mtart.

* Read FSC/HALB Mat.
  clear : it_fsc_mat, it_fsc_mat[].
  loop at it_mat where matnr in s_matnr
*                  AND WERKS = C_FSC_PLANT
                   and ( mtart = c_fsc or mtart = c_halb ).

* Engine -> Only Plant 'E001'
    if   it_mat-beskz = 'F'     "Procurement Type
     and it_mat-sobsl = '40'    "Special procurement type
     and it_mat-mtart = c_halb. "HALB.
      continue.
    endif.

    clear it_fsc_mat.
    move-corresponding it_mat  to it_fsc_mat .
    append it_fsc_mat.
    clear  it_fsc_mat.
    clear  it_mat.
  endloop.

  if it_fsc_mat[] is initial.
    message e076 with c_fsc c_fsc_plant.
  endif.

* RP Master Data
  clear : it_zvco_rp1, it_zvco_rp1[].
  select * into corresponding fields of table it_zvco_rp1
           from zvco_rp1
          where plnnr = c_rp_plnnr.
  if  it_zvco_rp1[] is initial.
    message e077.
  endif.

* Plant Information.
  clear t001w.
  select * into corresponding fields of table it_t001w
           from t001w.

* Read Active Cost Component Structure
  clear tckh4.
  select single * into corresponding fields of tckh4
                  from tckh4
                 where elehk = p_elehk
                   and aktiv = 'X'.
  if tckh4 is initial.
    message e000 with text-250.
  endif.

endform.                    " READ_BASE_INFO

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
*       Progress IND.
*----------------------------------------------------------------------*
*      -->P_%         %
*      -->P_TEXT      TEXT
*----------------------------------------------------------------------*
form progress_ind using    p_%
                           p_text.
  call function 'FI_PROGRESS_INDICATOR'
    exporting
      percentage          = p_%
      text                = p_text
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
            .
endform.                    " PROGRESS_IND

*&---------------------------------------------------------------------*
*&      Form  CAL_PERCENT_USING_KSBT
*&---------------------------------------------------------------------*
*       CALCULATE % (KSBT)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_percent_using_ksbt.

* -  Progress Ind.
  perform progress_ind using '15'
                             text-210.

** -> Indicator 5
  gv_tarkz = '001'.
  p_tvers  = '04'.

* Read DATA From COOMCO
  perform read_data_from_coomco.

* Read Coponent Values - KSBT
  perform read_comp_value_ksbt.

* Multiply with Period key
* Multiply data( X the number of period )
  perform multi_record_with_period_ksbt.

* Cal. Rate
  perform cal_percent_ksbt.

  clear : it_koat_p_5, it_koat_p_5[].
  it_koat_p_5[] = it_koat_p[].
  clear : it_koat_p, it_koat_p[].


** -> Indicator 1
  gv_tarkz = '001'.
  p_tvers  = '01'.

* Read DATA From COOMCO
  perform read_data_from_coomco.

* Read Coponent Values - KSBT
  perform read_comp_value_ksbt.

* Multiply with Period key
* Multiply data( X the number of period )
  perform multi_record_with_period_ksbt.

* Cal. Rate
  perform cal_percent_ksbt.

  clear : it_koat_p_1, it_koat_p_1[].
  it_koat_p_1[] = it_koat_p[].
  clear : it_koat_p, it_koat_p[].


endform.                    " CAL_PERCENT_USING_KSBT

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FROM_COOMCO
*&---------------------------------------------------------------------*
*       Read DATA From COOMCO
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data_from_coomco.


**// Begin of MOD. By Hyung Jin Youn 2004.07.06
* KSBT dose not have data by period in case of (BP/Qarter) planning
* Select data with date range

* Local Data Definition
  data : lv_percount like cosp-perbl. "Period Counter
  data : lv_cnt    like  cosp-perbl.
  data : lv_kadky  like coomco-kadky.
  data : lv_lsdate like coomco-kadky.


* Cal. the Counter
  lv_percount = w_perbi - p_perab + 1.

* Period Counter : Set From-Period .
  clear lv_cnt.
  lv_cnt = p_perab .

* Clear
  clear : it_coomco, it_coomco[].

* From period - To period
  do lv_percount times.

* KEPH -> 'C' COKL/KEKO/KEPH
* K_KKB_SPLITTING_CONVERT

* First Date of From Period
    clear lv_kadky.
    call function 'FIRST_DAY_IN_PERIOD_GET'
      exporting
        i_gjahr              = p_bdatj
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
        i_poper              = lv_cnt
      importing
        e_date               = lv_kadky
      exceptions
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        others               = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

* Last Date of From Period
    clear lv_lsdate.
    call function 'LAST_DAY_IN_PERIOD_GET'
      exporting
        i_gjahr              = p_bdatj
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
        i_poper              = lv_cnt
      importing
        e_date               = lv_lsdate
      exceptions
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        others               = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

* Comment : Functional Team could NOT find the reason
* why the Costing type and Costing Variant for specific
* version are not matched to the values on IMG
*  Cost Components for Cost of Goods Mfd is now created
*  only on PPC1/01 .
    case p_tvers.
      when '01'. "STD
*   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COOMCO
        select * appending corresponding fields of table it_coomco
                from coomco
               where gjahr = p_bdatj
                 and versn = p_versn
*                AND KLVAR = P_KLVAR
                 and tarkz = gv_tarkz
                 and tvers = p_tvers
*                AND KALKA = TCK03-KALKA
*                AND BWVAR = TCK03-BWVAR
*                AND LEDNR = '00'
                 and (     kadky =< lv_kadky
                       and bidat => lv_kadky )
                 and kkzma = space. "Additive - > X
      when '04'. "Actual
*   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COOMCO
        select * appending corresponding fields of table it_coomco
                from coomco
               where gjahr = p_bdatj
                 and versn = p_versn
*                AND KLVAR = P_KLVAR
                 and tarkz = gv_tarkz
                 and tvers = p_tvers
*                AND KALKA = TCK03-KALKA
*                AND BWVAR = TCK03-BWVAR
*                AND LEDNR = '00'
                 and (     kadky =< lv_lsdate
                       and kadky => lv_kadky )
                 and kkzma = space. "Additive - > X
    endcase.

    clear : it_coomco.

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

  if it_coomco[] is initial .
    message e081.
  endif.




** Local Data Definition
*  DATA : LV_KADKY LIKE COOMCO-KADKY.
*
** KEPH -> 'C' COKL/KEKO/KEPH
** K_KKB_SPLITTING_CONVERT
*
** First Date of From Period
*  CLEAR LV_KADKY.
*  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
*    EXPORTING
*      I_GJAHR              = P_BDATJ
**     I_MONMIT             = 00
*      I_PERIV              = TKA01-LMONA
*      I_POPER              = P_PERAB
*    IMPORTING
*      E_DATE               = LV_KADKY
*    EXCEPTIONS
*      INPUT_FALSE          = 1
*      T009_NOTFOUND        = 2
*      T009B_NOTFOUND       = 3
*      OTHERS               = 4.
*
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
** Comment : Functional Team could NOT find the reason
** why the Costing type and Costing Variant for specific
** version are not matched to the values on IMG
**  Cost Components for Cost of Goods Mfd is now created
**  only on PPC1/01 .
*  CLEAR : IT_COOMCO, IT_COOMCO[].
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COOMCO
*           FROM COOMCO
*          WHERE GJAHR = P_BDATJ
*            AND VERSN = P_VERSN
**           AND KLVAR = P_KLVAR
*            AND TARKZ = GV_TARKZ
*            AND TVERS = P_TVERS
**           AND KALKA = TCK03-KALKA
**           AND BWVAR = TCK03-BWVAR
**           AND LEDNR = '00'
*            AND KADKY => LV_KADKY
*            AND KKZMA = SPACE. "Additive - > X
*
*  CLEAR : IT_COOMCO.
*
*  IF IT_COOMCO[] IS INITIAL .
*    MESSAGE E081.
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
*   Object -> Kostl Key
    call function 'OBJECT_KEY_GET_KL'
         exporting
              objnr       = it_kostl_lstar_pct-objnr
         importing
              kokrs       = p_kokrs
              kostl       = it_kostl_lstar_pct-kostl
              lstar       = it_kostl_lstar_pct-lstar
         exceptions
              not_found   = 1
              wrong_obart = 2
              others      = 3.
    if sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

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
*&      Form  MULTI_RECORD_WITH_PERIOD_KSBT
*&---------------------------------------------------------------------*
*       Multiply with Period key KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form multi_record_with_period_ksbt.

  clear it_kostl_lstar_pct.


  data : it_l_tmp_coomco  like standard table of it_kostl_lstar_pct
                          with header line .
  data : lv_per_cnt       like it_kostl_lstar_pct-poper.

  it_l_tmp_coomco[] = it_kostl_lstar_pct[].

  clear : it_kostl_lstar_pct, it_kostl_lstar_pct[].

  loop at it_l_tmp_coomco.
    clear lv_per_cnt.
    lv_per_cnt = it_l_tmp_coomco-from_per.
    while lv_per_cnt <= it_l_tmp_coomco-to_per  .
      move-corresponding it_l_tmp_coomco to  it_kostl_lstar_pct.
      it_kostl_lstar_pct-poper = lv_per_cnt.
* Append
      append it_kostl_lstar_pct.
      clear  it_kostl_lstar_pct.
* Period Counter
      lv_per_cnt = lv_per_cnt + 1.
    endwhile.
  endloop.

* Check Period Range
*  SCREEN_PERIOD_D  IT_ADD_TMP_SCOST.

  sort it_kostl_lstar_pct by gjahr poper .

  if it_kostl_lstar_pct[] is initial .
    message e081.
  endif.

endform.                    " MULTI_RECORD_WITH_PERIOD_KSBT

*&---------------------------------------------------------------------*
*&      Form  CAL_PERCENT_KSBT
*&---------------------------------------------------------------------*
*       Cal. Rate KSBT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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

*&---------------------------------------------------------------------*
*&      Form  SET_FROM_TO_PERIOD
*&---------------------------------------------------------------------*
*       Period (From To) FSC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*&      Form  READ_PCC_DATA
*&---------------------------------------------------------------------*
*       Read PCC data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_pcc_data.

* -  Progress Ind.
  perform progress_ind using '20'
                             text-210.

* Read Object Key for PCC order .
  perform read_obj_for_pcc.

* Read PCC data with Object key
  perform read_pcc_data_with_pcc.

* Read Cost Document
  perform read_co_docs.

* Making Main TAB
  perform making_main_tab.

endform.                    " READ_PCC_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_OBJ_FOR_PCC
*&---------------------------------------------------------------------*
*       Read Object Key for PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_obj_for_pcc.

* Local DATA definition.
  data : it_l_e_vkks0	     like table of vkks0
                          with header line.
  data : it_tmp_fsc_mat   like standard table of it_fsc_mat
                          with header line .
* Copy DATA
  clear : it_tmp_fsc_mat, it_tmp_fsc_mat[].
  it_tmp_fsc_mat[] = it_fsc_mat[].
  clear : it_fsc_mat, it_fsc_mat[].

* Read PCC orders
  loop at it_tmp_fsc_mat.

    clear : it_l_e_vkks0, it_l_e_vkks0[].
    call function 'KK_F_PKOSA_FIND'
      exporting
        i_matnr                     = it_tmp_fsc_mat-matnr
        i_werks                     = it_tmp_fsc_mat-werks
        i_pwerk                     = it_tmp_fsc_mat-werks
*       I_PROCNR                    = ' '
*       I_SA_AUFNR                  = ' '
*       I_FA_AUFNR                  = ' '
*       I_VERID                     = ' '
*       I_STLAN                     = ' '
*       I_STLAL                     = ' '
*       I_PLNTY                     = ' '
*       I_PLNNR                     = ' '
*       I_PLNAL                     = ' '
*       I_DATE                      = '00000000'
*       I_POPUP                     = ' '
*       I_REM                       = ' '
        i_incl_loekz                = 'X'
*       I_NO_OLD_PKOSA              = ' '
*     IMPORTING
*       E_PROCNR                    =
*       E_VERID                     =
*       E_STLAN                     =
*       E_STLAL                     =
*       E_PLNTY                     =
*       E_PLNNR                     =
*       E_PLNAL                     =
*       E_AUFNR                     =
      tables
        e_vkks0                     = it_l_e_vkks0
*       E_PKOSA                     =
      exceptions
        none_found                  = 1
        wrong_input                 = 2
        none_picked                 = 3
        wrong_rule                  = 4
        rsh_not_valid               = 5
        wrong_characteristics       = 6
        no_rule                     = 7
        version_not_valid           = 8
        others                      = 9.

* if No PCC order, Skip the record .
    if sy-subrc <> 0.
      delete it_tmp_fsc_mat.
      continue.
    endif.
    if it_l_e_vkks0[]  is initial .
      delete it_tmp_fsc_mat.
      continue.
    endif.

    loop at it_l_e_vkks0.
* Copying Data
      move-corresponding it_tmp_fsc_mat to it_fsc_mat.
      it_fsc_mat-aufnr = it_l_e_vkks0-aufnr.
      it_fsc_mat-objnr = it_l_e_vkks0-objnr.
* Production Version
      clear ckmlmv001.
      call function 'CKML_MGV_PROCESS_READ'
        exporting
            i_kalnr           = it_l_e_vkks0-procnr
*           I_BUFFER          =
        importing
*           E_PROCESS         =
            e_ckmlmv001       = ckmlmv001
        exceptions
            not_found         = 1
            others            = 2.
      if sy-subrc <> 0.
      endif.
* production Version
      it_fsc_mat-verid = ckmlmv001-verid_nd.
* Process Number
      it_fsc_mat-par_proc_kalnr
                       = it_l_e_vkks0-procnr.
* Making ITAB for PCC orders
      collect   it_fsc_mat.
      clear     it_fsc_mat.
      clear     it_l_e_vkks0.
    endloop.

    clear it_tmp_fsc_mat.
  endloop.

  clear : it_tmp_fsc_mat, it_tmp_fsc_mat[].
  free  : it_tmp_fsc_mat.

endform.                    " READ_OBJ_FOR_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_DATA_WITH_PCC
*&---------------------------------------------------------------------*
*       Read PCC data with Object key
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_pcc_data_with_pcc.
* Local Data Definition
  data : it_lt_cosla like standard table of cosla
                     with header line .
  data : it_lt_cospa like standard table of cospa
                     with header line .
  data : it_lt_cossa like standard table of cossa
                     with header line .
  data : it_lt_cosba like standard table of cosba
                     with header line .

* Read Qty
  sort it_fsc_mat by objnr.
  clear : it_lt_cosla, it_lt_cosla[].
  select * from cosl
           into corresponding fields of table it_lt_cosla
           for all entries in it_fsc_mat
           where lednr = '00'
             and objnr = it_fsc_mat-objnr
             and gjahr = p_bdatj
             and wrttp = c_gp_wrttp
             and versn = p_versn.

  sort  it_lt_cosla by objnr.
  clear it_lt_cosla.

* Read COSP ( Only Debit data / BEKNZ = 'S' )
  clear : it_lt_cospa, it_lt_cospa[].
  select * from cosp
           into corresponding fields of table it_lt_cospa
           for all entries in it_fsc_mat
           where lednr = '00'
             and objnr = it_fsc_mat-objnr
             and gjahr = p_bdatj
             and wrttp = c_gp_wrttp
             and versn = p_versn
             and beknz = c_gp_beknz.
  sort  it_lt_cospa by objnr.
  clear it_lt_cospa.

* Read COSS ( Only Debit data / BEKNZ = 'S' )
  clear : it_lt_cossa, it_lt_cossa[].
  select * from coss
           into corresponding fields of table it_lt_cossa
           for all entries in it_fsc_mat
           where lednr = '00'
             and objnr = it_fsc_mat-objnr
             and gjahr = p_bdatj
             and wrttp = c_gp_wrttp
             and versn = p_versn
             and beknz = c_gp_beknz.
  sort  it_lt_cossa by objnr.
  clear it_lt_cossa.

* transfer data to Global Itabs
* Only for Miscellaneous/ Goods Issue / Confirmation
*C	Goods issues
*F	Confirmations
*I	Overhead
*L	Miscellaneous
*P	Goods receipt
*X	Settlement
*O	Distribution
  data : lv_kkb_beweg type kkb_beweg.
* COSL
  clear : it_cosla, it_cosla[].
  loop at  it_lt_cosla.
    move-corresponding it_lt_cosla to it_cosla.
    collect it_cosla.
    clear it_cosla.
    clear it_lt_cosla.
  endloop.
* COSP
  scr_sum_data p.
* COSS
  scr_sum_data s.

  clear : it_cosla, it_cospa, it_cossa.

endform.                    " READ_PCC_DATA_WITH_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_CO_DOCS
*&---------------------------------------------------------------------*
*       Read Cost Document
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_co_docs.

  clear : it_cosla, it_cospa, it_cossa.

* Clear
  clear : it_coep, it_coep[].

* Local Data Definition
  data : it_l_coep like standard table of it_coep
                   with header line .
  ranges : r_perio for  coep-perio.

* Period
  clear : r_perio, r_perio[].
  r_perio-low    = p_perab.
  r_perio-option = 'BT'.
  r_perio-sign   = 'I'.
  r_perio-high   = w_perbi.
  append r_perio.
  clear  r_perio.


* -  Progress Ind.
  perform progress_ind using '30'
                             text-211.
* Read CO DOC with COSP.
  loop at it_cospa.
    clear : it_l_coep, it_l_coep[].
    select * from coep
             into corresponding fields of table  it_l_coep
*             FOR ALL ENTRIES IN IT_COSPA
             where kokrs = p_kokrs
*             AND PERIO BETWEEN P_PERAB AND w_perbi
               and perio in r_perio
               and lednr = '00'
               and objnr = it_cospa-objnr
               and gjahr = it_cospa-gjahr
               and wrttp = it_cospa-wrttp
               and versn = it_cospa-versn
               and kstar = it_cospa-kstar
               and hrkft = it_cospa-hrkft
               and vrgng = it_cospa-vrgng.
*               AND BEKNZ = IT_COSPA-BEKNZ.
    clear it_l_coep.

    loop at it_l_coep.
      move-corresponding it_l_coep to it_coep.
      it_coep-beweg = it_cospa-beweg.
      clear : it_coep-hrkft,
              it_coep-vrgng,
              it_coep-beknz.
      collect it_coep.
      clear   it_coep.
      clear   it_l_coep.
    endloop.

    clear it_cospa.
  endloop.


* -  Progress Ind.
  perform progress_ind using '40'
                             text-211.
* Read CO DOC with COSS.
*  LOOP AT IT_COSSA.
  clear : it_l_coep, it_l_coep[].
  select * from coep
           into corresponding fields of table  it_l_coep
            for all entries in it_cossa
           where kokrs = p_kokrs
*             AND PERIO BETWEEN P_PERAB AND w_perbi
             and perio in r_perio
             and lednr = '00'
             and objnr = it_cossa-objnr
             and gjahr = it_cossa-gjahr
             and wrttp = it_cossa-wrttp
             and versn = it_cossa-versn
             and kstar = it_cossa-kstar
             and hrkft = it_cossa-hrkft
             and vrgng = it_cossa-vrgng.
*             AND BEKNZ = IT_COSSA-BEKNZ.
  clear it_l_coep.

  loop at it_l_coep.
    move-corresponding it_l_coep to it_coep.
    clear it_cossa.
    read table it_cossa
      with key
*             PERIO = IT_COEP-PERIO
              lednr = it_coep-lednr
              objnr = it_coep-objnr
              gjahr = it_coep-gjahr
              wrttp = it_coep-wrttp
              versn = it_coep-versn
              kstar = it_coep-kstar
              hrkft = it_coep-hrkft
              vrgng = it_coep-vrgng.
*              BEKNZ = IT_COEP-BEKNZ.
    it_coep-beweg = it_cossa-beweg.

    clear : it_coep-hrkft,
            it_coep-vrgng,
            it_coep-beknz.

    collect it_coep.
    clear   it_coep.
    clear   it_l_coep.
  endloop.

  clear it_cossa.
* ENDLOOP.


  clear : it_l_coep, it_l_coep[].
  free : it_l_coep.

endform.                    " READ_CO_DOCS

*&---------------------------------------------------------------------*
*&      Form  MAKING_MAIN_TAB
*&---------------------------------------------------------------------*
*       Making MAIN Itab
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form making_main_tab.
  data: l_matnr   like mara-matnr,
        l_gitype  type c.

* Clear
  clear : it_tmp_shopcost, it_tmp_shopcost[].

  clear : it_coep.
  clear : it_fsc_mat, it_mat .

* Local Data Definition
  data : wa_l_ionra	like	ionra.

*
  loop at it_coep.
    clear it_tmp_shopcost.
    move-corresponding it_coep to it_tmp_shopcost.
*   CO Area
    it_tmp_shopcost-kokrs = p_kokrs.
*   Period
    it_tmp_shopcost-bdatj = p_bdatj.
    it_tmp_shopcost-poper = it_coep-perio.
*   Currency / Unit
    it_tmp_shopcost-hwaer = tka01-waers.
    it_tmp_shopcost-meeht = it_coep-meinb.
*   Child  Material
    it_tmp_shopcost-llv_matnr =  it_coep-matnr.
*...ANDY FIX
    IF it_coep-matnr = space.
      SPLIT it_coep-sgtxt AT ';' INTO l_matnr l_gitype.
      IF l_matnr <> space.
        READ TABLE it_mat WITH KEY matnr = l_matnr.
        IF sy-subrc = 0.
          it_tmp_shopcost-llv_matnr = l_matnr.
        ENDIF.
      ENDIF.
    ENDIF.
*   Record Type
    it_tmp_shopcost-record_type = gv_record_type.
*   Item Category
    perform conv_beweg_to_typps
      using it_coep-beweg
            it_tmp_shopcost-typps.
*   Read Parents Material Info.
    clear it_fsc_mat.
    read table it_fsc_mat with key objnr = it_coep-objnr.
    move-corresponding it_fsc_mat to it_tmp_shopcost.
    it_tmp_shopcost-fsc_matnr = it_fsc_mat-matnr .
* CCTR/AT
*   IF  IT_TMP_SHOPCOST-TYPPS = 'E'. "Confirmation / nternal activity
*   ENDIF.
    if it_coep-parob ne space.
      clear wa_l_ionra.
      call function 'OBJECT_KEY_GET_APPL'
           exporting
                i_objnr = it_coep-parob
           importing
                e_ionra = wa_l_ionra.
      it_tmp_shopcost-kostl = wa_l_ionra-kostl.
      it_tmp_shopcost-lstar = wa_l_ionra-lstar.
    endif.

* Collect
    collect it_tmp_shopcost.
    clear   it_tmp_shopcost.
    clear  it_coep.
  endloop.

  clear   it_tmp_shopcost.

endform.                    " MAKING_MAIN_TAB

*&---------------------------------------------------------------------*
*&      Form  CONV_BEWEG_TO_TYPPS
*&---------------------------------------------------------------------*
*       Convert Btr to Item Category
*----------------------------------------------------------------------*
*      -->P_BEWEG   Business Transaction on Manufacturing Orders
*      -->P_TYPPS   Item category
*----------------------------------------------------------------------*
form conv_beweg_to_typps using    p_beweg like kkbcs_out-beweg
                                  p_typps type typps.
  case p_beweg.
    when 'C'.     p_typps = 'M'.
    when 'F'.     p_typps = 'E'.
    when 'L'.     p_typps = 'V'.
  endcase.

endform.                    " CONV_BEWEG_TO_TYPPS

*&---------------------------------------------------------------------*
*&      Form  SET_SHOP_BY_ITEM_CATE
*&---------------------------------------------------------------------*
*       SHOP information By Item Category. (Actual)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_shop_by_item_cate.

*  CLEAR IT_MAT.
*  CLEAR IT_FSC_MAT.
*  CLEAR IT_TMP_SHOPCOST.
*  CLEAR IT_COEP.

* -  Progress Ind.
  perform progress_ind using '80'
                             text-240.

* Sort  By Item Category
  perform sort_by_cate_n_per.

* Making Assingment of Cost element / Cost Components
  perform assingment_ce_n_ccomp.

* Only 'M', 'E', and 'V' will be considered
* (Request By Functional Team Memeber 2004.04.19)

* NOT KSTAR = '540300' and KSTAR = '5*'
* ( 'M' and 'V' ).
  perform ks_5_not_c_kstar_m_m_v.

* KSTAR = '540300'
* ( 'M' and 'V' ).
  perform ks_c_kstar_m_m_v.

* KSTAR = '836001', '836002'
* ( 'E' ).
  perform ks_n_kstar_c_e.

* KSTAR = '6*' / with 'V'
* ( 'V' ).
  perform ks_6_kstar_c_v.

endform.                    " SET_SHOP_BY_ITEM_CATE

*&---------------------------------------------------------------------*
*&      Form  SORT_BY_CATE_N_PER
*&---------------------------------------------------------------------*
*       Sort ITAB BY Item Category
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_by_cate_n_per.
  sort it_tmp_shopcost by kokrs bdatj poper typps kstar .
endform.                    " SORT_BY_CATE_N_PER

*&---------------------------------------------------------------------*
*&      Form  KS_5_NOT_C_KSTAR_M_M_V
*&---------------------------------------------------------------------*
*       NOT KSTAR = '540300' and KSTAR = '5*'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ks_5_not_c_kstar_m_m_v.

* Clear Container
  perform clear_container_ic.

  sort  it_tmp_shopcost
  by
      kokrs
      bdatj
      poper
*      AUFNR
      versn
      record_type
      fsc_matnr
      mbgbtr descending.

  clear it_tmp_shopcost.

  loop  at it_tmp_shopcost
     where   kstar <> c_kstar_m
       and ( kstar >= '0000500000' and kstar < '0000600000' ).

    if      it_tmp_shopcost-typps = 'M'.
* Good Issue
      perform cal_shop_m_for_manu.
    elseif  it_tmp_shopcost-typps = 'V'.
** Miscellaneous
      perform cal_shop_v_for_manu.
    endif.

    clear  it_tmp_shopcost .
  endloop.

* Re-Digging For 'V'
  perform dd_mat_for_v_for_manu.


endform.                    " KS_5_NOT_C_KSTAR_M_M_V

*&---------------------------------------------------------------------*
*&      Form  CLEAR_CONTAINER_IC
*&---------------------------------------------------------------------*
*       Clear Container
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_container_ic.
  clear : it_sc_m, it_sc_m[].
  clear : it_sc_v, it_sc_v[].
  clear : it_sc_e, it_sc_e[].
endform.                    " CLEAR_CONTAINER_IC

*&---------------------------------------------------------------------*
*&      Form  CAL_SHOP_M_FOR_MANU
*&---------------------------------------------------------------------*
*       Cal Shop Cost - Not 540300, Cat. 'M'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_shop_m_for_manu.
  case  it_tmp_shopcost-mtart.
    when c_halb.  "HALB
* Read Shop Info. Form Routing. (Parents Material)
* Read SHOP Info. from Routing/Production version
      perform read_shop_fr_routing_prv
                                 using it_tmp_shopcost-fsc_matnr
                                       it_tmp_shopcost-shop
                                       it_tmp_shopcost-werks
                                       it_tmp_shopcost-bdatj
                                       it_tmp_shopcost-poper
                                       it_tmp_shopcost-kokrs.
    when others.
* Read Shop Info. Form PP Side.
      clear it_zvco_rp1.
      read table it_zvco_rp1 with key usr00 = it_tmp_shopcost-vspvb.
      it_tmp_shopcost-shop = it_zvco_rp1-usr02.
  endcase.
* Read Cost Comp. Number
  perform read_cost_comp using it_tmp_shopcost-kstar
                               it_tmp_shopcost-elemt.

* Read ML Actual Unit Price
  clear ckmlcr.
  select single  *
               from ckmlcr
              where kalnr  = it_tmp_shopcost-chd_kalnr
                and bdatj  = it_tmp_shopcost-bdatj
                and poper  = it_tmp_shopcost-poper
                and untper = space.
*                AND CURTP  = 'C'.

*    WIP/SCRAP
  perform set_wip_scr_into_tab
    using it_tmp_shopcost-bdatj
          it_tmp_shopcost-poper
          it_tmp_shopcost-objnr
          it_tmp_shopcost-llv_matnr
          it_tmp_shopcost-bwkey
          it_tmp_shopcost-bwtar
*     <- value
          it_tmp_shopcost-mbgbtr
          it_tmp_shopcost-wkgbtr
*     -> value
          it_tmp_shopcost-wip_amt
          it_tmp_shopcost-scrap_amt
          it_tmp_shopcost-wip_quantity
          it_tmp_shopcost-actual_scrap
          it_tmp_shopcost-manu_qty
          it_tmp_shopcost-meeht
          it_tmp_shopcost-kostl
          it_tmp_shopcost-lstar
          '01'.

* Read ML Actual Unit Price
  clear ckmlcr.
  select single
         pvprs peinh
               into (it_tmp_shopcost-preis, it_tmp_shopcost-peinh)
               from ckmlcr
              where kalnr  = it_tmp_shopcost-chd_kalnr
                and bdatj  = it_tmp_shopcost-bdatj
                and poper  = it_tmp_shopcost-poper
                and untper = space.
*                AND CURTP  = 'C'.

* ML Actaul AMT
  it_tmp_shopcost-ml_act_preis
      = it_tmp_shopcost-mbgbtr
      * ( it_tmp_shopcost-preis / it_tmp_shopcost-peinh ).

*   Modify
  modify it_tmp_shopcost.

endform.                    " CAL_SHOP_M_FOR_MANU

*&---------------------------------------------------------------------*
*&      Form  PUT_SHOP_MAT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form put_shop_mat_info.

  clear : it_tmp_shopcost.

* Sort (Material - ALL)
  sort it_mat
    by  matnr
        werks
        bwkey
        bwtar.

* Sort
  sort it_tmp_shopcost by llv_matnr
                          werks
                          bwkey
                          bwtar.

  loop at it_tmp_shopcost.
* Child Material Info
    clear it_mat.
    read table  it_mat
      with key  matnr = it_tmp_shopcost-llv_matnr
                werks = it_tmp_shopcost-werks
                bwkey = it_tmp_shopcost-bwkey
                bwtar = it_tmp_shopcost-bwtar
      binary search.
    it_tmp_shopcost-beskz =  it_mat-beskz. "Procurement Type
    it_tmp_shopcost-sobsl =  it_mat-sobsl. "Special procurement type
    it_tmp_shopcost-vspvb =  it_mat-vspvb. "Proposed Supply Area
    it_tmp_shopcost-chd_kalnr
                          =  it_mat-kalnr. "Cost est number, w/o qty str
** Period (From / To)
*    PERFORM SET_FROM_TO_PERIOD
*       USING IT_TMP_SHOPCOST-KADAT
*             P_KOKRS
*             IT_TMP_SHOPCOST-BDATJ
*             IT_TMP_SHOPCOST-FROM_PER.
*    PERFORM SET_FROM_TO_PERIOD
*       USING IT_TMP_SHOPCOST-BIDAT
*             P_KOKRS
*             IT_TMP_SHOPCOST-BDATJ
*             IT_TMP_SHOPCOST-TO_PER.

* Modify
    modify it_tmp_shopcost.
    clear it_tmp_shopcost.
  endloop.

endform.                    " PUT_SHOP_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  ASSINGMENT_CE_N_CCOMP
*&---------------------------------------------------------------------*
*       Making Assingment of Cost element / Cost Components
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assingment_ce_n_ccomp.
* Read Cost Components
  clear : it_cskb, it_cskb[].
  loop at it_tmp_shopcost.
    on change of it_tmp_shopcost-kstar.
      it_cskb-kstar = it_tmp_shopcost-kstar.
      call function 'KKEK_COST_COMPONENT_ELEMENT'
        exporting
          elehk_imp                 = p_elehk
          ktopl_imp                 = tka01-ktopl
          kstar_imp                 = it_cskb-kstar
*         HRKFT_IMP                 =
*         ELEMT_IMP                 =
*         ELEHKNS_IMP               =
*         SCHKZ_IMP                 =
*         SCHKZNS_IMP               =
          message_on_screen         = space
        importing
*         KSTAR_EXP                 =
*         HRKFT_EXP                 =
          elemt_exp                 = it_cskb-elemt
*         ELEMTNS_EXP               =
*         FLG_FIX_ALLOWED_EXP       =
*         RC                        =
        exceptions
          calling_error             = 1
          others                    = 2.
      if sy-subrc = 0.
        collect it_cskb.
      endif.
    endon.
  endloop.

  clear it_cskb.

endform.                    " ASSINGMENT_CE_N_CCOMP

*&---------------------------------------------------------------------*
*&      Form  READ_COST_COMP
*&---------------------------------------------------------------------*
*       Read Cost Comp. Number
*----------------------------------------------------------------------*
*      -->P_KSTAR  Cost Element
*      -->P_ELEMT  Cost Components
*----------------------------------------------------------------------*
form read_cost_comp using    p_kstar
                             p_elemt.
  clear it_cskb.
  read table it_cskb with key kstar = p_kstar.
  p_elemt = it_cskb-elemt.

endform.                    " READ_COST_COMP

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_FR_ROUTING_PRV
*&---------------------------------------------------------------------*
*       Read SHOP Info. from Routing/Production version
*----------------------------------------------------------------------*
*      -->P_MATNR  Material
*      -->P_SHOP   Shop
*      -->P_WERKS  Plant
*      -->P_BDATJ  Year
*      -->P_POPER  Period
*      -->P_KOKRS  Controlling Area
*----------------------------------------------------------------------*
form read_shop_fr_routing_prv using    p_matnr
                                       p_shop
                                       p_werks
                                       p_bdatj
                                       p_poper
                                       p_kokrs.


  clear : crhd, plpo, plko.
* Read Shop From Routing
  data : lv_arbid like plpo-arbid.
  data : lv_fdper like sy-datum.
  data : lv_ldper like sy-datum.


  call function 'K_PERIODS_GET'
    exporting
      par_igjahr          = p_bdatj
      par_ipoper          = p_poper
      par_kokrs           = p_kokrs
*     PAR_PREVP           = ' '
*     PAR_SPEOK           = ' '
*     PAR_NEXTP           = ' '
    importing
*     PAR_ANZBP           =
*     PAR_ANZSP           =
*     PAR_EGJAHR          =
*     PAR_EPOPER          =
      par_fdper           = lv_fdper
      par_ldper           = lv_ldper
    exceptions
      kokrs_invalid       = 1
      poper_invalid       = 2
      others              = 3.

  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


  clear mkal.
  select single *  from mkal
                  where matnr = p_matnr
                    and werks = p_werks
                    and bdatu >= lv_ldper
                    and adatu <= lv_fdper.
*                   AND BDATU >= IT_TMP_SHOPCOST-BIDAT
*                   AND ADATU <= IT_TMP_SHOPCOST-KADAT.

  check sy-subrc = 0.

  select single  plpo~arbid  into lv_arbid
                  from plko inner join plpo
                    on plko~plnty = plpo~plnty
                   and plko~plnnr = plpo~plnnr
                 where
                    (     plko~plnty = mkal-pltyg
                     and  plko~plnnr = mkal-plnng
                     and  plko~plnal = mkal-alnag
                     and  plko~verwe = gv_verwe   )
                  or
                    (     plko~plnty = mkal-pltym
                     and  plko~plnnr = mkal-plnnm
                     and  plko~plnal = mkal-alnam
                     and  plko~verwe = gv_verwe   )
                  or
                    (     plko~plnty = mkal-plnty
                     and  plko~plnnr = mkal-plnnr
                     and  plko~plnal = mkal-alnal
                     and  plko~verwe = gv_verwe   ).


  check sy-subrc = 0.

  clear crhd.
  select single *  from crhd
                  where objid =  lv_arbid.

  check sy-subrc = 0.

* Work Center = Cost center (1:1)
  clear it_cctr.
  read table it_cctr with key kostl = crhd-arbpl.

  if sy-subrc = 0.
    p_shop = it_cctr-shop.
  endif.

endform.                    " READ_SHOP_FR_ROUTING_PRV

*&---------------------------------------------------------------------*
*&      Form  READ_SCRAP_WIP_QTY
*&---------------------------------------------------------------------*
*       Read Scrap/Wip
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_scrap_wip_qty.

* -  Progress Ind.
  perform progress_ind using '60'
                             text-260.

* Local Data definition
  data : it_l_qrp_list   type qrp_t_objnr_list,
         wa_l_qrp_list   type qrp_objnr_list.
  data : it_l_quantities type qrp_t_quantities,
         wa_l_quantities type qrp_quantities.
  data : it_l_output type qrp_t_wip_scrap,
         wa_l_output type qrp_s_wip_scrap.
  data : lv_objnr type j_objnr.

* Clear
  clear : it_wip_scrap, it_wip_scrap[].

  clear : it_fsc_mat.

* Bulid Object list
  loop at it_fsc_mat.
    wa_l_qrp_list-plant = it_fsc_mat-werks.
    wa_l_qrp_list-objnr = it_fsc_mat-objnr.
    append wa_l_qrp_list to it_l_qrp_list.
    clear  wa_l_qrp_list.
  endloop.

* Read Wip/Scrap
* From P_PERAB to w_perbi
  data : lv_p_cnt like p_perab.
  lv_p_cnt = p_perab.
* Call FM
  while lv_p_cnt <=  w_perbi.

    clear : it_l_quantities[].
    refresh it_l_quantities.

    call function 'QRP_APO_REPORTINGPOINT_READ'
         exporting
              if_period         = lv_p_cnt
              if_gjahr          = p_bdatj
              it_objnr_list     = it_l_qrp_list
              if_select_wip     = 'X'
              if_select_scrap   = 'X'
*             IF_PREFETCH_MAT   = 'X'
         importing
              et_quantity_table = it_l_quantities
         exceptions
              wrong_input       = 1
              others            = 2.

    if sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

    sort it_l_quantities by act_objnr objnr kaln1.

* Store data with Period Key
    loop at it_l_quantities into wa_l_quantities.
      wa_l_output-gjahr  = p_bdatj.
      wa_l_output-period = lv_p_cnt.
      move-corresponding wa_l_quantities to wa_l_output.
      wa_l_output-target_qty = wa_l_output-target_qty +
                               wa_l_output-variance_qty.
      if wa_l_quantities-act_objnr is initial.
        clear lv_objnr.
        lv_objnr(2) = 'VS'.
        lv_objnr+2 = wa_l_quantities-kzbws.
        lv_objnr+3 = wa_l_quantities-sobkz.
        lv_objnr+4 = wa_l_quantities-kaln1.
        call function 'QRP_APO_COMP_OBJNR_DECODE'
             exporting
                  if_f_objnr      = lv_objnr
                  if_complete_key = 'X'
             importing
                  ef_kaln1        = wa_l_output-kaln1
                  ef_kzbws        = wa_l_output-kzbws
                  ef_sobkz        = wa_l_output-sobkz
                  ef_matnr        = wa_l_output-material
                  ef_bwkey        = wa_l_output-bwkey
                  ef_bwtar        = wa_l_output-bwtar
                  ef_vbeln        = wa_l_output-vbeln
                  ef_posnr        = wa_l_output-posnr.
        append wa_l_output to it_l_output.
        clear wa_l_output.
      else.
        call function 'OBJECT_KEY_GET_KL'
             exporting
                  objnr  = wa_l_quantities-act_objnr
             importing
                  kostl  = wa_l_output-kostl
                  lstar  = wa_l_output-lstar
             exceptions
                  others = 1.
        if not sy-subrc is initial.
        else.
          append wa_l_output to it_l_output.
          clear wa_l_output.
        endif.
      endif.
    endloop.

* Period Count
    lv_p_cnt = lv_p_cnt + 1.

  endwhile .

* --> Global Tab
  loop at it_l_output into wa_l_output.
    move-corresponding  wa_l_output to it_wip_scrap.
    collect it_wip_scrap.
    clear   it_wip_scrap.
  endloop.

  sort it_wip_scrap by gjahr period objnr kostl lstar material.
  clear   it_wip_scrap.

endform.                    " READ_SCRAP_WIP_QTY

*&---------------------------------------------------------------------*
*&      Form  SET_WIP_SCR_INTO_TAB
*&---------------------------------------------------------------------*
*       WIP/SCRAP
*----------------------------------------------------------------------*
*      -->P_BDATJ      Fiscal Year
*      -->P_POPER      Period
*      -->P_OBJNR      PCC order
*      -->P_LLV_MATNR  Child Material
*      -->P_BWKEY      Valuation Key
*      -->P_BWTAR      Valuation Type
*      -->P_MBGBTR     Total quantity entered
*      -->P_WKGBTR     Total value in CO area currency
*      <--P_WIP_AMT    Value of WIP in Object Currency
*      <--P_SCRAP_AMT  Scrap Value in Controlling Area Currency
*      <--P_WIP_QUANTITY Work-in-Process Quantity Based On Components
*      <--P_ACTUAL_SCRAP Actual Scrap Quantity Based on Components
*      -->P_MEEHT        Unit
*      -->P_KOSTL        CCtr
*      -->P_LSTAR        AT
*----------------------------------------------------------------------*
form set_wip_scr_into_tab using    p_bdatj
                                   p_poper
                                   p_objnr
                                   p_llv_matnr
                                   p_bwkey
                                   p_bwtar
*
                                   p_mbgbtr
                                   p_wkgbtr
*
                                   p_wip_amt
                                   p_scrap_amt
                                   p_wip_quantity
                                   p_actual_scrap
                                   p_manu_qty
                                   p_meeht
*
                                   p_kostl
                                   p_lstar
                                   p_calos.


** Local Data Definition.
  data : lv_ld       like sy-datum.
  data : lv_ed       like sy-datum.
  data : lv_klvar    like cki64a-klvar.


  if not p_llv_matnr is initial.
    read table it_wip_calculated with key
                             gjahr    = p_bdatj
                             period   = p_poper
                             objnr    = p_objnr
                             material = p_llv_matnr
                             bwkey    = p_bwkey
                             bwtar    = p_bwtar.

  else.
    read table it_wip_calculated
                      with key
                           gjahr    = p_bdatj
                           period   = p_poper
                           objnr    = p_objnr
                           material = space
                           bwkey    = space
                           bwtar    = space
                           kostl    = p_kostl
                           lstar    = p_lstar.
  endif.

  check sy-subrc ne 0.

  clear it_wip_scrap.
* For Material Qty
  if   not p_llv_matnr is initial .
    read table it_wip_scrap
                      with key
                           gjahr    = p_bdatj
                           period   = p_poper
                           objnr    = p_objnr
                           material = p_llv_matnr
                           bwkey    = p_bwkey
                           bwtar    = p_bwtar.
  else.
    read table it_wip_scrap
                      with key
                           gjahr    = p_bdatj
                           period   = p_poper
                           objnr    = p_objnr
                           material = space
                           bwkey    = space
                           bwtar    = space
                           kostl    = p_kostl
                           lstar    = p_lstar.
  endif.

  if sy-subrc = 0.
    move: it_wip_scrap-target_qty to p_manu_qty.

* Check Unit
    if it_wip_scrap-unit <>  p_meeht.
      perform unit_converion
                         using it_wip_scrap-wip_quantity
                               it_wip_scrap-wip_quantity
                               it_wip_scrap-unit
                               p_meeht.
      perform unit_converion
                         using it_wip_scrap-actual_scrap
                               it_wip_scrap-actual_scrap
                               it_wip_scrap-unit
                               p_meeht.
      perform unit_converion
                         using p_manu_qty
                               p_manu_qty
                               it_wip_scrap-unit
                               p_meeht.
    endif.


* Check Cal. Logic
    if     p_calos = '02'.
* WIP
      p_wip_quantity = it_wip_scrap-wip_quantity.
      p_wip_amt   = p_wip_quantity * ( p_wkgbtr / p_mbgbtr ).
* Scrap
      p_actual_scrap = it_wip_scrap-actual_scrap.
      p_scrap_amt = p_actual_scrap * ( p_wkgbtr / p_mbgbtr ).

    elseif p_calos = '01'.

** Set Parameters in selection data from table 'KEKO'
* -> Same As Period Standard
      lv_klvar = c_std.
      gv_freig = 'X'.
*   Costing Variants
      clear tck03.
      select single * from tck03
                     where klvar = lv_klvar.
      if sy-subrc <> 0.
        message w000 with text-101 lv_klvar.
      endif.

** Read Keko
*   Get Period dates
      perform read_begin_ending_date
        using p_poper
              lv_ld
              lv_ed.

*   Read KEKO with a Date Range
      clear it_t001w.
      read table  it_t001w with key bwkey = p_bwkey.
      clear keko.
      select single max( kadky ) into keko-kadky
        from keko
*             WHERE TVERS = P_TVERS
               where tvers = '01'
                 and bwvar = tck03-bwvar
                 and kalka = tck03-kalka
                 and kkzma = space         "ALL Data
                 and matnr = p_llv_matnr
                 and werks = it_t001w-werks
                 and bwkey = p_bwkey
                 and bwtar = p_bwtar
                 and kokrs = p_kokrs
                 and ( kadat =< lv_ld and bidat => lv_ed )
                 and freig = gv_freig.

      select single * from keko
*             WHERE TVERS = P_TVERS
               where tvers = '01'
                 and bwvar = tck03-bwvar
                 and kalka = tck03-kalka
                 and kkzma = space         "ALL Data
                 and matnr = p_llv_matnr
                 and werks = it_t001w-werks
                 and bwkey = p_bwkey
                 and bwtar = p_bwtar
                 and kokrs = p_kokrs
                 and kadky = keko-kadky
                 and ( kadat =< lv_ld and bidat => lv_ed )
                 and freig = gv_freig.
*   FREIG = 'X' Only Mark Released - Standard Plan
*   FREIG = ' ' No   Mark Released - Business Plan
      if sy-subrc = 0.
        clear ckhs.
        select single * from ckhs
                       where lednr = '00'
                         and bzobj = keko-bzobj
                         and kalnr = keko-kalnr
                         and kalka = keko-kalka
                         and kadky = keko-kadky
                         and tvers = keko-tvers
                         and bwvar = keko-bwvar
                         and kkzma = keko-kkzma.
      endif.
* WIP
      p_wip_quantity = it_wip_scrap-wip_quantity.
      if ckmlcr-peinh ne space and ckmlcr-peinh ne '1'.
        p_wip_amt   = p_wip_quantity * ckhs-hwges / ckmlcr-peinh.
      else.
        p_wip_amt   = p_wip_quantity * ckhs-hwges.
      endif.
* Scrap
      p_actual_scrap = it_wip_scrap-actual_scrap.
      if ckmlcr-peinh ne space and ckmlcr-peinh ne '1'.
        p_scrap_amt = p_actual_scrap * ckhs-hwges / ckmlcr-peinh.
      else.
        p_scrap_amt = p_actual_scrap * ckhs-hwges.
      endif.

      if not p_llv_matnr is initial.
        move: p_bdatj to it_wip_calculated-gjahr,
              p_poper to it_wip_calculated-period,
              p_objnr to it_wip_calculated-objnr,
              p_llv_matnr to it_wip_calculated-material,
              p_bwkey to it_wip_calculated-bwkey,
              p_bwtar to it_wip_calculated-bwtar.
      else.
        move: p_bdatj to it_wip_calculated-gjahr,
              p_poper to it_wip_calculated-period,
              p_objnr to it_wip_calculated-objnr,
              space    to it_wip_calculated-material,
              space    to it_wip_calculated-bwkey,
              space    to it_wip_calculated-bwtar,
              p_kostl to it_wip_calculated-kostl,
              p_lstar to it_wip_calculated-lstar.
      endif.

      append it_wip_calculated.
    endif.
  endif.

endform.                    " SET_WIP_SCR_INTO_TAB

*&---------------------------------------------------------------------*
*&      Form  CAL_SHOP_V_FOR_MANU
*&---------------------------------------------------------------------*
*       Cal Shop Cost - Not 540300, Cat. 'V'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_shop_v_for_manu.

* Read Cost Comp. Number
  perform read_cost_comp using it_tmp_shopcost-kstar
                               it_tmp_shopcost-elemt.

* If PCC has material code, calculate ADD,
* MANU amount, quantiey with PCC amount and quanty.
  if it_tmp_shopcost-llv_matnr ne space.
    it_tmp_shopcost-add_wkgbtr = it_tmp_shopcost-wkgbtr.
    it_tmp_shopcost-add_mbgbtr = it_tmp_shopcost-mbgbtr.

* Get Unit Price
    if it_tmp_shopcost-add_mbgbtr <> 0.
      it_tmp_shopcost-preis
       = it_tmp_shopcost-add_wkgbtr / it_tmp_shopcost-add_mbgbtr.
    endif.

* Get ML Actual AMT (SHOP Actual)
    it_tmp_shopcost-ml_act_preis = it_tmp_shopcost-add_wkgbtr.

* Save Temp Cont.
    move-corresponding it_tmp_shopcost to it_sc_v .
    append it_sc_v.
    clear  it_sc_v.

  else.
* Read Additional Issue Infor.
    loop at  it_ztco_abispost
          where kokrs     =  it_tmp_shopcost-kokrs
            and gjahr     =  it_tmp_shopcost-bdatj
            and period    =  it_tmp_shopcost-poper
            and versn     =  it_tmp_shopcost-versn
            and kstar     =  it_tmp_shopcost-kstar
*             MATNR     =  IT_TMP_SHOPCOST-LLV_MATNR
            and werks     =  it_tmp_shopcost-werks
*             IO_AUFNR  =  IT_TMP_SHOPCOST-
            and pcc_aufnr =  it_tmp_shopcost-aufnr.

* Check Unit
      if it_tmp_shopcost-meeht eq space.
        it_tmp_shopcost-meeht = it_ztco_abispost-meinb.
      endif.
      if it_tmp_shopcost-typps = 'M'
      and it_ztco_abispost-meinb <>  it_tmp_shopcost-meeht.
        perform unit_converion
                           using it_ztco_abispost-mbgbtr
                                 it_ztco_abispost-mbgbtr
                                 it_ztco_abispost-meinb
                                 it_tmp_shopcost-meeht.
      endif.
      it_tmp_shopcost-add_wkgbtr = it_ztco_abispost-chg_wkgbtr.
      it_tmp_shopcost-add_mbgbtr = it_ztco_abispost-mbgbtr.
* Get Unit Price
      if it_tmp_shopcost-add_mbgbtr <> 0.
        it_tmp_shopcost-preis
         = it_tmp_shopcost-add_wkgbtr / it_tmp_shopcost-add_mbgbtr.
      endif.
* Get ML Actual AMT (SHOP Actual)
      it_tmp_shopcost-ml_act_preis = it_tmp_shopcost-add_wkgbtr.

* Save Temp Cont.
      move-corresponding it_tmp_shopcost to it_sc_v .
      it_sc_v-llv_matnr = it_ztco_abispost-matnr.
      append it_sc_v.
      clear  it_sc_v.
      clear it_ztco_abispost.
*---start wskim 03/18/2005
      clear it_tmp_shopcost-meeht.
*---end
    endloop.

* NOT modify
*  MODIFY IT_TMP_SHOPCOST.
  endif.
* Delete
  delete it_tmp_shopcost.
endform.                    " CAL_SHOP_V_FOR_MANU

*&---------------------------------------------------------------------*
*&      Form  READ_ABISPOST
*&---------------------------------------------------------------------*
*       Read Additional Issue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_abispost.
  data: it_ztco_abispost_tmp like it_ztco_abispost occurs 0
                                  with header line.
* Clear
  clear : it_ztco_abispost, it_ztco_abispost[].

  ranges : r_per for covja-perab.

  clear : r_per, r_per[].
  r_per-low    = p_perab.
  r_per-high   = w_perbi.
  r_per-sign   = 'I'.
  r_per-option = 'BT'.
  append r_per.
  clear  r_per.

  select kokrs  gjahr       period    versn    kstar    matnr
         werks  io_aufnr    pcc_aufnr
         chg_wkgbtr    waers
         mbgbtr        meinb
     from ztco_abispost
     into corresponding fields of table it_ztco_abispost_tmp
    where kokrs  = p_kokrs
      and gjahr  = p_bdatj
      and period in r_per
      and versn  = p_versn.

  loop at it_ztco_abispost_tmp.
    move: it_ztco_abispost_tmp to it_ztco_abispost.
    collect it_ztco_abispost.
  endloop.

  if  it_ztco_abispost[] is initial.
    write : / text-090.
  endif.

endform.                    " READ_ABISPOST

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONVERION
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_INPUT     Inout
*      -->P_OUTPUT    OutPut
*      -->P_UNIT_IN   unit In
*      -->P_UNIT_OUT  Unit Out
*----------------------------------------------------------------------*
form unit_converion using    p_input
                             p_output
                             p_unit_in
                             p_unit_out.

  call function 'UNIT_CONVERSION_SIMPLE'
    exporting
      input                      = p_input
*   NO_TYPE_CHECK              = ' '
*   ROUND_SIGN                 = ' '
      unit_in                    = p_unit_in
      unit_out                   = p_unit_out
    importing
*   ADD_CONST                  = '1'
*   DECIMALS                   =
*   DENOMINATOR                =
*   NUMERATOR                  =
      output                     = p_output
   exceptions
     conversion_not_found       = 1
     division_by_zero           = 2
     input_invalid              = 3
     output_invalid             = 4
     overflow                   = 5
     type_invalid               = 6
     units_missing              = 7
     unit_in_not_found          = 8
     unit_out_not_found         = 9
     others                     = 10
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " UNIT_CONVERION

*&---------------------------------------------------------------------*
*&      Form  DD_MAT_FOR_V_FOR_MANU
*&---------------------------------------------------------------------*
*       Re-Digging For 'V'
*       Cal Shop Cost - Not 540300, Cat. 'V'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dd_mat_for_v_for_manu.

  clear  it_sc_v.

  loop at  it_sc_v.
* Read Material Info
* Same Valuation Str. (Child and Parents)
    clear it_mat.
    read table it_mat
      with key matnr = it_sc_v-llv_matnr
               werks = it_sc_v-werks
               bwkey = it_sc_v-bwkey
               bwtar = it_sc_v-bwtar.

    it_sc_v-beskz     = it_mat-beskz.
    it_sc_v-sobsl     = it_mat-sobsl.
    it_sc_v-vspvb     = it_mat-vspvb.
    it_sc_v-chd_kalnr = it_mat-kalnr.

    case  it_mat-mtart.
      when c_halb.  "HALB
* Read Shop Info. Form Routing. (Parents Material)
* Read SHOP Info. from Routing/Production version
        perform read_shop_fr_routing_prv
                                   using it_sc_v-llv_matnr
                                         it_sc_v-shop
                                         it_sc_v-werks
                                         it_sc_v-bdatj
                                         it_sc_v-poper
                                         it_sc_v-kokrs.
      when others.
* Read Shop Info. Form PP Side.
        clear it_zvco_rp1.
        read table it_zvco_rp1 with key usr00 = it_sc_v-vspvb.
        it_sc_v-shop = it_zvco_rp1-usr02.
    endcase.

* Clear
    clear : it_sc_v-wkgbtr,  it_sc_v-mbgbtr.

    modify it_sc_v.
    clear  it_sc_v.
  endloop.

* Append
  append lines of it_sc_v to it_tmp_shopcost.
  clear it_tmp_shopcost.

endform.                    " DD_MAT_FOR_V_FOR_MANU

*&---------------------------------------------------------------------*
*&      Form  KS_C_KSTAR_M_M_V
*&---------------------------------------------------------------------*
*       KSTAR = '540300'
*       ( 'M' and 'V' ).
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ks_c_kstar_m_m_v.

* Clear Container
  perform clear_container_ic.

  clear it_tmp_shopcost.

  loop  at it_tmp_shopcost
     where   kstar = c_kstar_m.
    if      it_tmp_shopcost-typps = 'M'.
* Good Issue
      perform cal_shop_m_for_matc.
    elseif  it_tmp_shopcost-typps = 'V'.
* Miscellaneous
      perform cal_shop_v_for_matc.
    endif.
    clear  it_tmp_shopcost .
  endloop.


*   C_KSTAR_M = 540300 'M'
  perform cal_sub_ext_item_act.

*   C_KSTAR_M = 540300 'V'
  perform dd_mat_for_v_for_matc.


endform.                    " KS_C_KSTAR_M_M_V

*&---------------------------------------------------------------------*
*&      Form  CAL_SHOP_M_FOR_MATC
*&---------------------------------------------------------------------*
*       Cal Shop Cost - 540300, Cat. 'M'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_shop_m_for_matc.

* Local Data Definition
  data : lv_shop  like  it_tmp_shopcost-shop.
  data : lv_werks like  t001w-werks.
  clear lv_shop.
* Check Procurement Type / Spc Proc. Type
* -> Engine In Plant 'P001' -> 'E001'
  if   it_tmp_shopcost-beskz = 'F'
   and it_tmp_shopcost-sobsl = '40'.
*    LV_SHOP  = C_SHP_M_E001.
    lv_werks = 'E001'.
  else.
*    LV_SHOP  = C_SHP_M_P001.
    lv_werks = it_tmp_shopcost-werks.
  endif.

* Read SHOP Info. from Routing/Production version
  perform read_shop_fr_routing_prv
                             using it_tmp_shopcost-llv_matnr
                                   lv_shop
                                   lv_werks
                                   it_tmp_shopcost-bdatj
                                   it_tmp_shopcost-poper
                                   it_tmp_shopcost-kokrs.

* Store DATA which should be exploded later .
  move-corresponding it_tmp_shopcost to it_sc_m.
* Change Plant/Shop
  it_sc_m-shop  = lv_shop.

** Exception.
*  IT_SC_M-WERKS = LV_WERKS.
  it_sc_m-werks = it_tmp_shopcost-werks.
**

* Bwkey
  clear it_t001w.
  read table it_t001w with key werks = it_sc_m-werks .
  it_sc_m-bwkey = it_t001w-bwkey.
* Leave BWTAR
  append it_sc_m.
  clear  it_sc_m.

* Delete Original Data
  delete  it_tmp_shopcost.

endform.                    " CAL_SHOP_M_FOR_MATC

*&---------------------------------------------------------------------*
*&      Form  CAL_SUB_EXT_ITEM_ACT
*&---------------------------------------------------------------------*
*       Sub Extraction for KSTAR = 540300
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_sub_ext_item_act.
  check not it_sc_m[] is initial.

* Read Cost Components from CK13N
  perform read_costcomp_for_matc_ck13n tables it_sc_m.

  perform cal_val_m_for_matc.
endform.                    " CAL_SUB_EXT_ITEM_ACT

*&---------------------------------------------------------------------*
*&      Form  READ_MLCC
*&---------------------------------------------------------------------*
*       Read MLCCS (Cost Components in ML )
*----------------------------------------------------------------------*
*      -->P_CHD_KALNR     The CEN of Child Material
*      -->P_BWKEY         Valuation Key
*      -->P_MATNR         Material (Child)
*      -->P_BDATJ         Year
*      -->P_POPER         Period
*      -->P_PAR_PROC_KAL  Process Number Of Mather Material
*      -->P_MLCCT         Type of Cost Component Split in ML
*----------------------------------------------------------------------*
form read_mlcc using    p_chd_kalnr
*                        P_BWKEY
*                        P_LLV_MATNR
                        p_bdatj
                        p_poper
                        p_par_proc_kal
                        p_mlcct like ckmlkeph-mlcct.

  data : it_l_keph      like standard table of keph
                        with header line .

  data : wa_l_ckmlkeph like ckmlkeph.

  clear : it_l_keph, it_l_keph[].
  loop at it_keph_mlcd into wa_l_ckmlkeph
    where bdatj = p_bdatj
      and poper = p_poper
      and kalnr = p_chd_kalnr
      and categ = 'VN'
      and ptyp  = 'VF'
      and bvalt = p_par_proc_kal
      and keart = 'H'
      and mlcct = p_mlcct
      and kkzst = space.

    move-corresponding wa_l_ckmlkeph to it_l_keph .
    append it_l_keph .
    clear  it_l_keph .
  endloop.

  call function 'K_KKB_SPLITTING_CONVERT'
       exporting
            i_elehk     = 'H1'
            i_sicht     = '01'
            i_keart     = 'H'
            i_losfx     = space
            i_waers     = 'USD'
       tables
            t_keph      = it_l_keph
            t_kkb_split = it_kkb_split.

* Delete the records which have "Zero" value
  delete it_kkb_split where w000 eq space.

endform.                    " READ_MLCC

*&---------------------------------------------------------------------*
*&      Form  READ_KEPH_MLCD
*&---------------------------------------------------------------------*
*       Read KEPH
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_keph_mlcd
      using p_bdatj
            p_poper
            p_llv_matnr
            p_chd_kalnr
            p_bwkey
            p_chd.

  data : fl_refresh_buffer   type    boole_d ,
         it_l_kalnr          type    ckmv0_matobj_tbl  .

  data : it_l_et_keph_mlcd	
         type ccs01_t_keph_mlcd,
         it_l_et_keph_mlcd_not_alloc
         type ccs01_t_keph_mlcd.

  data : wa_l_ckmv0_matobj_str  type ckmv0_matobj_str .
  data : wa_l_ckmlkeph like ckmlkeph.

  fl_refresh_buffer = 'X'.

  wa_l_ckmv0_matobj_str-kalnr = p_chd_kalnr.
  wa_l_ckmv0_matobj_str-bwkey = p_bwkey.
  wa_l_ckmv0_matobj_str-matnr = p_llv_matnr.
  append wa_l_ckmv0_matobj_str to it_l_kalnr.

  call function 'MLCCS_KEPH_MLCD_READ'
    exporting
      i_refresh_buffer             = fl_refresh_buffer
      it_kalnr                     = it_l_kalnr
      i_from_bdatj                 = p_bdatj
      i_from_poper                 = p_poper
      i_to_bdatj                   = p_bdatj
      i_to_poper                   = p_poper
*     I_RUNID                      =
*     IR_KEART                     =
*     IR_MLCCT                     =
*     IR_KKZST                     =
*     IR_PATNR                     =
*     IR_DIPA                      =
*     IR_CURTP                     =
    importing
      et_keph_mlcd                 = it_l_et_keph_mlcd
      et_keph_mlcd_not_alloc       = it_l_et_keph_mlcd_not_alloc.

  if p_chd = 'X'.
    loop at it_l_et_keph_mlcd into wa_l_ckmlkeph
       where categ = 'VN'
         and ptyp  = 'VF'
*      AND BVALT = P_BVALT
         and keart = 'H'
         and ( mlcct = 'V' or mlcct =  space )
         and kkzst = space.

      move-corresponding wa_l_ckmlkeph to it_keph_mlcd.
      append it_keph_mlcd.
      clear  it_keph_mlcd.
    endloop.
  else.
    loop at it_l_et_keph_mlcd into wa_l_ckmlkeph
       where categ = 'ZU'
         and ptyp  = 'BF'
*      AND BVALT = P_BVALT
         and keart = 'H'
         and ( mlcct = 'E' or mlcct =  space )
         and kkzst = space.

      move-corresponding wa_l_ckmlkeph to it_keph_mlcd.
      append it_keph_mlcd.
      clear  it_keph_mlcd.
    endloop.
  endif.


endform.                    " READ_KEPH_MLCD

*&---------------------------------------------------------------------*
*&      Form  CAL_SHOP_V_FOR_MATC
*&---------------------------------------------------------------------*
*       Cal Shop Cost - 540300, Cat. 'V'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_shop_v_for_matc.

* If PCC has material code, calculate ADD,
* MANU amount, quantiey with PCC amount and quanty.
  if it_tmp_shopcost-llv_matnr ne space.
    it_tmp_shopcost-add_wkgbtr = it_tmp_shopcost-wkgbtr.
    it_tmp_shopcost-add_mbgbtr = it_tmp_shopcost-mbgbtr.

* Get Unit Price
    if it_tmp_shopcost-add_mbgbtr <> 0.
      it_tmp_shopcost-preis
       = it_tmp_shopcost-add_wkgbtr / it_tmp_shopcost-add_mbgbtr.
    endif.

* Get ML Actual AMT (SHOP Actual)
    it_tmp_shopcost-ml_act_preis = it_tmp_shopcost-add_wkgbtr.

* Save Temp Cont.
    move-corresponding it_tmp_shopcost to it_sc_v .
    append it_sc_v.
    clear  it_sc_v.
  else.
* Read Additional Issue Infor.
    loop at  it_ztco_abispost
          where kokrs     =  it_tmp_shopcost-kokrs
            and gjahr     =  it_tmp_shopcost-bdatj
            and period    =  it_tmp_shopcost-poper
            and versn     =  it_tmp_shopcost-versn
            and kstar     =  it_tmp_shopcost-kstar
*             MATNR     =  IT_TMP_SHOPCOST-LLV_MATNR
            and werks     =  it_tmp_shopcost-werks
*             IO_AUFNR  =  IT_TMP_SHOPCOST-
            and pcc_aufnr =  it_tmp_shopcost-aufnr.

* Check Unit
      if it_tmp_shopcost-meeht eq space.
        it_tmp_shopcost-meeht = it_ztco_abispost-meinb.
      endif.
      if it_ztco_abispost-meinb <>  it_tmp_shopcost-meeht.
        perform unit_converion
                           using it_ztco_abispost-mbgbtr
                                 it_ztco_abispost-mbgbtr
                                 it_ztco_abispost-meinb
                                 it_tmp_shopcost-meeht.
      endif.
      it_tmp_shopcost-add_wkgbtr = it_ztco_abispost-chg_wkgbtr.
      it_tmp_shopcost-add_mbgbtr = it_ztco_abispost-mbgbtr.
** Additional AMT = ML Actual AMT
      it_tmp_shopcost-ml_act_preis = it_tmp_shopcost-add_wkgbtr.
* Save Temp Cont.
      move-corresponding it_tmp_shopcost to it_sc_v .
      it_sc_v-llv_matnr = it_ztco_abispost-matnr.
      append it_sc_v.
      clear  it_sc_v.
      clear it_ztco_abispost.
    endloop.
  endif.

* Delete Original Data
  delete  it_tmp_shopcost.

endform.                    " CAL_SHOP_V_FOR_MATC

*&---------------------------------------------------------------------*
*&      Form  DD_MAT_FOR_V_FOR_MATC
*&---------------------------------------------------------------------*
*       Re-Digging For 'V'
*       Cal Shop Cost - '540300', Cat. 'V'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dd_mat_for_v_for_matc.

  check not it_sc_v[] is initial.

* Read Material Information and SHOP
  perform read_shop_mat_of_v_matc.

* Read Cost Components from CK13N
  perform read_costcomp_for_matc_ck13n tables it_sc_v.

* Cal Rest Values of 'V'/'540300'
  perform cal_val_v_for_matc.

endform.                    " DD_MAT_FOR_V_FOR_MATC

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_MAT_OF_V_MATC
*&---------------------------------------------------------------------*
*       Read Material Information / SHOP
*       Cal Shop Cost - '540300', Cat. 'V'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_shop_mat_of_v_matc.
* Local Data Definition
  data : lv_shop  like  it_tmp_shopcost-shop.
  data : lv_werks like  t001w-werks.
  clear lv_shop.

  clear  it_sc_v.

  loop at  it_sc_v.
* Read Material Info
* Same Valuation Str. (Child and Parents)
    clear it_mat.
    read table it_mat
      with key matnr = it_sc_v-llv_matnr
               werks = it_sc_v-werks
               bwkey = it_sc_v-bwkey
               bwtar = it_sc_v-bwtar.

    it_sc_v-beskz     = it_mat-beskz.
    it_sc_v-sobsl     = it_mat-sobsl.
    it_sc_v-vspvb     = it_mat-vspvb.
    it_sc_v-chd_kalnr = it_mat-kalnr.

* Check Procurement Type / Spc Proc. Type
* -> Engine In Plant 'P001' -> 'E001'
    if   it_sc_v-beskz = 'F'
     and it_sc_v-sobsl = '40'.
*    LV_SHOP  = C_SHP_M_E001.
      lv_werks = 'E001'.
    else.
*    LV_SHOP  = C_SHP_M_P001.
      lv_werks = it_sc_v-werks.
    endif.

* Read SHOP Info. from Routing/Production version
    perform read_shop_fr_routing_prv
                               using it_sc_v-llv_matnr
                                     lv_shop
                                     lv_werks
                                     it_sc_v-bdatj
                                     it_sc_v-poper
                                     it_sc_v-kokrs.

* Change Plant/Shop
    it_sc_v-shop  = lv_shop.
    it_sc_v-werks = lv_werks.
* Bwkey
    clear it_t001w.
    read table it_t001w with key werks = it_sc_v-werks .
    it_sc_v-bwkey = it_t001w-bwkey.

* Clear
    clear : it_sc_v-wkgbtr,  it_sc_v-mbgbtr.

    modify it_sc_v.
    clear  it_sc_v.
  endloop.

endform.                    " READ_SHOP_MAT_OF_V_MATC

*&---------------------------------------------------------------------*
*&      Form  READ_COSTCOMP_FOR_MATC_CK13N
*&---------------------------------------------------------------------*
*       Cal Shop Cost - '540300', Cat. 'V'
*       Cost Components (CK13N)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_costcomp_for_matc_ck13n tables pt_sc structure it_tmp_shopcost
.

* Clear
  clear : it_ckikekokey,      it_ckikekokey[].
  clear : it_kkb_split_ck13n, it_kkb_split_ck13n[].

* Read Key For CComp
  perform read_costcomp_key tables pt_sc.
* Read Data For CComp
  perform read_costcomp_data.

endform.                    " READ_COSTCOMP_FOR_MATC_CK13N

*&---------------------------------------------------------------------*
*&      Form  READ_BEGIN_ENDING_DATE
*&---------------------------------------------------------------------*
*       Get Period dates
*----------------------------------------------------------------------*
*      -->P_PECNT  Period
*      -->P_LD     Latest Date
*      -->P_ED     Eariest Date
*----------------------------------------------------------------------*
form read_begin_ending_date using    p_pecnt
                                     p_ld
                                     p_ed.
* last Date
  clear p_ld.
  call function 'LAST_DAY_IN_PERIOD_GET'
    exporting
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_pecnt
    importing
      e_date               = p_ld
    exceptions
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      others               = 4.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* First Date
  clear p_ed.
  call function 'FIRST_DAY_IN_PERIOD_GET'
    exporting
      i_gjahr              = p_bdatj
*     I_MONMIT             = 00
      i_periv              = tka01-lmona
      i_poper              = p_pecnt
    importing
      e_date               = p_ed
    exceptions
      input_false          = 1
      t009_notfound        = 2
      t009b_notfound       = 3
      others               = 4.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " READ_BEGIN_ENDING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_COSTCOMP_KEY
*&---------------------------------------------------------------------*
*       Cal Shop Cost - '540300', Cat. 'V'
*       Cost Components (CK13N)
*       KEY PART
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_costcomp_key tables pt_sc structure it_tmp_shopcost.

** Local Data Definition.
  data : lv_ld       like sy-datum.
  data : lv_ed       like sy-datum.
  data : lv_klvar    like cki64a-klvar.
  data : it_l_tmp_ckikekokey
         like standard table of it_ckikekokey
         with header line .

** Set Parameters in selection data from table 'KEKO'
* -> Same As Period Standard
  lv_klvar = c_std.
*     GV_RECORD_TYPE = 'S'.
*     GV_VERWE = '1'.
*     GV_TARKZ = '001'.
  gv_freig = 'X'.
*   Costing Variants
  clear tck03.
  select single * from tck03
                 where klvar = lv_klvar.
  if sy-subrc <> 0.
    message w000 with text-101 lv_klvar.
  endif.

** Read Keko
  loop at pt_sc.
*   Get Period dates
    perform read_begin_ending_date
      using pt_sc-poper
            lv_ld
            lv_ed.

*   Read KEKO with a Date Range
    clear : it_l_tmp_ckikekokey, it_l_tmp_ckikekokey[].
    select * from keko
             into corresponding fields of table  it_l_tmp_ckikekokey
*             WHERE TVERS = P_TVERS
             where tvers = '01'
               and bwvar = tck03-bwvar
               and kalka = tck03-kalka
               and kkzma = space         "ALL Data
               and matnr = pt_sc-llv_matnr
               and werks = pt_sc-werks
               and bwkey = pt_sc-bwkey
               and bwtar = pt_sc-bwtar
               and kokrs = pt_sc-kokrs
               and ( kadat =< lv_ld and bidat => lv_ed )
               and freig = gv_freig.
*   FREIG = 'X' Only Mark Released - Standard Plan
*   FREIG = ' ' No   Mark Released - Business Plan
*Issue  requested by HS CHO
*Changed by wskim,on 03/19/2005
*-----Start
    sort it_l_tmp_ckikekokey
       by  bzobj  kalnr kalka  kadky descending.

    delete adjacent duplicates from it_l_tmp_ckikekokey comparing kalnr.
*-----End

* Set Period / Append .
    loop at it_l_tmp_ckikekokey.
      it_l_tmp_ckikekokey-bdatj = pt_sc-bdatj.
      it_l_tmp_ckikekokey-poper = pt_sc-poper.
      move-corresponding it_l_tmp_ckikekokey to it_ckikekokey.
      append it_ckikekokey.
      clear  it_ckikekokey.
    endloop.
    clear pt_sc.
  endloop.

  clear it_ckikekokey.

** Make Unique Key
  sort it_ckikekokey
    by
      bzobj
      kalnr
      kalka
      kadky
      tvers
      bwvar
      kkzma.
  delete adjacent duplicates from it_ckikekokey.

** Check Valid Data
  if it_ckikekokey[] is initial .
    message e026.
  endif.

endform.                    " READ_COSTCOMP_KEY

*&---------------------------------------------------------------------*
*&      Form  READ_COSTCOMP_DATA
*&---------------------------------------------------------------------*
*       Cal Shop Cost - '540300', Cat. 'V'
*       Cost Components (CK13N)
*       DATA PART
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_costcomp_data.
* Only COGM data should be considered
* -> view : '01'

  data : it_l_keph    like standard table of keph
                      with header line .
  data : it_l_kkb_split
                      like standard table of  kkb_split
                      with header line .
  data : wa_l_ckikekokey like ckikekokey .

  loop at it_ckikekokey.
* Move Keys
    move-corresponding it_ckikekokey to wa_l_ckikekokey  .
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
      message e079 with it_ckikekokey-matnr
                        it_ckikekokey-werks.
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
*W000       -> Total
*W001       -> Fixed
*W002       -> Variable
    loop at it_l_kkb_split
      where w000 ne space.
      move-corresponding  it_ckikekokey  to   it_kkb_split_ck13n.
      it_kkb_split_ck13n-elemt = it_l_kkb_split-elemt.
      it_kkb_split_ck13n-w000  = it_l_kkb_split-w000.
      append  it_kkb_split_ck13n.
      clear   it_kkb_split_ck13n.
    endloop.
  endloop.

  clear   it_kkb_split_ck13n.

endform.                    " READ_COSTCOMP_DATA

*&---------------------------------------------------------------------*
*&      Form  Cal_VAL_V_FOR_MATC
*&---------------------------------------------------------------------*
*       Cal Shop Cost - '540300', Cat. 'V'
*       Cal Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_val_v_for_matc.

  data :  it_l_sub_mip like standard table of it_tmp_shopcost
                       with header line .
  clear : it_l_sub_mip, it_l_sub_mip[].

  sort it_kkb_split_ck13n by bdatj poper matnr werks bwkey bwtar.
  sort it_sc_v            by bdatj poper llv_matnr werks bwkey bwtar.

  loop at  it_sc_v.
    loop at it_kkb_split_ck13n
                   where matnr = it_sc_v-llv_matnr
                     and werks = it_sc_v-werks
                     and bwkey = it_sc_v-bwkey
                     and bwtar = it_sc_v-bwtar
                     and bdatj = it_sc_v-bdatj
                     and poper = it_sc_v-poper.
* Copy   From IT_SC_V   to IT_L_SUB_MIP
      move-corresponding it_sc_v to it_l_sub_mip.
* CostComp Number
      it_l_sub_mip-elemt = it_kkb_split_ck13n-elemt.
* ML Actual Unit Price
      it_l_sub_mip-preis = it_kkb_split_ck13n-w000.
* (Additional AMT = ML Actual AMT) from CK13n
      clear : it_l_sub_mip-add_wkgbtr, it_l_sub_mip-ml_act_preis.
      it_l_sub_mip-ml_act_preis
      = it_l_sub_mip-add_wkgbtr
      = it_l_sub_mip-add_mbgbtr * it_l_sub_mip-preis.
* Append
      append it_l_sub_mip.
      clear  it_l_sub_mip.

      clear it_kkb_split_ck13n.
    endloop.
    clear it_sc_v.
  endloop.

* Append
  append lines of it_l_sub_mip to it_tmp_shopcost.
  clear it_tmp_shopcost.

endform.                    " Cal_VAL_V_FOR_MATC

*&---------------------------------------------------------------------*
*&      Form  KS_N_KSTAR_C_E
*&---------------------------------------------------------------------*
*       KSTAR = '836001', '836002'
*       ( 'E' ).
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ks_n_kstar_c_e.

* Clear Container
  perform clear_container_ic.

  clear it_tmp_shopcost.
  clear it_koat_p_1.
  clear it_koat_p_5.

* Read SHOP Info. and Convert the UOM -> HR
  perform read_shop_uom_for_e.

* Read M/H table - ZTCO_MHPCPOST
  perform read_mh_data.

* CAL. Additional Qty / AMT from M/H Table
  perform cal_add_qty_amt_from_mh.

* Read Default Unit of ATs
  perform read_de_unit_of_csla.

* CAL. CostComp / ML Actual Unit Price
  perform cal_costcomp_ml_act.

* Cal. Aditional Amt/PCC Current Amt/WIP/SCRAP
  perform cal_add_cur_amt.

* Cal. ML_ACT_PREIS.  IT_SC_E.
  perform cal_ml_act_preis_e.

  if not it_sc_e[] is initial.
* Append
    append lines of it_sc_e to it_tmp_shopcost.
    clear it_tmp_shopcost.
  endif.

endform.                    " KS_N_KSTAR_C_E

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_UOM_FOR_E
*&---------------------------------------------------------------------*
*       Read SHOP Info. and Convert the UOM -> HR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_shop_uom_for_e.

* Read SHOP
* Work Center = Cost center (1:1)
  loop  at it_tmp_shopcost
                     where   typps = 'E'.
    clear it_cctr.
    read table it_cctr with key kostl = it_tmp_shopcost-kostl.
    it_tmp_shopcost-shop = it_cctr-shop.

* Unit Conversion - MEEHT
    call function 'UNIT_CONVERSION_SIMPLE'
      exporting
        input                      = it_tmp_shopcost-mbgbtr
*       NO_TYPE_CHECK              = 'X'
*       ROUND_SIGN                 = ' '
        unit_in                    = it_tmp_shopcost-meeht
        unit_out                   = 'STD'
      importing
*       ADD_CONST                  =
*       DECIMALS                   =
*       DENOMINATOR                =
*       NUMERATOR                  =
        output                     = it_tmp_shopcost-mbgbtr
      exceptions
        conversion_not_found       = 1
        division_by_zero           = 2
        input_invalid              = 3
        output_invalid             = 4
        overflow                   = 5
        type_invalid               = 6
        units_missing              = 7
        unit_in_not_found          = 8
        unit_out_not_found         = 9
        others                     = 10.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      it_tmp_shopcost-meeht = 'STD'.
    endif.

* Copy Data
    move-corresponding it_tmp_shopcost to it_sc_e.
    append it_sc_e.
    clear  it_sc_e.

* Delete
    delete it_tmp_shopcost.
    clear  it_tmp_shopcost.
  endloop.

endform.                    " READ_SHOP_UOM_FOR_E

*&---------------------------------------------------------------------*
*&      Form  READ_MH_DATA
*&---------------------------------------------------------------------*
*       Read M/H table - ZTCO_MHPCPOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_mh_data.

* Local Data Definition
  data : it_tmp_mhpcpost like standard table of ztco_mhpcpost
                         with header line .

* Clear
  clear : it_mhsum, it_mhsum[].

* 'Posted / Not reversed'
  select
           gjahr perid matnr werks aufnr kostl lstar meinh varquan

           into corresponding fields of table it_tmp_mhpcpost
           from ztco_mhpcpost
          where gjahr = p_bdatj
            and perid between p_perab and w_perbi
            and rueck ne space
            and rmzhl ne space
            and reversed eq space
            and matnr in s_matnr.
* Common
*                AND  MHDOC IN S_MHDOC
*                AND  ERDAT IN S_ERDAT
*                AND  ERZET IN S_ERZET
*                AND  ERNAM IN S_ERNAM
*                AND  AEDAT IN S_AEDAT
*                AND  AEZET IN S_AEZET
*                AND  AENAM IN S_AENAM .

  if it_tmp_mhpcpost[] is initial.
    write : / text-091.
  endif.

* Collect Data
  loop at it_tmp_mhpcpost.
* Transaferring Data
    move-corresponding  it_tmp_mhpcpost to it_mhsum.
* Unit Conversion - MEEHT
    call function 'UNIT_CONVERSION_SIMPLE'
      exporting
        input                      = it_mhsum-varquan
*       NO_TYPE_CHECK              = 'X'
*       ROUND_SIGN                 = ' '
        unit_in                    = it_mhsum-meinh
        unit_out                   = 'STD'
      importing
*       ADD_CONST                  =
*       DECIMALS                   =
*       DENOMINATOR                =
*       NUMERATOR                  =
        output                     = it_mhsum-varquan
      exceptions
        conversion_not_found       = 1
        division_by_zero           = 2
        input_invalid              = 3
        output_invalid             = 4
        overflow                   = 5
        type_invalid               = 6
        units_missing              = 7
        unit_in_not_found          = 8
        unit_out_not_found         = 9
        others                     = 10.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      it_mhsum-meinh = 'STD'.
    endif.

* Collect
    collect it_mhsum.
    clear   it_mhsum.
    clear   it_tmp_mhpcpost.
  endloop.

* Sort
  sort  it_mhsum    by  gjahr perid matnr werks aufnr kostl lstar.

endform.                    " READ_MH_DATA

*&---------------------------------------------------------------------*
*&      Form  CAL_ADD_QTY_AMT_FROM_MH
*&---------------------------------------------------------------------*
*       CAL. Additional Qty / AMT from M/H Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_add_qty_amt_from_mh.

  clear : it_mhsum.
  clear : it_sc_e.

  sort  it_mhsum    by  gjahr perid matnr werks aufnr kostl lstar elemt.

* Set Add. Qty/AMT
  loop at it_sc_e.
    clear it_mhsum.
    read table it_mhsum
            with key gjahr = it_sc_e-bdatj
                     perid = it_sc_e-poper
                     matnr = it_sc_e-fsc_matnr
*                    WERKS
                     aufnr = it_sc_e-aufnr
                     kostl = it_sc_e-kostl
                     lstar = it_sc_e-lstar.
    if sy-subrc = 0.
      move : it_mhsum-varquan to it_sc_e-add_mbgbtr.
* APPEND
      modify it_sc_e.
    endif.
    clear it_sc_e.
  endloop.

endform.                    " CAL_ADD_QTY_AMT_FROM_MH

*&---------------------------------------------------------------------*
*&      Form  CAL_COSTCOMP_ML_ACT
*&---------------------------------------------------------------------*
*       CAL. CostComp / ML Actual Unit Price
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_costcomp_ml_act.

* Read Unit Price of  AT / From KSBT
  perform read_unit_price_fr_ksbt
                            using '04'
                                  '005'.
* Read Unit Price By CostComp
  cal_koat 5.

* Set ML Actual Unit Price
  perform set_act_un_price_e.

endform.                    " CAL_COSTCOMP_ML_ACT

*&---------------------------------------------------------------------*
*&      Form  READ_UNIT_PRICE_FR_KSBT
*&---------------------------------------------------------------------*
*       Read Unit Price of  AT / From KSBT
*----------------------------------------------------------------------*
*  -->  P_WRTTP        Value Type
*  <--  P_TARKZ        Price Ind.
*----------------------------------------------------------------------*
form read_unit_price_fr_ksbt
                       using p_wrttp
                             p_tarkz.

* Local Data Definition
  data : it_l_cost like standard table of cost
                   with header line .
  data : lv_cnt    like p_perab.
  field-symbols : <fs_tkg> type any, <fs_tke> type any.
  data : lv_tkg(30) value 'IT_L_COST-TKGxxx',
         lv_tke(30) value 'IT_L_COST-TKExxx'.

* Select
  select * into corresponding fields of table it_l_cost
           from cost
          where lednr  = '00'
*           and OBJNR
            and gjahr  = p_bdatj
            and wrttp  = p_wrttp                  "Actual 04 / Plan 01
            and versn  = p_versn
            and tarkz  = p_tarkz.                 "Price Ind. : 5,1

* Get data by Period (From P_PERAB to w_perbi)
* Clear
  clear : it_ccr1t, it_ccr1t[].

  loop at it_l_cost.
    clear lv_cnt.
    lv_cnt  = p_perab.
    while lv_cnt <= w_perbi.
* Trans. Data
      move-corresponding it_l_cost to it_ccr1t.
      move lv_cnt to: lv_tkg+13, lv_tke+13.
      assign: (lv_tkg) to <fs_tkg>,
              (lv_tke) to <fs_tke>.
      it_ccr1t-tkgxxx = <fs_tkg> / <fs_tke>.
* Set Period.
      it_ccr1t-periode = lv_cnt.
* Kostl/Lstar
      call function 'OBJECT_KEY_GET_KL'
           exporting
                objnr       = it_ccr1t-objnr
           importing
                kokrs       = p_kokrs
                kostl       = it_ccr1t-kostl
                lstar       = it_ccr1t-lstar
           exceptions
                not_found   = 1
                wrong_obart = 2
                others      = 3.
      if sy-subrc <> 0.
      endif.
* Append
      append it_ccr1t.
      clear  it_ccr1t.
* Period Counter.
      lv_cnt = lv_cnt + 1.
    endwhile .
    clear  it_l_cost.
  endloop.

  clear  it_ccr1t.

endform.                    " READ_UNIT_PRICE_FR_KSBT

*&---------------------------------------------------------------------*
*&      Form  SET_ACT_UN_PRICE_E
*&---------------------------------------------------------------------*
*       Set ML Actual Unit Price
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_act_un_price_e.

* Local Data definition
  data : it_tmp_sc_e like standard table of it_sc_e
                     with header line .

* Copy
  clear : it_tmp_sc_e, it_tmp_sc_e[].
  it_tmp_sc_e[] =  it_sc_e[].
  clear : it_sc_e, it_sc_e[].

* W/o Additional Info.
  loop at it_tmp_sc_e.
    loop at it_koat_p_5
                  where gjahr = it_tmp_sc_e-bdatj
                    and poper = it_tmp_sc_e-poper
                    and kostl = it_tmp_sc_e-kostl
                    and lstar = it_tmp_sc_e-lstar.
* Copy
      move-corresponding it_tmp_sc_e to it_sc_e.

* Set ML Actual Unit Price
      it_sc_e-preis = it_koat_p_5-tkgxxx.
* Set Cost Comp. Num.
      it_sc_e-elemt = it_koat_p_5-elemt.
* Collect
      collect it_sc_e.
      clear   it_sc_e.
      clear it_koat_p_5.
    endloop.
    clear it_tmp_sc_e.
  endloop.

  clear   it_sc_e.

* CAL. Current Qty
  loop at it_sc_e.
    it_sc_e-mbgbtr = it_sc_e-mbgbtr - it_sc_e-add_mbgbtr.
    modify it_sc_e.
    clear  it_sc_e.
  endloop.

  clear   it_sc_e.

endform.                    " SET_ACT_UN_PRICE_E

*&---------------------------------------------------------------------*
*&      Form  CAL_ADD_CUR_AMT
*&---------------------------------------------------------------------*
*       Calculate Additional Amt/Current Amt/WIP/SCRAP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_add_cur_amt.

* Read Unit Price of  AT / From KSBT
  perform read_unit_price_fr_ksbt
                            using '01'
                                  '001'.
* Read Unit Price By CostComp
  cal_koat 1.

* Set Curr/Add Amt/WIP/SCRAP
  perform set_curr_add_amt_e.

endform.                    " CAL_ADD_CUR_AMT

*&---------------------------------------------------------------------*
*&      Form  SET_CURR_ADD_AMT_E
*&---------------------------------------------------------------------*
*       Set Curr/Add Amt/WIP/SCRAP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_curr_add_amt_e.
*
  loop at it_sc_e.
    clear it_koat_p_1 .
    read table it_koat_p_1
                  with key
                    gjahr = it_sc_e-bdatj
                    poper = it_sc_e-poper
                    kostl = it_sc_e-kostl
                    lstar = it_sc_e-lstar
                    elemt = it_sc_e-elemt.
* Curr. Amt.
    clear it_sc_e-wkgbtr.
    it_sc_e-wkgbtr = it_sc_e-mbgbtr * it_koat_p_1-tkgxxx.

* Add. Amt.
    clear it_sc_e-add_wkgbtr.
    it_sc_e-add_wkgbtr = it_sc_e-add_mbgbtr * it_koat_p_1-tkgxxx.

* WIP / SCrap
    perform set_wip_scr_into_tab
      using it_sc_e-bdatj
            it_sc_e-poper
            it_sc_e-objnr
            it_sc_e-llv_matnr
            it_sc_e-bwkey
            it_sc_e-bwtar
*     <- value
            it_sc_e-mbgbtr
            it_sc_e-wkgbtr
*     -> value
            it_sc_e-wip_amt
            it_sc_e-scrap_amt
            it_sc_e-wip_quantity
            it_sc_e-actual_scrap
            it_sc_e-manu_qty
            it_sc_e-meeht
            it_sc_e-kostl
            it_sc_e-lstar
            '02'.

    modify it_sc_e.
    clear it_sc_e.
  endloop.

  clear   it_sc_e.

endform.                    " SET_CURR_ADD_AMT_E

*&---------------------------------------------------------------------*
*&      Form  KS_6_KSTAR_C_V
*&---------------------------------------------------------------------*
*       KSTAR = '6*' / with 'V'
*       ( 'V' ).
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ks_6_kstar_c_v.

* Clear Container
  perform clear_container_ic.

  clear it_tmp_shopcost.
  clear it_koat_p_1.
  clear it_koat_p_5.

  loop at it_tmp_shopcost
      where  typps = c_cat_v
        and
         ( kstar >= '0000600000' and kstar < '0000700000' ).
* SET SHOP = 'MXTX'
    it_tmp_shopcost-shop  = 'MXTX'.
* Read Cost Comp. Number
    perform read_cost_comp using it_tmp_shopcost-kstar
                                 it_tmp_shopcost-elemt.
* ML Actual Amt = PCC Current Amt
    it_tmp_shopcost-ml_act_preis = it_tmp_shopcost-wkgbtr.
* Modify
    modify it_tmp_shopcost.
    clear  it_tmp_shopcost.
  endloop.

endform.                    " KS_6_KSTAR_C_V

*&---------------------------------------------------------------------*
*&      Form  DEL_DATA_FR_ZTCO_SHOPCOST_AT
*&---------------------------------------------------------------------*
*       - > Always deletion -> Refresh data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form del_data_fr_ztco_shopcost_at.


  delete from ztco_shopcost_at
        where
              kokrs        =  p_kokrs
          and bdatj        =  p_bdatj
          and poper   between p_perab and w_perbi
*          AND KLVAR        =  P_KLVAR
          and versn        =  p_versn
          and record_type  =  gv_record_type
          and fsc_matnr    in s_matnr.
  if sy-subrc = 0.
  endif.
* No- Error Check In Deletion Phase
  commit work and wait.

endform.                    " DEL_DATA_FR_ZTCO_SHOPCOST_AT

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_SHOPCOST_AT
*&---------------------------------------------------------------------*
*       Update/Insert
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_shopcost_at.

  loop at it_tmp_shopcost.
* LOG
    it_tmp_shopcost-erdat = sy-datum.
    it_tmp_shopcost-erzet = sy-uzeit.
    it_tmp_shopcost-ernam = sy-uname.
* CURKY
    if it_tmp_shopcost-hwaer eq space .
      it_tmp_shopcost-hwaer = tka01-waers.
    endif.
    clear ztco_shopcost_at.
    move-corresponding  it_tmp_shopcost to ztco_shopcost_at.
    insert ztco_shopcost_at .
    if sy-subrc <> 0.
*      WRITE : / ZTCO_SHOPCOST_AT .
      message e044.
    endif.
    clear it_tmp_shopcost.
  endloop.

endform.                    " UPDATE_ZTCO_SHOPCOST_AT

*&---------------------------------------------------------------------*
*&      Form  UP_INS_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form up_ins_log.
* LOG
  describe table it_tmp_shopcost lines sy-tfill.
  write : / 'No. of Created Records : ' , sy-tfill.
  write : / 'Created date           : ' , sy-datum.
  write : / 'Created By             : ' , sy-uname.
  skip 1.
  write : / text-190.

* Success
  message s009 with 'Data Creation'.

endform.                    " UP_INS_LOG

*&---------------------------------------------------------------------*
*&      Form  READ_DE_UNIT_OF_CSLA
*&---------------------------------------------------------------------*
*       Read default Unit of CSLA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_de_unit_of_csla.

* Local Data Definition
  data : lv_datum like sy-datum.
  data : lv_input type i value '1'.

* Read CSLA
  clear : it_csla, it_csla[].
  select * into corresponding fields of table it_csla
           from csla inner join t006
             on csla~leinh = t006~msehi
            and t006~dimid = 'TIME'.

  if  it_csla[] is initial.
    message e000 with text-103.
  endif.

* Read Deb. Num.
  loop at it_csla.
* To STD
    it_csla-leinh_out = 'STD'.

* get Numerator / Denominator
    call function 'UNIT_CONVERSION_SIMPLE'
         exporting
              input                      = lv_input
*             NO_TYPE_CHECK              = 'X'
*             ROUND_SIGN                 = ' '
              unit_in                    = it_csla-leinh
              unit_out                   = it_csla-leinh_out
         importing
*             ADD_CONST                  =
*             DECIMALS                   =
              denominator                = it_csla-denominator
              numerator                  = it_csla-numerator
*             OUTPUT                     =
         exceptions
              conversion_not_found       = 1
              division_by_zero           = 2
              input_invalid              = 3
              output_invalid             = 4
              overflow                   = 5
              type_invalid               = 6
              units_missing              = 7
              unit_in_not_found          = 8
              unit_out_not_found         = 9
              others                     = 10.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

* Modify
    modify it_csla.
    clear  it_csla.

  endloop.

endform.                    " READ_DE_UNIT_OF_CSLA

*&---------------------------------------------------------------------*
*&      Form  CAL_ML_ACT_PREIS_E
*&---------------------------------------------------------------------*
*       Cal. ML Actual AMT (SHOP Actual)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_ml_act_preis_e.

* ( MBGBTR + ADD_MBGBTR ) * PREIS = ML_ACT_PREIS.

  clear  it_sc_e.

  loop at  it_sc_e.
    it_sc_e-ml_act_preis
     = ( it_sc_e-mbgbtr + it_sc_e-add_mbgbtr ) * it_sc_e-preis .
    modify it_sc_e.
    clear  it_sc_e.
  endloop.

endform.                    " CAL_ML_ACT_PREIS_E

*&---------------------------------------------------------------------*
*&      Form  SET_PER_BF_UP
*&---------------------------------------------------------------------*
*       Insert Period Key
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_per_bf_up.

* Always Insertion
  clear it_tmp_shopcost.


**// Begin of Mod. By Hyung Jin Youn  2004.07.06
*    If data with Period "000", the data should have period.
*    This case can be occured when wrong data were generated in
*    standard system (No SHOP information)
* Local Data Definition
  data : lv_percount like cosp-perbl. "Period Counter
  data : lv_cnt  like  cosp-perbl.
  data : it_l_per000 like  standard table of it_tmp_shopcost
                     with header line .
  data : it_l_perxxx like  standard table of it_tmp_shopcost
                     with header line .

  loop at it_tmp_shopcost where poper = space.
    move-corresponding it_tmp_shopcost to it_l_per000.
    append it_l_per000.
    clear  it_l_per000.
* Delete
    delete it_tmp_shopcost.
    clear  it_tmp_shopcost.
  endloop.

* Clear
  clear : it_l_perxxx,  it_l_perxxx[].

* Cal. the Counter
  lv_percount = w_perbi - p_perab + 1.

* Period Counter : Set From-Period .
  clear lv_cnt.
  lv_cnt = p_perab .

* From period - To period
  do lv_percount times.
    loop at it_l_per000.
      move-corresponding it_l_per000 to it_l_perxxx.
      it_l_perxxx-poper = lv_cnt.
      append it_l_perxxx.
      clear  it_l_perxxx.
      clear  it_l_per000.
    endloop.
* Period Counter
    lv_cnt = lv_cnt + 1.
  enddo.

* Appending
  append lines of  it_l_perxxx  to it_tmp_shopcost.
  clear it_tmp_shopcost.

**// End of Mod.

endform.                    " SET_PER_BF_UP

*&---------------------------------------------------------------------*
*&      Form  CAL_ACT_MANU_DATA
*&---------------------------------------------------------------------*
*       Cal. Actual Manufacture qty. and Amt.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_act_manu_data.
* Actual Manufacture Qty
* = (Previous Month) WIP Qty + Current Qty + Additional Issue Qty -
*   Current WIP Qty.
* Actual Manufacture Amt
* = (Previous Month) WIP Amt + Actual Input Amt - Current WIP Amt.

  data : lv_pr_wip_amt      like ztco_shopcost_at-wip_amt.
  data : lv_pr_wip_quantity like ztco_shopcost_at-wip_quantity.
  data : lv_previous_period like  ckmlpp-poper,
         lv_previous_year   like  ckmlpp-bdatj.
  data : wa_l_shop          like it_tmp_shopcost.


  loop at it_tmp_shopcost.
* Previous Period
* Using TKA01-LMONA
    clear : lv_previous_period, lv_previous_year.
    call function 'CKML_F_GET_PREVIOUS_PERIOD'
         exporting
              input_period    = it_tmp_shopcost-poper
              input_year      = it_tmp_shopcost-bdatj
              input_periv     = tka01-lmona
         importing
              previous_period = lv_previous_period
              previous_year   = lv_previous_year.

**
    clear : lv_pr_wip_amt, lv_pr_wip_quantity.
* From Itab
    clear wa_l_shop.
    read table it_tmp_shopcost with key
                       kokrs       = it_tmp_shopcost-kokrs
                       bdatj       = lv_previous_year
                       poper       = lv_previous_period
                       aufnr       = it_tmp_shopcost-aufnr
                       versn       = it_tmp_shopcost-versn
                       record_type = it_tmp_shopcost-record_type
                       fsc_matnr   = it_tmp_shopcost-fsc_matnr
                       shop        = it_tmp_shopcost-shop
                       llv_matnr   = it_tmp_shopcost-llv_matnr
                       typps       = it_tmp_shopcost-typps
                       kstar       = it_tmp_shopcost-kstar
                       kalnr       = it_tmp_shopcost-kalnr
                       elemt       = it_tmp_shopcost-elemt
                       kostl       = it_tmp_shopcost-kostl
                       lstar       = it_tmp_shopcost-lstar
                    into wa_l_shop.
    if sy-subrc = 0.
      lv_pr_wip_amt      = wa_l_shop-wip_amt.
      lv_pr_wip_quantity = wa_l_shop-wip_quantity.
    else.
* From table
      select single
             wip_amt  wip_quantity
                      from ztco_shopcost_at
                      into (lv_pr_wip_amt, lv_pr_wip_quantity)
                     where
                            kokrs       = it_tmp_shopcost-kokrs
                       and  bdatj       = lv_previous_year
                       and  poper       = lv_previous_period
                       and  aufnr       = it_tmp_shopcost-aufnr
                       and  versn       = it_tmp_shopcost-versn
                       and  record_type = it_tmp_shopcost-record_type
                       and  fsc_matnr   = it_tmp_shopcost-fsc_matnr
                       and  shop        = it_tmp_shopcost-shop
                       and  llv_matnr   = it_tmp_shopcost-llv_matnr
                       and  typps       = it_tmp_shopcost-typps
                       and  kstar       = it_tmp_shopcost-kstar
                       and  kalnr       = it_tmp_shopcost-kalnr
                       and  elemt       = it_tmp_shopcost-elemt
                       and  kostl       = it_tmp_shopcost-kostl
                       and  lstar       = it_tmp_shopcost-lstar.
    endif.

* Cal Manufacture Data

* If Catagory is 'V', manufactur quantity is additional quantity.
    if it_tmp_shopcost-typps eq 'V'.
      it_tmp_shopcost-manu_qty = it_tmp_shopcost-add_mbgbtr.
    endif.

* If material type is 'HALB' and parent material is BIP, BIW,
* recalculate manufacture quantity
    if       it_tmp_shopcost-mtart eq 'HALB' and
       not ( it_tmp_shopcost-fsc_matnr+4(2) eq 'XX' and
             it_tmp_shopcost-fsc_matnr+4(2) eq 'XY' ).
      it_tmp_shopcost-manu_qty = lv_pr_wip_quantity
                               + it_tmp_shopcost-mbgbtr
                               + it_tmp_shopcost-add_mbgbtr
                               - it_tmp_shopcost-wip_quantity.
    endif.

    perform calculate_manu_amt using lv_pr_wip_amt lv_pr_wip_quantity.

    modify it_tmp_shopcost.
    clear  it_tmp_shopcost.
  endloop.

endform.                    " CAL_ACT_MANU_DATA

*&---------------------------------------------------------------------*
*&      Form  DISP_FRACTION_AMT
*&---------------------------------------------------------------------*
*       Fractional Amount
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form disp_fraction_amt.
* Local data definition
  data : it_lt_cosba like standard table of cosba
                     with header line .
* Clear
  clear : it_cosba, it_cosba[].
  clear : it_frpcc, it_frpcc[].
  clear : it_coep.

  loop at it_coep.
    move-corresponding it_coep to it_frpcc.
*   Item Category
    perform conv_beweg_to_typps
      using it_coep-beweg
            it_frpcc-typps.
*   Collect
    collect it_frpcc.
    clear   it_frpcc.
  endloop.


* Read COSB ( Only Debit data / BEKNZ = 'S' )
  data : lv_kkb_beweg type kkb_beweg.
  clear : it_lt_cosba, it_lt_cosba[].
  select * from cosb
           into corresponding fields of table it_lt_cosba
           for all entries in it_fsc_mat
           where lednr = '00'
             and objnr = it_fsc_mat-objnr
             and gjahr = p_bdatj
             and wrttp in ('32', '30')
             and versn = p_versn
             and beknz = c_gp_beknz.
  sort  it_lt_cosba by objnr.
  clear it_lt_cosba.

* COSB
  scr_sum_data b.

  clear : it_cosba.

* By Period
  field-symbols: <fs1> type any.
  data : lv_cosb_wkg(30). " VALUE 'IT_COSBA-WKG'.
  data : lv_cnt  like  cosp-perbl.
  data : gv_percount like cosp-perbl. "Period Counter

  gv_percount = w_perbi - p_perab + 1.

  loop at it_cosba.
* Period Counter : Set From-Period . /w_perbi
    clear lv_cnt.
    lv_cnt = p_perab .
*
    do gv_percount times.
* Key part
      move-corresponding it_cosba to it_frpcc.
* period
      it_frpcc-perio = lv_cnt.
* Item Category
      perform conv_beweg_to_typps
        using it_cosba-beweg
              it_frpcc-typps.
* Value
      clear lv_cosb_wkg.
      concatenate 'IT_COSBA-WKG'   lv_cnt
             into lv_cosb_wkg.
      assign (lv_cosb_wkg) to <fs1>.
* Scrap/Wip
      case it_cosba-wrttp.
        when '32'. "WIP
          it_frpcc-wip_amt   = <fs1>.
        when '30'. "SCrap
          it_frpcc-scrap_amt = <fs1>.
      endcase.
* Collect
      collect it_frpcc.
      clear   it_frpcc.
* Period Counter
      lv_cnt = lv_cnt + 1.
    enddo.
    clear it_cosba.
  endloop.

  clear  it_frpcc.
  clear  it_tmp_shopcost.

endform.                    " DISP_FRACTION_AMT

*&---------------------------------------------------------------------*
*&      Form  DISP_FRACTION_BY_CATE
*&---------------------------------------------------------------------*
*       Fr. With  PCC data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form disp_fraction_by_cate.

* Local Data definition
  data : it_l_tmp_frpcc like standard table of it_frpcc
                        with header line .

  clear  it_frpcc.
  clear  it_tmp_shopcost.

  sort it_tmp_shopcost by typps kstar.

* From Shop data
  loop at it_tmp_shopcost where kstar  < '600000'
                            or  kstar >= '700000'.
    move-corresponding it_tmp_shopcost to it_l_tmp_frpcc.
*
    it_l_tmp_frpcc-twaer  =  it_tmp_shopcost-hwaer.
    it_l_tmp_frpcc-gjahr  =  it_tmp_shopcost-bdatj.
    it_l_tmp_frpcc-perio  =  it_tmp_shopcost-poper.
*   Collect
    collect it_l_tmp_frpcc.
    clear   it_l_tmp_frpcc.
  endloop.

  clear it_l_tmp_frpcc.

* Compare PCC with SHOP data
  sort it_frpcc
    by      typps
            perio
            objnr
            gjahr
            versn
            kstar .

  sort it_l_tmp_frpcc
    by      typps
            perio
            objnr
            gjahr
            versn
            kstar .

  data : it_l_tmp_pccshop
         like standard table of it_tmp_shopcost
         with header line .

  clear : it_l_tmp_pccshop, it_l_tmp_pccshop[].

  loop at it_frpcc.
*
    clear it_l_tmp_pccshop.
    move-corresponding  it_frpcc to  it_l_tmp_pccshop.
*   CO Area
    it_l_tmp_pccshop-kokrs = p_kokrs.
*   Period
    it_l_tmp_pccshop-bdatj = p_bdatj.
    it_l_tmp_pccshop-poper = it_frpcc-perio.
*   Currency / Unit
    it_l_tmp_pccshop-hwaer = tka01-waers.
*   Record Type
    it_l_tmp_pccshop-record_type = gv_record_type.
*   Read Parents Material Info.
    clear it_fsc_mat.
    read table it_fsc_mat with key objnr = it_frpcc-objnr.
    move-corresponding it_fsc_mat to it_l_tmp_pccshop.
    it_l_tmp_pccshop-fsc_matnr = it_fsc_mat-matnr .

**// Mod. By Hyung Jin Youn 2004.08.18
* NO Wip/Scrap Adjustment
* Do not cal. Fraction amount for Wip and Scrap

** Wip /Scrap ("-" Value - IT_FRPCC)
    clear it_l_tmp_frpcc.
    read table it_l_tmp_frpcc
     with key typps = it_l_tmp_pccshop-typps
              perio = it_l_tmp_pccshop-poper
              objnr = it_l_tmp_pccshop-objnr
              gjahr = it_l_tmp_pccshop-bdatj
              versn = it_l_tmp_pccshop-versn
              kstar = it_l_tmp_pccshop-kstar
              twaer = it_l_tmp_pccshop-hwaer.
** Wip
    clear it_l_tmp_pccshop-wip_amt.
*    IT_L_TMP_PCCSHOP-WIP_AMT
*      = ( IT_L_TMP_PCCSHOP-WIP_AMT * ( -1 ) )
*      -   IT_L_TMP_FRPCC-WIP_AMT.
** Scrap
    clear it_l_tmp_pccshop-scrap_amt.
*    IT_L_TMP_PCCSHOP-SCRAP_AMT
*      = ( IT_L_TMP_PCCSHOP-SCRAP_AMT * ( -1 ) )
*      -   IT_L_TMP_FRPCC-SCRAP_AMT.
**// End of Mod.

* Current AMT and Additional Amt
    clear : it_l_tmp_pccshop-wkgbtr, it_l_tmp_pccshop-add_wkgbtr.

    case it_l_tmp_pccshop-typps.
      when 'E'.
        it_l_tmp_pccshop-wkgbtr
          = it_frpcc-wkgbtr
          - ( it_l_tmp_frpcc-wkgbtr + it_l_tmp_frpcc-add_wkgbtr ).
      when 'M'.
        it_l_tmp_pccshop-wkgbtr
          = it_frpcc-wkgbtr
          - it_l_tmp_frpcc-wkgbtr.
      when 'V'.
        it_l_tmp_pccshop-add_wkgbtr
          = it_frpcc-wkgbtr
          - it_l_tmp_frpcc-add_wkgbtr.
    endcase.

* Collect
    collect it_l_tmp_pccshop.
    clear   it_l_tmp_pccshop.
  endloop.

* Delete Init. Val.
  delete it_l_tmp_pccshop
   where wip_amt    is initial
     and scrap_amt  is initial
     and wkgbtr     is initial
     and add_wkgbtr is initial.

  clear   it_l_tmp_pccshop.

* Append
  append lines of it_l_tmp_pccshop to it_tmp_shopcost.
  clear it_tmp_shopcost.

endform.                    " DISP_FRACTION_BY_CATE

*&---------------------------------------------------------------------*
*&      Form  DISP_FRACTION_BY_MLCC
*&---------------------------------------------------------------------*
*       Fr. With  ML CC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form disp_fraction_by_mlcc.

* Local Data definition
  data : it_l_tmp_comp  like standard table of it_sc_m
                        with header line .
  data : begin of it_l_fsc_klanr occurs 0,
           bdatj     like it_sc_m-bdatj    ,
           poper     like it_sc_m-poper    ,
           matnr     like mara-matnr,
           kalnr     like it_sc_m-kalnr,
           bwkey     like it_sc_m-bwkey,
         end of   it_l_fsc_klanr.
  data : begin of it_l_frmlcc occurs 0.
          include structure it_tmp_shopcost.
  data : end of it_l_frmlcc.

  clear it_tmp_shopcost.

* Sum Manufacture Amt
* Clear
  clear : it_frml, it_frml[].

  loop at it_tmp_shopcost where not elemt is initial.
    move-corresponding it_tmp_shopcost to it_frml.
    collect it_frml.
    clear   it_frml.
  endloop.

  sort  it_frml  by  bdatj poper objnr versn elemt.

  clear   it_frml.

* Read ML Cost Component
* Clear
  clear : it_keph_mlcd, it_keph_mlcd[].
  clear : it_l_fsc_klanr, it_l_fsc_klanr[].
  clear : it_l_frmlcc, it_l_frmlcc[].
  clear   it_fsc_mat.


  loop at it_frml.
    loop at it_fsc_mat where objnr = it_frml-objnr.
      move-corresponding it_frml    to it_l_fsc_klanr.
      move-corresponding it_fsc_mat to it_l_fsc_klanr.

      move-corresponding it_frml    to it_l_frmlcc.
      move-corresponding it_fsc_mat to it_l_frmlcc.
      clear it_l_frmlcc-elemt.
      clear it_l_frmlcc-manu_amt.
      it_l_frmlcc-fsc_matnr = it_fsc_mat-matnr.

      collect it_l_fsc_klanr.
      clear   it_l_fsc_klanr.

      collect it_l_frmlcc.
      clear   it_l_frmlcc.

      clear it_fsc_mat.
    endloop.
    clear it_frml.
  endloop.

  sort it_l_fsc_klanr by bdatj poper.

  loop at it_l_fsc_klanr.
* Read MLCD_KEPH
    perform read_keph_mlcd
      using it_l_fsc_klanr-bdatj
            it_l_fsc_klanr-poper
            it_l_fsc_klanr-matnr
            it_l_fsc_klanr-kalnr
            it_l_fsc_klanr-bwkey
            space.
  endloop.

  clear : it_keph_mlcd.

* Read Cost Component (From ML View - Preliminary View)
  data : it_l_keph      like standard table of keph
                        with header line .
  data : wa_l_ckmlkeph like ckmlkeph.
  data : wa_l_e_process	like	ckml_s_process,
         wa_l_e_ckmlmv001	like	ckmlmv001.
* Clear
  clear : it_l_tmp_comp, it_l_tmp_comp[].

  loop at it_l_frmlcc.
    clear : it_kkb_split, it_kkb_split[].
    clear : it_l_keph, it_l_keph[].
    clear : wa_l_ckmlkeph.


**  Process Number / CKMLMV001 / CKML_MGV_PROCESS_READ
*   Read Process Number for 'BF' process in FSC itfelf
    clear : wa_l_e_process, wa_l_e_ckmlmv001.

    call function 'CKML_MGV_PROCESS_READ'
      exporting
        i_kalnr           = it_l_frmlcc-par_proc_kalnr
*       I_BUFFER          =
      importing
        e_process         = wa_l_e_process
        e_ckmlmv001       = wa_l_e_ckmlmv001
      exceptions
        not_found         = 1
        others            = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if wa_l_e_ckmlmv001-btyp <> 'BF'.
      continue.
    endif.
    loop at it_keph_mlcd into wa_l_ckmlkeph
      where bdatj = it_l_frmlcc-bdatj
        and poper = it_l_frmlcc-poper
        and kalnr = it_l_frmlcc-kalnr
        and categ = 'ZU'
        and ptyp  = wa_l_e_ckmlmv001-btyp
        and bvalt = wa_l_e_ckmlmv001-kalnr
        and keart = 'H'
        and mlcct = space
        and kkzst = space.

      move-corresponding wa_l_ckmlkeph to it_l_keph .
      append it_l_keph .
      clear  it_l_keph .
    endloop.

    call function 'K_KKB_SPLITTING_CONVERT'
         exporting
              i_elehk     = 'H1'
              i_sicht     = '06'
              i_keart     = 'H'
              i_losfx     = space
              i_waers     = 'USD'
         tables
              t_keph      = it_l_keph
              t_kkb_split = it_kkb_split.

* Delete the records which have "Zero" value
    delete it_kkb_split where w000 eq space.

    loop at it_kkb_split.
      move-corresponding  it_l_frmlcc  to  it_l_tmp_comp.
      it_l_tmp_comp-elemt  = it_kkb_split-elemt.
      it_l_tmp_comp-manu_amt
                           = it_kkb_split-w000 .
      collect it_l_tmp_comp.
      clear   it_l_tmp_comp.
    endloop.
    clear it_l_frmlcc.
  endloop.

  clear   it_l_tmp_comp.

* Calculation Discrepency
  data :  it_l_frmlcc_shopcost
          like standard table of it_tmp_shopcost
          with header line .

  loop at it_frml.
    loop at it_l_tmp_comp
      where bdatj = it_frml-bdatj
        and poper = it_frml-poper
        and objnr = it_frml-objnr
        and versn = it_frml-versn
        and elemt = it_frml-elemt
        and hwaer = it_frml-hwaer.

      move-corresponding it_frml to it_l_frmlcc_shopcost.
* Missing Key
      it_l_frmlcc_shopcost-kokrs       = p_kokrs.
      it_l_frmlcc_shopcost-record_type = gv_record_type.
      it_l_frmlcc_shopcost-typps       = 'V'.

* Read Parents Material Info.
      clear it_fsc_mat.
      read table it_fsc_mat with key objnr = it_l_frmlcc_shopcost-objnr.
      move-corresponding it_fsc_mat to it_l_frmlcc_shopcost.
      it_l_frmlcc_shopcost-fsc_matnr = it_fsc_mat-matnr .
* Value
      it_l_frmlcc_shopcost-manu_amt
       = it_l_tmp_comp-manu_amt  - it_frml-manu_amt.
* Collect
      collect it_l_frmlcc_shopcost.
      clear it_l_frmlcc_shopcost.
    endloop.
  endloop.

* Append
  append lines of it_l_frmlcc_shopcost to it_tmp_shopcost.
  clear it_tmp_shopcost.

endform.                    " DISP_FRACTION_BY_MLCC


*
*&---------------------------------------------------------------------*
*&      Form  cal_val_m_for_matc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_val_m_for_matc.
* Local Data definition
  data : it_l_tmp_comp  like standard table of it_sc_m
                        with header line .
  data : begin of it_l_chd_klanr occurs 0,
           bdatj     like it_sc_m-bdatj    ,
           poper     like it_sc_m-poper    ,
           llv_matnr like it_sc_m-llv_matnr,
           chd_kalnr like it_sc_m-chd_kalnr,
           bwkey     like it_sc_m-bwkey,
         end of   it_l_chd_klanr.

* Clear
  clear : it_keph_mlcd, it_keph_mlcd[].

  clear  it_tmp_shopcost.
  clear  it_sc_m.

* Extract MLCCS Tab
  loop at it_sc_m.
    move-corresponding it_sc_m to it_l_chd_klanr.
    collect it_l_chd_klanr.
    clear it_l_chd_klanr.
  endloop.

  sort it_l_chd_klanr by bdatj poper.

  loop at it_l_chd_klanr.
* Read MLCD_KEPH
    perform read_keph_mlcd
      using it_l_chd_klanr-bdatj
            it_l_chd_klanr-poper
            it_l_chd_klanr-llv_matnr
            it_l_chd_klanr-chd_kalnr
            it_l_chd_klanr-bwkey
            'X'.
  endloop.

  clear : it_keph_mlcd.

* Read Cost Components data
  clear : it_l_tmp_comp, it_l_tmp_comp[].

  data: lw_index type i.
  loop at it_sc_m.
* Read Current amount
    loop at it_kkb_split_ck13n
                   where matnr = it_sc_m-llv_matnr
                     and werks = it_sc_m-werks
                     and bwkey = it_sc_m-bwkey
                     and bwtar = it_sc_m-bwtar
                     and bdatj = it_sc_m-bdatj
                     and poper = it_sc_m-poper.
      lw_index = lw_index + 1.
      clear   it_l_tmp_comp.
      move-corresponding  it_sc_m  to  it_l_tmp_comp.
* Clear ML Actual AMT.
      clear : it_l_tmp_comp-ml_act_preis.

      it_l_tmp_comp-elemt  = it_kkb_split_ck13n-elemt.
      it_l_tmp_comp-wkgbtr = it_kkb_split_ck13n-w000 *
                             it_l_tmp_comp-mbgbtr.


      perform get_wip_scrap using it_l_tmp_comp-bdatj
                                  it_l_tmp_comp-poper
                                  it_l_tmp_comp-objnr
                                  it_l_tmp_comp-llv_matnr
                                  it_l_tmp_comp-bwkey
                                  it_l_tmp_comp-bwtar
                                  it_l_tmp_comp-kostl
                                  it_l_tmp_comp-lstar
                                  it_l_tmp_comp-llv_matnr
                                  it_l_tmp_comp-meeht
                                  it_l_tmp_comp-wip_amt
                                  it_l_tmp_comp-wip_quantity
                                  it_l_tmp_comp-scrap_amt
                                  it_l_tmp_comp-actual_scrap
                                  it_l_tmp_comp-manu_qty
                                  lw_index.

      collect it_l_tmp_comp.
      clear   it_l_tmp_comp.
    endloop.

* Read Cost Component (From ML View - Actual View)
    clear : it_kkb_split, it_kkb_split[].
    perform read_mlcc
                using it_sc_m-chd_kalnr
*                      IT_SC_M-BWKEY
*                      IT_SC_M-LLV_MATNR
                      it_sc_m-bdatj
                      it_sc_m-poper
                      it_sc_m-par_proc_kalnr
                      space.

    loop at it_kkb_split.
      move-corresponding  it_sc_m  to  it_l_tmp_comp.
* Clear Current Qty
      clear : it_l_tmp_comp-mbgbtr,
              it_l_tmp_comp-wkgbtr,
              it_l_tmp_comp-wip_amt,
              it_l_tmp_comp-scrap_amt.

      it_l_tmp_comp-elemt  = it_kkb_split-elemt.
      it_l_tmp_comp-ml_act_preis
                           = it_kkb_split-w000 .
      collect it_l_tmp_comp.
      clear   it_l_tmp_comp.
    endloop.
  endloop.

* ML Actual Unit Price / WIP / SCRAP
  data : lv_werks like  t001w-werks.

  loop at it_l_tmp_comp.
*  ML Actual Unit Price
    if it_l_tmp_comp-mbgbtr is initial.
      it_l_tmp_comp-preis = space.
    else.
      it_l_tmp_comp-preis =
      it_l_tmp_comp-ml_act_preis / it_l_tmp_comp-mbgbtr.
    endif.

** Exception . Recover Plant Code P001 - > E001
* Check Procurement Type / Spc Proc. Type
    if   it_l_tmp_comp-beskz = 'F'
     and it_l_tmp_comp-sobsl = '40'.
      it_l_tmp_comp-werks = 'E001'.
* Bwkey
      clear it_t001w.
      read table it_t001w with key werks = it_l_tmp_comp-werks .
      it_l_tmp_comp-bwkey = it_t001w-bwkey.
    endif.
**

*  Modify
    modify it_l_tmp_comp.
    clear  it_l_tmp_comp.
  endloop.

  clear  it_l_tmp_comp.

* Append
  append lines of it_l_tmp_comp to it_tmp_shopcost.
  clear it_tmp_shopcost.
endform.                    " cal_val_m_for_matc
*&---------------------------------------------------------------------*
*&      Form  get_wip_scrap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_wip_scrap using pw_bdatj    pw_poper
                         pw_objnr    pw_llv_matnr
                         pw_bwkey    pw_bwtar
                         pw_kostl    pw_lstar
                         pw_matnr    pw_meeht
                         pw_wipamt   pw_wipqty
                         pw_scrapamt pw_scrapqty
                         pw_manu_qty
                         pw_index.

  if not pw_matnr is initial.
    read table it_wip_calculated with key
                             gjahr    = pw_bdatj
                             period   = pw_poper
                             objnr    = pw_objnr
                             material = pw_llv_matnr
                             bwkey    = pw_bwkey
                             bwtar    = pw_bwtar.

  else.
    read table it_wip_calculated
                      with key
                           gjahr    = pw_bdatj
                           period   = pw_poper
                           objnr    = pw_objnr
                           material = space
                           bwkey    = space
                           bwtar    = space
                           kostl    = pw_kostl
                           lstar    = pw_lstar.
  endif.

  check sy-subrc ne 0.

  clear: it_wip_scrap.

  if   not pw_matnr is initial .
    read table it_wip_scrap
                      with key
                           gjahr    = pw_bdatj
                           period   = pw_poper
                           objnr    = pw_objnr
                           material = pw_llv_matnr
                           bwkey    = pw_bwkey
                           bwtar    = pw_bwtar.
  else.
    read table it_wip_scrap
                      with key
                           gjahr    = pw_bdatj
                           period   = pw_poper
                           objnr    = pw_objnr
                           material = space
                           bwkey    = space
                           bwtar    = space
                           kostl    = pw_kostl
                           lstar    = pw_lstar.
  endif.

  if sy-subrc eq 0.
    if it_wip_scrap-unit <> pw_meeht.
      perform unit_converion
                         using it_wip_scrap-wip_quantity
                               it_wip_scrap-wip_quantity
                               it_wip_scrap-unit
                               pw_meeht.
      perform unit_converion
                         using it_wip_scrap-actual_scrap
                               it_wip_scrap-actual_scrap
                               it_wip_scrap-unit
                               pw_meeht.
      perform unit_converion
                         using it_wip_scrap-target_qty
                               it_wip_scrap-target_qty
                               it_wip_scrap-unit
                               pw_meeht.
    endif.

    move: it_wip_scrap-wip_quantity to pw_wipqty,
          it_wip_scrap-actual_scrap to pw_scrapqty.

    pw_wipamt   = it_kkb_split_ck13n-w000 * pw_wipqty.
    pw_scrapamt = it_kkb_split_ck13n-w000 * pw_scrapqty.
    pw_manu_qty = it_wip_scrap-target_qty.
  endif.
endform.                    " get_wip_scrap
*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_date.
  move: p_perab to w_perbi.
endform.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  calculate_manu_amt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_manu_amt using pw_pr_wip_amt pw_pr_wip_quantity.
  data: lw_quantity like it_tmp_shopcost-wip_quantity.

  if it_tmp_shopcost-wip_quantity > 0 and
     it_tmp_shopcost-wip_amt      = 0.
    it_tmp_shopcost-wip_quantity = 0.
  endif.

  if it_tmp_shopcost-typps = 'V'.
    it_tmp_shopcost-manu_amt = it_tmp_shopcost-ml_act_preis.
    exit.
  endif.

  if it_tmp_shopcost-mbgbtr eq 0.
    if pw_pr_wip_quantity eq 0.
      it_tmp_shopcost-manu_amt = 0.
    else.
      if it_tmp_shopcost-wip_quantity = 0.
        it_tmp_shopcost-manu_amt = it_tmp_shopcost-manu_qty *
                                 ( pw_pr_wip_amt / pw_pr_wip_quantity ).
      else.
        it_tmp_shopcost-manu_amt = it_tmp_shopcost-manu_qty *
                               ( pw_pr_wip_amt / pw_pr_wip_quantity ) +
                                   pw_pr_wip_amt -
                                 ( pw_pr_wip_quantity *
                                   ( it_tmp_shopcost-wip_amt /
                                     it_tmp_shopcost-wip_quantity )
                                 ).
      endif.
    endif.
  else.
    it_tmp_shopcost-manu_amt = it_tmp_shopcost-manu_qty *
                               ( it_tmp_shopcost-wkgbtr /
                                 it_tmp_shopcost-mbgbtr  ) +
                               ( it_tmp_shopcost-mbgbtr +
                                 it_tmp_shopcost-add_mbgbtr ) *
                               ( it_tmp_shopcost-preis -
                                 ( it_tmp_shopcost-wkgbtr /
                                   it_tmp_shopcost-mbgbtr ) ) +
                                 pw_pr_wip_amt -
                               ( pw_pr_wip_quantity *
                                 ( it_tmp_shopcost-wkgbtr /
                                   it_tmp_shopcost-mbgbtr )
                               ).

  endif.
endform.                    " calculate_manu_amt
