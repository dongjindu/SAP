*----------------------------------------------------------------------*
***INCLUDE ZACO16L_F001 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_INIT
*&---------------------------------------------------------------------*
*       Default Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_init.
  clear : s_mtart, s_mtart[],
          s_aufnr, s_aufnr[],
          s_kstar, s_kstar[].

* Material Code
  s_mtart-low    = 'ROH'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  append s_mtart.
  s_mtart-low    = 'ROH1'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  append s_mtart.
  s_mtart-low    = 'HALB'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  append s_mtart.

* internal Order
  s_aufnr-low    = 'CP001'.
  s_aufnr-sign   = 'I'.
  s_aufnr-option = 'EQ'.
  append s_aufnr.
  s_aufnr-low    = 'CE001'.
  s_aufnr-sign   = 'I'.
  s_aufnr-option = 'EQ'.
  append s_aufnr.

* Cost element
* All account for I/O 'P001' and 'E001'.
  s_kstar-low    = '0000540000'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540010'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540100'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540110'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540200'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540300'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.
  s_kstar-low    = '0000540400'.
  s_kstar-sign   = 'I'.
  s_kstar-option = 'EQ'.
  append s_kstar.

endform.                    " SELECT_INIT

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
*&      Form  READ_IO_DATA
*&---------------------------------------------------------------------*
*       Read Internal Order DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_io_data.
* Progress Ind.
  perform progress_ind using '10'
                             text-021.

* Read Object Key for Internal order .
  perform read_obj_for_io.
* Read I/O data from Table.
  perform read_io_data_fr_coep.

endform.                    " READ_IO_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_OBJ_FOR_IO
*&---------------------------------------------------------------------*
*       Read Object Key for Internal Order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_obj_for_io.
  clear : r_io_objnr, r_io_objnr[].

  loop at s_aufnr.
    call function 'K_AUFNR_OBJECT_KEY_GET'
         exporting
              aufnr = s_aufnr-low
              kokrs = p_kokrs
         importing
              objnr = r_io_objnr-low.
    r_io_objnr-sign = 'I'.
    r_io_objnr-option = 'EQ'.
    append  r_io_objnr.
    clear : r_io_objnr, s_aufnr.
  endloop.

endform.                    " READ_OBJ_FOR_IO

*&---------------------------------------------------------------------*
*&      Form  READ_IO_DATA_FR_COEP
*&---------------------------------------------------------------------*
*       Read Data from COEP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_io_data_fr_coep.
*DATA : BEGIN OF IT_IO_COEP OCCURS 1000.
*DATA :  MATNR  LIKE COEP-MATNR,
*        WERKS  LIKE COEP-WERKS,
*        WKGBTR LIKE COEP-WKGBTR,
*        WAERS  LIKE TKA01-WAERS.
*DATA : END OF   IT_IO_COEP.

* WKGBTR - Total value in CO area currency
* Index COEP - 1

  clear : it_io_coep, it_io_coep[].
  select   objnr kstar wkgbtr werks matnr
*// Mod. By Hyung Jin Youn 2004.04.16
* Additional Field for Quantity data
           mbgbtr meinb refbn gjahr buzei
*// End of Mod.
           into corresponding fields of table it_io_coep
           from covp
          where kokrs = p_kokrs
            and lednr = '00'    "Standard Ledger
            and perio = p_perio
            and gjahr = p_gjahr
            and kstar in s_kstar
            and objnr in r_io_objnr
            and matnr in s_matnr
            and wrttp = p_wrttp
            and versn = p_versn
            and exists ( select * from mara
                                       where matnr = covp~matnr
                                         and mtart in s_mtart ).

  if it_io_coep[] is initial.
    message e058.
  endif.

* Replacing the original data into collected data
  data : it_l_tmp_coep like standard table of it_io_coep
                       with header line .
  it_l_tmp_coep[] = it_io_coep[].
  clear : it_io_coep, it_io_coep[].
  sort  it_l_tmp_coep by matnr .

  loop at it_l_tmp_coep.
    clear: mseg.
    select single bwart into mseg-bwart
      from mseg
     where mblnr = it_l_tmp_coep-refbn
       and mjahr = it_l_tmp_coep-gjahr
       and zeile = it_l_tmp_coep-buzei.

    move-corresponding it_l_tmp_coep to it_io_coep.
    case mseg-bwart.
      when '905' or '906'.
        move: 'KEYIN' to it_io_coep-stype.
      when others.
        move: 'OS&D'  to it_io_coep-stype.
    endcase.

    clear: it_io_coep-refbn, it_io_coep-gjahr,  it_io_coep-buzei.

    collect it_io_coep.
    clear it_io_coep.
    clear it_l_tmp_coep.
  endloop.

  clear it_io_coep.
  clear : it_l_tmp_coep, it_l_tmp_coep[].

* Strange Posting (Amt is not STD)
  delete it_io_coep
   where mbgbtr = 0.


* Fill up currency fields with Controlling Area Currency
*  WKGBTR - Total value in CO area currency
  it_io_coep-waers = tka01-waers.
  modify it_io_coep transporting waers where waers eq space.

* Get Production scheduler
  loop at it_io_coep.
    select single fevor into it_io_coep-fevor
      from marc
     where matnr = it_io_coep-matnr
       and werks = it_io_coep-werks.

    if sy-subrc eq 0.
      case it_io_coep-objnr+2.
        when 'CP001'.
          if not ( it_io_coep-fevor eq c_blank or      "Blank
                   it_io_coep-fevor eq c_coil ).       "Coil
            clear: it_io_coep-fevor.
          endif.
        when 'CE001'.
          if not it_io_coep-fevor eq c_3c.             "3C
            clear: it_io_coep-fevor.
          endif.
      endcase.
    else.
      clear: it_io_coep-fevor.
    endif.

    modify it_io_coep.
  endloop.
endform.                    " READ_IO_DATA_FR_COEP

*&---------------------------------------------------------------------*
*&      Form  SET_MTYPE_FK
*&---------------------------------------------------------------------*
*       Set Material Type to select materials
*       to be used as allocation Factors
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_mtype_fk.
* Only FERT and HALB
  clear : r_fk_mtart, r_fk_mtart[].

  r_fk_mtart-low    = 'FERT'.
  r_fk_mtart-sign   = 'I'.
  r_fk_mtart-option = 'EQ'.
  append r_fk_mtart.
  clear  r_fk_mtart.

  r_fk_mtart-low    = 'HALB'.
  r_fk_mtart-sign   = 'I'.
  r_fk_mtart-option = 'EQ'.
  append r_fk_mtart.
  clear  r_fk_mtart.

endform.                    " SET_MTYPE_FK

*&---------------------------------------------------------------------*
*&      Form  READ_B_F_DATA
*&---------------------------------------------------------------------*
*       Read B/F data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_b_f_data.
* Progress Ind.
  perform progress_ind using '30'
                             text-022.
* Read Object Key for PCC order .
  perform read_obj_for_pcc.
* Read B/F data in PCC
  perform read_bf_data_in_pcc.

endform.                    " READ_B_F_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_OBJ_FOR_PCC
*&---------------------------------------------------------------------*
*       Read Object Key for PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_obj_for_pcc.
*
** Local DATA definition.
*  DATA : it_l_e_vkks0	   LIKE TABLE OF vkks0
*                           WITH HEADER LINE.
*  DATA : it_l_tmp_pcc_mat  LIKE STANDARD TABLE OF it_pcc_mat
*                           WITH HEADER LINE.
*
*  CLEAR : it_pcc_mat, it_pcc_mat[].
*  CLEAR : it_l_tmp_pcc_mat, it_l_tmp_pcc_mat[].
** INDEX : MARA-T
*  SELECT matnr werks fevor
*         INTO CORRESPONDING FIELDS OF TABLE it_l_tmp_pcc_mat
*         FROM ma61v
*        WHERE mtart IN  r_fk_mtart.
*
*  IF it_l_tmp_pcc_mat[] IS INITIAL.
*    MESSAGE e059.
*  ENDIF.
*
** Read PCC orders
*  LOOP AT it_l_tmp_pcc_mat.
*
*    CLEAR : it_l_e_vkks0, it_l_e_vkks0[].
*    CALL FUNCTION 'KK_F_PKOSA_FIND'
*      EXPORTING
*        i_matnr                     = it_l_tmp_pcc_mat-matnr
*        i_werks                     = it_l_tmp_pcc_mat-werks
*        i_pwerk                     = it_l_tmp_pcc_mat-werks
**       I_PROCNR                    = ' '
**       I_SA_AUFNR                  = ' '
**       I_FA_AUFNR                  = ' '
**       I_VERID                     = ' '
**       I_STLAN                     = ' '
**       I_STLAL                     = ' '
**       I_PLNTY                     = ' '
**       I_PLNNR                     = ' '
**       I_PLNAL                     = ' '
**       I_DATE                      = '00000000'
**       I_POPUP                     = ' '
**       I_REM                       = ' '
**       I_INCL_LOEKZ                = ' '
**       I_NO_OLD_PKOSA              = ' '
**     IMPORTING
**       E_PROCNR                    =
**       E_VERID                     =
**       E_STLAN                     =
**       E_STLAL                     =
**       E_PLNTY                     =
**       E_PLNNR                     =
**       E_PLNAL                     =
**       E_AUFNR                     =
*      TABLES
*        e_vkks0                     = it_l_e_vkks0
**       E_PKOSA                     =
*      EXCEPTIONS
*        none_found                  = 1
*        wrong_input                 = 2
*        none_picked                 = 3
*        wrong_rule                  = 4
*        rsh_not_valid               = 5
*        wrong_characteristics       = 6
*        no_rule                     = 7
*        version_not_valid           = 8
*        OTHERS                      = 9.
*
** if No PCC order, Skip the record .
*    IF sy-subrc <> 0.
*      DELETE it_l_tmp_pcc_mat.
*      CONTINUE.
*    ENDIF.
*    IF it_l_e_vkks0[]  IS INITIAL .
*      DELETE it_l_tmp_pcc_mat.
*      CONTINUE.
*    ENDIF.
*
*    LOOP AT it_l_e_vkks0.
** Copying Data
*      MOVE-CORRESPONDING it_l_tmp_pcc_mat TO it_pcc_mat.
*      it_pcc_mat-aufnr = it_l_e_vkks0-aufnr.
*      it_pcc_mat-objnr = it_l_e_vkks0-objnr.
** Making ITAB for PCC orders
*      COLLECT   it_pcc_mat.
*      CLEAR     it_pcc_mat.
*      CLEAR     it_l_e_vkks0.
*    ENDLOOP.
*
*    CLEAR it_l_tmp_pcc_mat.
*  ENDLOOP.
*
*  CLEAR : it_l_tmp_pcc_mat, it_l_tmp_pcc_mat[].
*  FREE  : it_l_tmp_pcc_mat.
*


  data: begin of lt_pcc occurs 0,
          objnr       like aufk-objnr,
          aufnr       like aufk-aufnr,
          pkosa       like ckmlmv013-pkosa,  "Cost Collector
          kalnr_proc  like ckmlmv013-kalnr_proc,
          prwrk       like ckmlmv013-prwrk,
          pmatn       like ckmlmv013-pmatn,
          verid       like ckmlmv013-verid,
          klvarp      like afko-klvarp,      "CostingVariant-plan

        end of lt_pcc.

  data : it_tmp_fsc_mat   like standard table of it_pcc_mat
                          with header line .
  data: ef_ok type  c.

* Create index of CKMLMV013!!!  (MANDT/PRWRK/PMATN)
  select aufk~objnr aufk~aufnr
         ckmlmv013~pkosa   ckmlmv013~kalnr_proc
         ckmlmv013~prwrk   ckmlmv013~pmatn      ckmlmv013~verid
         afko~klvarp
     into corresponding fields of table lt_pcc
     from ckmlmv013
        inner join aufk
* by IG.MOON {
            on aufk~autyp = ckmlmv013~autyp
           and aufk~werks = ckmlmv013~prwrk
*  }
           and aufk~aufnr   = ckmlmv013~pkosa
        inner join afko
           on afko~aufnr   = aufk~aufnr
     where ckmlmv013~autyp = '05'        "PCC
       and ckmlmv013~loekz = space        "deletion
       and aufk~kokrs      = p_kokrs.

  loop at lt_pcc .
    clear: it_pcc_mat.
    it_pcc_mat-matnr      = lt_pcc-pmatn.
    it_pcc_mat-werks      = lt_pcc-prwrk.
    it_pcc_mat-objnr      = lt_pcc-objnr.
    it_pcc_mat-aufnr      = lt_pcc-aufnr.
    it_pcc_mat-objnr      = lt_pcc-objnr.
    it_pcc_mat-verid      = lt_pcc-verid.

*    CALL FUNCTION 'QRP_IS_APO_ORDER'
*         EXPORTING
*              if_objnr = it_pcc_mat-objnr
*         IMPORTING
*              ef_ok    = ef_ok.
*    IF ef_ok = 'X'.
*      it_pcc_mat-categ = 'DI'.
*    ELSE.
*      IF it_pcc_mat-sauft = 'X'.
*        it_pcc_mat-categ = 'REM'.
*      ELSE.
*        it_pcc_mat-categ = 'MTO'.
*      ENDIF.
*    ENDIF.

* Get Production Scheduler
    select single fevor into it_pcc_mat-fevor
      from marc
     where matnr = it_pcc_mat-matnr
       and werks = it_pcc_mat-werks.
    if sy-subrc eq 0.
      if not ( it_pcc_mat-fevor eq c_press or      "Press
               it_pcc_mat-fevor eq c_blank or      "Blank
               it_pcc_mat-fevor eq c_engine ).     "Engine
        clear: it_pcc_mat-fevor.
      endif.
    endif.

    append   it_pcc_mat.
  endloop.

  if it_pcc_mat[] is initial.
    message e026.
  endif.

  sort it_pcc_mat by objnr.

endform.                    " READ_OBJ_FOR_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_BF_DATA_IN_PCC
*&---------------------------------------------------------------------*
*       Read B/F data in PCC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_bf_data_in_pcc.
* Local Data Definition
  data  : begin of lt_pcc_coep occurs 100,
             objnr    like coep-objnr,
             kstar    like coep-kstar,
             werks    like coep-werks,
             matnr    like coep-matnr,
             meinb    like coep-meinb,
             wkgbtr   like coep-wkgbtr,
             megbtr   like coep-megbtr,
             mefbtr   like coep-mefbtr,
             mbgbtr   like coep-mbgbtr,
             mbfbtr   like coep-mbfbtr,
             chk(1)   type c,
          end of lt_pcc_coep.
*  ranges: lr_matnr for coep-matnr.
*  refresh lr_matnr.
*  lr_matnr-option = 'EQ'.
*  lr_matnr-sign   = 'I'.
*  loop at it_io_coep.
*    lr_matnr-low = it_io_coep-matnr.
*    append lr_matnr.
*  endloop.
*  sort lr_matnr.
*  delete adjacent duplicates from lr_matnr.
*  sort lr_matnr by low.

  sort it_io_coep by matnr.

* Index COEP - 1
  refresh: lt_pcc_coep.
*  LOOP AT it_pcc_mat.
*    SELECT
*          objnr kstar werks matnr
*          meinb
*          SUM( wkgbtr )
*          SUM( megbtr )
*          SUM( mefbtr )
*          SUM( mbgbtr )
*          SUM( mbfbtr )
*             APPENDING TABLE lt_pcc_coep
*             FROM coep
*            WHERE
*                  objnr =  it_pcc_mat-objnr  "PCC order OBJ
*              AND gjahr = p_gjahr
*              AND versn = p_versn
*              AND wrttp = p_wrttp
*              AND perio = p_perio
*              AND kstar IN s_kstar
*              AND lednr = '00'               "Standard Ledger
*              AND kokrs = p_kokrs
*              AND matnr in lr_matnr
*              AND matnr <> it_pcc_mat-matnr  "Only for child materials
*              AND werks =  it_pcc_mat-werks
*              AND scope = 'PRODT'           "Production
*       GROUP by objnr kstar wkgbtr werks matnr meinb.
*  ENDLOOP.


  select
        a~objnr a~kstar a~werks a~matnr
        a~meinb
        sum( wkgbtr )
        sum( megbtr )
        sum( mefbtr )
        sum( mbgbtr )
        sum( mbfbtr )
      into table lt_pcc_coep

      from
        ckmlmv013
        inner join aufk
* by IG.MOON {
            on aufk~autyp   = ckmlmv013~autyp
           and aufk~werks = ckmlmv013~prwrk
*  }
           and aufk~aufnr   = ckmlmv013~pkosa
        inner join afko
           on afko~aufnr   = aufk~aufnr
        inner join coep as a
           on aufk~objnr = a~objnr
      where
            ckmlmv013~autyp = '05'        "PCC
        and ckmlmv013~loekz = space        "deletion
        and aufk~kokrs      = p_kokrs
        and a~kstar in s_kstar
        and a~gjahr = p_gjahr
        and a~perio = p_perio
        and a~versn = p_versn
        and a~wrttp = p_wrttp
        and a~kokrs = p_kokrs
        and a~scope = 'PRODT'           "Production

     group by a~objnr a~kstar a~wkgbtr a~werks a~matnr a~meinb.

  data: l_idx like sy-tabix.
  loop at lt_pcc_coep.
    l_idx = sy-tabix.
    read table it_pcc_mat with key objnr = lt_pcc_coep-objnr
                          binary search.
*----"Only for child materials
    if sy-subrc <> 0 or it_pcc_mat-matnr = lt_pcc_coep-matnr.
      lt_pcc_coep-chk = 'D'.
      modify lt_pcc_coep index l_idx transporting chk.
    endif.
  endloop.
  sort lt_pcc_coep by chk.
  delete lt_pcc_coep where chk = 'D'.

  if lt_pcc_coep[] is initial.
    message e058.
  endif.

* by IG.MOON 11/05/2007 {
  sort it_io_coep by matnr.
* }

* Collect data
  clear : it_pcc_coep,        it_pcc_coep[].
  loop at lt_pcc_coep.
    read table it_io_coep with key matnr = lt_pcc_coep-matnr
    binary search.

    if sy-subrc = 0.
      move-corresponding lt_pcc_coep to it_pcc_coep.
      collect it_pcc_coep.
      clear   it_pcc_coep.
    endif.
  endloop.

* Fill up currency fields with Controlling Area Currency
*  WKGBTR - Total value in CO area currency
  it_pcc_coep-waers = tka01-waers.
  modify it_pcc_coep transporting waers where waers eq space.

*// Mod. by Hyung Jin Youn 2004.04.21
* If some records have minus qty, remove them .
* Those records can not be used as allocation factor.
* (Requested by Functional Team Member)
  delete it_pcc_coep
   where mbgbtr <= 0.

* End of Mod.
endform.                    " READ_BF_DATA_IN_PCC

*&---------------------------------------------------------------------*
*&      Form  CAL_COST_RATIO
*&---------------------------------------------------------------------*
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_cost_ratio.
*
** Local Data Definition
*  DATA : BEGIN OF IT_L_CHM_RATE OCCURS 0.
*  DATA :  KSTAR  LIKE COEP-KSTAR,
*          MATNR  LIKE COEP-MATNR,
*          WERKS  LIKE COEP-WERKS,
**          OBJNR  LIKE COEP-OBJNR,
*          PCC_WKGBTR LIKE COEP-WKGBTR.
*  DATA : END OF  IT_L_CHM_RATE.
*
*  DATA : BEGIN OF IT_L_TOT_RATE OCCURS 0.
*  DATA :
**          KSTAR  LIKE COEP-KSTAR,
**          MATNR  LIKE COEP-MATNR,
*          WERKS  LIKE COEP-WERKS,
*          OBJNR  LIKE COEP-OBJNR,
*          TOT_WKGBTR LIKE COEP-WKGBTR,
*          KSTAR_RATE LIKE ZTCO_ABISPOST-KSTAR_RATE.
*  DATA : END OF  IT_L_TOT_RATE.
*
*  DATA : BEGIN OF IT_L_PCCSUM OCCURS 0,
*            WERKS  LIKE COEP-WERKS,
*            WK_SUM LIKE COEP-WKGBTR,
*         END OF   IT_L_PCCSUM.
*
** Calculation the cost ratio
*  CLEAR : IT_L_CHM_RATE, IT_L_CHM_RATE[].
*  CLEAR : IT_L_TOT_RATE, IT_L_TOT_RATE[].
*  CLEAR : IT_L_PCCSUM  , IT_L_PCCSUM[]  .
*
*  SORT IT_PCC_COEP BY KSTAR MATNR WERKS OBJNR.
*
*  LOOP AT  IT_PCC_COEP.
** Calculation the total cost by child materials
*    MOVE-CORRESPONDING  IT_PCC_COEP TO  IT_L_CHM_RATE.
*    IT_L_CHM_RATE-PCC_WKGBTR = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_CHM_RATE.
*    CLEAR   IT_L_CHM_RATE.
** Calculation the total cost by PCC (Parents materials)
*    MOVE-CORRESPONDING  IT_PCC_COEP TO  IT_L_TOT_RATE.
*    IT_L_TOT_RATE-TOT_WKGBTR = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_TOT_RATE.
*    CLEAR   IT_L_TOT_RATE.
** PCC SUM
*    IT_L_PCCSUM-WERKS  = IT_PCC_COEP-WERKS.
*    IT_L_PCCSUM-WK_SUM = IT_PCC_COEP-WKGBTR.
*    COLLECT IT_L_PCCSUM.
*    CLEAR   IT_L_PCCSUM.
*    CLEAR IT_PCC_COEP.
*  ENDLOOP.
*
** Copy to Global
*  CLEAR : IT_TOT_RATE, IT_TOT_RATE[].
*  LOOP AT IT_L_TOT_RATE.
*    CLEAR IT_L_PCCSUM.
*    READ TABLE IT_L_PCCSUM WITH KEY WERKS = IT_L_TOT_RATE-WERKS.
*    IF SY-SUBRC <> 0.
*      CONTINUE.
*    ENDIF.
*    IT_L_TOT_RATE-KSTAR_RATE
*      = IT_L_TOT_RATE-TOT_WKGBTR  / IT_L_PCCSUM-WK_SUM.
*    MODIFY IT_L_TOT_RATE.
** Copy to global Itab
*    MOVE-CORRESPONDING  IT_L_TOT_RATE TO IT_TOT_RATE.
*    APPEND IT_TOT_RATE.
*    CLEAR  IT_TOT_RATE.
*    CLEAR IT_L_TOT_RATE.
*  ENDLOOP.
*
** Sort
*  SORT IT_L_CHM_RATE BY KSTAR MATNR WERKS.
*  CLEAR   IT_L_CHM_RATE.
**
**  RATE_CHILD(16)    LIKE ZTCO_ABISPOST-RATE_CHILD.
*  LOOP AT IT_PCC_COEP.
** ratio (Child)
*    CLEAR   IT_L_CHM_RATE.
*    READ TABLE IT_L_CHM_RATE WITH KEY KSTAR = IT_PCC_COEP-KSTAR
*                                      MATNR = IT_PCC_COEP-MATNR
*                                      WERKS = IT_PCC_COEP-WERKS.
*    IF   SY-SUBRC = 0
*     AND IT_L_CHM_RATE-PCC_WKGBTR NE SPACE.
*      IT_PCC_COEP-RATE_CHILD
*       = IT_PCC_COEP-WKGBTR / IT_L_CHM_RATE-PCC_WKGBTR.
*    ENDIF.
*    MODIFY IT_PCC_COEP.
*    CLEAR  IT_PCC_COEP.
*  ENDLOOP.
*
*  CLEAR  IT_PCC_COEP.
*  CLEAR  IT_TOT_RATE.
*
endform.                    " CAL_COST_RATIO

*&---------------------------------------------------------------------*
*&      Form  MAKE_PCC_POST_TABLE
*&---------------------------------------------------------------------*
*       Making PCC post data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_pcc_post_table.

* Local Data Definition
  data : lv_sum_qty like it_io_coep-mbgbtr.
  data : lv_res_qty like it_io_coep-mbgbtr.
  data : lw_fevor   like marc-fevor.

* Sort
  sort it_pcc_coep
                   ascending  by  kstar matnr stype werks objnr
                   descending     rate_child.
  sort it_tot_rate descending by  kstar_rate.
  sort it_io_coep  by kstar matnr stype werks objnr.

* Read Objnr for Internal Order

  clear aufk.
  select * into corresponding fields of table  it_l_aufk
           from aufk
          where aufnr in s_aufnr.

  clear : it_post, it_post[].
  sort : it_l_aufk by objnr,
         it_pcc_mat by objnr.

  loop at it_io_coep.
* SUM of Qty (Issued Qty from MM)
    clear : lv_sum_qty.
    clear : it_post.
** Child Mat. rate
    loop at it_pcc_coep where kstar = it_io_coep-kstar
                          and matnr = it_io_coep-matnr
                          and werks = it_io_coep-werks

*/// START : Issue:20041206-001 Changed on 2004.11.24, Changed by BSBAE
                          and rate_child <> 0.
*/// END   : Issue:20041206-001 Changed on 2004.11.24, Changed by BSBAE

      it_post-waers = it_io_coep-waers.
      it_post-kstar = it_pcc_coep-kstar.
      it_post-matnr = it_io_coep-matnr.
      it_post-werks = it_io_coep-werks.
      it_post-stype = it_io_coep-stype.
*  read PCC order
      clear it_pcc_mat.
      read table it_pcc_mat with key objnr  = it_pcc_coep-objnr
      binary search.
      it_post-pcc_aufnr = it_pcc_mat-aufnr.
*  read IO order
      clear it_l_aufk.
      read table it_l_aufk  with key objnr  = it_io_coep-objnr
      binary search.

      it_post-io_aufnr = it_l_aufk-aufnr.

      it_post-type = 'Q'.

*  Child rate
      it_post-rate_child = it_pcc_coep-rate_child.

*// Mod. by Hyung Jin Youn 2004.04.19
*  Base on Qty Ratio.
*FIXME EA only?
      if it_io_coep-meinb eq 'EA'.
        cal_by_qty rate_child.
      else.
        it_post-mbgbtr = it_io_coep-mbgbtr * it_post-rate_child.
        it_post-chg_wkgbtr = it_io_coep-wkgbtr * it_post-rate_child.
      endif.
*  Unit
      it_post-meinb  = it_io_coep-meinb.
**  Cal. Cost
*     IT_POST-CHG_WKGBTR  = IT_IO_COEP-WKGBTR
*                         * IT_POST-RATE_CHILD.
*// End of Mod.
* Qty Rounding :-  "UD1K920991

* Check UoM Rounding
      call function 'UNIT_CONVERSION_SIMPLE'
           exporting
                input      = it_post-mbgbtr
                round_sign = 'X'
                unit_in    = it_io_coep-meinb
                unit_out   = it_io_coep-meinb
           importing
                output     = it_post-mbgbtr.


* Append
      append it_post.
      clear  it_post.
      clear it_pcc_coep.
    endloop.
** Total PCC Rate
    if sy-subrc <> 0.
*  Get Production Scheduler
      perform get_parent_production_schedule using lw_fevor.
      perform make_ppc_post_table_for_no_bf using lw_fevor.

*
**  Check relationship between Plant and IO
*      LOOP AT it_tot_rate WHERE werks = it_io_coep-werks
*                            AND fevor = lw_fevor.
*        it_post-waers = it_io_coep-waers.
*        it_post-kstar = it_io_coep-kstar.
*        it_post-matnr = it_io_coep-matnr.
*        it_post-werks = it_io_coep-werks.
**  read PCC order
*        CLEAR it_pcc_mat.
*        READ TABLE it_pcc_mat WITH KEY objnr  = it_tot_rate-objnr.
*        it_post-pcc_aufnr = it_pcc_mat-aufnr.
**  read IO order
*        CLEAR it_l_aufk.
*        READ TABLE it_l_aufk  WITH KEY objnr  = it_io_coep-objnr.
*        it_post-io_aufnr = it_l_aufk-aufnr.
**  parentes rate
*        it_post-kstar_rate = it_tot_rate-kstar_rate.
**// Mod. by Hyung Jin Youn 2004.04.19
**  Base on Qty Ratio.
*        cal_by_qty kstar_rate.
**  Unit
*        it_post-meinb  = it_io_coep-meinb.
*
*        it_post-type   = 'A'.
**// End of Mod.
*
** Append
*                append it_post.
*        CLEAR  it_post.
*        CLEAR it_tot_rate.
*      ENDLOOP.
    endif.
    clear it_io_coep.
  endloop.
  clear  it_post.

* Remove Initial Value
  delete it_post where chg_wkgbtr eq 0
                    or mbgbtr     eq 0.


endform.                    " MAKE_PCC_POST_TABLE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA_TO_TABLE
*&---------------------------------------------------------------------*
*       Update the result data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_data_to_table.


  sort it_post by kstar io_aufnr pcc_aufnr.
  loop at it_post.
    clear ztco_abispost.
    move-corresponding it_post to ztco_abispost.
* Period
    ztco_abispost-kokrs  = p_kokrs.
    ztco_abispost-gjahr  = p_gjahr.
    ztco_abispost-period = p_perio.
    ztco_abispost-versn  = p_versn.
* Log
    ztco_abispost-erdat  = sy-datum.
    ztco_abispost-erzet  = sy-uzeit.
    ztco_abispost-ernam  = sy-uname.
*// Mod. by Hyung Jin Youn 2004.04.19
* Read Production version from PCC
    perform read_verid_from_pcc.
*// End Of Mod
    insert ztco_abispost.
    if sy-subrc <> 0.
      rollback work.
      message e045.
    endif.
  endloop.
* Commit Work
  commit work and wait.
  message s000 with 'The process for reporting was Completed'.
endform.                    " UPDATE_DATA_TO_TABLE

*&---------------------------------------------------------------------*
*&      Form  enqueue
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form enqueue.

  call function 'ENQUEUE_EZCO_ZTCO_ABISPO'
   exporting
     mode_ztco_abispost       = 'E'
     mandt                    = sy-mandt
     kokrs                    = p_kokrs
     gjahr                    = p_gjahr
     period                   = p_perio
     versn                    = p_versn
*     KSTAR                    =
*     MATNR                    =
*     WERKS                    =
*     IO_AUFNR                 =
*     PCC_AUFNR                =
*     X_KOKRS                  = ' '
*     X_GJAHR                  = ' '
*     X_PERIOD                 = ' '
*     X_VERSN                  = ' '
*     X_KSTAR                  = ' '
*     X_MATNR                  = ' '
*     X_WERKS                  = ' '
*     X_IO_AUFNR               = ' '
*     X_PCC_AUFNR              = ' '
*     _SCOPE                   = '2'
*     _WAIT                    = ' '
*     _COLLECT                 = ' '
   exceptions
     foreign_lock             = 1
     system_failure           = 2
     others                   = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " enqueue

*&---------------------------------------------------------------------*
*&      Form  CONF_REPLACE_DATA
*&---------------------------------------------------------------------*
*       Replacing data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form conf_replace_data.
  if p_conf <> 'X'.
    message e056.
  endif.

  delete from ztco_abispost
                      where kokrs  = p_kokrs
                        and gjahr  = p_gjahr
                        and period = p_perio
                        and versn  = p_versn
                        and matnr in s_matnr.
* Do not check subrc.

endform.                    " CONF_REPLACE_DATA

*&---------------------------------------------------------------------*
*&      Form  CONFIRM_MESSAGE
*&---------------------------------------------------------------------*
*       Show Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form confirm_message.
  if p_conf = 'X'.
*    DATA : lv_answer.
*    CLEAR  lv_answer.
*    CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
*      EXPORTING
*        textline1           = text-030
*        textline2           = text-031
*        titel               = text-032
**       START_COLUMN        = 25
**       START_ROW           = 6
**       DEFAULTOPTION       = 'N'
*      IMPORTING
*        answer              = lv_answer.
*
*    IF lv_answer <> 'J'.
    message w000(zz) with text-030.
*    ENDIF.
**    MESSAGE S060 .
  endif.
endform.                    " CONFIRM_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  POST_DATA
*&---------------------------------------------------------------------*
*       Post data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_data.
  case 'X'.
* Do not Post
    when p_report.

    when p_frpost.
* Read data from  ZTCO_ABISPOST
      perform read_data_fr_ztco_abispost.
* Posting FM
      perform post_with_fm.
* Update Doc. No.
      perform update_doc_no.
  endcase.
endform.                    " POST_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_ZTCO_ABISPOST
*&---------------------------------------------------------------------*
*       Read data from ZTCO_ABISPOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data_fr_ztco_abispost.
* Use IT_POST
  clear : it_post, it_post[].
*
  select * into corresponding fields of table   it_post
           from ztco_abispost
          where kokrs  = p_kokrs
            and gjahr  = p_gjahr
            and period = p_perio
            and versn  = p_versn
            and matnr  in s_matnr
            and belnr = space.

* Collect data
  clear : it_post_fin, it_post_fin[].
  loop at it_post.
    move-corresponding it_post to it_post_fin.
    append it_post_fin.
  endloop.

endform.                    " READ_DATA_FR_ZTCO_ABISPOST

*&---------------------------------------------------------------------*
*&      Form  POST_WITH_FM
*&---------------------------------------------------------------------*
*       Posting FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_with_fm.
* The number of Item Records should not be greater than 200
  data : it_post_idx like sy-tabix.
  data : lv_mod      like sy-tabix.
  data : lv_div      like sy-tabix.
  data : lv_from_idx like sy-tabix,
         lv_to_idx   like sy-tabix,
         lv_skip.

* Make one document per 200 records
  sort it_post_fin by io_aufnr pcc_aufnr kstar.

* Read the number of Index of "IT_POST"
  describe table it_post_fin lines it_post_idx.
  lv_div = it_post_idx  div  200.
  lv_mod = it_post_idx  mod  200.
  if lv_mod > 0.
    lv_div = lv_div + 1.
  endif.

* CALL POST FM
  do lv_div times.
* Check Index of IT_POST
* Cal. MOD. DIV.
    lv_to_idx   =  sy-index * 200 .
    lv_from_idx =  lv_to_idx - 199.
* From
    clear lv_skip.
    clear it_post_fin. read table it_post_fin index lv_from_idx.
    if sy-subrc <> 0. lv_skip = 'X'. endif.
* TO
    clear it_post_fin. read table it_post_fin index lv_to_idx.
    if sy-subrc <> 0. lv_to_idx = lv_from_idx + lv_mod - 1 . endif.

    if lv_skip <> 'X'.
* Run Post FM
      perform call_post_fm_res using  lv_from_idx lv_to_idx .
    endif.
  enddo.

endform.                    " POST_WITH_FM

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM_RES
*&---------------------------------------------------------------------*
*       Run Post FM
*----------------------------------------------------------------------*
*      -->P_LV_FR  From-Index
*      -->P_LV_TO  To-Index
*----------------------------------------------------------------------*
form call_post_fm_res  using    p_lv_fr
                                p_lv_to.
* Local Data definition
  data : wa_l_doc_header	like	bapidochdru12p.
  data : it_l_doc_items	      like  standard table of 	bapircitm
                              with  header line .
  data : it_return	      like	standard table of 	bapiret2
                              with  header line.
  data : lv_doc_no            like	bapidochdru12p-doc_no.

** Header data
  clear wa_l_doc_header.
  wa_l_doc_header-co_area  = p_kokrs.
  wa_l_doc_header-period   = p_perio.
* Posting date - Last date in period
  call function 'LAST_DAY_IN_PERIOD_GET'
       exporting
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_perio
       importing
            e_date  = wa_l_doc_header-postgdate.
  wa_l_doc_header-variant  = 'SAP02'.
  concatenate text-010 sy-repid
            sy-uname sy-datum sy-uzeit
       into wa_l_doc_header-doc_hdr_tx
       separated by space.
  wa_l_doc_header-username = sy-uname.

* Currency
* All currency in data is controlling area currency = 'USD'
* -> transaction Currency
  wa_l_doc_header-trans_curr = tka01-waers.

** Item DATA
  clear : it_l_doc_items, it_l_doc_items[].
  loop at it_post_fin  from p_lv_fr to  p_lv_to.
    it_l_doc_items-sen_order  = it_post_fin-io_aufnr.
    it_l_doc_items-rec_order  = it_post_fin-pcc_aufnr.
    it_l_doc_items-cost_elem  = it_post_fin-kstar.
    it_l_doc_items-value_tcur = it_post_fin-chg_wkgbtr.
    it_l_doc_items-quantity   = it_post_fin-mbgbtr.
    it_l_doc_items-postquun   = it_post_fin-meinb.

    concatenate it_post_fin-matnr ';' it_post_fin-stype
           into it_l_doc_items-seg_text.

    append it_l_doc_items.
    clear  it_l_doc_items.
    clear it_post_fin.
  endloop.

** BAPI
  clear : it_return, it_return[].
  clear : lv_doc_no .
  call function 'BAPI_ACC_PRIMARY_COSTS_POST'
    exporting
      doc_header            = wa_l_doc_header
*     IGNORE_WARNINGS       = ' '
    importing
      doc_no                = lv_doc_no
    tables
      doc_items             = it_l_doc_items
      return                = it_return.

* reverse
*BAPI_ACC_ACT_POSTINGS_REVERSE

* CHECK ERROR
  clear  it_return.
  loop at it_return.
    if it_return-type = 'E'.
      break-point.
    endif.

    gt_return = it_return.
    append gt_return.
*    MESSAGE ID     it_return-id
*            TYPE   it_return-type
*            NUMBER it_return-number
*            WITH   it_return-message_v1
*                   it_return-message_v2
*                   it_return-message_v3
*                   it_return-message_v4.
*    CLEAR it_return.
  endloop.

* Save Doc. No.
  loop at it_post_fin  from p_lv_fr to  p_lv_to.
    it_post_fin-belnr =   lv_doc_no.
    modify it_post_fin.
    clear  it_post_fin.
  endloop.

endform.                    " CALL_POST_FM_RES

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DOC_NO
*&---------------------------------------------------------------------*
*       Save Doc No.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_doc_no.
  clear it_post_fin.
  loop at it_post.
    clear it_post_fin.
    read table it_post_fin with key   kstar      = it_post-kstar
                                      io_aufnr   = it_post-io_aufnr
                                      pcc_aufnr  = it_post-pcc_aufnr.
    it_post-belnr =  it_post_fin-belnr.
* Log
    it_post-aedat = sy-datum.
    it_post-aezet = sy-uzeit.
    it_post-aenam = sy-uname.
* Update
    clear ztco_abispost.
    move-corresponding it_post to ztco_abispost.
    update ztco_abispost.
    if sy-subrc <> 0.
      rollback work.
      message e030 with 'ZTCO_ABISPOST'.
    endif.
  endloop.
* Commit Work.
  commit work and wait.
  message s000 with 'The process for posting was Completed'.
endform.                    " UPDATE_DOC_NO

*&---------------------------------------------------------------------*
*&      Form  ALPHA_KSTAR
*&---------------------------------------------------------------------*
*       Kstar - > Alpha Numeric
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alpha_kstar.
  loop at s_kstar.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
         exporting
              input  = s_kstar-low
         importing
              output = s_kstar-low.

    modify s_kstar.
    clear  s_kstar.
  endloop.
endform.                    " ALPHA_KSTAR

*&---------------------------------------------------------------------*
*&      Form  ADJUSTMENT_DATA
*&---------------------------------------------------------------------*
*       Adjustment
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form adjustment_data.
* Progress Ind.
  perform progress_ind using '80'
                             text-024.

* Local data definition
  data :  lv_sum_wkgbtr like coep-wkgbtr.
  data :  lv_modify.

  sort it_io_coep  by  matnr stype werks kstar objnr.
  sort it_post     by  matnr stype werks kstar  ascending
                       stype io_aufnr
                       chg_wkgbtr         descending .

  loop at it_io_coep .
    loop at it_post where matnr    = it_io_coep-matnr
                      and werks    = it_io_coep-werks
                      and kstar    = it_io_coep-kstar
                      and stype    = it_io_coep-stype
                      and io_aufnr = it_io_coep-objnr+2(5).
      clear  lv_modify.
      clear  lv_sum_wkgbtr.
* Check different amount
      at end of io_aufnr.
        sum .
        lv_sum_wkgbtr = it_post-chg_wkgbtr.
        if lv_sum_wkgbtr <> it_io_coep-wkgbtr .
          lv_modify = 'X'.
        endif.
      endat.
      at end of werks.
      endat.
      at end of kstar.
      endat.
* modify
      if lv_modify = 'X'.
        it_post-chg_wkgbtr =
        it_post-chg_wkgbtr + ( it_io_coep-wkgbtr - lv_sum_wkgbtr ).
        modify it_post.
      endif.
      clear it_post.
    endloop.
    clear it_io_coep.
  endloop.

  delete it_post where chg_wkgbtr eq 0
                    or mbgbtr     eq 0.
endform.                    " ADJUSTMENT_DATA

*&---------------------------------------------------------------------*
*&      Form  IND_POST_N_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ind_post_n_report.
*P_CONF
  loop at screen.
    check screen-group1 = 'ZPA'.
    if p_conf = 'X'.
      screen-invisible = ' '.
      screen-input     = '1'.
    else.
      screen-invisible = '1'.
      screen-input     = ' '.
    endif.
    modify screen.
  endloop.

endform.                    " IND_POST_N_REPORT

*&---------------------------------------------------------------------*
*&      Form  CAL_COST_RATIO_02
*&---------------------------------------------------------------------*
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_cost_ratio_02.

* Local Data Definition
  data : begin of it_l_chm_rate occurs 0.
  data :  kstar  like coep-kstar,
          matnr  like coep-matnr,
          werks  like coep-werks,
          fevor  like marc-fevor,
          pcc_mbgbtr like coep-mbgbtr.
  data : end of  it_l_chm_rate.

  data : begin of it_l_tot_rate occurs 0.
  data :  werks  like coep-werks,
          objnr  like coep-objnr,
          fevor  like marc-fevor,
          tot_wkgbtr like coep-wkgbtr,
*          tot_mbgbtr LIKE coep-mbgbtr,
          kstar_rate like ztco_abispost-kstar_rate.
  data : end of  it_l_tot_rate.

  data : begin of it_l_pccsum occurs 0,
            werks  like coep-werks,
            fevor  like marc-fevor,
            wk_sum like coep-wkgbtr,
         end of   it_l_pccsum.

* Calculation the cost ratio
  clear : it_l_chm_rate, it_l_chm_rate[].
  clear : it_l_tot_rate, it_l_tot_rate[].
  clear : it_l_pccsum  , it_l_pccsum[]  .

  sort it_pcc_coep by kstar matnr werks objnr.

  sort it_pcc_mat by objnr.

  loop at  it_pcc_coep.
    read table it_pcc_mat with key objnr  = it_pcc_coep-objnr
    binary search.

* Calculation the total cost by child materials
    move-corresponding  it_pcc_coep to  it_l_chm_rate.
    it_l_chm_rate-pcc_mbgbtr = it_pcc_coep-mbgbtr.
    collect it_l_chm_rate.
    clear   it_l_chm_rate.
* Calculation the total cost by PCC (Parents materials)
    move-corresponding  it_pcc_coep to  it_l_tot_rate.
    it_l_tot_rate-tot_wkgbtr = it_pcc_coep-wkgbtr.
    it_l_tot_rate-fevor      = it_pcc_mat-fevor.
    collect it_l_tot_rate.
    clear   it_l_tot_rate.
* PCC SUM
    it_l_pccsum-werks  = it_pcc_coep-werks.
    it_l_pccsum-fevor  = it_pcc_mat-fevor.
    it_l_pccsum-wk_sum = it_pcc_coep-wkgbtr.
    collect it_l_pccsum.
    clear   it_l_pccsum.
    clear it_pcc_coep.
  endloop.

* Copy to Global
  sort it_l_pccsum by werks fevor.
  clear : it_tot_rate, it_tot_rate[].
  loop at it_l_tot_rate.
    clear it_l_pccsum.
    read table it_l_pccsum with key werks = it_l_tot_rate-werks
                                    fevor = it_l_tot_rate-fevor
                                    binary search.
    if sy-subrc <> 0.
      continue.
    endif.
    it_l_tot_rate-kstar_rate
      = it_l_tot_rate-tot_wkgbtr  / it_l_pccsum-wk_sum.
    modify it_l_tot_rate.
* Copy to global Itab
    move-corresponding  it_l_tot_rate to it_tot_rate.
    append it_tot_rate.
    clear  it_tot_rate.
    clear it_l_tot_rate.
  endloop.

* Sort
  sort it_l_chm_rate by kstar matnr werks.
  clear   it_l_chm_rate.
*
*  RATE_CHILD(16)    LIKE ZTCO_ABISPOST-RATE_CHILD.

  loop at it_pcc_coep.
* ratio (Child)
    clear   it_l_chm_rate.
    read table it_l_chm_rate with key kstar = it_pcc_coep-kstar
                                      matnr = it_pcc_coep-matnr
                                      werks = it_pcc_coep-werks
                                      binary search.
    if   sy-subrc = 0
     and it_l_chm_rate-pcc_mbgbtr ne space.
      it_pcc_coep-rate_child
       = it_pcc_coep-mbgbtr / it_l_chm_rate-pcc_mbgbtr.
    endif.
    modify it_pcc_coep.
    clear  it_pcc_coep.
  endloop.

  clear  it_pcc_coep.
  clear  it_tot_rate.

endform.                    " CAL_COST_RATIO_02

*&---------------------------------------------------------------------*
*&      Form  CAL_QTY_RATIO_02
*&---------------------------------------------------------------------*
*       Ratio By qty
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_qty_ratio_02.

  clear  it_pcc_coep.
  clear  it_tot_rate.
  clear  it_io_coep.

* Progress Ind.
  perform progress_ind using '60'
                             text-023.

endform.                    " CAL_QTY_RATIO_02

*&---------------------------------------------------------------------*
*&      Form  READ_VERID_FROM_PCC
*&---------------------------------------------------------------------*
*       Read Production version from PCC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_verid_from_pcc.

  tables : pkosa, ckmlmv001.

  clear pkosa.
  pkosa-aufnr = ztco_abispost-pcc_aufnr.
  call function 'KK_F_PKOSA_FILL'
       exporting
            pkosanr_imp  = pkosa-aufnr
       importing
            pkosa_exp    = pkosa
       exceptions
            aufnr_not_ok = 1
            others       = 2.
  if sy-subrc <> 0.
  endif.

  clear ckmlmv001.
  call function 'CKML_MGV_PROCESS_READ'
    exporting
        i_kalnr           = pkosa-procnr
*       I_BUFFER          =
    importing
*       E_PROCESS         =
        e_ckmlmv001       = ckmlmv001
    exceptions
        not_found         = 1
        others            = 2.
  if sy-subrc <> 0.
  endif.

  ztco_abispost-verid = ckmlmv001-verid_nd.
  ztco_abispost-fsc_matnr = pkosa-matnr.
endform.                    " READ_VERID_FROM_PCC

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
*&      Form  GET_PARENT_PRODUCTION_SCHEDULE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_FEVOR  text
*----------------------------------------------------------------------*
form get_parent_production_schedule using pw_fevor.
  case it_io_coep-objnr+2.
    when 'CP001'.
      case it_io_coep-fevor.
        when c_blank.
          move: c_press to pw_fevor.
        when c_coil.
          move: c_blank to pw_fevor.
        when others.
          move: space   to pw_fevor.
      endcase.
    when 'CE001'.
      case it_io_coep-fevor.
        when c_3c.
          move: c_engine to pw_fevor.
        when others.
          move: space    to pw_fevor.
      endcase.
    when others.
      move: space to pw_fevor.
  endcase.
endform.                    " GET_PARENT_PRODUCTION_SCHEDULE
*&---------------------------------------------------------------------*
*&      Form  make_ppc_post_table_for_no_bf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_ppc_post_table_for_no_bf using pw_fevor.
  data: lw_netpr      type p decimals 3,
        lw_netpr_tmp  like it_io_coep-wkgbtr.
  data : lv_sum_qty like it_io_coep-mbgbtr.
  data : lv_res_qty like it_io_coep-mbgbtr.

  if it_io_coep-werks eq 'E001' or
     pw_fevor         eq c_3c.
    loop at it_tot_rate where werks = it_io_coep-werks.
      move: it_io_coep-waers  to it_post-waers,
            it_io_coep-kstar  to it_post-kstar,
            it_io_coep-matnr  to it_post-matnr,
            it_io_coep-werks  to it_post-werks,
            it_io_coep-meinb  to it_post-meinb,
            it_io_coep-stype  to it_post-stype,
            'A'               to it_post-type.

*  read PCC order
      clear it_pcc_mat.
      read table it_pcc_mat with key objnr  = it_tot_rate-objnr
      binary search.
      it_post-pcc_aufnr = it_pcc_mat-aufnr.
*  read IO order
      clear it_l_aufk.
      read table it_l_aufk  with key objnr  = it_io_coep-objnr
      binary search.
      it_post-io_aufnr = it_l_aufk-aufnr.
*  parentes rate
      it_post-rate_child = it_tot_rate-kstar_rate.

      if it_io_coep-meinb eq 'EA'.
        cal_by_qty rate_child.
      else.
        it_post-mbgbtr = it_io_coep-mbgbtr * it_post-rate_child.
        it_post-chg_wkgbtr = it_io_coep-wkgbtr * it_post-rate_child.
      endif.

      append it_post.
      clear  it_post.
    endloop.
  else.
    loop at it_tot_rate where werks = it_io_coep-werks
                          and fevor = pw_fevor.
      move: it_io_coep-waers  to it_post-waers,
            it_io_coep-kstar  to it_post-kstar,
            it_io_coep-matnr  to it_post-matnr,
            it_io_coep-werks  to it_post-werks,
            it_io_coep-meinb  to it_post-meinb,
            it_io_coep-stype  to it_post-stype,
            'A'               to it_post-type.

*  read PCC order
      clear it_pcc_mat.
      read table it_pcc_mat with key objnr  = it_tot_rate-objnr
      binary search.
      it_post-pcc_aufnr = it_pcc_mat-aufnr.
*  read IO order
      clear it_l_aufk.
      read table it_l_aufk  with key objnr  = it_io_coep-objnr
      binary search.
      it_post-io_aufnr = it_l_aufk-aufnr.
*  parentes rate
      it_post-rate_child = it_tot_rate-kstar_rate.

      if it_io_coep-meinb eq 'EA'.
        cal_by_qty rate_child.
      else.
        it_post-mbgbtr = it_io_coep-mbgbtr * it_post-rate_child.
        it_post-chg_wkgbtr = it_io_coep-wkgbtr * it_post-rate_child.
      endif.

      append it_post.
      clear  it_post.
    endloop.
  endif.
endform.                    " make_ppc_post_table_for_no_bf
*&---------------------------------------------------------------------*
*&      Form  rounding_uom_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rounding_uom_amount using pw_netpr.
*  DATA: lw_wkgbtr     LIKE it_io_coep-mbgbtr,
*        lw_netpr_half TYPE p DECIMALS 3,
*        lw_sum        LIKE it_io_coep-wkgbtr,
*        lw_gap        LIKE it_io_coep-wkgbtr,
*        lw_mod        TYPE p DECIMALS 3.
*
*
*  lw_netpr_half = pw_netpr / 2.
*
*  LOOP AT it_post_tmp.
*    lw_mod = it_post_tmp-chg_wkgbtr MOD pw_netpr.
*    IF lw_mod EQ 0.
*      CONTINUE.
*    ENDIF.
*
*    IF lw_mod >= lw_netpr_half.
*      lw_wkgbtr = it_post_tmp-chg_wkgbtr - lw_mod + pw_netpr.
*    ELSE.
*      lw_wkgbtr = it_post_tmp-chg_wkgbtr - lw_mod.
*    ENDIF.
*
*    it_post_tmp-chg_wkgbtr = lw_wkgbtr.
*    lw_sum = lw_sum + lw_wkgbtr.
*    MODIFY it_post_tmp.
*  ENDLOOP.
*
*  lw_gap = it_io_coep-wkgbtr - lw_sum.
*  IF lw_gap NE 0.
*    SORT it_post_tmp BY chg_wkgbtr DESCENDING rate_child DESCENDING.
*
*    READ TABLE it_post_tmp INDEX 1.
*    it_post_tmp-chg_wkgbtr = it_post_tmp-chg_wkgbtr + lw_gap.
*    MODIFY it_post_tmp INDEX 1.
*  ENDIF.
endform.                    " rounding_uom_amount

*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0811   text
*      -->P_0812   text
*      -->P_0813   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
form pop_up using    p_text p_text2 p_canc
            changing p_answer.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       importing
            answer         = p_answer.


endform.                    " POP_UP
