*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120031469 0000234174                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZML_VALUE_FLOW_ANALYZER
*& Object Header   PROG ZML_VALUE_FLOW_ANALYZER
*&-------------------------------------------------------------------*
*& REPORT ZML_VALUE_FLOW_ANALYZER
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report           ZML_VALUE_FLOW_ANALYZER                         *
*&                                                                     *
*&---------------------------------------------------------------------*

*include zmlvfatop.
*&---------------------------------------------------------------------*
*& Include ZMLVFATOP                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
report zml_value_flow_analyzer message-id c+ .

type-pools: slis, ckmv0.

tables: mlkey, mara, marv, marc, mbew, tcurm, t001, t001k, ckmlhd,
        ckmlpp, ckmlrunperiod, sscrfields.

ranges: r_bwkey for ckmlhd-bwkey.      "Hilfsrange Bewertungskreis

types:
* Material/valuated sales order/project stock data
  begin of s_mats,
     kalnr type ckmlhd-kalnr,
     matnr type ckmlhd-matnr,
     bwkey type ckmlhd-bwkey,
     bwtar type ckmlhd-bwtar,
     sobkz type ckmlhd-sobkz,
     vbeln type ckmlhd-vbeln,
     posnr type ckmlhd-posnr,
     pspnr type ckmlhd-pspnr,
     bklas type mbew-bklas,
     mtart type mara-mtart,
     matkl type mara-matkl,
     spart type mara-spart,
     prctr type marc-prctr,
     meins type mara-meins,
  end of s_mats,
  ty_mats type standard table of s_mats with key kalnr,
* Output
  begin of s_out,
     kalnr type ckmlhd-kalnr,
     bdatj type ckmlpp-bdatj,
     poper type ckmlpp-poper,
     untper type ckmlpp-untper,
     curtp type ckmlcr-curtp,
     matnr type ckmlhd-matnr,
     bwkey type ckmlhd-bwkey,
     bwtar type ckmlhd-bwtar,
     vbeln type ckmlhd-vbeln,
     posnr type ckmlhd-posnr,
     pspnr type ckmlhd-pspnr,
     bklas type mbew-bklas,
     mtart type mara-mtart,
     matkl type mara-matkl,
     spart type mara-spart,
     prctr type marc-prctr,
     meins type ckmlpp-meins,
     status type ckmlpp-status,
     lbkum type ckmlpp-lbkum,
     menge type kkb_ml_menge,
     pbpopo type ckmlpp-pbpopo,
     salk3 type ckmlcr-salk3,
     wert type kkb_ml_bewer,
     stprs type ckmlcr-stprs,
     pvprs type ckmlcr-pvprs,
     peinh type ckmlcr-peinh,
     waers type ckmlcr-waers,
     pbprd_o type ckmlcr-pbprd_o,
     pbkdm_o type ckmlcr-pbkdm_o,
     estprd type ckml_estprd,
     estkdm type ckml_estkdm,
     mstprd type ckml_mstprd,
     mstkdm type ckml_mstkdm,
     estdif type ck_singlelevel_dif,
     mstdif type ck_multilevel_dif,
     prdif type ck_sum_prdif,
     krdif type ck_sum_krdif,
     sumdif type ck_sum_dif,
     pos_type(3),
     pos_type_text(40),
     color(3) type c,
   end of s_out,
   ty_out type standard table of s_out with key kalnr.

*----------------------------------------------------------------------*
*     Tabellen
*----------------------------------------------------------------------*
data: t_t001k like t001k occurs 0 with header line,
      t_out type ty_out,
      t_mats type ty_mats,
      t_mats_portion type ty_mats,
      t_ckmlpp type standard table of ckmlpp
               with key kalnr bdatj poper
               with header line,
      t_ckmlcr type standard table of ckmlcr
               with key kalnr bdatj poper curtp
               with header line,
      t_mlcd type standard table of mlcd
               with key kalnr bdatj poper untper categ ptyp bvalt curtp
               with header line,
      t_mlcd_not_alloc type standard table of mlcd
               with key kalnr bdatj poper untper categ ptyp bvalt curtp
               with header line,
      t_plants type ckml_run_t_plant.

*----------------------------------------------------------------------*
*     Feldleisten                                                      *

*----------------------------------------------------------------------*
data: s_runperiod type ckml_run_period_data,
      s_mats type s_mats.

*----------------------------------------------------------------------*
*     Globale Hilfsfelder                                              *
*----------------------------------------------------------------------*
data: h_last_bwkey type bwkey,
      h_sele_lauf type boole_d,
      h_expan type boole_d,
      h_lines type i,
      h_index type sy-tabix,
      h_counter type i,
      h_portion_size type i value 100.


include lckm0top_status.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*

*include zmlvfa_para.
selection-screen skip 2.

select-options: r_matnr for mlkey-matnr memory id mat,
                r_bukrs for mlkey-bukrs memory id buk,
                r_werks for mlkey-werks memory id wrk,
                r_bwtar for mlkey-bwtar memory id bwt.

selection-screen skip.
selection-screen begin of block sele with frame title text-011.
* Lauf
selection-screen begin of block lauf with frame title text-001.
parameters: p_lauf like ckmlrunperiod-run_type
                        memory id ckml_run_type modif id lau.
selection-screen begin of line.
selection-screen comment 1(29) text-009 for field p_lpop modif id lau.
parameters: p_lpop like ckmlrunperiod-poper memory id mlp modif id lau,
            p_lgja like ckmlrunperiod-gjahr memory id mlj modif id lau.
selection-screen comment 40(1) text-010 for field p_lpop modif id lau.
parameters: p_appl like ckmlrunperiod-appl modif id lau.
selection-screen end of line.
parameters: p_lday like ckmlrunperiod-last_day no-display modif id lau.
selection-screen end of block lauf.
* Periode
selection-screen begin of block periode with frame title text-003.
parameters: p_poper like cki_doc_ml-sl_periode memory id mlp
                                               modif id per,
            p_bdatj like mlkey-bdatj memory id mlj modif id per.
selection-screen end of block periode.
selection-screen pushbutton /1(25) knopf user-command sele.
selection-screen end of block sele.

selection-screen begin of block radio with frame title text-004.
parameters: p_notdis radiobutton group g default 'X',
            p_notinc radiobutton group g,
            p_all radiobutton group g.
selection-screen end of block radio.

selection-screen pushbutton /1(30) expan user-command expan.
selection-screen begin of block extra with frame title text-002.

select-options: r_vbeln for  ckmlhd-vbeln  modif id puk,
                r_posnr for  ckmlhd-posnr  modif id puk,
                r_pspnr for  ckmlhd-pspnr  modif id puk,
                r_bklas for  mbew-bklas    modif id puk,
                r_mtart for  mara-mtart    memory id mta modif id puk,
                r_matkl for  mara-matkl    memory id mtl modif id puk,
                r_spart for  mara-spart    memory id spa modif id puk,
                r_prctr for  marc-prctr    memory id prc modif id puk.
selection-screen end of block extra.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*

***************************
at selection-screen output.
***************************
  if tcurm-bwkrs_cus is initial.
    read table tcurm.
  endif.
* Display Select-Option R_WERKS or R_BUKRS
  if tcurm-bwkrs_cus = '3'.            "Bewertungsebene BURKS
    loop at screen.
      if screen-name cs 'R_WERKS'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  else.                                                     "sonst
    loop at screen.
      if screen-name cs 'R_BUKRS'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  endif.
  if not h_sele_lauf is initial.
    knopf = text-003.
    loop at screen.
      if screen-group1 = 'PER'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  else.
    knopf = text-001.
    loop at screen.
      if screen-group1 = 'LAU'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  endif.
  if not h_expan is initial.
    concatenate text-013 text-002
                into expan separated by space.
    loop at screen.
      if screen-group1 = 'PUK'.
        screen-invisible = '0'.
        screen-active    = '1'.
        modify screen.
      endif.
    endloop.
  else.
    refresh: r_vbeln, r_posnr, r_pspnr, r_bklas, r_mtart, r_matkl,
             r_spart, r_prctr.
    concatenate text-012 text-002
                into expan separated by space.
    loop at screen.
      if screen-group1 = 'PUK'.
        screen-invisible = '1'.
        screen-active    = '0'.
        modify screen.
      endif.
    endloop.
  endif.

********************
at selection-screen.
********************
  if not r_bukrs[] is initial.
    r_bwkey[] = r_bukrs[].
  else.
    r_bwkey[] = r_werks[].
  endif.
  if sscrfields-ucomm = 'SELE'.
    if h_sele_lauf is initial.
      h_sele_lauf = 'X'.
    else.
      clear: h_sele_lauf, p_lauf.
    endif.
  endif.
  if sscrfields-ucomm = 'EXPAN'.
    if h_expan is initial.
      h_expan = 'X'.
    else.
      clear: h_expan.
      refresh: r_vbeln, r_posnr, r_pspnr, r_bklas, r_mtart, r_matkl,
               r_spart, r_prctr.
    endif.
  endif.

*******************
start-of-selection.
*******************

  if not p_lauf is initial.
    call function 'CKML_RUN_PERIOD_GET'
      exporting
*       I_RUN_ID               =
        i_run_type             = p_lauf
*       I_LAST_DAY             = p_lday
*       I_LANGU                = SY-LANGU
        i_poper                = p_lpop
        i_gjahr                = p_lgja
        i_appl                 = p_appl
      importing
        es_runperiod           = s_runperiod
      exceptions
        run_not_existent       = 1
        others                 = 2.
    if sy-subrc <> 0.
      clear: s_runperiod.
      message s112(ckmlrun) with p_lauf p_lpop p_lgja p_appl.
      submit zml_value_flow_analyzer via selection-screen.
    else.
      p_bdatj = s_runperiod-gjahr.
      p_poper = s_runperiod-poper.
      call function 'CKML_RUN_PLANTS_GET'
        exporting
          i_run_id               = s_runperiod-run_id
*         I_RUN_TYPE             =
*         I_LAST_DAY             =
*         I_POPER                =
*         I_GJAHR                =
*         I_APPL                 = CKRU0_CO_APPL_ACT
        importing
          et_plants              = t_plants
        exceptions
          run_not_existent       = 1
          no_plants              = 2
          others                 = 3.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
  endif.

  refresh: t_t001k.

* Bewertungskreise bestimmen
* ACHTUNG! Funktioniert so nicht mehr, wenn Bewertung auf Buchungskreis!
  if not t_plants is initial.
    select bwkey bukrs mlbwa from t001k
                             into corresponding fields of table t_t001k
                             for all entries in t_plants
                             where bwkey = t_plants-werks
                             and   bwkey in r_bwkey.
  else.
    select bwkey bukrs mlbwa from t001k
                             into corresponding fields of table t_t001k
                             where bwkey in r_bwkey.
  endif.
  loop at t_t001k.
    refresh: t_mats, t_mats_portion, t_ckmlpp, t_ckmlcr.
    if not t_t001k-mlbwa is initial.
      perform get_materials using t_t001k
                            changing p_bdatj
                                     p_poper
                                     t_mats.
*     Portionieren!
      describe table t_mats lines h_lines.
      loop at t_mats into s_mats.
        h_index = sy-tabix.
        h_counter = h_counter + 1.
        append s_mats to t_mats_portion.
        if h_counter >= h_portion_size or h_index = h_lines.
          perform get_material_periods using t_mats_portion
                                             t_ckmlpp[]
                                             t_ckmlcr[].
          perform get_mlcd_data using t_mats_portion
                                changing t_mlcd[]
                                         t_mlcd_not_alloc[].
          perform find_bad_boys using t_mats_portion
                                      t_ckmlpp[]
                                      t_ckmlcr[]
                                      t_mlcd[]
                                      t_mlcd_not_alloc[]
                                changing t_out[].
          call function 'CKMS_BUFFER_REFRESH_COMPLETE'.
          refresh: t_mats_portion, t_ckmlpp, t_ckmlcr, t_mlcd,
                   t_mlcd_not_alloc.
          clear: h_counter.
        endif.
      endloop.
    endif.
  endloop.

* Ausgabe
  if not t_out[] is initial.
    perform ausgabe.
  else.
    message s154.
    if sy-batch is initial.
      submit zml_value_flow_analyzer via selection-screen.
    endif.
  endif.


*----------------------------------------------------------------------*
************************************************************************
*    FORM-Routinen                                                     *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
form get_materials using    pf_t001k type t001k
                   changing pl_bdatj type bdatj
                            pl_poper type poper
                            pt_mats like t_mats[].

  data: lt_kalnr type ckmv0_matobj_tbl,
        ls_mats type s_mats,
        ls_kalnr type ckmv0_matobj_str.

* Aktuelle Periode als Default / Hausw?rung lesen
  if pl_bdatj is initial or pl_poper is initial.
    if marv-bukrs <> pf_t001k-bukrs.
      select single * from marv where bukrs = pf_t001k-bukrs.
      if sy-subrc <> 0.
        clear: marv.
      endif.
    endif.
    if not marv is initial.
      pl_bdatj = marv-lfgja.
      if pl_poper is initial.
        pl_poper = marv-lfmon.
      endif.
    endif.
    call function 'T001_SINGLE_READ'
      exporting
*       KZRFB            = ' '
*       MAXTZ            = 0
        bukrs            = pf_t001k-bukrs
      importing
        wt001            = t001
      exceptions
        not_found        = 1
        wrong_call       = 2
        others           = 3.
    if sy-subrc <> 0.
      clear: t001.
    endif.
  endif.
  refresh: pt_mats.
  if not r_vbeln is initial.
    select h~bwkey h~kalnr h~matnr h~bwtar
           h~sobkz h~vbeln h~posnr h~pspnr
           m~mtart m~matkl m~spart m~meins c~prctr b~bklas
       into corresponding fields of table pt_mats
       from ( ( ckmlhd as h join mara as m
              on h~matnr = m~matnr )
              join marc as c
              on h~matnr = c~matnr and h~bwkey = c~werks )
            join ebew as b
            on h~matnr = b~matnr and h~bwkey = b~bwkey
            and h~bwtar = b~bwtar and h~sobkz = b~sobkz
            and h~posnr = b~posnr
       where h~bwkey = pf_t001k-bwkey
       and   h~matnr in r_matnr
       and   h~bwtar in r_bwtar
       and   h~vbeln in r_vbeln
       and   h~posnr in r_posnr
       and   h~pspnr in r_pspnr
       and   h~mlast = '3'
       and   b~bklas in r_bklas
       and   m~mtart in r_mtart
       and   m~matkl in r_matkl
       and   m~spart in r_spart
       and   c~prctr in r_prctr.
  elseif not r_pspnr is initial.
    select h~bwkey h~kalnr h~matnr h~bwtar
           h~sobkz h~vbeln h~posnr h~pspnr
           m~mtart m~matkl m~spart m~meins c~prctr b~bklas
       into corresponding fields of table pt_mats
       from ( ( ckmlhd as h join mara as m
              on h~matnr = m~matnr )
              join marc as c
              on h~matnr = c~matnr and h~bwkey = c~werks )
            join qbew as b
            on h~matnr = b~matnr and h~bwkey = b~bwkey
            and h~bwtar = b~bwtar and h~sobkz = b~sobkz
            and h~pspnr = b~pspnr
       where h~bwkey = pf_t001k-bwkey
       and   h~matnr in r_matnr
       and   h~bwtar in r_bwtar
       and   h~vbeln in r_vbeln
       and   h~posnr in r_posnr
       and   h~pspnr in r_pspnr
       and   h~mlast = '3'
       and   b~bklas in r_bklas
       and   m~mtart in r_mtart
       and   m~matkl in r_matkl
       and   m~spart in r_spart
       and   c~prctr in r_prctr.
  else.
    select h~bwkey h~kalnr h~matnr h~bwtar
           h~sobkz h~vbeln h~posnr h~pspnr
           m~mtart m~matkl m~spart m~meins c~prctr b~bklas
       into corresponding fields of table pt_mats
       from ( ( ckmlhd as h join mara as m
              on h~matnr = m~matnr )
              join marc as c
              on h~matnr = c~matnr and h~bwkey = c~werks )
            join mbew as b
            on h~matnr = b~matnr and h~bwkey = b~bwkey
            and h~bwtar = b~bwtar
       where h~bwkey = pf_t001k-bwkey
       and   h~matnr in r_matnr
       and   h~bwtar in r_bwtar
       and   h~vbeln in r_vbeln
       and   h~posnr in r_posnr
       and   h~pspnr in r_pspnr
       and   h~mlast = '3'
       and   b~bklas in r_bklas
       and   m~mtart in r_mtart
       and   m~matkl in r_matkl
       and   m~spart in r_spart
       and   c~prctr in r_prctr.
  endif.

endform.                               " get_materials
*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
form find_bad_boys using pt_mats like t_mats[]
                         pt_ckmlpp like t_ckmlpp[]
                         pt_ckmlcr like t_ckmlcr[]
                         pt_mlcd like t_mlcd[]
                         pt_mlcd_not_alloc like t_mlcd[]
                   changing pt_out like t_out[].

  data: ls_out_ndi type s_out,
        ls_out_cum type s_out,
        ls_out_nin type s_out,
        ls_mats type s_mats,
        ls_ckmlpp type ckmlpp,
        ls_ckmlcr type ckmlcr,
        ls_mlcd type mlcd,
        ls_mlcd_not_alloc type mlcd,
        l_color(3) type c,
        l_ab_menge like mlcd-lbkum,
        l_nin type boole_d,
        l_kalnr_old like mlcd-kalnr.

  loop at pt_mats into ls_mats.
    read table pt_ckmlpp into ls_ckmlpp
                         with key kalnr = ls_mats-kalnr
                                  bdatj = p_bdatj
                                  poper = p_poper
                                  untper = s_runperiod-untper
                                  binary search.
    check sy-subrc = 0.
    clear: ls_out_ndi, ls_out_cum, ls_out_nin.
    move-corresponding ls_ckmlpp to ls_out_ndi.
    move-corresponding ls_ckmlpp to ls_out_cum.
    move-corresponding ls_ckmlpp to ls_out_nin.
    if ls_ckmlpp-status >= y_einstufig_abgerechnet.
      read table pt_ckmlcr with key kalnr = ls_mats-kalnr
                                    bdatj = p_bdatj
                                    poper = p_poper
                                    untper = s_runperiod-untper
                                    binary search
                                    transporting no fields.
      loop at pt_ckmlcr into ls_ckmlcr from sy-tabix.
        if ls_ckmlcr-kalnr <> ls_ckmlpp-kalnr or
           ls_ckmlcr-bdatj <> ls_ckmlpp-bdatj or
           ls_ckmlcr-poper <> ls_ckmlpp-poper or
           ls_ckmlcr-untper <> ls_ckmlpp-untper.
          exit.
        endif.
*       Kumulierter Bestand
        move-corresponding ls_ckmlcr to ls_out_ndi.
        move-corresponding ls_ckmlcr to ls_out_cum.
        move-corresponding ls_ckmlcr to ls_out_nin.
        ls_out_cum-estprd = ls_ckmlcr-abprd_o + ls_ckmlcr-zuprd_o +
                            ls_ckmlcr-vpprd_o.
        ls_out_cum-estkdm = ls_ckmlcr-abkdm_o + ls_ckmlcr-zukdm_o +
                            ls_ckmlcr-vpkdm_o.
*       ACHTUNG: Bei Status "Abschluss storniert" k?nte das falsch
*       sein, da evtl. nur einstufig preisermittelt wurde!
        if ls_ckmlpp-status >= y_mehrstufig_abgerechnet.
          ls_out_cum-mstprd = ls_ckmlcr-abprd_mo + ls_ckmlcr-zuprd_mo.
          ls_out_cum-mstkdm = ls_ckmlcr-abkdm_mo + ls_ckmlcr-zukdm_mo.
        else.
          ls_out_cum-mstprd = ls_ckmlcr-abprd_mo.
          ls_out_cum-mstkdm = ls_ckmlcr-abkdm_mo.
        endif.
*       Gibt's nicht verteilte Differenzen?
        ls_out_ndi-estprd = ls_out_cum-estprd.
        ls_out_ndi-estkdm = ls_out_cum-estkdm.
        ls_out_ndi-mstprd = ls_out_cum-mstprd.
        ls_out_ndi-mstkdm = ls_out_cum-mstkdm.
        ls_out_ndi-estprd = ls_out_ndi-estprd -
                            ( ls_ckmlcr-vnprd_ea + ls_ckmlcr-ebprd_ea ).
        ls_out_ndi-estkdm = ls_out_ndi-estkdm -
                            ( ls_ckmlcr-vnkdm_ea + ls_ckmlcr-ebkdm_ea ).
        ls_out_ndi-mstprd = ls_out_ndi-mstprd -
                            ( ls_ckmlcr-vnprd_ma + ls_ckmlcr-ebprd_ma ).
        ls_out_ndi-mstkdm = ls_out_ndi-mstkdm -
                            ( ls_ckmlcr-vnkdm_ma + ls_ckmlcr-ebkdm_ma ).
        ls_out_ndi-sumdif = ls_out_ndi-estprd + ls_out_ndi-estkdm +
                            ls_out_ndi-mstprd + ls_out_ndi-mstkdm.
*       Gibt's eine 'Nicht verrechnet'-Zeile?
        read table pt_mlcd_not_alloc into ls_mlcd_not_alloc
                                     with key kalnr = ls_ckmlcr-kalnr
                                              bdatj = ls_ckmlcr-bdatj
                                              poper = ls_ckmlcr-poper
                                             untper = ls_ckmlcr-untper
                                              curtp = ls_ckmlcr-curtp
                                              binary search.
        if sy-subrc = 0.
          l_nin = 'X'.
        else.
          clear: l_nin.
        endif.
        if not ls_out_ndi-sumdif is initial or
           not l_nin is initial.
          if ls_out_ndi-kalnr <> l_kalnr_old.
            l_kalnr_old = ls_out_ndi-kalnr.
            if l_color = 'C21'.
              l_color = 'C20'.
            else.
              l_color = 'C21'.
            endif.
          endif.
          read table pt_mats into ls_mats
                             with key kalnr = ls_out_ndi-kalnr.
          if sy-subrc = 0.
            move-corresponding ls_mats to ls_out_cum.
            move-corresponding ls_mats to ls_out_ndi.
            move-corresponding ls_mats to ls_out_nin.
          endif.
          if ( not p_notdis is initial or not p_all is initial ) and
             not ls_out_ndi-sumdif is initial.
            ls_out_ndi-pos_type = 'NDI'.
            ls_out_ndi-color = l_color.
            ls_out_ndi-pos_type_text = text-006.
            clear: ls_out_ndi-menge, ls_out_ndi-wert.
            ls_out_ndi-prdif = ls_out_ndi-estprd + ls_out_ndi-mstprd.
            ls_out_ndi-krdif = ls_out_ndi-estkdm + ls_out_ndi-mstkdm.
            ls_out_ndi-estdif = ls_out_ndi-estprd + ls_out_ndi-estkdm.
            ls_out_ndi-mstdif = ls_out_ndi-mstprd + ls_out_ndi-mstkdm.
            append ls_out_ndi to pt_out.
          endif.
          if not p_all is initial.
            ls_out_cum-pos_type = 'CUM'.
            ls_out_cum-color = l_color.
            ls_out_cum-pos_type_text = text-008.
*           Anfangsbestands-Menge (R?kbuchungen) aus MLCD
            clear: l_ab_menge, ls_out_cum-pbpopo.
            read table pt_mlcd transporting no fields
                               with key kalnr = ls_ckmlcr-kalnr
                                        bdatj = ls_ckmlcr-bdatj
                                        poper = ls_ckmlcr-poper
                                        untper = ls_ckmlcr-untper
                                        categ = 'AB'
                                        curtp = ls_ckmlcr-curtp
                                        binary search.
            if sy-subrc = 0.
              loop at pt_mlcd from sy-tabix into ls_mlcd.
                if ls_mlcd-kalnr <> ls_ckmlcr-kalnr or
                   ls_mlcd-bdatj <> ls_ckmlcr-bdatj or
                   ls_mlcd-poper <> ls_ckmlcr-poper or
                   ls_mlcd-untper <> ls_ckmlcr-untper or
                   ls_mlcd-categ <> 'AB' or
                   ls_mlcd-curtp <> ls_ckmlcr-curtp.
                  exit.
                endif.
                l_ab_menge = l_ab_menge + ls_mlcd-lbkum.
              endloop.
            endif.
            ls_out_cum-prdif = ls_out_cum-estprd + ls_out_cum-mstprd.
            ls_out_cum-krdif = ls_out_cum-estkdm + ls_out_cum-mstkdm.
            ls_out_cum-estdif = ls_out_cum-estprd + ls_out_cum-estkdm.
            ls_out_cum-mstdif = ls_out_cum-mstprd + ls_out_cum-mstkdm.
            ls_out_cum-sumdif = ls_out_cum-estprd + ls_out_cum-estkdm +
                                  ls_out_cum-mstprd + ls_out_cum-mstkdm.
            ls_out_cum-menge = ls_ckmlpp-abkumo + ls_ckmlpp-zukumo +
                               ls_ckmlpp-vpkumo + l_ab_menge.
            ls_out_cum-wert = ls_out_cum-menge * ls_out_cum-stprs /
                              ls_out_cum-peinh.
            append ls_out_cum to pt_out.
          endif.
          if ( not p_notinc is initial or not p_all is initial ) and
             not l_nin is initial.
            ls_out_nin-pos_type = 'NIN'.
            ls_out_nin-color = l_color.
            ls_out_nin-pos_type_text = text-007.
            clear: ls_out_nin-menge, ls_out_nin-wert.
            ls_out_nin-estprd = ls_mlcd_not_alloc-estprd.
            ls_out_nin-estkdm = ls_mlcd_not_alloc-estkdm.
            ls_out_nin-mstprd = ls_mlcd_not_alloc-mstprd.
            ls_out_nin-mstkdm = ls_mlcd_not_alloc-mstkdm.
            ls_out_nin-prdif = ls_out_nin-estprd + ls_out_nin-mstprd.
            ls_out_nin-krdif = ls_out_nin-estkdm + ls_out_nin-mstkdm.
            ls_out_nin-estdif = ls_out_nin-estprd + ls_out_nin-estkdm.
            ls_out_nin-mstdif = ls_out_nin-mstprd + ls_out_nin-mstkdm.
            ls_out_nin-sumdif = ls_out_nin-estprd + ls_out_nin-estkdm +
                                  ls_out_nin-mstprd + ls_out_nin-mstkdm.
            append ls_out_nin to pt_out.
          endif.
        endif.
      endloop.
    else.
*   Da kommt noch was!
    endif.
  endloop.


endform.                               " find_bad_boys
*&---------------------------------------------------------------------*
*&      Form  ausgabe
*&---------------------------------------------------------------------*
form ausgabe.

  data: lt_fieldcat type slis_t_fieldcat_alv,
        lt_sort type slis_t_sortinfo_alv,
        lt_filter type slis_t_filter_alv,
        ls_fieldcat type slis_fieldcat_alv,
        ls_layout type slis_layout_alv,
        ls_variant like disvariant,
        ls_sort type slis_sortinfo_alv,
        ls_filter type slis_filter_alv,
        l_index type i,
        l_repid type sy-repid,
        l_bwtar_space like ckmlhd-bwtar,
        l_vbeln_space like ckmlhd-vbeln,
        l_posnr_space like ckmlhd-posnr,
        l_pspnr_space like ckmlhd-pspnr.


  clear: l_bwtar_space, l_vbeln_space, l_posnr_space,
         l_pspnr_space.

  refresh lt_fieldcat. clear lt_fieldcat.

  if not p_all is initial.
    clear ls_fieldcat.
    ls_fieldcat-fieldname = 'POS_TYPE_TEXT'.
    ls_fieldcat-tabname = 'T_OUT'.
    ls_fieldcat-ref_tabname = 'CKML_MULTI_TREE'.
    ls_fieldcat-ref_fieldname = 'LTEXT'.
    l_index = l_index + 1.
    ls_fieldcat-col_pos = l_index.
*   ls_fieldcat-key = y_x.
    append ls_fieldcat to lt_fieldcat.
  endif.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MATNR'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'BWKEY'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  if tcurm-bwkrs_cus = '1'.          "Bewertungsebene = Werk
    ls_fieldcat-ref_tabname = 'MLKEY'.
    ls_fieldcat-ref_fieldname = 'WERKS'.
  endif.
  if tcurm-bwkrs_cus = '3'.          "Bewertungsebene = BUKRS
    ls_fieldcat-ref_tabname = 'MLKEY'.
    ls_fieldcat-ref_fieldname = 'BUKRS'.
  endif.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'BWTAR'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  loop at t_out transporting no fields
                where not bwtar = l_bwtar_space.
    exit.
  endloop.
  if sy-subrc <> 0.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'VBELN'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  loop at t_out transporting no fields
                where not vbeln = l_vbeln_space.
    exit.
  endloop.
  if sy-subrc <> 0.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'POSNR'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  loop at t_out transporting no fields
                where not posnr = l_posnr_space.
    exit.
  endloop.
  if sy-subrc <> 0.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'PSPNR'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLHD'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  loop at t_out transporting no fields
                where not pspnr = l_pspnr_space.
    exit.
  endloop.
  if sy-subrc <> 0.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'BKLAS'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MBEW'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MTART'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MARA'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MATKL'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MARA'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'SPART'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MARA'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'PRCTR'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MARC'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'SUMDIF'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  ls_fieldcat-ref_fieldname = 'SUM_DIF'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'PRDIF'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  ls_fieldcat-ref_fieldname = 'SUM_PRDIF'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'KRDIF'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  ls_fieldcat-ref_fieldname = 'SUM_KRDIF'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'ESTDIF'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  ls_fieldcat-ref_fieldname = 'SINGLELEVEL_DIF'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MSTDIF'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKI_DOC_ML'.
  ls_fieldcat-ref_fieldname = 'MULTILEVEL_DIF'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'ESTPRD'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MLCD'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'ESTKDM'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MLCD'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MSTPRD'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MLCD'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MSTKDM'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'MLCD'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'WERT'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'KKB_ML_POS'.
  ls_fieldcat-ref_fieldname = 'BEWER'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  if p_all is initial.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'SALK3'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLCR'.
  ls_fieldcat-ctabname = 'T_OUT'.
  ls_fieldcat-cfieldname = 'WAERS'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'CURTP'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLCR'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'WAERS'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLCR'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MENGE'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'KKB_ML_POS'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  if p_all is initial.
    ls_fieldcat-no_out = 'X'.
  endif.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'PBPOPO'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLPP'.
*  ls_fieldcat-seltext_s = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
*  ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
  ls_fieldcat-no_zero = 'X'.
* ls_fieldcat-key = y_x.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'LBKUM'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLPP'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  ls_fieldcat-no_zero = 'X'.
  ls_fieldcat-no_out = 'X'.
  append ls_fieldcat to lt_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-fieldname = 'MEINS'.
  ls_fieldcat-tabname = 'T_OUT'.
  ls_fieldcat-ref_tabname = 'CKMLPP'.
  l_index = l_index + 1.
  ls_fieldcat-col_pos = l_index.
* ls_fieldcat-key = y_x.
  append ls_fieldcat to lt_fieldcat.

* Varianten
  clear ls_variant.
  ls_variant-report = 'ML_VALUE_FLOW_MONITOR'.

* Sortierung
  refresh lt_sort.
  clear: lt_sort, l_index.

  clear ls_sort.
  l_index = l_index + 1.
  ls_sort-spos = l_index.
  ls_sort-fieldname = 'MATNR'.
  ls_sort-tabname = 'T_OUT'.
  ls_sort-up = 'X'.
  append ls_sort to lt_sort.

  clear ls_sort.
  l_index = l_index + 1.
  ls_sort-spos = l_index.
  ls_sort-fieldname = 'BWKEY'.
  ls_sort-tabname = 'T_OUT'.
  ls_sort-up = 'X'.
  append ls_sort to lt_sort.

  clear ls_sort.
  l_index = l_index + 1.
  ls_sort-spos = l_index.
  ls_sort-fieldname = 'BWTAR'.
  ls_sort-tabname = 'T_OUT'.
  ls_sort-up = 'X'.
  append ls_sort to lt_sort.

* Filter nach lokaler W?rung (CURTP = 10)
  refresh: lt_filter.
  clear: ls_filter.
  ls_filter-fieldname = 'CURTP'.
  ls_filter-tabname = 'T_OUT'.
  ls_filter-valuf = '10'.
  ls_filter-valuf_int = '10'.
  ls_filter-sign0 = 'I'.
  ls_filter-optio = 'EQ'.
  append ls_filter to lt_filter.

* Layout
  clear ls_layout.
  ls_layout-f2code = 'DETA'.
  ls_layout-key_hotspot = 'X'.
  ls_layout-colwidth_optimize = 'X'.
  ls_layout-zebra = 'X'.
  ls_layout-info_fieldname = 'COLOR'.

  l_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
*     I_INTERFACE_CHECK                 = ' '
      i_buffer_active                   = 'X'
      i_callback_program                = l_repid
*      i_callback_pf_status_set          = 'ALV_PF_STATUS_SET'
      i_callback_user_command           = 'ALV_USER_COMMAND'
      i_callback_top_of_page            = 'ALV_TOP_OF_LIST'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
      i_structure_name                  = 'T_OUT'
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
      is_layout                         = ls_layout
      it_fieldcat                       = lt_fieldcat
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort                           = lt_sort
      it_filter                         = lt_filter
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
      i_save                            = 'A'
      is_variant                        = ls_variant
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_ADD_FIELDCAT                   =
*     IT_HYPERLINK                      =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    tables
      t_outtab                          = t_out
    exceptions
      program_error                     = 1
      others                            = 2
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


endform.                                                    " ausgabe
*&---------------------------------------------------------------------*
*&      Form  alv_top_of_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alv_top_of_list.

  data: lt_comment type slis_t_listheader,
        ls_comment type slis_listheader,
        l_jahrper type jahrper.

  clear: ls_comment.
  ls_comment-typ = 'H'.
  ls_comment-info = text-005.
  append ls_comment to lt_comment.

  clear: ls_comment.
  ls_comment-typ = 'S'.
  ls_comment-key = text-014.
  concatenate p_bdatj p_poper into l_jahrper.
  write l_jahrper to ls_comment-info.
  append ls_comment to lt_comment.

  if p_all is initial.
    clear: ls_comment.
    ls_comment-typ = 'S'.
    ls_comment-key = text-004.
    if not p_notdis is initial.
      write text-006 to ls_comment-info.
    elseif not p_notinc is initial.
      write text-007 to ls_comment-info.
    endif.
    append ls_comment to lt_comment.
  endif.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           i_logo             = 'ML_LADDERS'
            it_list_commentary = lt_comment.


endform.                    " alv_top_of_list
*&---------------------------------------------------------------------*
*&      Form  get_mlcd_data
*&---------------------------------------------------------------------*
form get_mlcd_data using pt_mats like t_mats[]
                   changing pt_mlcd like t_mlcd[]
                            pt_mlcd_not_alloc like t_mlcd[].

  data: lt_kalnr type ckmv0_matobj_tbl,
        ls_mats type s_mats,
        ls_kalnr type ckmv0_matobj_str.

  refresh: lt_kalnr.
  loop at pt_mats into ls_mats.
    clear: ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    append ls_kalnr to lt_kalnr.
  endloop.
  call function 'CKMCD_MLCD_READ'
      exporting
        i_from_bdatj            = p_bdatj
        i_from_poper            = p_poper
*       I_TO_BDATJ              =
*       I_TO_POPER              =
        i_untper                = s_runperiod-untper
*       I_RUN_ID                =
*       I_NO_BUFFER             =
*       I_REFRESH_BUFFER        =
        i_online                = ' '
*       I_NO_MLCD_CREATE        =
      tables
        it_kalnr                = lt_kalnr
        ot_mlcd                 = pt_mlcd
        ot_mlcd_not_alloc       = pt_mlcd_not_alloc
      exceptions
        data_error              = 1
        others                  = 2
              .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  sort pt_mlcd by kalnr bdatj poper untper categ curtp.
  sort pt_mlcd_not_alloc by kalnr bdatj poper untper curtp.

endform.                    " get_mlcd_data
*&---------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&---------------------------------------------------------------------*
form alv_user_command using r_ucomm like sy-ucomm
                            rs_selfield type slis_selfield.

  data: lt_out type ty_out,
        ls_out type s_out,
        l_kalnr_old type ckmlhd-kalnr,
        l_pbpopo_space type ckmlpp-pbpopo,
        l_counter type i,
        l_answer type c.

  case r_ucomm.
    when 'DELPL'.
      call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
        exporting
          defaultoption        = 'N'
          diagnosetext1        = text-016
          diagnosetext2        = text-017
*         DIAGNOSETEXT3        = ' '
          textline1            = text-018
*         textline2            =
          titel                = text-015
*         START_COLUMN         = 25
*         START_ROW            = 6
          cancel_display       = ' '
        importing
          answer               = l_answer
                .
      if l_answer = 'J'.
        clear: l_pbpopo_space.
        lt_out = t_out.
        sort lt_out by pos_type kalnr.
        read table lt_out with key pos_type = 'NDI'
                          binary search
                          transporting no fields.
        if sy-subrc = 0.
          loop at lt_out into ls_out from sy-tabix.
            if ls_out-pos_type <> 'NDI'.
              exit.
            endif.
            check: ls_out-status < y_abschlussbuchung_erfolgt.
            if ls_out-kalnr <> l_kalnr_old.
              l_kalnr_old = ls_out-kalnr.
              update ckmlpp set pbpopo = l_pbpopo_space
                                status = y_mengen_und_werte_erfasst
                            where kalnr = ls_out-kalnr
                            and   bdatj = ls_out-bdatj
                            and   poper = ls_out-poper
                            and  untper = ls_out-untper.
              l_counter = l_counter + 1.
              if l_counter = 50.
                clear: l_counter.
                commit work.
              endif.
            endif.
          endloop.
        endif.
        commit work.
      endif.
  endcase.


endform.                    " ALV_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
form alv_pf_status_set using rt_extab type slis_t_extab.

  set pf-status 'STANDARD_ALV' excluding rt_extab.

endform.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_material_periods
*&---------------------------------------------------------------------*
form get_material_periods using pt_mats like t_mats[]
                                pt_ckmlpp like t_ckmlpp[]
                                pt_ckmlcr like t_ckmlcr[].

  data: lt_kalnr type ckmv0_matobj_tbl,
        ls_mats type s_mats,
        ls_kalnr type ckmv0_matobj_str.

  if pt_mats[] is initial.
    refresh: pt_ckmlpp, pt_ckmlcr.
    exit.
  endif.
* Periodens?ze lesen
  refresh: lt_kalnr.
  loop at pt_mats into ls_mats.
    clear: ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    append ls_kalnr to lt_kalnr.
  endloop.
  call function 'CKMS_PERIOD_READ_WITH_ITAB'
    exporting
*     I_REFRESH_BUFFER                =
*     I_READ_ONLY_BUFFER              = ' '
*     I_USE_BUFFER                    = 'X'
*     I_BUILD_SMBEW                   =
      i_bdatj_1                       = p_bdatj
      i_poper_1                       = p_poper
*     I_BDATJ_2                       =
*     I_POPER_2                       =
*     I_BDATJ_3                       =
*     I_POPER_3                       =
*     I_BETWEEN_1_AND_2               =
      i_untper                        = s_runperiod-untper
      i_call_by_reporting             = 'X'
      i_no_chk_periods_complete       = 'X'
    tables
      t_kalnr                         = lt_kalnr
      t_ckmlpp                        = pt_ckmlpp
      t_ckmlcr                        = pt_ckmlcr
*     T_MISS_CKMLPP                   =
*     T_MISS_CKMLCR                   =
    exceptions
      no_data_found                   = 1
      input_data_inconsistent         = 2
      buffer_inconsistent             = 3
      others                          = 4.
  if sy-subrc <> 0 and
     not ( sy-subrc = 1 and
           not ( pt_ckmlpp[] is initial and pt_ckmlpp[] is initial ) ).
*   Probleme
    refresh: pt_mats, pt_ckmlpp, pt_ckmlcr.
    exit.
  endif.
  sort: pt_ckmlpp, pt_ckmlcr.

endform.                    " get_material_periods
*>>>> END OF INSERTION <<<<<<
