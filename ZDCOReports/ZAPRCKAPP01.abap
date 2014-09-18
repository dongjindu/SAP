*&---------------------------------------------------------------------*
*& Report  SAPRCKAPP01                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*& liest Daten für die MiniApp 'neue zu kalkulierende Materialien' und *
*& legt diese Daten als Extrakt ab                                     *
*&---------------------------------------------------------------------*

* 4.6C SP
* P9CK084388 19102000 note 351276
* P9CK079392 05102000 note 337584

report  saprckapp01.
*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments PA8 20020823
initialization.
  data: mgv_matnr_prog like rsvar-report,
        mgv_matnr_selopt_tab like rsldbdfs occurs 0 with header line.
  field-symbols <mgv_matnr_selopt_conv> type standard table.
  mgv_matnr_prog = sy-repid.
  mgv_matnr_selopt_tab-name = 'CK_MATNR' .
  append mgv_matnr_selopt_tab.
  call function 'MGV_SELOP_AFTER_INITIALIZATION'
       exporting
            program        = mgv_matnr_prog
       tables
            selop          = mgv_matnr_selopt_tab
       exceptions
            no_programname = 1
            others         = 2.

start-of-selection.
  loop at mgv_matnr_selopt_tab.
    concatenate mgv_matnr_selopt_tab-name'[]' into
    mgv_matnr_selopt_tab-name.
    assign (mgv_matnr_selopt_tab-name) to <mgv_matnr_selopt_conv>.
    if sy-subrc is initial.
      call function 'MGV_SELOP_AFTER_START_OF_SEL'
           exporting
                selopt_name = mgv_matnr_selopt_tab-name
           tables
                range       = <mgv_matnr_selopt_conv>.
    endif.
  endloop.
*{SEL-OPT End}

  tables: mara, mackw.

  data: t_mtart like t134 occurs 0 with header line,
        t_mat_to_cost type ckapp_mat_to_cost occurs 0 with header line,
        t_tck03 like tck03 occurs 0 with header line,
        t_mastb type mastb occurs 0 with header line,
        t_stkob type stkob occurs 0 with header line,
        t_stzub type stzub occurs 0 with header line,
        f_tck19 like tck19,
        f_mtcom like mtcom,
        f_mtcor like mtcor,
        f_mack4 like mack4,
        f_mack3 like mack3,
        k_beskz like t460a-beskz,
        k_losgr like mastb-losvn,
        k_psttr like plaf-psttr,
        k_sel   like sy-index,
        k_cut_off type c.

  data: begin of t_dummy occurs 0,
          dummy,
        end of t_dummy.

  selection-screen skip 1.
  parameters: x_bom type ckapp_bom as checkbox,
              x_wqs type ckapp_wqs as checkbox,
              x_plo type ckapp_plo as checkbox,
              p_new as checkbox,
              p_year type BDATJ default sy-datum(4) memory id BDTJ.

*----------------------------------------------------------*
  selection-screen skip 1.
  selection-screen begin of block extract with frame title text-001.
  parameters p_save type ckapp_save as checkbox default ' '.
  parameters p_user like sy-uname default sy-uname.
  parameters p_batch like sy-batch no-display.
  parameters p_maxsel type tbmaxsel default 100.
  selection-screen end of block extract.
*----------------------------------------------------------*

at selection-screen.

start-of-selection.
* store internally the material types that are allowed to be costed
  refresh t_mtart.
  clear t_mtart.
  refresh t_mat_to_cost.
  clear t_mat_to_cost.

  select * from t134 into t_mtart.
    check t_mtart-pstat ca 'G'.
    append t_mtart.
  endselect.


get mara fields matnr mtart pstat.                         "note 351276
* select only materials that can have a costing view
  read table t_mtart with key mtart = mara-mtart.
  check sy-subrc = 0.


get mackw.
  check select-options.
  f_mtcom-matnr = mackw-matnr.
  f_mtcom-werks = mackw-werks.

* don't select configurable materials                      "note 337584
  check mackw-kzkfg is initial or                          "note 337584
    not mackw-cuobj is initial.                            "note 337584

* select only materials which are allowed to be costed
  check mackw-ncost is initial.


  if not x_wqs is initial.                                  "note526792
    check mara-pstat ca 'G'.                                "note526792
  endif.                                                    "note526792

  if mara-pstat ca 'G'.
    f_mtcom-kenng = 'MACK4'.
    call function 'MATERIAL_READ_MAKA'
         exporting
              schluessel = f_mtcom
         importing
              matdaten   = f_mack4
         tables
              seqmat01   = t_dummy
         exceptions
              others     = 8.

    check sy-subrc = 0.                                     "note526792

    if sy-subrc = 0.
*     select only materials without current and marked cost estimate

*-----ANDY start
* MBEW
      if p_new = 'X'.
        check f_mack4-pprdl is initial and
              f_mack4-pprdz is initial.
      endif.

      check f_mack4-pdatl <> p_year or f_mack4-EKALR = space.

*-----ANDY end

*     if desired, select only material marked 'WQS'
      if not x_wqs is initial.
        check not f_mack4-ekalr is initial.
      endif.
    endif.
  endif.

  if not x_bom is initial.
*   determine procurement type
    f_mtcom-kenng = 'MACK3'.
    call function 'MATERIAL_READ_MAKA'
         exporting
              schluessel = f_mtcom
         importing
              matdaten   = f_mack3
         tables
              seqmat01   = t_dummy
         exceptions
              others     = 8.

    check sy-subrc = 0.                                     "note526792

    if sy-subrc = 0.
      clear k_beskz.
      call function 'CK_F_READ_SPECIAL_PROCUREMENT'
           exporting
                dispo_sonderbeschaffung = f_mack3-sobsl
                kalk_sonderbeschaffung  = f_mack3-sobsk
                werk                    = mackw-werks
           importing
                sonderbeschaffung       = k_beskz
           exceptions
                others                  = 2.

      if k_beskz is initial.
        k_beskz = f_mack3-beskz.
      endif.
    endif.

    check k_beskz = 'E' or k_beskz = 'X'.                   "note526792

    if k_beskz = 'E' or k_beskz = 'X'.
*     check existence of a suitable bom
      call function 'CK_F_GET_CURR_STD_KLVAR'
           exporting
                werks_imp = mackw-werks
           tables
                t_tck03   = t_tck03.

      read table t_tck03 index 1.
      check sy-subrc = 0.

      call function 'CK_F_TCK03_HIERARCHY_READING'
           exporting
                p_klvar = t_tck03-klvar
                p_werks = mackw-werks
           importing
                f_tck19 = f_tck19.

      k_losgr = f_mack3-losgr.                             "note 392005

      call function 'CS_ALT_SELECT_MAT'
           exporting
                capid    = f_tck19-capid
                losgr    = k_losgr
                datuv    = sy-datlo
                matnr    = mackw-matnr
                werks    = mackw-werks
           tables
                mastb_wa = t_mastb
                stkob_wa = t_stkob
                stzub_wa = t_stzub
           exceptions
                others   = 7.

      check sy-subrc = 0.

    endif.
  endif.

* User-Exit reduces further the set of materials

  if not x_plo is initial.
*   determine the planned order which is the nearest in future
    clear k_psttr.
    select min( psttr ) into k_psttr from plaf
                                     where matnr = mackw-matnr
                                     and   plwrk = mackw-werks.

    if sy-subrc = 0 and not k_psttr is initial.
      t_mat_to_cost-psttr = k_psttr.
    else.
      clear t_mat_to_cost.
    endif.
  endif.

  t_mat_to_cost-matnr = mackw-matnr.
  t_mat_to_cost-werks = mackw-werks.
  append t_mat_to_cost.
  add 1 to k_sel.

* Stop selection at maximum number of materials
  if not p_maxsel is initial and
     k_sel ge p_maxsel.
    k_cut_off = 'X'.
    stop.
  endif.


end-of-selection.

  if not x_plo is initial.
*   sort the materials by the date of probable first production
    t_mat_to_cost-psttr = '99991231'.
    modify t_mat_to_cost transporting psttr where psttr is initial.
    sort t_mat_to_cost by psttr matnr werks.
    clear t_mat_to_cost-psttr.
    modify t_mat_to_cost transporting psttr where psttr = '99991231'.
  endif.

  if not p_save is initial.
*   save result as extract
    data l_repid like sy-repid.
    l_repid = sy-repid.
    call function 'CKAPP_EXTRACT_SAVE'
         exporting
              i_report    = l_repid
              i_variant   = sy-slset
              i_user      = p_user
              i_tabname   = 'CKAPP_T_MAT_TO_COST'
              i_strucname = 'CKAPP_MAT_TO_COST'
              i_cut_off   = k_cut_off
         tables
              it_table    = t_mat_to_cost.

  endif.

  if sy-batch is initial and p_batch is initial.
*   if online, display result immediately
    call function 'REUSE_ALV_GRID_DISPLAY'
         exporting
              i_structure_name = 'CKAPP_MAT_TO_COST'
              i_default        = ''
         tables
              t_outtab         = t_mat_to_cost.
  endif.
