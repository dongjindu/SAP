FUNCTION Z_CO_GET_WIP_DETAILS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IF_PERIOD) TYPE  CO_PERIO
*"     VALUE(IF_GJAHR) TYPE  GJAHR
*"     REFERENCE(IT_OBJNR_LIST) TYPE  QRP_T_OBJNR_LIST
*"     VALUE(IF_SELECT_WIP) TYPE  C DEFAULT 'X'
*"     VALUE(IF_SELECT_SCRAP) TYPE  C DEFAULT 'X'
*"     VALUE(IF_PREFETCH_MAT) TYPE  C DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(ET_QUANTITY_TABLE) TYPE  Z_QRP_T_QUANTITIES
*"  EXCEPTIONS
*"      WRONG_INPUT
*"----------------------------------------------------------------------
  data: lf_gjper type cpzp-gjper,
          lf_gjper_read type cpzp-gjper,
          ls_objnr_list type qrp_objnr_list,
          ls_quantities type ZQRP_QUANTITIES,
          l_write_quantities type c.                        "DI46C2

  data: lt_cpzp like cpzp occurs 0 with header line,
        lt_kaln1 type ckml_t_inkalnr.


  if     if_select_wip is initial
     and if_select_scrap is initial.
    exit.
  endif.

  if    if_period is initial
     or if_gjahr is initial.
    raise wrong_input.
    exit.
*   Geben Sie Geschäftsjahr und Periode ein
  endif.

* Geschäftsjahr/Periode umrechnen
* determine GJPER from year and period
  perform get_gjper_from_gj_per using if_gjahr
                                      if_period
                                changing lf_gjper.

  loop at it_objnr_list into ls_objnr_list.
* Zählpunkttabelle zu Kontierungsobjekt (Produktkostensammler) lesen
* read table CPZP for PCC
    call function 'QRP_APO_CPZP_READ'
         EXPORTING
              if_objnr = ls_objnr_list-objnr
              if_gjper = lf_gjper
              if_werks = ls_objnr_list-plant
         IMPORTING
              ef_gjper = lf_gjper_read
         TABLES
              et_cpzp  = lt_cpzp
         EXCEPTIONS
              others   = 1.
    if sy-subrc is initial.
      if lf_gjper_read le lf_gjper.
* if component data are in future period, they are ignored
        loop at lt_cpzp.
          clear ls_quantities.                              "DI46C2
          if lt_cpzp-f_objnr(2) eq con_obj_vs.              "DI46C2
*------------------------------ Komponente -----------------------------
*------------------------------- Component -----------------------------
* Objektnummer der Komponente entschlüsseln
* decode component's object key
            call function 'QRP_APO_COMP_OBJNR_DECODE'
                 EXPORTING
                      if_f_objnr = lt_cpzp-f_objnr
                 IMPORTING
                      ef_kaln1   = ls_quantities-kaln1
                      ef_kzbws   = ls_quantities-kzbws
                      ef_sobkz   = ls_quantities-sobkz
                 EXCEPTIONS
                      others     = 1.
            if sy-subrc is initial.
              l_write_quantities = con_x.
              append ls_quantities-kaln1 to lt_kaln1.
            endif.
          else.                                             "DI46C2
*-------------------------------- Leistung -----------------------------
*-------------------------------- Activity -----------------------------
            l_write_quantities = con_x.                     "DI46C2
            ls_quantities-act_objnr = lt_cpzp-f_objnr.      "DI46C2
          endif.                                            "DI46C2
* Mengen in Ausgabetabelle schreiben
* write quantities
          if not l_write_quantities is initial.             "DI46C2
            ls_quantities-objnr = lt_cpzp-objnr.
            ls_quantities-wip_quantity = lt_cpzp-istmn - lt_cpzp-gmsum.
            ls_quantities-PREV_WIP     = lt_CPZP-ISTMN - Lt_CPZP-GMPER.
            ls_quantities-CURR_WIP     = lt_CPZP-ISTMN - lT_CPZP-GMSUM.

* WIP auch aus Vorperiode relevant
            if lf_gjper_read eq lf_gjper.
* Die folgenden Mengen sind nur in der laufenden Periode relevant
* The following quantities are only relevant in current period
              ls_quantities-actual_scrap = lt_cpzp-xmper.
              ls_quantities-planned_scrap = lt_cpzp-xmsum.  "DI46C2
              ls_quantities-variance_qty = lt_cpzp-varmn.   "DI46C2
              ls_quantities-actual_qty_stpc = lt_cpzp-gmper."DI46C2
              ls_quantities-target_qty = lt_cpzp-gmsum      "DI46C2
                                          - lt_cpzp-xmper.  "DI46C2
* Begin of changes - Manju
*              ls_quantities-PREV_WIP = lt_CPZP-ISTMN - Lt_CPZP-GMPER.
*              ls_quantities-CURR_WIP = lt_CPZP-ISTMN - lT_CPZP-GMSUM.
              ls_quantities-APO_INPUT_QTY = lt_cpzp-gmper.
              ls_quantities-APO_OUTPUT_QTY = lt_cpzp-gmSUM.
* End of changes - Manju
              if lt_cpzp-zaehl eq con_key_fix.              "DI46C2
                ls_quantities-flg_lsi = con_x.              "DI46C2
              else.                                         "DI46C2
                clear ls_quantities-flg_lsi.                "DI46C2
              endif.                                        "DI46C2
            endif.
            ls_quantities-unit = lt_cpzp-meinh.
            if ( not if_select_wip is initial
             and not ls_quantities-wip_quantity is initial )
            or
            ( not if_select_scrap is initial
              and ( not ls_quantities-actual_scrap is initial
                   or  not ls_quantities-planned_scrap is initial
                   or  not ls_quantities-variance_qty is initial
                   or  not ls_quantities-actual_qty_stpc is initial
                   or  not ls_quantities-target_qty is initial )
            ).
              append ls_quantities to et_quantity_table.
            endif.
          endif.
        endloop.
        if not if_prefetch_mat is initial.
* Materialtabellen vorselektieren für spätere Bewertung
* prefetch on material tables for valuation
          call method cl_wrap_material_ck=>prefetch_with_kaln1
               exporting
                  costing_numbers = lt_kaln1.
        endif.
      endif.
    endif.


  endloop.

ENDFUNCTION.
