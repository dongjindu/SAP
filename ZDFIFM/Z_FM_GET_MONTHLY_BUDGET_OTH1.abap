FUNCTION Z_FM_GET_MONTHLY_BUDGET_OTH1.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_FIKRS) LIKE  FMCI-FIKRS
*"     REFERENCE(I_GJAHR) LIKE  FMCI-GJAHR
*"  TABLES
*"      T_GEBER STRUCTURE  ZSFM0001 OPTIONAL
*"      T_FICTR STRUCTURE  ZSFM0002 OPTIONAL
*"      T_FIPEX STRUCTURE  ZSFM0003 OPTIONAL
*"      T_PROFIL STRUCTURE  ZSFM0004 OPTIONAL
*"      T_POTYP STRUCTURE  ZSFM0005 OPTIONAL
*"      T_ITAB STRUCTURE  ZSFM0008
*"  EXCEPTIONS
*"      NO_FM_AREA
*"      NO_FUND
*"      NO_FUNDS_CENTER
*"      NO_COMMITMENT_ITEM
*"      NO_PROFILE
*"      NO_CATEGORY
*"      NO_ORIGINAL
*"      NO_DATA_FOUND
*"----------------------------------------------------------------------

*---// Initialization
  CLEAR: v_fikrs, v_gjahr,
         r_geber, r_fictr, r_fipex, r_profil, r_potyp, it_budget.
  REFRESH: r_geber, r_fictr, r_fipex, r_profil, r_potyp, it_budget.

  MOVE: i_fikrs TO v_fikrs,
        i_gjahr TO v_gjahr.

  r_geber[] = t_geber[]. r_fictr[]  = t_fictr[].
  r_fipex[] = t_fipex[]. r_profil[] = t_profil[].
  r_potyp[] = t_potyp[].

  PERFORM check_rtn.
  PERFORM get_budget_oth1.
  PERFORM get_actual_oth1.
  PERFORM get_available_oth1.
  PERFORM set_blank_category_oth1.
  PERFORM set_text_n_total_oth1.

  t_itab[] = it_budget[].
*> 2005/12/13 add.
  read table t_itab index 1.
  if sy-subrc <> 0.
    raise no_data_found.
  endif.
*<




ENDFUNCTION.
