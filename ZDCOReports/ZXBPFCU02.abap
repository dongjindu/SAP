*----------------------------------------------------------------------*
*   INCLUDE ZXBPFCU02                                                  *
*----------------------------------------------------------------------*
*I_APPLIK	        Application
*I_VALUE_TYPE	        Assigned-value type
*I_ACTIVITY	        Posting transaction
*I_FISCAL_YEAR   Fiscal year
*I_USER	        User name
*I_OBJNR          Object number
*I_POSIT          Position
*I_FUND	        Fund
*I_DISTRIBUTABLE   Distributable budget / release
*I_ASSIGNED	        Assigned budget / release (entered amount)
*I_ORG_LEVEL          Original action in the availability control
*
*E_CUST_LEVEL	        New action for the availability control
*E_MAX_DISTRIBUTABLE  Maximum budget / release that can be distributed


*To set up availability control activity as well as various tolerance
*limits, ensure that the maximum budget to be distributed is
*recalculated, and, if necessary, ensure that the maximum budget release
*for the BS Elements (with budget) is recalculated.
*

*By Andy Choi.... for HMMA

*// 2011.08.29 insert by YN.Kim
data: lv_FAREA  LIKE   BPJA-FAREA.
clear: lv_FAREA.
*// ============================== //*

*FM only
CHECK i_applik     = 'M'   "Funds management
  AND i_value_type = '43'. "payment budget

*No rollup checking... in Standard SAP.

*FSH201.... Top position & No error
IF i_objnr+6(4) = 'HMMA' AND i_org_level = '0'.
  DATA: l_profile  TYPE bp_bprofil.
  DATA: l_objnr LIKE fmfctr-ctr_objnr.

* Use budget profile from V_TBPFM_1
  l_objnr = i_objnr.
  l_objnr+6 = '*'.  "not working with space.

* Profile from TBPFM table.
  clear l_profile.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
       EXPORTING
            i_objnr         = l_objnr
            i_posit         = i_posit
            i_geber         = i_fund
            i_gjahr         = i_fiscal_year
* add insert.
            i_farea         = lv_farea
       IMPORTING
            e_profil        = l_profile
       EXCEPTIONS
            no_profil_found = 01.

*  IF NOT sy-subrc IS INITIAL.
*    CALL FUNCTION 'FM5B_GET_PROFILE'
*  ENDIF.

  IF l_profile CA 'YHQM'.  "year/half/q/month
*1  Warning
*2  Warning with MAIL to person responsible
*3  Error message
    e_cust_level = '3'.
    e_max_distributable = 0.
  ENDIF.
ENDIF.
