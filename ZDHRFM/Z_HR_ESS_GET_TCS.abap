FUNCTION z_hr_ess_get_tcs .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PERNR) TYPE  PERNR_D
*"     VALUE(BEGDA) TYPE  BEGDA
*"     VALUE(ENDDA) TYPE  ENDDA
*"  EXPORTING
*"     VALUE(TOT_DIRECT) TYPE  CMP_TCS_DATA_AMOUNT
*"     VALUE(TOT_INDIRECT) TYPE  CMP_TCS_DATA_AMOUNT
*"     VALUE(TOT_OTHER) TYPE  CMP_TCS_DATA_AMOUNT
*"     VALUE(TOT_ALL) TYPE  CMP_TCS_DATA_AMOUNT
*"     VALUE(PER_DIRECT) TYPE  NUMC2
*"     VALUE(PER_INDIRECT) TYPE  NUMC2
*"     VALUE(PER_OTHER) TYPE  NUMC2
*"  TABLES
*"      TCS_DATA_TAB TYPE  HRCM_TCS_DATA_TAB
*"      TCS_MASTERDATA_TAB TYPE  HRCM_TCS_MASTERDATA_TAB
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 10/24/2012 Valerian   UD1K955704  Initial Program Development
* 02/06/2013 Valerian   UD1K956627  Get additional data from function
*                                   module HRCM_TCS_MASTERDATA_READ
* 04/04/2013 Valerian   UD1K956864  Correct Typo Error in Subcategory
* 04/30/2013 Valerian   UD1K956976  Remove decimal points in percentage
*-----------------------------------------------------------------------

  DATA: it_pernr_tab TYPE hrcm_tcs_pernr_tab,
        wa_pernr_tab TYPE hrcm_tcs_pernr,
        it_tcs_data  TYPE hrcm_tcs_data_tab,
        wa_tcs_data  TYPE hrcm_tcs_data,

        it_pernr      TYPE hrcm_pernr_tab,                  "UD1K956627
        wa_pernr      TYPE hrcm_pernr,                      "UD1K956627
        it_masterdata TYPE hrcm_tcs_masterdata_tab.         "UD1K956627

* Refresh data
  CLEAR: tot_direct, tot_indirect, tot_other, tot_all,
         per_direct, per_indirect, per_other.

* Assign data
  wa_pernr_tab-pernr = pernr.
  wa_pernr_tab-molga = '10'.
  APPEND wa_pernr_tab TO it_pernr_tab.

* BEGIN OF UD1K956627
  wa_pernr-pernr = pernr.
  APPEND wa_pernr TO it_pernr.
  CALL FUNCTION 'HRCM_TCS_MASTERDATA_READ'
    EXPORTING
      pernr_tab          = it_pernr
      keyda              = sy-datum
    IMPORTING
      tcs_masterdata_tab = it_masterdata.

  tcs_masterdata_tab[] = it_masterdata[].

* Get TM's Name
*  SELECT ename INTO ename
*    FROM pa0001
*   UP TO 1 ROWS
*   WHERE pernr = pernr
*     AND endda >= sy-datum
*     AND begda <= sy-datum.
*  ENDSELECT.
* END OF UD1K956627

* Read Compensation Data
  CALL FUNCTION 'HRCM_TCS_DATA_READ'
    EXPORTING
      pernr_tab    = it_pernr_tab
      begda        = begda
      endda        = endda
    IMPORTING
      tcs_data_tab = it_tcs_data.

* Delete zero amount
  DELETE it_tcs_data WHERE amount = 0.

* Calculate Total Direct Cost
  LOOP AT it_tcs_data INTO wa_tcs_data
                     WHERE subcateg = 'ZSL' OR
                           subcateg = 'ZOV' OR
                           subcateg = 'ZSH' OR
                           subcateg = 'ZTI' OR
                           subcateg = 'ZBN' OR
** By Furong on 04/05/14 *
                           subcateg = 'ZHA' OR
                           subcateg = 'ZAA' OR
                           subcateg = 'ZHO' OR
                           subcateg = 'ZDP' OR
                           subcateg = 'ZSU' OR
                           subcateg = 'ZCP' OR
                           subcateg = 'ZBO' OR
                           subcateg = 'ZMR' OR
                           subcateg = 'ZGA' OR
                           subcateg = 'ZIS' OR
*                           subcateg = 'ZOB' OR
** (
                           subcateg = 'ZTP' OR
                           subcateg = 'ZSV'.

    tot_direct = tot_direct + wa_tcs_data-amount.
  ENDLOOP.

* Calculate Total Indirect Cost
  LOOP AT it_tcs_data INTO wa_tcs_data
                     WHERE subcateg = 'ZME' OR
                           subcateg = 'ZVI' OR
                           subcateg = 'ZAD' OR
                           subcateg = 'ZBL' OR
                           subcateg = 'ZST' OR
                           subcateg = 'ZLT' OR
                           subcateg = 'ZRE' OR
                           subcateg = 'ZSS' OR
                           subcateg = 'ZMC' OR
                           subcateg = 'ZUI'.

    tot_indirect = tot_indirect + wa_tcs_data-amount.
  ENDLOOP.

* Calculate Total Other Cost
  LOOP AT it_tcs_data INTO wa_tcs_data
                     WHERE subcateg = 'ZTU' OR
                           subcateg = 'ZCR' OR
*                          subcateg = 'ZCA' OR              "UD1K956864
                           subcateg = 'ZRL'.

    tot_other = tot_other + wa_tcs_data-amount.
  ENDLOOP.

* Calculate Total
  tot_all = tot_direct + tot_indirect + tot_other.

* Calculate Percentage
  per_indirect = tot_indirect / tot_all * 100.
  per_other    = tot_other / tot_all * 100.
  per_direct   = 100 - per_indirect - per_other.

* Transfer data to output structure
  tcs_data_tab[] = it_tcs_data[].

ENDFUNCTION.
