FUNCTION zsapbf_cpzp_correct_single.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_YEAR) TYPE  GJAHR DEFAULT SY-DATUM(4)
*"     VALUE(IV_MONTH) TYPE  MONAT DEFAULT SY-DATUM+4(2)
*"     VALUE(IV_AUFNR) TYPE  AUFNR
*"     VALUE(IV_TEST) TYPE  FLAG DEFAULT 'X'
*"     VALUE(IV_ERROR) TYPE  FLAG
*"     VALUE(IV_UPDCUR) TYPE  FLAG OPTIONAL
*"     VALUE(IS_PARALLEL) TYPE  PPC_PARALLEL OPTIONAL
*"     VALUE(IV_WPSSUB) TYPE  INT4 DEFAULT 0
*"  EXPORTING
*"     VALUE(EV_UPDATE_ERROR) TYPE  CHAR1
*"     VALUE(EV_AUFNR) TYPE  AUFNR
*"  TABLES
*"      ET_CPZP TYPE  ZSAPBF_TT_CPZP OPTIONAL
*"      ET_CPZP_BK TYPE  ZSAPBF_TT_CPZP OPTIONAL
*"----------------------------------------------------------------------
  DATA: lt_cpzp TYPE zsapbf_tt_cpzp,
        lt_cpzp_backup TYPE zsapbf_tt_cpzp.

  DATA: lv_error_flag TYPE flag.

** HMMA Tuning
*  PERFORM programm IN PROGRAM zsapbf_cpzp_correction_report
    PERFORM programm IN PROGRAM zsapbf_cpzp_correction_report2
** Tuning End
                   USING iv_year
                         iv_month
                         iv_aufnr
                         iv_test
                         iv_updcur
                         is_parallel "Added by James Sung-Kon Kim 2011.03.02
                         iv_wpssub   "Added by James Sung-Kon Kim 2011.03.02
                CHANGING ev_update_error
                         lt_cpzp
                         lt_cpzp_backup
                         lv_error_flag.
  ev_aufnr = iv_aufnr.

******* Start; Commentated by James Sung-Kon Kim 2011.03.02
*  IF iv_test IS NOT INITIAL.
*    APPEND LINES OF lt_cpzp TO gt_cpzp.
*    APPEND LINES OF lt_cpzp_backup TO gt_cpzp_backup.
*  ENDIF.
*
*  et_cpzp[] = lt_cpzp[].
*  et_cpzp_bk[] = lt_cpzp_backup[].
******* End; Commentated by James Sung-Kon Kim 2011.03.02

******* Start; Added by James Sung-Kon Kim 2011.03.02
  IF iv_test IS NOT INITIAL.
    IF is_parallel IS INITIAL.
      " Because, When Sequential Processing, there is receiving processing for the et_cpzp / et_cpzp_bk
      APPEND LINES OF lt_cpzp TO gt_cpzp.
      APPEND LINES OF lt_cpzp_backup TO gt_cpzp_backup.
    ELSE.
      et_cpzp[] = lt_cpzp[].
      et_cpzp_bk[] = lt_cpzp_backup[].
    ENDIF.
  ENDIF.
******* End; Added by James Sung-Kon Kim 2011.03.02


**&---------------------------------------------------------------------*
**& fill ALV-structure and display                                      *
**&---------------------------------------------------------------------*
*    IF iv_test IS NOT INITIAL.
*      PERFORM alv_cpzp_output IN PROGRAM  zsapbf_cpzp_correction_report
*                              USING iv_error
*                                    lt_cpzp
*                                    lt_cpzp_backup
*                                    .
*    ENDIF.

ENDFUNCTION.
