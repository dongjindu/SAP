*&---------------------------------------------------------------------*
*& Report  ZSAPBF_CPZP_CORRECTION_MASSIVE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

INCLUDE zsapbf_cpzp_correction_masstop.
INCLUDE zsapbf_cpzp_correction_masssel.
INCLUDE zsapbf_cpzp_correction_massf01.
INCLUDE zsapbf_cpzp_correction_massf02.

INITIALIZATION.
  PERFORM intial_screen.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  PERFORM screen_check.


*&---------------------------------------------------------------------*
*& Start of selection                                                  *
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  DATA: lt_aufnr TYPE tt_aufnr.
  DATA: ls_parallel TYPE ppc_parallel.
*  DATA: lt_aufnr_task TYPE tt_aufnr_task.

* Add by SAPCD10 on 2010.07.09
***** Start *****
  CHECK gv_ucomm = 'ONLI' OR gv_ucomm = 'SPOS' OR
        gv_ucomm = 'SJOB' OR gv_ucomm = 'PRIN'.
***** End   *****

** HMMA Tuning start
*  PERFORM determin_read_ippe IN PROGRAM zsapbf_cpzp_correction_report.
  PERFORM determin_read_ippe IN PROGRAM zsapbf_cpzp_correction_report2.
** HMMA End
  IF p_backup = 'X'.
* Backup CPZP table
    PERFORM cpzp_backup USING p_year
                              p_month
                              p_sel
*                              p_all
                              p_full.

  ELSE.
* Correct CPZP
* Commented by SAPCD10 on2010.07.08
*    PERFORM get_pcc CHANGING lt_aufnr.
    IF p_par IS INITIAL.
* for sequency update
      PERFORM sequential USING p_year
                               p_month
                               p_test
                               p_error
                               p_updcur
*                               lt_aufnr
                      CHANGING gt_aufnr_task
                               .
*      PERFORM parallelization_test_run USING lt_aufnr
*                                             ls_parallel
*                                    CHANGING gt_aufnr_task
*                                    .

    ELSE.
* for parallelization update

      PERFORM parallel_fill USING ls_parallel
                                  .

      PERFORM parallelization USING ls_parallel
*                                    lt_aufnr
*                           CHANGING gt_aufnr_task
                                    .
    ENDIF.
    IF p_run = 'X'.
      PERFORM output_result USING gt_aufnr_task.
    ENDIF.

  ENDIF.
