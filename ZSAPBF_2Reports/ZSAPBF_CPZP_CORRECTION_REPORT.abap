*&---------------------------------------------------------------------*
*& Report  ZSAPBF_CPZP_CORRECTION_REPORT                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zsapbf_cpzp_correction_report MESSAGE-ID zsapbf_qrprp.

INCLUDE zsapbf_cpzp_correction_reptop.
INCLUDE zsapbf_cpzp_correction_repsel.
INCLUDE zsapbf_cpzp_correction_repf01.
INCLUDE zsapbf_cpzp_correction_repf02.
INCLUDE zsapbf_cpzp_correction_repf03.
INCLUDE zsapbf_cpzp_correction_repf04.


* procnr must not be ready for input
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.


AT SELECTION-SCREEN.
  PERFORM screen_check.

INITIALIZATION.
  PERFORM initial_screen.

*&---------------------------------------------------------------------*
*& Start of selection                                                  *
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lv_update_error TYPE char1,
        lt_cpzp TYPE zsapbf_tt_cpzp,
        lt_cpzp_backup TYPE zsapbf_tt_cpzp.
*        lv_current TYPE flag.
  DATA: lv_error_flag TYPE flag.

******** Start; Parallelization Functionality; Added by James Sung-Kon Kim 2011.03.02
  DATA: ls_parallel TYPE ppc_parallel,
        l_free_wps  TYPE i,
        l_par_process TYPE int4 . "Added by James Sung-Kon Kim 2011.03.02
******** End; Parallelization Functionality; Added by James Sung-Kon Kim 2011.03.02

  PERFORM determin_read_ippe.

******** Start; Parallelization Functionality; Added by James Sung-Kon Kim 2011.03.02
  PERFORM parallel_fill CHANGING ls_parallel.

  PERFORM check_parallel_servergroup CHANGING l_free_wps. "Do Perform, Again.

  l_par_process = p_wps * p_wpssub . "Added by James Sung-Kon Kim 2011.03.02

*  IF l_free_wps <  p_wps. "Disabled by James Sung-Kon Kim 2011.03.02
  IF l_free_wps <  l_par_process. "Added by James Sung-Kon Kim 2011.03.02

    MESSAGE s011 WITH l_free_wps l_par_process DISPLAY LIKE 'E'.
*    EXIT. "Disabled by James Sung-Kon Kim 2011.03.02
*    STOP. "Added by James Sung-Kon Kim 2011.03.02
    EXIT.

  ENDIF.
******** End; Parallelization Functionality; Added by James Sung-Kon Kim 2011.03.02

  PERFORM programm USING p_year
                         p_month
                         p_aufnr
                         p_test
                         p_updcur
                         ls_parallel "Added by James Sung-Kon Kim 2011.03.02
                         p_wpssub    "Added by James Sung-Kon Kim 2011.03.02
                CHANGING lv_update_error
                         lt_cpzp
                         lt_cpzp_backup
                         lv_error_flag.

***Start; Commentated by Sung-Kon James Kim 2011/01/27
*  IF lv_error_flag = 'X'.
*    EXIT.
*  ENDIF.
***End ; Commentated by Sung-Kon James Kim 2011/01/27

*&---------------------------------------------------------------------*
*& fill ALV-structure and display                                      *
*&---------------------------------------------------------------------*
  IF p_test IS NOT INITIAL.

***Start; Added by Sung-Kon James Kim 2011/01/27
    IF lv_error_flag = 'X'.

      IF lv_update_error = 'E'. "No data for the posting period (Successful)
        MESSAGE i407 WITH p_aufnr.
        EXIT.
      ELSE.
        EXIT.
      ENDIF.

    ELSE.
***End ; Added by Sung-Kon James Kim 2011/01/27

      PERFORM alv_cpzp_output USING p_error
                                    lt_cpzp
                                    0
                           CHANGING lt_cpzp_backup
                                    .
    ENDIF. "Added by Sung-Kon James Kim 2011/01/27


  ELSE.

***Start; Commentated by Sung-Kon James Kim 2011/01/27
*    IF lv_update_error IS INITIAL.
*      MESSAGE i411 WITH p_aufnr.
*    ELSE.
*      MESSAGE i412 WITH p_aufnr.
*    ENDIF.
***End ; Commentated by Sung-Kon James Kim 2011/01/27

***Start; Added by Sung-Kon James Kim 2011/01/27
    CASE lv_update_error.
      WHEN 'P'.                     "There is no difference (Successful)
        MESSAGE i409 WITH p_aufnr.
      WHEN 'E'.                     "No data for the posting period (Successful)
        MESSAGE i407 WITH p_aufnr.
      WHEN ' '.                     "Update Successful (Successful)
        MESSAGE i411 WITH p_aufnr.
      WHEN OTHERS.
        MESSAGE i412 WITH p_aufnr.  "Some Error
    ENDCASE.
***Start; Added by Sung-Kon James Kim 2011/01/27

  ENDIF.
