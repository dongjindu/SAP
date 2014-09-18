************************************************************************
* Program Name      : ZEMM_CHANGE_PR_CLOSED
* Creation Date     : 10/14/2013
* Development Request No :
* Addl Documentation:
* Description       : Change PR closed status
*                     (Possible Manual job or Batch job
*                         for change PR Closed status)
*  - PR : Purchase Requisition
*
* Modification Logs
* Date            Developer        RequestNo      Description
*& 10/14/2013   C.H.Jeong                  First development
*
************************************************************************

REPORT zemm_change_pr_closed  MESSAGE-ID zmmm.

*--------------------------------------------------------------------*
*   INCLUDE                                                          *
*--------------------------------------------------------------------*
INCLUDE : zemm_change_pr_closed_top,
          zemm_change_pr_closed_f01,  "Subroutine 1
          zemm_change_pr_closed_f02,  "Subroutine 2
          zemm_change_pr_closed_o01,  "PBO
          zemm_change_pr_closed_i01.  "PAI


*--------------------------------------------------------------------*
*   INITIALIZATION                                                   *
*--------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_default_values.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN Event
*--------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST  FOR p_fname.
  PERFORM selection_screen USING p_fname.


*--------------------------------------------------------------------*
*   START-OF-SELECTION                                               *
*--------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR   : it_pr,   it_pr_num.
  REFRESH : it_pr[], it_pr_num[].

  CASE 'X'.
    WHEN ra_1.
      IF sy-batch IS INITIAL.
        PERFORM display_progress_bar  USING text-010.
      ENDIF.
      PERFORM get_pr_data.

    WHEN ra_2.
      PERFORM get_upload_file .
      PERFORM get_pr_data_of_excel.
  ENDCASE.

  SORT it_pr  BY banfn    "PR No.
                 bnfpo.   "PR item No.
  PERFORM get_etc_data.

*--------------------------------------------------------------------*
*   END-OF-SELECTION                                                 *
*--------------------------------------------------------------------*
END-OF-SELECTION.
  IF sy-batch IS INITIAL.
    IF NOT it_pr[] IS INITIAL.
      CALL SCREEN 100.
    ELSE.
      MESSAGE i015.
    ENDIF.
  ELSE.
*   "when Batch job !!!!!!!
    PERFORM when_batch_job.
    PERFORM change_pr_status.    "BAPI
    PERFORM result_batch_job.
  ENDIF.
