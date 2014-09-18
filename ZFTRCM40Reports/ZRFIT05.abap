*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 29/10/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-5
*& Development Request No : UD1K903444
*& Addl documentation     :
*& Description  : Collect actual data .
*&                This is developed use ALV.
*& Modification Log
*& Date        Developer  Request           Description
*& 11/28/06    Manju      UD1K923235        Production fix
*& 09/13/2011  Yn.kim     UP1K920015        ECC6.Upgrade
*&--------------------------------------------------------------------
REPORT zrfif05 MESSAGE-ID zmfi NO STANDARD PAGE HEADING
                               LINE-SIZE 200
                               LINE-COUNT 65.

include zrfif05_t01.
include zrfit05_f01.

INITIALIZATION.

  PERFORM default_.
  perform  init_screen.

************************************************************************
*     START-OF-SELECTION
************************************************************************
START-OF-SELECTION.

  perform init_proc.

  IF p_dbread = 'X'.   " db read
    PERFORM get_ztfi_cmal.  "actual line item
* collect data
  ELSE.
    PERFORM read_setting.
*   PERFORM enqueue_zcmta. "lock object
    PERFORM select_data.

*//     PERFORM delete_setoff.

    READ TABLE it_zcmal INDEX 1.
    IF sy-subrc <> 0.
      MESSAGE s001. EXIT.
    ENDIF.

    IF p_test = space.
      PERFORM exec_save.

      IF p_run_p EQ 'X'.
        PERFORM run_fm_payment.
      ENDIF.

    ENDIF.
  ENDIF.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.

* ALV HEADER & FIELD SETTING
  PERFORM : alvprn_basic01,
            alvprn_field01 USING 'IT_ZCMAL'.

* ALV DISPLAY
  it_list[] = it_zcmal[].
* DELETE it_list WHERE gubun EQ 'D'.
  PERFORM display_list TABLES it_list.
*
