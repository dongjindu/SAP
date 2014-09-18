*----------------------------------------------------------------------*
***INCLUDE ZSAPBF_CPZP_CORRECTION_REPF04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PARALLEL_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_PARALLEL  text
*----------------------------------------------------------------------*
FORM parallel_fill CHANGING cs_parallel TYPE ppc_parallel .

  CLEAR cs_parallel.
  cs_parallel-para_flag       = p_par.
  cs_parallel-servergroup     = p_sgr.
  cs_parallel-wps_request     = p_wps. "Important, Be carefull, Still use p_wps by James 2011.03.02
  cs_parallel-del_history     = p_del_hl.
  cs_parallel-waittime        = p_wttime.

  cs_parallel-retry_commun    = p_retryc.
  cs_parallel-retry_system    = p_retrys.
  cs_parallel-retry_resource  = p_retryr.

ENDFORM.                    " PARALLEL_FILL
*&---------------------------------------------------------------------*
*&      Form  CHECK_PARALLEL_SERVERGROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_FREE_WPS  text
*----------------------------------------------------------------------*
FORM check_parallel_servergroup  CHANGING p_free_wps.

  DATA: l_max_wps  TYPE i,
        l_free_wps TYPE i.

* Determine Free work process
  CALL FUNCTION 'SPBT_INITIALIZE'
    EXPORTING
      group_name                     = p_sgr
    IMPORTING
      max_pbt_wps                    = l_max_wps
      free_pbt_wps                   = p_free_wps
    EXCEPTIONS
      invalid_group_name             = 1
      internal_error                 = 2
      pbt_env_already_initialized    = 3
      currently_no_resources_avail   = 4
      no_pbt_resources_found         = 5
      cant_init_different_pbt_groups = 6
      OTHERS                         = 7.

  CASE sy-subrc.

    WHEN 0.
* do nothing

    WHEN 1. "Added by James Sung-Kon Kim 2011.03.02
      "MESSAGE ID 'ppc1pr' TYPE 'E' NUMBER 503.
      MESSAGE s503(ppc1pr) DISPLAY LIKE 'E'.   "  RAISING init_error.
      STOP.
*      EXIT.

    WHEN 3.
** Remarks: General message table should be updated with the messages
** Also, messages with type 'X' should be converted to 'E' and the
** program should be stopped from running.
      CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
        IMPORTING
          max_pbt_wps                 = l_max_wps
          free_pbt_wps                = p_free_wps
        EXCEPTIONS
          internal_error              = 1
          pbt_env_not_initialized_yet = 2
          OTHERS                      = 3.

      IF sy-subrc <> 0.
        p_free_wps = 0.
      ENDIF.

    WHEN OTHERS.
      p_free_wps = 0.
  ENDCASE.

ENDFORM.                    " CHECK_PARALLEL_SERVERGROUP
