*&---------------------------------------------------------------------*
*& Report  H99_DISPLAY_PAYRESULT                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Display of payroll results (for all countries)                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT h99_display_payresult MESSAGE-ID h99_dp_messages
                             NO STANDARD PAGE HEADING
                             LINE-SIZE 132.

INCLUDE: h99_dp_types,
*         h99_dp_container,
*         h99_dp_alv_utilities,
*         h99_dp_alv,
*         h99_dp_country,
*         h99_dp_employee,
*         h99_dp_rgdir,
*         h99_dp_error,
*         h99_dp_employee_cont,
*         h99_dp_payresult,
*         h99_dp_main_control,
         h99_dp_top.

************************************************************************
START-OF-SELECTION.
************************************************************************

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
       EXPORTING
            tcode  = 'PC00_MNA_CC_ADM'
       EXCEPTIONS
            ok     = 0
            not_ok = 2
            OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE e172(00) WITH 'PC00_MNA_CC_ADM'.
  ENDIF.

  g_repid = sy-repid.
  PERFORM modify_select_options.

  CALL SCREEN 2000.

************************************************************************
TOP-OF-PAGE.
************************************************************************

  CALL METHOD g_main_control->display_header.
