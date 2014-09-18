*----------------------------------------------------------------------*
*   INCLUDE ZTHR_WTMNG_FORMS                                           *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       get payroll period
*----------------------------------------------------------------------*
FORM get_payroll_period using v_abkrs
                     changing v_permo v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.
*... modified on 2004.01.30
  CASE V_ABKRS.
    WHEN '11'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '1'
                    AND ZCOLN = '2'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '1'
                    AND ZCOLN = '1'.
    WHEN '13'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '1'
                    AND ZCOLN = '1'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '1'
                    AND ZCOLN = '2'.
  ENDCASE.
*
  if v_abkrs ne space.
    CALL FUNCTION 'PA03_PERIODDATES_GET'
         EXPORTING
              F_ABKRS                     = v_abkrs
         IMPORTING
              F_PERMO                     = v_permo
              F_CURRENT_BEGDA             = v_begda
              F_CURRENT_ENDDA             = v_endda
              F_ABKRS_TEXT                = v_abkrt
         CHANGING
              F_CURRENT_PERIOD            = v_pabrp
              F_CURRENT_YEAR              = v_pabrj
         EXCEPTIONS
              PCR_DOES_NOT_EXIST          = 1
              ABKRS_DOES_NOT_EXIST        = 2
              PERIOD_DOES_NOT_EXIST       = 3
              OTHERS                      = 4.
    if sy-subrc <> 0.
      message e003 with v_pabrp v_pabrj.
    endif.
  endif.
ENDFORM.                    " get_payroll_period
*&---------------------------------------------------------------------*
*&      Form  check_comparison_parm
*&---------------------------------------------------------------------*
FORM check_comparison_parm using v_abkrc v_abrpc v_abrjc
                           changing v_abkrr v_abrpr v_abrjr.
  if v_abrpr = space.
    v_abkrr = v_abkrc.
    v_abrjr = v_abrjc.
    v_abrpr = v_abrpc - 1.
    if v_abrpc = 1.
      v_abrpr = 1.
    endif.
  endif.
ENDFORM.                    " check_comparison_parm
