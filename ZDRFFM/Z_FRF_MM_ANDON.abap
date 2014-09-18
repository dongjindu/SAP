FUNCTION z_frf_mm_andon.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_MATNR) LIKE  MLGN-MATNR
*"     VALUE(P_REFNR) LIKE  T311-REFNR DEFAULT 0
*"     VALUE(P_USER) LIKE  SYST-UNAME OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"----------------------------------------------------------------------
  DATA: it_ztmm_andon LIKE TABLE OF ztmm_andon_1 WITH HEADER LINE.

  it_ztmm_andon-matnr = p_matnr.
  it_ztmm_andon-refnr = p_refnr.
  it_ztmm_andon-nuser = p_user.
  it_ztmm_andon-e_date = sy-datum.
  it_ztmm_andon-e_time = sy-uzeit.

  INSERT INTO ztmm_andon_1 VALUES it_ztmm_andon.
  IF sy-subrc EQ 0.
    e_mess  = text-m22.
    zresult = text-m04.
    COMMIT WORK.
  ELSE.
    e_mess  = text-w15.
    zresult = text-m02.
    ROLLBACK WORK.
  ENDIF.
ENDFUNCTION.
