FUNCTION zh_hr_if_py_master.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BRACD) TYPE  HROBJID
*"     VALUE(IV_BEGDA) TYPE  BEGDA
*"     VALUE(IV_ENDDA) TYPE  ENDDA
*"  TABLES
*"      ET_HRPY_RGDIR STRUCTURE  ZGHRSHRPY_RGDIR OPTIONAL
*"      ET_PC207 STRUCTURE  ZGHRSPC207 OPTIONAL
*"      ET_VERSC STRUCTURE  ZGHRSVERSC OPTIONAL
*"      ET_WPBP STRUCTURE  ZGHRSWPBP OPTIONAL
*"      ET_ZGHRLT0005 STRUCTURE  ZGHRSS0015 OPTIONAL
*"----------------------------------------------------------------------

  DATA: lv_lncnt TYPE zghrspc207-lncnt.

  DATA: ls_t500l TYPE t500l.
  DATA: ls_rt       LIKE LINE OF gs_pay_result-inter-rt.
  DATA: ls_wpbp     LIKE LINE OF gs_pay_result-inter-wpbp.

  CLEAR: et_hrpy_rgdir[], et_pc207[].

  SELECT SINGLE * INTO ls_t500l FROM t500l WHERE molga = '10'.

  CHECK sy-subrc = 0.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE et_hrpy_rgdir
   FROM  hrpy_rgdir
   WHERE fpbeg <= iv_endda AND fpend >= iv_begda.
*    rundt BETWEEN iv_begda AND iv_endda.
  IF sy-subrc = 0.
    PERFORM fill_bracd_code USING iv_bracd 3 CHANGING et_hrpy_rgdir[].
  ENDIF.

  LOOP AT et_hrpy_rgdir.
    CLEAR : gs_pay_result.
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        clusterid                    = ls_t500l-relid
        employeenumber               = et_hrpy_rgdir-pernr
        sequencenumber               = et_hrpy_rgdir-seqnr
        check_read_authority         = space
        read_only_international      = 'X'
      CHANGING
        payroll_result               = gs_pay_result
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        OTHERS                       = 8.

*&--- RT
    et_pc207-zcbracd = iv_bracd.
    et_pc207-pernr   = et_hrpy_rgdir-pernr.
    et_pc207-seqnr   = et_hrpy_rgdir-seqnr.
*    et_pc207-rundt   = et_hrpy_rgdir-rundt.

    lv_lncnt = 1.
    LOOP AT gs_pay_result-inter-rt INTO ls_rt.
      et_pc207-lncnt = lv_lncnt.
      MOVE-CORRESPONDING ls_rt TO et_pc207.
      APPEND et_pc207.

      lv_lncnt = lv_lncnt + 1.
    ENDLOOP.

*&--- VERSC
    CLEAR et_versc.
    MOVE-CORRESPONDING gs_pay_result-inter-versc TO et_versc.
    et_versc-zcbracd = iv_bracd.
    et_versc-pernr   = et_hrpy_rgdir-pernr.
    et_versc-seqnr   = et_hrpy_rgdir-seqnr.
    APPEND et_versc.

*&--- WPBP
    et_wpbp-zcbracd = iv_bracd.
    et_wpbp-pernr   = et_hrpy_rgdir-pernr.
    et_wpbp-seqnr   = et_hrpy_rgdir-seqnr.

    lv_lncnt = 1.
    LOOP AT gs_pay_result-inter-wpbp INTO ls_wpbp.
      et_wpbp-lncnt = lv_lncnt.
      MOVE-CORRESPONDING ls_wpbp TO et_wpbp.
      APPEND et_wpbp.

      lv_lncnt = lv_lncnt + 1.
    ENDLOOP.

  ENDLOOP.
ENDFUNCTION.
