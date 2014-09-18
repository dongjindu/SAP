FUNCTION z_fmm_get_vendor_stock.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZTMM_VENDOR_STO STRUCTURE  ZTMM_VENDOR_STO
*"----------------------------------------------------------------------

  DATA: lw_jobnam     LIKE tbtcjob-jobname,
        lw_prognam    LIKE sy-repid,
        lw_int        LIKE tbtcjob-jobcount,
        lw_zzret      LIKE ztmm_vendor_sto-zzret.


  CHECK NOT it_ztmm_vendor_sto[] IS INITIAL.
*/Begin of Added by Hakchin(20040428)
*Set Default value.
  LOOP AT it_ztmm_vendor_sto.
    IF it_ztmm_vendor_sto-doc_type IS INITIAL.
      it_ztmm_vendor_sto-doc_type = '101'.
    ENDIF.

    IF it_ztmm_vendor_sto-werks IS INITIAL.
      it_ztmm_vendor_sto-werks = 'P001'.
    ENDIF.

    IF it_ztmm_vendor_sto-lgort IS INITIAL.
      it_ztmm_vendor_sto-lgort = 'P190'.
    ENDIF.

    IF it_ztmm_vendor_sto-matnr_v IS INITIAL.
      it_ztmm_vendor_sto-matnr_v = 'AG7N'.
    ENDIF.

    IF it_ztmm_vendor_sto-budat IS INITIAL.
      it_ztmm_vendor_sto-budat = sy-datum.
    ENDIF.
    MODIFY it_ztmm_vendor_sto.
  ENDLOOP.
*/End of Added by Hakchin(20040428)


*** create ztable 'ZTMM_VENDOR_STO' (zzret : table insert flag)
  INSERT ztmm_vendor_sto FROM TABLE it_ztmm_vendor_sto
                         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc EQ 0.
    lw_zzret = 'S'.
  ELSE.
    lw_zzret = 'E'.
  ENDIF.

*** execute batch pgm 'EMMGM13_VENDOR_STO'
  lw_jobnam = 'HYSCO_STOCK_GR HYSCO->SAP'.
  lw_prognam = 'ZEMMGM13_VENDOR_STO'.

** open job
  PERFORM call_job_open   USING    lw_jobnam
                          CHANGING lw_int.

** modify jobcount and export jobcount
  LOOP AT it_ztmm_vendor_sto.
    it_ztmm_vendor_sto-zzret     = lw_zzret.
    it_ztmm_vendor_sto-jobcount  = lw_int.
    MODIFY it_ztmm_vendor_sto TRANSPORTING zzret jobcount.
  ENDLOOP.
  MODIFY ztmm_vendor_sto FROM TABLE it_ztmm_vendor_sto.

* submit pgm
  SUBMIT (lw_prognam) WITH     p_count = lw_int
                      WITH     r1         = 'X'
                      USER     sy-uname
                      VIA JOB  lw_jobnam
                      NUMBER   lw_int
                      AND RETURN.

*/Begin of Commented by Hakchin(20040518)
** call job
*  PERFORM call_job_submit   USING lw_int
*                                  lw_jobnam
*                                  lw_prognam.
*/End of Commented by Hakchin(20040518)


* close job
  PERFORM call_job_close   USING lw_int
                                 lw_jobnam.

* export itab modify
  WAIT UP TO 3 SECONDS.
  LOOP AT it_ztmm_vendor_sto.
    SELECT SINGLE * INTO *ztmm_vendor_sto
           FROM ztmm_vendor_sto
          WHERE doc_type  EQ  it_ztmm_vendor_sto-doc_type
          AND   ebeln     EQ  it_ztmm_vendor_sto-ebeln
          AND   ebelp     EQ  it_ztmm_vendor_sto-ebelp
          AND   matnr     EQ  it_ztmm_vendor_sto-matnr
          AND   charg     EQ  it_ztmm_vendor_sto-charg.
    CHECK sy-subrc EQ 0.
    MODIFY it_ztmm_vendor_sto FROM *ztmm_vendor_sto.
  ENDLOOP.

ENDFUNCTION.
