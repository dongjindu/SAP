FUNCTION z_fmm_6009_02.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(LIFEX_001) LIKE  BDCDATA-FVAL DEFAULT 'HMMA1234'
*"     VALUE(IM_ZDOCNO) TYPE  NUM10 OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPLLMOB' '0107'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LIKP-LIFEX'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=NEXT'.
  PERFORM bdc_field       USING 'LIKP-LIFEX'
                                lifex_001.
  PERFORM bdc_dynpro      USING 'SAPLLMOB' '0631'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LEMOBDATA-RDDEL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=SAVE'.
  PERFORM bdc_dynpro      USING 'SAPLLMOB' '0998'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=NEXT'.
  DATA: ls_ztmm_6009_01 LIKE ztmm_6009_01.
  DATA: logno_h TYPE num10.
  PERFORM bdc_transaction TABLES messtab
  USING                         'LM74'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    ls_ztmm_6009_01-zzret = 'E'.       "Failure
*    EXIT.
  ELSE.
    ls_ztmm_6009_01-zzret = 'S'.       "Success
  ENDIF.

  PERFORM number_get_next USING    '00'
                                   'ZMMNRO0002'
                          CHANGING logno_h.

* BDC Logging to the tables ZTMM_6009_01 and ZBDCMSGCOLL.
  ls_ztmm_6009_01-logno_h   = logno_h.
  ls_ztmm_6009_01-uname     = sy-uname.
  ls_ztmm_6009_01-datum     = sy-datum.
  ls_ztmm_6009_01-uzeit     = sy-uzeit.
  ls_ztmm_6009_01-lifex_001 = lifex_001.

  INSERT INTO ztmm_6009_01 VALUES ls_ztmm_6009_01.

  DATA: lv_ztcode    TYPE tcode.
  DATA: lv_zprogramm TYPE programm.
  lv_ztcode    = 'ZMME91'.
  lv_zprogramm = 'SAPMZEMMPM22E_6009'.
*  PERFORM bdc_log_to_zbdcmsgcoll TABLES messtab
*                                 USING  logno_h
*                                        im_zdocno
*                                        lv_ztcode
*                                        lv_zprogramm.

  PERFORM bdc_log_to_ztlog TABLES messtab
                           USING  logno_h
                                  im_zdocno
                                  lv_ztcode
                                  lv_zprogramm.



  PERFORM close_group USING     ctu.
ENDFUNCTION.
