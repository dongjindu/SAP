FUNCTION Z_FMM_6003_01_MK01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              TA_ZSMM_6003_01 STRUCTURE  ZSMM_6003_01 OPTIONAL
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  READ TABLE ta_zsmm_6003_01 INDEX 1. "To use header of itab
  subrc = 0.

  ta_zsmm_6003_01-ctu     = 'X'.
  ta_zsmm_6003_01-zmode   = 'N'.
  ta_zsmm_6003_01-zupdate = 'L'.
  ta_zsmm_6003_01-nodata  = '/'.

  ta_zsmm_6003_01-reprf_027 = 'X'.

  PERFORM bdc_nodata      USING ta_zsmm_6003_01-nodata.

* Make bdcdata of MK01(Create Vendor)
  PERFORM open_group      USING ta_zsmm_6003_01-zgroup
                                ta_zsmm_6003_01-zuser
                                ta_zsmm_6003_01-keep
                                ta_zsmm_6003_01-holddate
                                ta_zsmm_6003_01-ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0107'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF02K-KTOKK'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                ta_zsmm_6003_01-lifnr_001.
  PERFORM bdc_field       USING 'RF02K-EKORG'
                                ta_zsmm_6003_01-ekorg_003.
  PERFORM bdc_field       USING 'RF02K-KTOKK'
                                ta_zsmm_6003_01-ktokk_004.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFA1-TELFX'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_field       USING 'LFA1-NAME1'
                                ta_zsmm_6003_01-name1_005.
  PERFORM bdc_field       USING 'LFA1-SORTL'
                                ta_zsmm_6003_01-sortl_006.
  PERFORM bdc_field       USING 'LFA1-STRAS'
                                ta_zsmm_6003_01-stras_010.
  PERFORM bdc_field       USING 'LFA1-PSTLZ'
                                ta_zsmm_6003_01-pstlz_013.
  PERFORM bdc_field       USING 'LFA1-LAND1'
                                ta_zsmm_6003_01-land1_015.
  PERFORM bdc_field       USING 'LFA1-TELF1'
                                ta_zsmm_6003_01-telf1_019.
  PERFORM bdc_field       USING 'LFA1-TELFX'
                                ta_zsmm_6003_01-telx1_018.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFA1-KUNNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFBK-BANKS(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFM1-ZTERM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.
  PERFORM bdc_field       USING 'LFM1-WAERS'
                                ta_zsmm_6003_01-waers_029.
  PERFORM bdc_field       USING 'LFM1-ZTERM'
                                ta_zsmm_6003_01-zterm_030.

  PERFORM bdc_transaction TABLES messtab
  USING                         'MK01'
                                ta_zsmm_6003_01-ctu
                                ta_zsmm_6003_01-zmode
                                ta_zsmm_6003_01-zupdate.

  IF sy-subrc <> 0.
    subrc = sy-subrc.
    ta_zsmm_6003_01-zzret = 'E'.       "Failure
*    EXIT.
  ELSE.
    ta_zsmm_6003_01-zzret = 'S'.       "Success
  ENDIF.

  MODIFY ta_zsmm_6003_01 INDEX 1.      "Update Internal table

  DATA: logno_h TYPE num10.
  PERFORM number_get_next USING    '00'
                                   'ZMMNRO0002'
                          CHANGING logno_h.

  DATA: ls_ztmm_6003_01 LIKE ztmm_6003_01.
  MOVE-CORRESPONDING ta_zsmm_6003_01 TO ls_ztmm_6003_01.
  MOVE logno_h   TO ls_ztmm_6003_01-logno_h. "Log number

*/ Begin of Time stamp
  MOVE sy-uname  TO ls_ztmm_6003_01-zuser.   "Created User ID
  MOVE sy-datum  TO ls_ztmm_6003_01-zsdat.   "SEND FILE CREATED DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zstim.   "SEND FILE CREATED TIME
  MOVE sy-datum  TO ls_ztmm_6003_01-zedat.   "SAP INTERFACE DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zetim.   "SAP INTERFACE TIME
  MOVE sy-datum  TO ls_ztmm_6003_01-zbdat.   "SAP BDC EXECUTED DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zbtim.   "SAP BDC EXECUTED TIME
  MOVE sy-uname  TO ls_ztmm_6003_01-zbnam.   "BDC User ID
  MOVE 'C'       TO ls_ztmm_6003_01-zmode.   "(Create/Update/Delete)
*Result of the Processing
  MOVE ta_zsmm_6003_01-zzret TO ls_ztmm_6003_01-zresult.
  MOVE space     TO ls_ztmm_6003_01-zmsg.    "Message text
*/ End of Time stamp

* BDC Logging to the tables ZTMM_6003_01 and ZBDCMSGCOLL.
  INSERT INTO ztmm_6003_01 VALUES ls_ztmm_6003_01.
  PERFORM bdc_log_to_zbdcmsgcoll TABLES messtab
                                 USING  logno_h.

  PERFORM close_group USING     ta_zsmm_6003_01-ctu.
ENDFUNCTION.
*INCLUDE zbdcrecxy.
