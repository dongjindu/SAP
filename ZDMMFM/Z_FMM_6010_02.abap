FUNCTION z_fmm_6010_02.
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
*"     VALUE(LGNUM_001) LIKE  BDCDATA-FVAL DEFAULT 'P01'
*"     VALUE(VBELN_002) LIKE  BDCDATA-FVAL DEFAULT '180000631'
*"     VALUE(ALAKT_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPML03T' '0151'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL03T-DUNKL'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LTAK-LGNUM'
                                lgnum_001.
  PERFORM bdc_field       USING 'VBLKK-VBELN'
                                vbeln_002.
  PERFORM bdc_field       USING 'RL03T-ALAKT'
                                alakt_003.

*/Begin of Added by Hakchin(20040415)
  DATA: lv_posnr_idx TYPE i. CLEAR lv_posnr_idx.
  SELECT COUNT( * )
    INTO lv_posnr_idx
    FROM lips
    WHERE vbeln = vbeln_002
    GROUP by vbeln.
  ENDSELECT.

  IF lv_posnr_idx > 1.
    PERFORM bdc_dynpro      USING 'SAPML03T' '0154'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'LTAK-LGNUM'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
  ELSE.  "i.e. lv_posnr_idx = 1.
    PERFORM bdc_dynpro      USING 'SAPML03T' '0104'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=BU'.
  ENDIF.
*/End of Commented by Hakchin(20040415)

  PERFORM bdc_transaction TABLES messtab
  USING                         'LT03'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
