FUNCTION z_fmm_60xx_md01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"             VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"             VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"             VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"             VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"             VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"             VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"             VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"             VALUE(WERKS_001) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"             VALUE(VERSL_002) LIKE  BDCDATA-FVAL DEFAULT 'NETCH'
*"             VALUE(BANER_003) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(LIFKZ_004) LIKE  BDCDATA-FVAL DEFAULT '3'
*"             VALUE(DISER_005) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(PLMOD_006) LIKE  BDCDATA-FVAL DEFAULT '3'
*"             VALUE(TRMPL_007) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(DISPD_008) LIKE  BDCDATA-FVAL
*"         DEFAULT '12/14/2003'
*"             VALUE(UXKEY_009) LIKE  BDCDATA-FVAL DEFAULT '001'
*"             VALUE(UXPAR_010) LIKE  BDCDATA-FVAL DEFAULT 'P01'
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMM61X' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM61X-UXPAR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RM61X-WERKS'
                                werks_001.
  PERFORM bdc_field       USING 'RM61X-VERSL'
                                versl_002.
  PERFORM bdc_field       USING 'RM61X-BANER'
                                baner_003.
  PERFORM bdc_field       USING 'RM61X-LIFKZ'
                                lifkz_004.
  PERFORM bdc_field       USING 'RM61X-DISER'
                                diser_005.
  PERFORM bdc_field       USING 'RM61X-PLMOD'
                                plmod_006.
  PERFORM bdc_field       USING 'RM61X-TRMPL'
                                trmpl_007.
  PERFORM bdc_field       USING 'RM61X-DISPD'
                                dispd_008.
  PERFORM bdc_field       USING 'RM61X-UXKEY'
                                uxkey_009.
  PERFORM bdc_field       USING 'RM61X-UXPAR'
                                uxpar_010.
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=XBAC'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'MD01'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
