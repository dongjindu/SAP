FUNCTION z_fmm_60xx_cs01.
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
*"     VALUE(MATNR_001) LIKE  BDCDATA-FVAL
*"     VALUE(WERKS_002) LIKE  BDCDATA-FVAL
*"     VALUE(STLAN_003) LIKE  BDCDATA-FVAL
*"     VALUE(AENNR_004) LIKE  BDCDATA-FVAL
*"     VALUE(DATUV_005) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(BMENG_006) LIKE  BDCDATA-FVAL
*"     VALUE(STLST_007) LIKE  BDCDATA-FVAL
*"     VALUE(IDNRK_01_008) LIKE  BDCDATA-FVAL
*"     VALUE(MENGE_01_009) LIKE  BDCDATA-FVAL
*"     VALUE(MEINS_01_010) LIKE  BDCDATA-FVAL
*"     VALUE(POSTP_01_011) LIKE  BDCDATA-FVAL
*"     VALUE(POSNR_012) LIKE  BDCDATA-FVAL
*"     VALUE(IDNRK_013) LIKE  BDCDATA-FVAL
*"     VALUE(MENGE_014) LIKE  BDCDATA-FVAL
*"     VALUE(MEINS_015) LIKE  BDCDATA-FVAL
*"     VALUE(SANKA_016) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29N-AENNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RC29N-MATNR'
                                matnr_001.
  PERFORM bdc_field       USING 'RC29N-WERKS'
                                werks_002.
  PERFORM bdc_field       USING 'RC29N-STLAN'
                                stlan_003.
  PERFORM bdc_field       USING 'RC29N-AENNR'
                                aennr_004.
  PERFORM bdc_field       USING 'RC29N-DATUV'
                                datuv_005.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0110'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RC29K-BMENG'
                                bmeng_006.
  PERFORM bdc_field       USING 'RC29K-STLST'
                                stlst_007.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29K-EXSTL'.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0111'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29K-LABOR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0140'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29P-POSTP(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=FCBU'.
  PERFORM bdc_field       USING 'RC29P-IDNRK(01)'
                                idnrk_01_008.
  PERFORM bdc_field       USING 'RC29P-MENGE(01)'
                                menge_01_009.
  PERFORM bdc_field       USING 'RC29P-MEINS(01)'
                                meins_01_010.
  PERFORM bdc_field       USING 'RC29P-POSTP(01)'
                                postp_01_011.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0130'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29P-POSNR'.
  PERFORM bdc_field       USING 'RC29P-POSNR'
                                posnr_012.
  PERFORM bdc_field       USING 'RC29P-IDNRK'
                                idnrk_013.
  PERFORM bdc_field       USING 'RC29P-MENGE'
                                menge_014.
  PERFORM bdc_field       USING 'RC29P-MEINS'
                                meins_015.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0131'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RC29P-POTX1'.
  PERFORM bdc_field       USING 'RC29P-SANKA'
                                sanka_016.
  PERFORM bdc_dynpro      USING 'SAPLCSDI' '0138'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'ZEITM'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'CS01'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
