FUNCTION z_fmm_60xx_mb01.
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
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL
*"     VALUE(XFULL_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(WVERS1_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(BWARTWE_005) LIKE  BDCDATA-FVAL DEFAULT '101'
*"     VALUE(VLIEF_006) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMM07M' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MKPF-BUDAT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=NFBL'.
  PERFORM bdc_field       USING 'MKPF-BLDAT'
                                bldat_001.
  PERFORM bdc_field       USING 'MKPF-BUDAT'
                                budat_002.
  PERFORM bdc_field       USING 'XFULL'
                                xfull_003.
  PERFORM bdc_field       USING 'RM07M-WVERS1'
                                wvers1_004.
  PERFORM bdc_dynpro      USING 'SAPMM07M' '1201'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM07M-BWARTWE'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=OK'.
  PERFORM bdc_field       USING 'RM07M-BWARTWE'
                                bwartwe_005.
  PERFORM bdc_field       USING 'RM07M-VLIEF'
                                vlief_006.
  PERFORM bdc_dynpro      USING 'SAPMM07M' '0221'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MSEG-ERFMG(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'MB01'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.


ENDFUNCTION.
