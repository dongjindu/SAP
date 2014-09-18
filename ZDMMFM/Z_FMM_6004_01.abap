FUNCTION z_fmm_6004_01.
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
*"             VALUE(LENUM_001) LIKE  BDCDATA-FVAL
*"         DEFAULT '7000000063'
*"             VALUE(BWLVS_002) LIKE  BDCDATA-FVAL DEFAULT '999'
*"             VALUE(DUNKL_003) LIKE  BDCDATA-FVAL DEFAULT 'H'
*"             VALUE(LETYP_004) LIKE  BDCDATA-FVAL DEFAULT 'BB'
*"             VALUE(NLTYP_005) LIKE  BDCDATA-FVAL DEFAULT '100'
*"             VALUE(NLPLA_006) LIKE  BDCDATA-FVAL DEFAULT 'AA-01'
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPML03T' '0173'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LEIN-LENUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TAM'.
  PERFORM bdc_field       USING 'LEIN-LENUM'
                                lenum_001.
  PERFORM bdc_field       USING 'LTAK-BWLVS'
                                bwlvs_002.
  PERFORM bdc_field       USING 'RL03T-DUNKL'
                                dunkl_003.

  PERFORM bdc_dynpro      USING 'SAPML03T' '0171'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                '*LTAP-NLPLA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TA'.
  PERFORM bdc_field       USING 'LEIN-LETYP'
                                letyp_004.

  PERFORM bdc_field       USING 'RL03T-SQUIT' "Indicator: confirm
                                'X'.    " Internally hardcoded.

  PERFORM bdc_field       USING '*LTAP-NLTYP'
                                nltyp_005.
  PERFORM bdc_field       USING '*LTAP-NLPLA'
                                nlpla_006.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0171'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTAK-LGNUM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'LT09'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
*    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.

ENDFUNCTION.
INCLUDE bdcrecxy .
