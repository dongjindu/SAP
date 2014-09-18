FUNCTION z_fmm_6012_01.
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
*"     VALUE(BWLVS_002) LIKE  BDCDATA-FVAL DEFAULT '999'
*"     VALUE(MATNR_003) LIKE  BDCDATA-FVAL DEFAULT '327003K100'
*"     VALUE(ANFME_004) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(WERKS_005) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(LGORT_006) LIKE  BDCDATA-FVAL DEFAULT 'P400'
*"     VALUE(ANFME_007) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(ALTME_008) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"     VALUE(VLTYP_009) LIKE  BDCDATA-FVAL DEFAULT '434'
*"     VALUE(VLPLA_010) LIKE  BDCDATA-FVAL DEFAULT 'AA-01-11'
*"     VALUE(NLTYP_011) LIKE  BDCDATA-FVAL DEFAULT '443'
*"     VALUE(NLPLA_012) LIKE  BDCDATA-FVAL DEFAULT 'TS-01'
*"     VALUE(REFNR_013) LIKE  BDCDATA-FVAL
*"     VALUE(BESTQ_014) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(WEMPF_015) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPML03T' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTAP-LGORT'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.  "Enter
                                '=TAE'.  "Single Item
  PERFORM bdc_field       USING 'LTAK-LGNUM'
                                lgnum_001.
  PERFORM bdc_field       USING 'LTAK-REFNR'
                                refnr_013.   "Group(Feeder)
  PERFORM bdc_field       USING 'LTAK-BWLVS'
                                bwlvs_002.
  PERFORM bdc_field       USING 'LTAP-MATNR'
                                matnr_003.
  PERFORM bdc_field       USING 'RL03T-ANFME'
                                anfme_004.
  PERFORM bdc_field       USING 'LTAP-WERKS'
                                werks_005.
  PERFORM bdc_field       USING 'LTAP-LGORT'
                                lgort_006.

**--- insert by stlim (2004/04/15)
  IF bestq_014 NE space.
    PERFORM bdc_field       USING 'LTAP-BESTQ'
                                  bestq_014.
  ENDIF.
**--- end of insert

  PERFORM bdc_dynpro      USING 'SAPML03T' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LTAP-NLPLA'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
*  PERFORM bdc_field       USING 'RL03T-ANFME'
*                                anfme_007.
*  PERFORM bdc_field       USING 'LTAP-ALTME'
*                                altme_008.
  PERFORM bdc_field       USING 'LTAP-VLTYP'
                                vltyp_009.
  PERFORM bdc_field       USING 'LTAP-VLPLA'
                                vlpla_010.
  PERFORM bdc_field       USING 'LTAP-NLTYP'
                                nltyp_011.
  PERFORM bdc_field       USING 'LTAP-NLPLA'
                                nlpla_012.
  PERFORM bdc_field       USING 'LTAP-WEMPF'
                                wempf_015.

  PERFORM bdc_transaction TABLES messtab
  USING                         'LT01'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.


ENDFUNCTION.
INCLUDE bdcrecxy .
