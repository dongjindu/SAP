FUNCTION Z_FCO_PCC_ORDER_CRE_WITH_PDV.
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
*"             VALUE(MATNR_001) LIKE  BDCDATA-FVAL
*"             VALUE(WERKS_002) LIKE  BDCDATA-FVAL
*"             VALUE(KTEXT_004) LIKE  BDCDATA-FVAL
*"             VALUE(LOSGR_005) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(VERID_007) LIKE  BDCDATA-FVAL
*"             VALUE(P_FIRST) TYPE  CHAR1
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPMKOSA' '0101'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'PKOSA-MATNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'PKOSA-MATNR'
                                MATNR_001.
  PERFORM BDC_FIELD       USING 'PKOSA-WERKS'
                                WERKS_002.
*perform bdc_field       using 'PKOSA-AUART'
*                              AUART_003.

  IF P_FIRST = 'X'.
    PERFORM BDC_DYNPRO      USING 'SAPMSSY0' '0120'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  '04/10'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=&NT1'.
  ENDIF.

  PERFORM BDC_DYNPRO      USING 'SAPMKOSA' '0111'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'PROCPARAM-VERID'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=SAVE'.
  PERFORM BDC_FIELD       USING 'PKOSA-KTEXT'
                                KTEXT_004.
  PERFORM BDC_FIELD       USING 'PROCPARAM-LOSGR'
                                LOSGR_005.
*perform bdc_field       using 'PROCPARAM-PWERK'
*                              PWERK_006.
  PERFORM BDC_FIELD       USING 'PROCPARAM-VERID'
                                VERID_007.
*perform bdc_field       using 'PKOSA-KLVARP'
*                              KLVARP_008.
*perform bdc_field       using 'PKOSA-KLVARI'
*                              KLVARI_009.
*perform bdc_field       using 'PKOSA-AWSLS'
*                              AWSLS_010.
*perform bdc_field       using 'PKOSA-ABGSL'
*                              ABGSL_011.
*perform bdc_field       using 'PKOSA-KALSM'
*                              KALSM_012.
  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'KKF6'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
INCLUDE BDCRECXY .
