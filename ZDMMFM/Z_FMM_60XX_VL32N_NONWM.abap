FUNCTION Z_FMM_60XX_VL32N_NONWM.
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
*"     VALUE(VBELN_001) LIKE  VBUP-VBELN
*"     VALUE(BLDAT_002) LIKE  BDCDATA-FVAL
*"     VALUE(LFDAT_LA_003) LIKE  BDCDATA-FVAL
*"     VALUE(LFUHR_LA_004) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      T_LIPS STRUCTURE  LIPS
*"----------------------------------------------------------------------
  DATA: L_PIKMG_FIELD(30),
        L_POS(2) TYPE N,
        L_NUBER(13) TYPE N,
        L_PIKMG TYPE BDCDATA-FVAL.

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPMV50A' '4104'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LIKP-VBELN'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'LIKP-VBELN'
                                VBELN_001.
  PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=T\04'.
  PERFORM BDC_FIELD       USING 'LIKP-BLDAT'
                                BLDAT_002.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LIPS-MATNR(02)'.
  PERFORM BDC_FIELD       USING 'RV50A-LFDAT_LA'
                                LFDAT_LA_003.
*  PERFORM BDC_FIELD       USING 'RV50A-LFUHR_LA'
*                                LFUHR_LA_004.

* PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=T\04'.

*perform bdc_field       using 'LIKP-BTGEW'
*                              record-BTGEW_005.
*perform bdc_field       using 'LIKP-GEWEI'
*                              record-GEWEI_006.
  PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'LIKP-BLDAT'
                                BLDAT_002.

  LOOP AT T_LIPS.
    IF T_LIPS-PSTYV = 'ZELN'.
    ELSE.
      L_POS = T_LIPS-POSNR.
      L_PIKMG = L_NUBER = T_LIPS-LFIMG.
      CONCATENATE 'LIPSD-PIKMG(' L_POS ')' INTO L_PIKMG_FIELD.

      PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'LIPSD-PIKMG(01)'.
                                    L_PIKMG_FIELD.
*perform bdc_field       using 'RV50A-LFDAT_LA'
*                              record-LFDAT_LA_008.
*perform bdc_field       using 'RV50A-LFUHR_LA'
*                              record-LFUHR_LA_009.
***
      PERFORM BDC_FIELD       USING L_PIKMG_FIELD
*  PERFORM BDC_FIELD       USING 'LIPSD-PIKMG(01)'     " qty
                                     L_PIKMG.
    ENDIF.
  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPMV50A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=WABU_T'.

  PERFORM BDC_FIELD       USING 'LIKP-BLDAT'
                                BLDAT_002.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LIPS-MATNR(02)'.
  PERFORM BDC_FIELD       USING 'RV50A-LFDAT_LA'
                                LFDAT_LA_003.
*perform bdc_field       using 'RV50A-LFUHR_LA'
*                              LFUHR_LA_004.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'VL32N'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.

ENDFUNCTION.
