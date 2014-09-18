FUNCTION z_fmm_60xx_lt12.
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
*"     VALUE(TANUM_001) LIKE  BDCDATA-FVAL DEFAULT '302'
*"     VALUE(SHORT) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(LGNUM_002) LIKE  BDCDATA-FVAL DEFAULT 'P01'
*"     VALUE(DUNKL_003) LIKE  BDCDATA-FVAL DEFAULT 'D'
*"     VALUE(QENTR_004) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(QENTN_005) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(QTRAN_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(KZDIF_008) LIKE  BDCDATA-FVAL
*"     VALUE(NISTA_010) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.


  IF QENTN_005 = 'X'.
     PERFORM bdc_dynpro      USING 'SAPML03T' '0111'.
     PERFORM bdc_field       USING 'BDC_CURSOR'
                                   'RL03T-DUNKL'.
     PERFORM bdc_field       USING 'BDC_OKCODE'
                                   '=QLI'.
     PERFORM bdc_field       USING 'LTAK-TANUM'
                                   tanum_001.
     PERFORM bdc_field       USING 'LTAK-LGNUM'
                                   lgnum_002.
     PERFORM bdc_field       USING 'RL03T-DUNKL'
                                   dunkl_003.
     PERFORM bdc_field       USING 'RL03T-QENTR'
                                   qentr_004.
     PERFORM bdc_field       USING 'RL03T-QENTN'
                                   qentn_005.
     PERFORM bdc_field       USING 'RL03T-QTRAN'
                                   qtran_006.
     IF SHORT = 'Y'.

        perform bdc_dynpro      using 'SAPML03T' '0113'.
        perform bdc_field       using 'BDC_CURSOR'
                                      'LTAP1-EIDIF(01)'.
        perform bdc_field       using 'BDC_OKCODE'
                                      '=QU'.
        perform bdc_field       using 'LTAP1-EIDIF(01)'
                                      'X'.
        perform bdc_dynpro      using 'SAPML03T' '0112'.
        perform bdc_field       using 'BDC_CURSOR'
                                      'LTAP-NISTA'.
        perform bdc_field       using 'BDC_OKCODE'
                                      '/00'.
        perform bdc_field       using 'LTAP-KZDIF'
                                      KZDIF_008.
*   perform bdc_field       using 'RL03T-NAMEI'
*                                 'EA'.
        perform bdc_field       using 'RL03T-NQUIT'
                                      'S'.
        perform bdc_field       using 'LTAP-NISTA'
                                       NISTA_010.
        perform bdc_dynpro      using 'SAPML03T' '0115'.
        perform bdc_field       using 'BDC_CURSOR'
                                      'LTAP-DLTYP'.
        perform bdc_field       using 'BDC_OKCODE'
                                      '=DIBE'.
        perform bdc_dynpro      using 'SAPML03T' '0113'.
        perform bdc_field       using 'BDC_CURSOR'
                                      'LTAK-LGNUM'.
        perform bdc_field       using 'BDC_OKCODE'
                                      '=BU'.
*   ELSE.
*
*        perform bdc_dynpro      using 'SAPML03T' '0113'.
*        perform bdc_field       using 'BDC_CURSOR'
*                                      'LTAK-LGNUM'.
*        perform bdc_field       using 'BDC_OKCODE'
*                                      '=BU'.
   ENDIF.
 ELSE.

    PERFORM bdc_dynpro      USING 'SAPML03T' '0111'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RL03T-DUNKL'.
    IF SHORT = 'Y'.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '=QSA'.
    ELSE.
       PERFORM bdc_field       USING 'BDC_OKCODE'
                                     '/00'.
    ENDIF.
    PERFORM bdc_field       USING 'LTAK-TANUM'
                                  tanum_001.
    PERFORM bdc_field       USING 'LTAK-LGNUM'
                                  lgnum_002.
    PERFORM bdc_field       USING 'RL03T-DUNKL'
                                  dunkl_003.
    PERFORM bdc_field       USING 'RL03T-QENTR'
                                  qentr_004.
    PERFORM bdc_field       USING 'RL03T-QENTN'
                                  qentn_005.
    PERFORM bdc_field       USING 'RL03T-QTRAN'
                                  qtran_006.
    IF QTRAN_006 = 'X'.
       perform bdc_dynpro      using 'SAPML03T' '0114'.
       perform bdc_field       using 'BDC_OKCODE'
                                     '=BU'.
       perform bdc_field       using 'BDC_CURSOR'
                                     'LTAP-NISTA(01)'.
       perform bdc_field       using 'LTAP-NISTA(01)'
                                      NISTA_010.
       perform bdc_dynpro      using 'SAPML03T' '0115'.
       perform bdc_field       using 'BDC_CURSOR'
                                     'LTAP-DLTYP'.
       perform bdc_field       using 'BDC_OKCODE'
                                     '=DIBE'.
    ENDIF.
 ENDIF.
 PERFORM bdc_transaction TABLES messtab
 USING                         'LT12'
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
