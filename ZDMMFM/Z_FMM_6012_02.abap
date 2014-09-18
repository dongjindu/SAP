FUNCTION Z_FMM_6012_02.
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
*"     VALUE(TANUM_001) LIKE  BDCDATA-FVAL DEFAULT '813'
*"     VALUE(LGNUM_002) LIKE  BDCDATA-FVAL DEFAULT 'P01'
*"     VALUE(STDAT_003) LIKE  BDCDATA-FVAL DEFAULT '11/11/2003'
*"     VALUE(STUZT_004) LIKE  BDCDATA-FVAL DEFAULT '10:36:48'
*"     VALUE(ENDAT_005) LIKE  BDCDATA-FVAL DEFAULT '11/11/2003'
*"     VALUE(ENUZT_006) LIKE  BDCDATA-FVAL DEFAULT '11:36:48'
*"     VALUE(BNAME_007) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(PERNR_008) LIKE  BDCDATA-FVAL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPML03T' '0126'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LTAK-LGNUM'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'LTAK-TANUM'
                                TANUM_001.
  PERFORM BDC_FIELD       USING 'LTAK-LGNUM'
                                LGNUM_002.
  PERFORM BDC_DYNPRO      USING 'SAPML03T' '0103'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LTAK-PERNR'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=BU'.
** Added by Furong on 04/15/08 Requested by Ben
  PERFORM BDC_FIELD       USING 'LTAK-PERNR'
                               PERNR_008.
** End of additon

  PERFORM BDC_FIELD       USING 'LTAK-STDAT'
                                STDAT_003.
  PERFORM BDC_FIELD       USING 'LTAK-STUZT'
                                STUZT_004.
  PERFORM BDC_FIELD       USING 'LTAK-ENDAT'
                                ENDAT_005.
  PERFORM BDC_FIELD       USING 'LTAK-ENUZT'
                                ENUZT_006.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                 'LTAK-ENUZT'.

*  PERFORM bdc_field       USING 'LTAK-BNAME'
*                                bname_007.
*

  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'LT1A'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
