FUNCTION Z_FPM_CHAGE_LONGTEXT.
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
*"     VALUE(AUFNR) LIKE  ZSPM_COMP-AUFNR
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      T_TLINE STRUCTURE  TLINE OPTIONAL
*"----------------------------------------------------------------------
  DATA : WA_TABIX(2)  TYPE N.
  DATA : WA_FIELD_NAME(20).

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '0101'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-AUFNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=VGUE'.
  PERFORM BDC_FIELD       USING 'CAUFVD-AUFNR'
                                AUFNR.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=LATX'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LTICON-LTOPR(01)'.

  PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=TXBA'.

  LOOP AT T_TLINE.
    WA_TABIX = SY-TABIX + 2.
    CONCATENATE 'RSTXT-TXLINE('  WA_TABIX ')' INTO WA_FIELD_NAME.
    PERFORM BDC_FIELD       USING WA_FIELD_NAME
                                  T_TLINE-TDLINE.
  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW32'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
