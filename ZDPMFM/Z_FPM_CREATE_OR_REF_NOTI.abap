FUNCTION Z_FPM_CREATE_OR_REF_NOTI.
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
*"     VALUE(QMART) LIKE  RIWO00-QMART DEFAULT 'M1'
*"     VALUE(QWRNUM) LIKE  RIWO00-QWRNUM
*"     VALUE(AUART) LIKE  RIWO00-AUART DEFAULT 'PM01'
*"     VALUE(ZSPM_COUNTER) LIKE  ZSPM_COUNTER STRUCTURE  ZSPM_COUNTER
*"     VALUE(NO_TASK) TYPE  CHECK OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  DATA: WA_IDAT2(10).

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  WRITE ZSPM_COUNTER-IDAT2 TO WA_IDAT2 MM/DD/YYYY.

***** Initial Screen...
***** Create Notification reference Old noti
***** New Noti Type : 'M1'
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'RIWO00-QMART'
                                QMART.
  PERFORM BDC_FIELD       USING 'RIWO00-QWRNUM'
                                QWRNUM.
**** Create Notification
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=10\TAB10'.
  PERFORM BDC_FIELD       USING 'VIQMEL-QMTXT'
                                ZSPM_COUNTER-KTEXT.
*  PERFORM BDC_FIELD       USING 'RIWO1-TPLNR'
*                                GEWRK_004.
*  PERFORM BDC_FIELD       USING 'RIWO1-EQUNR'
*                                SWERK_005.
*  PERFORM BDC_FIELD       USING 'VIQMEL-INGRP'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'VIQMEL-IWERK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'RIWO00-GEWRK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'RIWO00-SWERK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'VIQMEL-QMDAT'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'VIQMEL-MZEIT'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'RIWO00-HEADKTXT'
*                                ANWS_04_006.

  IF NO_TASK EQ SPACE.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=MALL'.
    PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=PSDL'.
    PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0100'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=YES'.
  ENDIF.

**** Change Task Planned finish date
*  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=20\TAB03'.
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '7204'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=COAE'.
*  PERFORM BDC_FIELD       USING 'VIQMSM-PETER(01)'
*                                WA_IDAT2.

**** Create Order reference notification...
**** Order type : 'PM01'
  PERFORM BDC_DYNPRO      USING 'SAPLIQS0' '8030'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=WEIT'.
  PERFORM BDC_FIELD       USING 'RIWO00-AUART'
                                AUART.

*  PERFORM BDC_FIELD       USING 'RIWO00-GEWRK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'RIWO00-SWERK'
*                                ANWS_04_006.


**** Long text...
  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=VGUE'.
  PERFORM BDC_FIELD       USING 'CAUFVD-GSTRP'
                                 WA_IDAT2.
  PERFORM BDC_FIELD       USING 'CAUFVD-GLTRP'
                                 WA_IDAT2.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=LATX'.
  PERFORM BDC_FIELD       USING 'RC27X-FLG_SEL(01)'
                                'X'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'LTICON-LTOPR(01)'.

  PERFORM BDC_DYNPRO      USING 'SAPLSTXX' '1100'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=TXBA'.
  PERFORM BDC_FIELD       USING 'RSTXT-TXLINE(03)'
                                '.'.
*****************
  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
***{ 2003/12/24 CHANGE
*                                '=ANST'.
*** 2003/12/24 CHANGE }

*  PERFORM BDC_FIELD       USING 'CAUFVD-KTEXT'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-INGPR'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-VAPLZ'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-VAWRK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-ILART'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-GSTRP'
*                                WA_IDAT2.
*  PERFORM BDC_FIELD       USING 'CAUFVD-PRIOK'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-GLTRP'
*                                WA_IDAT2.
*  PERFORM BDC_FIELD       USING 'CAUFVD-TPLNR'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'CAUFVD-EQUNR'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-LTXA1'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-INDET'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-ARBPL'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-WERKS'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-STEUS'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-ARBEH'
*                                ANWS_04_006.
*  PERFORM BDC_FIELD       USING 'AFVGD-DAUNE'
*                                ANWS_04_006.

***{ 2003/12/24 DELETE
**** Change User Status
*  PERFORM BDC_DYNPRO      USING 'SAPLBSVA' '0201'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=OKAY'.
*  PERFORM BDC_FIELD       USING 'J_STMAINT-ANWS(04)'
*                                'X'.
**** 2003/12/24 DELETE}

**** Release
  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=FREI'.
**** Save
  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW21'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
