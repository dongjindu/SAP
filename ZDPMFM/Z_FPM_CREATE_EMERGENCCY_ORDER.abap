************************************************************************
* Program Name      : Z_FPM_CREATE_EMERGENCCY_ORDER
* Author            : Myoungho, Park
* Creation Date     : 2003.10.30.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Emergenccy Order Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

FUNCTION Z_FPM_CREATE_EMERGENCCY_ORDER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE OPTIONAL
*"     VALUE(PM_AUFART) LIKE  AUFPAR-PM_AUFART DEFAULT 'PM02'
*"     VALUE(PRIOK) LIKE  CAUFVD-PRIOK DEFAULT '1'
*"     VALUE(TPLNR) LIKE  CAUFVD-TPLNR OPTIONAL
*"     VALUE(EQUNR) LIKE  CAUFVD-EQUNR OPTIONAL
*"     VALUE(KTEXT) LIKE  CAUFVD-KTEXT OPTIONAL
*"     VALUE(GSTRP) LIKE  CAUFVD-GSTRP OPTIONAL
*"     VALUE(ATP_CHECK) LIKE  BAPICM61V-DIAFL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      RESBD STRUCTURE  ZSPM_RESBD OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  CLEAR : WA_ATP_CHECK.

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'AUFPAR-PM_AUFART'
                                 PM_AUFART.
  PERFORM BDC_FIELD       USING 'CAUFVD-PRIOK'
                                 PRIOK.
  PERFORM BDC_FIELD       USING 'CAUFVD-TPLNR'
                                 TPLNR.
  PERFORM BDC_FIELD       USING 'CAUFVD-EQUNR'
                                 EQUNR.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=MUEB'.
  PERFORM BDC_FIELD       USING 'CAUFVD-KTEXT'
                                 KTEXT.

*  PERFORM BDC_FIELD       USING 'CAUFVD-INGPR'
*                                 CAUFVD-INGPR.
*  PERFORM BDC_FIELD       USING 'CAUFVD-VAPLZ'
*                                 CAUFVD-VAPLZ.
*  PERFORM BDC_FIELD       USING 'CAUFVD-VAWRK'
*                                 CAUFVD-VAWRK.
*  PERFORM BDC_FIELD       USING 'CAUFVD-ILART'
*                                 CAUFVD-ILART.
*  PERFORM BDC_FIELD       USING 'CAUFVD-GSTRP'
*                                 CAUFVD-GSTRP.
*  PERFORM BDC_FIELD       USING 'CAUFVD-PRIOK'
*                                 CAUFVD-PRIOK.
*  PERFORM BDC_FIELD       USING 'CAUFVD-GLTRP'
*                                 CAUFVD-GLTRP.
*perform bdc_field       using 'CAUFVD-TPLNR'
*                              CAUFVD-TPLNR.
*perform bdc_field       using 'CAUFVD-EQUNR'
*                              CAUFVD-EQUNR.
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUSVN'
*                                 VIQMEL-AUSVN.
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTV'
*                                 VIQMEL-AUZTV.
*  PERFORM BDC_FIELD       USING 'VIQMEL-AUZTB'
*                                VIQMEL-AUZTB.
*  PERFORM BDC_FIELD       USING 'VIQMEL-MAUEH'
*                                 VIQMEL-MAUEH.
*  PERFORM BDC_FIELD       USING 'AFVGD-INDET'
*                                 AFVGD-INDET.
*  PERFORM BDC_FIELD       USING 'AFVGD-ARBPL'
*                                 AFVGD-ARBPL.
*  PERFORM BDC_FIELD       USING 'AFVGD-WERKS'
*                                 AFVGD-WERKS.
*  PERFORM BDC_FIELD       USING 'AFVGD-STEUS'
*                                 AFVGD-STEUS.
*  PERFORM BDC_FIELD       USING 'AFVGD-ARBEH'
*                                 AFVGD-ARBEH.
*  PERFORM BDC_FIELD       USING 'AFVGD-DAUNE'
*                                 AFVGD-DAUNE.

  WA_ATP_CHECK = ATP_CHECK.

  data $i type i.
  data $ix type i.

  describe table RESBD lines $i.

  LOOP AT RESBD.

    $ix = sy-tabix.

*    IF WA_ATP_CHECK EQ SPACE.
*      PERFORM ATP_CHECK_RTN USING GSTRP
*                                  RESBD-WERKS
*                                  RESBD-MATNR
*                                  RESBD-MENGE
*                                  resbd-LGORT
*                                  WA_ATP_CHECK.
****
*    ENDIF.

    IF SY-TABIX EQ 1.
      PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    'P++'.
      PERFORM BDC_FIELD       USING 'RESBD-MATNR(01)'
                                     RESBD-MATNR.
      PERFORM BDC_FIELD       USING 'RESBD-MENGE(01)'
                                     RESBD-MENGE.
      PERFORM BDC_FIELD       USING 'RESBD-LGORT(01)'
                                     RESBD-LGORT.
      PERFORM BDC_FIELD       USING 'RESBD-WERKS(01)'
                                     RESBD-WERKS.

      PERFORM BDC_DYNPRO      USING 'SAPLCOMD' '3910'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    'WEIT'.

    ELSE.
      PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM BDC_FIELD       USING 'RESBD-MATNR(02)'
                                     RESBD-MATNR.
      PERFORM BDC_FIELD       USING 'RESBD-MENGE(02)'
                                     RESBD-MENGE.
      PERFORM BDC_FIELD       USING 'RESBD-LGORT(02)'
                                     RESBD-LGORT.
      PERFORM BDC_FIELD       USING 'RESBD-WERKS(02)'
                                     RESBD-WERKS.

      PERFORM BDC_DYNPRO      USING 'SAPLCOMD' '3910'.
      PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                    'WEIT'.
      if $ix < $i.
        PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
        PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                      'P+'.
      endif.
    ENDIF.


  ENDLOOP.

***S> 08/08/11 Paul : ECC6.0
*  PERFORM BDC_DYNPRO      USING 'SAPLCOMD' '3910'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                'WEIT'.
***

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-KTEXT'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                'FREI'.

  IF WA_ATP_CHECK EQ 'X'.
    PERFORM BDC_DYNPRO      USING 'SAPLSPO2' '0300'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=OPT1'.
  ENDIF.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'CAUFVD-KTEXT'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW31'
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
