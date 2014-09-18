*& Report  ZKAKALX8                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZKAKALX8 LINE-SIZE 153.

* A short documentation for this report:
* This report checks the balance of CO internal cost flows
* on company code, business area or functional area level.
* For this purpose the report selects all CO line items which
* correspond to CO internal cost flows.
* The sum of all displayed balances
* should be zero in a controlling area since there are
* no allocations possible which are cross controlling area.
* If there are no cross company cost flows, then the
* flows should sum up to zero even in each company code.
* The report can be started to check one of the three balances
* (cross company code, cross business area or cross functional area).
* For this purpose there are the three radio buttons on the selection
* screen: XCOCDE, XBUSA and XFAREA.
* E.g.: If XBUSA = 'X', then the report summarizes all CO line items
* and displays the remaining balance on business area level.
* If there are no corrupted CO documents, then the sum of all displayed
* balances should be zero!
* The Flag XLOGS is switched on, then the report selects only those
* line items, where the logical system is space or equal to
* the logical system of the client (T000-LOGSYS).
* (only those line items are contained in the reconciliation ledger.

* The lists displayed by this report can be compared with the lists
* displayed by report RKAKALR2 (transaction KAL7).
* The balances should be the same as in the corresponding lists
* of report RKAKALR2.
* The lists displayed in report RKAKALR2 are based on
* reconciliation ledger data.
* In contrary the data displayed in this report are based on the
* CO line items, i.e. CO data.
* Consequence: If you detect any differences between the data displayed
* by RKAKALR2 and by this report, then there might be an inconsistency
* between reconciliation ledger and CO data.
* In this case please update the reconciliation ledger data using
* transaction KAL1.
* If this doesn't help, please contact SAP.

TABLES:
    COEP.

*------------- global data ---------------------------------------------
RANGES: LT_LOGSYS  FOR T000-LOGSYS.

DATA:
      LT_COEP  LIKE  COEP OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF LT_COEP_BALANCE OCCURS 0,
        FIELD(20),
        PARTNER_FIELD(20),
        WKGBTR LIKE  COEP-WKGBTR,
        WOGBTR LIKE  COEP-WOGBTR,
        WTGBTR LIKE  COEP-WTGBTR,
      END OF LT_COEP_BALANCE.

DATA:
      GD_CURSOR      TYPE  CURSOR,
      GS_TKA01       LIKE  TKA01.

*------------- selection screen ----------------------------------------
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK A.

PARAMETER:
  COAREA LIKE  COEP-KOKRS MEMORY ID CAC OBLIGATORY.

SELECT-OPTIONS:
  COCODE FOR COEP-BUKRS  MEMORY ID BUK.

PARAMETERS:
  YEAR   LIKE COEP-GJAHR  MEMORY ID GJR OBLIGATORY,
  PERIOD LIKE COEP-PERIO  MEMORY ID VPE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK A.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK B.
PARAMETERS:
  XCOCDE LIKE BOOLE-BOOLE DEFAULT 'X' RADIOBUTTON GROUP A,
  XBUSA  LIKE BOOLE-BOOLE             RADIOBUTTON GROUP A,
  XFAREA LIKE BOOLE-BOOLE             RADIOBUTTON GROUP A.

SELECTION-SCREEN SKIP 1.

PARAMETERS:
  XLOGS  LIKE BOOLE-BOOLE DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B.

*=======================================================================
INITIALIZATION.
*=======================================================================

  GET PARAMETER ID 'BUK' FIELD COCODE.
  GET PARAMETER ID 'GJR' FIELD YEAR.
  GET PARAMETER ID 'VPE' FIELD PERIOD.

*=======================================================================
AT SELECTION-SCREEN.
*=======================================================================

  SET PARAMETER ID 'BUK' FIELD COCODE.
  SET PARAMETER ID 'GJR' FIELD YEAR.
  SET PARAMETER ID 'VPE' FIELD PERIOD.

*=======================================================================
START-OF-SELECTION.
*=======================================================================

* get currency of controlling area:
  CALL FUNCTION 'K_KOKRS_READ'
       EXPORTING
            KOKRS   = COAREA
       IMPORTING
            E_TKA01 = GS_TKA01.


* activate message handler for collecting messages:
  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  IF ( XLOGS = 'X' ).
    LT_LOGSYS-SIGN = 'I'.
    LT_LOGSYS-OPTION = 'EQ'.
    LT_LOGSYS-LOW = SPACE.
    LT_LOGSYS-HIGH = SPACE.
    APPEND LT_LOGSYS.
    DATA: LD_LOGSYS  LIKE  T000-LOGSYS.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
         IMPORTING
              OWN_LOGICAL_SYSTEM             = LD_LOGSYS
         EXCEPTIONS
              OWN_LOGICAL_SYSTEM_NOT_DEFINED = 1.

    IF SY-SUBRC = 0.
      LT_LOGSYS-LOW = LD_LOGSYS.
      APPEND  LT_LOGSYS.
    ENDIF.
  ENDIF.

  OPEN CURSOR WITH HOLD GD_CURSOR FOR
  SELECT * FROM COEP
            WHERE  KOKRS    =  COAREA
            AND    BUKRS    IN COCODE
            AND    GJAHR    =  YEAR
            AND    PERIO    =  PERIOD
            AND    VERSN    =  '000'
            AND    WRTTP    =  '04'
            AND    PAROB1   <> SPACE
            AND    VRGNG    <> 'COIN'
            AND    VRGNG    <> 'COIE'
            AND    LOGSYSO  IN LT_LOGSYS
            AND    LOGSYSP  IN LT_LOGSYS
            ORDER BY PRIMARY KEY.

  REFRESH  LT_COEP_BALANCE.

  DO.
    REFRESH LT_COEP.
    FETCH NEXT CURSOR GD_CURSOR INTO TABLE LT_COEP
          PACKAGE SIZE '1000'.

    IF ( SY-SUBRC <> 0 ).
      CLOSE CURSOR GD_CURSOR.
      EXIT.
    ENDIF.

    LOOP AT  LT_COEP.
      MOVE-CORRESPONDING LT_COEP TO LT_COEP_BALANCE.
      IF ( XCOCDE = 'X' ).
        LT_COEP_BALANCE-FIELD = LT_COEP-BUKRS.
        LT_COEP_BALANCE-PARTNER_FIELD = LT_COEP-PBUKRS.
      ELSEIF ( XBUSA = 'X' ).
        LT_COEP_BALANCE-FIELD = LT_COEP-GSBER.
        LT_COEP_BALANCE-PARTNER_FIELD = LT_COEP-PARGB.
      ELSEIF ( XFAREA = 'X' ).
        LT_COEP_BALANCE-FIELD = LT_COEP-FKBER.
        LT_COEP_BALANCE-PARTNER_FIELD = LT_COEP-PFKBER.
      ENDIF.
      COLLECT LT_COEP_BALANCE.

*     check consistency of currencies:
      IF ( LT_COEP-TWAER   = LT_COEP-OWAER   AND
           LT_COEP-WTGBTR <> LT_COEP-WOGBTR )
      OR ( LT_COEP-TWAER   = GS_TKA01-WAERS  AND
           LT_COEP-WTGBTR <> LT_COEP-WKGBTR )
      OR ( LT_COEP-OWAER   = GS_TKA01-WAERS  AND
           LT_COEP-WOGBTR <> LT_COEP-WKGBTR ).
*       same currencies but different values:
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB = 'K5'
                  MSGTY = 'E'
                  MSGV1 = 'Inconsistency found in currencies and values'
                  MSGV2 = 'in CO line item'
                  MSGV3 = LT_COEP-BELNR
                  MSGV4 = LT_COEP-BUZEI
                  TXTNR = '001'.
      ENDIF.
    ENDLOOP.

  ENDDO.

  DELETE LT_COEP_BALANCE WHERE WKGBTR = 0.

*=======================================================================
END-OF-SELECTION.
*=======================================================================
  DATA:  LD_SUM  LIKE   COEP-WKGBTR.

  SORT LT_COEP_BALANCE BY FIELD PARTNER_FIELD WKGBTR.

  LOOP AT LT_COEP_BALANCE.
    WRITE: /1      '|',
            2(10)  LT_COEP_BALANCE-FIELD,
                   '|',
            14(10) LT_COEP_BALANCE-PARTNER_FIELD,
                   '|',
            26(20) LT_COEP_BALANCE-WKGBTR.
    LD_SUM = LD_SUM + LT_COEP_BALANCE-WKGBTR.
  ENDLOOP.

  IF ( LT_COEP_BALANCE[] IS INITIAL ).
    WRITE: / 'List contains no data'(100).
  ELSE.
    WRITE: / SY-ULINE.
    WRITE: /1      '|',
            2(10)  'SUMMARY',
            25(1)  '|',
            26(20)  LD_SUM.
  ENDIF.

  WRITE: / SY-ULINE.

* display messages if some appeared:
  CALL FUNCTION 'MESSAGES_SHOW'
       EXCEPTIONS
            NO_MESSAGES = 0.


*=======================================================================
TOP-OF-PAGE.
*=======================================================================

  WRITE: / 'The following list contains the balance <> 0 records:'.
  WRITE: /.

  WRITE: / SY-ULINE.
  DATA: LD_FIELD_TXT(20),
        LD_PFIELD_TXT(20).

  IF ( XCOCDE = 'X' ).
    LD_FIELD_TXT  = 'CO CODE'.
    LD_PFIELD_TXT = 'PCO CODE'.
  ELSEIF ( XBUSA = 'X' ).
    LD_FIELD_TXT = 'BUS AREA'.
    LD_PFIELD_TXT = 'PBUS AREA'.
  ELSEIF ( XFAREA = 'X' ).
    LD_FIELD_TXT = 'FAREA'.
    LD_PFIELD_TXT = 'PFAREA'.
  ENDIF.

  WRITE: /1        '|',
          2(10)     LD_FIELD_TXT,
          13       '|',
          14(10)   LD_PFIELD_TXT,
          25(1)    '|',
          26(20)   'BALANCE'(003).

  WRITE: / SY-ULINE.
