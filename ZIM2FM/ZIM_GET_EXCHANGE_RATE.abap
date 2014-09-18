FUNCTION ZIM_GET_EXCHANGE_RATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_WAERS) LIKE  TCURC-WAERS
*"     REFERENCE(P_DATE) LIKE  SY-DATUM
*"     REFERENCE(P_KURST) LIKE  TCURR-KURST DEFAULT 'M'
*"     REFERENCE(P_TO_WAERS) LIKE  TCURC-WAERS DEFAULT 'KRW'
*"  EXPORTING
*"     VALUE(P_EXRT) LIKE  ZTBL-ZFEXRT
*"     VALUE(P_FFACT) LIKE  TCURF-FFACT
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NO_INPUT
*"----------------------------------------------------------------------


*DATA: L_CDATE    LIKE TCURR-GDATU,
*      L_WAERS    LIKE TCURC-WAERS,
*      L_KURST    LIKE TCURR-KURST.
*
*
*  IF P_WAERS IS INITIAL.
*     RAISE NO_INPUT.
*  ELSE.
*     SELECT SINGLE WAERS INTO L_WAERS
*                         FROM TCURC
*                         WHERE WAERS EQ P_WAERS.
*     IF SY-SUBRC NE 0.
*        RAISE NOT_FOUND.
*     ENDIF.
*  ENDIF.
** 환율테이블의 날짜포맷으로 변환
*  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
*       EXPORTING
*                INPUT     =     P_DATE
*       IMPORTING
*                OUTPUT    =     L_CDATE.
*
*  IF P_KURST IS INITIAL.
*     L_KURST = 'M'.
*  ELSE.
*     L_KURST = P_KURST.
*  ENDIF.
*
*  CLEAR : TCURR, TCURF.
*  SELECT SINGLE UKURS INTO TCURR-UKURS   "* Exchange rate
*         FROM  TCURR
*         WHERE KURST    EQ     L_KURST
**        AND   FCURR    EQ     'KRW'
**        AND   TCURR    EQ     P_WAERS
*         AND   FCURR    EQ     P_WAERS
*         AND   TCURR    EQ     P_TO_WAERS
*         AND   GDATU    GE     L_CDATE.
*
*  SELECT SINGLE TFACT INTO TCURF-TFACT   "* Ratio for the "to" currency
*         FROM  TCURF
*         WHERE KURST    EQ     L_KURST
**        AND   FCURR    EQ     'KRW'
**        AND   TCURR    EQ     P_WAERS
*         AND   FCURR    EQ     P_WAERS
*         AND   TCURR    EQ     P_TO_WAERS
*         AND   GDATU    GE     L_CDATE.
*
*  P_EXRT  =     TCURR-UKURS.
*  P_FFACT =     TCURF-FFACT / TCURF-TFACT.
**  P_EXRT  =     TCURR-UKURS   *    TCURF-TFACT.
*
DATA : L_TEXT_EXRATE(255)    TYPE C,
       L_FOREIGN_FACTOR(255) TYPE C,
       L_FACTOR              TYPE P,
       W_FIXED_RATE          LIKE TCURS-SPRED.

  IF P_WAERS EQ P_TO_WAERS.
     P_FFACT = 1.
     P_EXRT  = 1.
     EXIT.
  ENDIF.
*>>>
  CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
              DATE             = P_DATE
              FOREIGN_AMOUNT   = 0
              FOREIGN_CURRENCY = P_WAERS
              LOCAL_CURRENCY   = P_TO_WAERS
              TYPE_OF_RATE     = P_KURST
        IMPORTING
              EXCHANGE_RATE    = L_TEXT_EXRATE
              FOREIGN_FACTOR   = L_FOREIGN_FACTOR
*              LOCAL_AMOUNT     = W_LOCAL_AMT
              FIXED_RATE       = W_FIXED_RATE
        EXCEPTIONS
              OTHERS           = 01.

  IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING NOT_FOUND.
  ENDIF.

  PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01) CHANGING  L_TEXT_EXRATE.
  PERFORM    P2000_WRITE_NO_MASK(SAPMZIM01) CHANGING  L_FOREIGN_FACTOR.

  P_EXRT   = L_TEXT_EXRATE.
  L_FACTOR = L_FOREIGN_FACTOR.
  P_FFACT  = L_FACTOR.

  IF P_EXRT LT 0.
     P_EXRT = P_EXRT * -1.
  ENDIF.

ENDFUNCTION.
