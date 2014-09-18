FUNCTION ZIM_CC_TAX_CALCULATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  CHANGING
*"     VALUE(ZTIDS) LIKE  ZTIDS STRUCTURE  ZTIDS
*"----------------------------------------------------------------------
DATA: W_ZFCUTAMT LIKE ZTIDS-ZFCUTAMT.

**> B/L HEADER SELECT.
*  SELECT SINGLE *
*         FROM ZTBL
*         WHERE   ZFBLNO  EQ ZTIDS-ZFBLNO.
*
**>> 통관수수료 COMPUTE.
*  CLEAR  : ZTIMIMG07.
*  SELECT SINGLE *  FROM ZTIMIMG07
*  WHERE  BUKRS     EQ   ZTIDS-BUKRS
**  AND    ZFPONC    EQ   ZTIDS-ZFPONC
**  AND    ZFITKD    EQ   ZTIDS-ZFITKD
*  AND    ZFCUT     EQ   ZTIDS-ZFCUT
*  AND    ZFAPLDT   EQ   ( SELECT MAX( ZFAPLDT )  FROM ZTIMIMG07
*                          WHERE  BUKRS   EQ  ZTIDS-BUKRS
**                          AND    ZFPONC  EQ  ZTIDS-ZFPONC
**                          AND    ZFITKD  EQ  ZTIDS-ZFITKD
*                          AND    ZFCUT   EQ  ZTIDS-ZFCUT ).
*  IF SY-SUBRC NE 0.
*     SELECT  *  FROM  ZTIMIMG07 UP TO 1 ROWS
*                WHERE BUKRS   EQ ZTIDS-BUKRS
*                AND   ZFCUT   EQ ZTIDS-ZFCUT
*                AND   ZFAPLDT EQ ( SELECT MAX( ZFAPLDT )
*                                   FROM   ZTIMIMG07
*                                   WHERE  BUKRS   EQ  ZTIDS-BUKRS
*                                   AND    ZFCUT   EQ  ZTIDS-ZFCUT ).
*     ENDSELECT.
*  ENDIF.
*
*  IF SY-SUBRC EQ 0.
*     ZTIDS-ZFCUTAMT = ZTIMIMG07-ZFIRATE * ZTIDS-ZFTBAK.
*
**>> 통관수수료 원단위 절사 KYK 2001.12.27
*     W_ZFCUTAMT = TRUNC( ZTIDS-ZFCUTAMT ).
*     ZTIDS-ZFCUTAMT = TRUNC( W_ZFCUTAMT / 10 ) * 10.
*
*     IF ZTIDS-ZFCUTAMT < ZTIMIMG07-ZFMNAMT.
*        MOVE ZTIMIMG07-ZFMNAMT TO ZTIDS-ZFCUTAMT.
*     ENDIF.
*     IF ZTIDS-ZFCUTAMT > ZTIMIMG07-ZFMXAMT.
*        MOVE ZTIMIMG07-ZFMXAMT TO ZTIDS-ZFCUTAMT.
*     ENDIF.
*
**> 무환일 경우만.. 정액금액 적용함. 2001.12.05. KSB MODIFY...
*     IF ZTBL-ZFPOYN EQ 'N'.
*        IF ZTIMIMG07-ZFSAMT > 0.
*           MOVE ZTIMIMG07-ZFSAMT TO ZTIDS-ZFCUTAMT.
*        ENDIF.
*     ENDIF.
*  ELSE.
*    MESSAGE  S562.
*  ENDIF.

ENDFUNCTION.
