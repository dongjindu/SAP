*&---------------------------------------------------------------------*
*& Report  ZGHRDACTION
*&
*&---------------------------------------------------------------------*
REPORT  ZGHRDACTION.

TABLES:
            PRELP ,   RP50D.


PERFORM SET_ZGHR9883.
*&---------------------------------------------------------------------*
*&      Form  SET_ZGHR9883
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ZGHR9883 .

  DATA:
                LS_PA0001   TYPE PA0001,
                L_NULL,
                L_ENDDA       TYPE DATUM,
                LS_PA9883   TYPE PA9883,
                LS_HRP9870  TYPE HRP9870.


  SELECT SINGLE * FROM PA0001
    INTO LS_PA0001
    WHERE PERNR   = PRELP-PERNR
          AND SUBTY   = PRELP-SUBTY
          AND OBJPS   = PRELP-OBJPS
          AND SPRPS   = PRELP-SPRPS
          AND ENDDA  = PRELP-ENDDA
*          AND BEGDA = PRELP-BEGDA
          AND SEQNR   = PRELP-SEQNR.

*1. [GHRIS] Group decision
*   PERSG = 9  Expatriate(E),
* others  HCN

  IF LS_PA0001-PERSG EQ '9'.
    RP50D-ZZCGSJBGRP = 'E'.
  ELSE.
    RP50D-ZZCGSJBGRP = '1'.
  ENDIF.

* 2. [GHRIS] TRACK,  GRADE decision
*
*1) PA0001-PLANS read HRP9870
*      - GHRIS Track = HRP9870-TRACK,
*        GHRIS Grade = HRP9870-GRADE

*     . HRP9870-TRACK = 1 (Managerial, R&D)   GHRIS Track M
*                                   =  2 (Technical)  GHRIS Track T
*
*     . HRP9870-GRADE = G1~G5  GHRIS Grade MG1~MG5)
*          GHRIS Track =T  G1~G3  GHRIS Grade TG1~TG3

* If Infortype 0001's Job is Group Leader skip.

  IF LS_PA0001-PLANS IS NOT INITIAL AND LS_PA0001-STELL NE '10000968' .

    SELECT SINGLE * FROM HRP9870 INTO LS_HRP9870
      WHERE PLVAR  = '01'
            AND OTYPE = 'S'
            AND   OBJID = LS_PA0001-PLANS
            AND   ISTAT = '1'
            AND   BEGDA <= LS_PA0001-ENDDA
            AND   ENDDA >= LS_PA0001-BEGDA.

    CASE LS_HRP9870-TRACK .
      WHEN '1' .
        RP50D-ZZCGSJIKUN = 'M'.
        CONCATENATE 'M' LS_HRP9870-GRADE INTO RP50D-ZZCGSJIKUB.
      WHEN  '2'.
        RP50D-ZZCGSJIKUN = 'T'.
        CONCATENATE 'T' LS_HRP9870-GRADE INTO RP50D-ZZCGSJIKUB.
    ENDCASE.

    IF RP50D-ZZCGSJIKUN IS NOT INITIAL AND RP50D-ZZCGSJIKUB IS NOT INITIAL.
      RETURN.
    ENDIF.

  ENDIF.

*2) Determined on the basis of STELL.

  CASE LS_PA0001-STELL.
    WHEN '10001141' OR '90000039'.
      RP50D-ZZCGSJIKUN = 'M'.
      RP50D-ZZCGSJIKUB = 'MG1'.
    WHEN '90000312' OR '90005178' OR '90005179' OR '10011142'.
      RP50D-ZZCGSJIKUN = 'M'.
      RP50D-ZZCGSJIKUB = 'MG2'.
    WHEN '90000656'  OR '10001143'.
      RP50D-ZZCGSJIKUN = 'M'.
      RP50D-ZZCGSJIKUB = 'MG3'.
    WHEN '90000269' OR '90000657'  OR '10001144' OR '10015731'.
      RP50D-ZZCGSJIKUN = 'M'.
      RP50D-ZZCGSJIKUB = 'MG4'.
    WHEN '90000040' OR '90000658' OR '90000659' OR '90000661' OR '90002147'  OR '10032613'.
      RP50D-ZZCGSJIKUN = 'C'.
      RP50D-ZZCGSJIKUB = 'MG5'.
    WHEN '10000969' OR '10000970' .
      RP50D-ZZCGSJIKUN = 'T'.
      RP50D-ZZCGSJIKUB = 'TG1'.
    WHEN '10001477' .
      RP50D-ZZCGSJIKUN = 'T'.
      RP50D-ZZCGSJIKUB = 'TG2'.
    WHEN '10000968' .
      RP50D-ZZCGSJIKUN = 'T'.
      RP50D-ZZCGSJIKUB = 'TG3'.
    WHEN OTHERS.
  ENDCASE.

* 3) Fondness else is null.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = PRELP-BEGDA
      DAYS      = 1
      MONTHS    = 0
      SIGNUM    = '-'
      YEARS     = 0
    IMPORTING
      CALC_DATE = L_ENDDA.

  SELECT SINGLE * FROM PA9883
    INTO LS_PA9883
    WHERE PERNR   = PRELP-PERNR
          AND SUBTY   = PRELP-SUBTY
          AND OBJPS   = PRELP-OBJPS
          AND SPRPS   = PRELP-SPRPS
          AND ENDDA  = L_ENDDA
          AND SEQNR   = PRELP-SEQNR.

  IF RP50D-ZZCGSJIKUN IS INITIAL.

    IF SY-SUBRC EQ 0.
      RP50D-ZZCGSJIKUN = LS_PA9883-ZZCGSJIKUN.
      RP50D-ZZCGSJIKUB = LS_PA9883-ZZCGSJIKUB.
    ENDIF.

  ENDIF.

*Expatriate, MG3, MG4, MG5, who agreed to accept in the past, set the agree.
*
*  IF rp50d-zzcgsjbgrp EQ 'E'..
*    rp50d-zzcgsagree = 'X'.
*  ENDIF.
*
*  IF rp50d-zzcgsjikub EQ 'MG3' OR
*    rp50d-zzcgsjikub EQ 'MG4' OR
*    rp50d-zzcgsjikub EQ 'MG5' .
*
*    rp50d-zzcgsagree = 'X'.
*
*  ENDIF.
*
*  IF rp50d-zzcgsagree IS INITIAL AND ls_pa9883-zzcgsagree EQ 'X'.
*    rp50d-zzcgsagree = 'X'.
*  ENDIF.
ENDFORM.                    " SET_ZGHR9883
