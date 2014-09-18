REPORT ylexicographical_temp .
TABLES: tmcnv.

PARAMETERS: p_lexflg LIKE tmcnv-lexflag.

SELECT SINGLE * FROM tmcnv WHERE convid = 'MATCONV'.
IF sy-subrc NE 0.
  MESSAGE e000(zz) WITH text-m01.
ENDIF.

IF tmcnv-lexflag NE p_lexflg.
  IF p_lexflg EQ 'X'.
    MOVE: p_lexflg TO tmcnv-lexflag.
    MOVE: ''      TO tmcnv-flag0.
*  move: 40     to tmcnv-LMASKE.
*  move: 40     to tmcnv-ATRENN.
*  move: 40     to tmcnv-ATRENNX.
*  move: 40     to tmcnv-LNGEFF.
*  move: 40     to tmcnv-LNGEFF.
*  move: 40     to tmcnv-lmatnr_total.
    UPDATE tmcnv.
  ELSE.
    MOVE: p_lexflg TO tmcnv-lexflag.
    MOVE: ''      TO tmcnv-flag0.
*        18     to tmcnv-LMATNR,
*        18     to tmcnv-LNGEFF,
*        18     to tmcnv-lmatnr_total.
    UPDATE tmcnv.
  ENDIF.
  MESSAGE s000(zz) WITH text-m02.
ELSE.
  IF tmcnv-lexflag EQ 'X'.
    MESSAGE e000(zz) WITH text-m03.
  ELSE.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDIF.
