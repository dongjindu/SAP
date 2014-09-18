FUNCTION Z_FFI_GET_FM_ACTUAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(RYEAR) LIKE  FMIT-RYEAR
*"     REFERENCE(RFONDS) LIKE  FMIT-RFONDS
*"  TABLES
*"      OUT STRUCTURE  ZFI_FM_ACTUAL
*"----------------------------------------------------------------------
DATA : IT_OUT LIKE ZFI_FM_ACTUAL OCCURS 0  WITH HEADER  LINE.

SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUT
FROM FMIT
WHERE RYEAR  = RYEAR
AND   RFONDS = RFONDS.

LOOP AT IT_OUT.
  MOVE-CORRESPONDING IT_OUT TO OUT.
  APPEND OUT.
  CLEAR  OUT.
ENDLOOP.

ENDFUNCTION.
