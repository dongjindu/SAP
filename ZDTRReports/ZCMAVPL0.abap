REPORT zcmavpl0.
TABLES: fdsb, fdsr, t037, t001, fdes.

PARAMETERS: p_archv AS CHECKBOX.
SELECT-OPTIONS: s_archk FOR fdes-archk.

RANGES: r_bukrs FOR t001-bukrs.
r_bukrs-sign   = 'I'.
r_bukrs-option = 'EQ'.
SELECT * FROM t001 WHERE xfdis <> space.
  r_bukrs-low = t001-bukrs.
* necessary because rffdzz00 builds up only those company codes which
* are active for cash management (see source code of rffdzz00)
  APPEND r_bukrs.
ENDSELECT.
CHECK sy-subrc = 0.
SELECT * FROM t037.
  DELETE FROM fdsb WHERE ebene = t037-ebene AND bukrs IN r_bukrs.
  DELETE FROM fdsr WHERE ebene = t037-ebene AND bukrs IN r_bukrs.
ENDSELECT.

IF p_archv = 'X'.
  DELETE FROM fdes
    WHERE bukrs IN r_bukrs
      AND archk <> space
      AND archk IN s_archk.
ENDIF.

COMMIT WORK.

SUBMIT rffdzz00
       WITH s_bukrs IN r_bukrs   "not really necessary, does it anyhow
       WITH p_fdes = 'X'
       WITH p_chck = 'X'
       WITH p_test = space.
