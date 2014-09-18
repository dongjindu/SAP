REPORT zmreskzk .
*tables: mara.
INCLUDE fm03vcdt.

PARAMETERS: p_matnr LIKE mara-matnr OBLIGATORY.

SELECT SINGLE * FROM mara WHERE matnr = p_matnr.

MOVE-CORRESPONDING mara TO *mara.
IF mara-kzkfg NE space.
  mara-kzkfg = space.
  objectid = mara-matnr.
  tcode = sy-tcode.
  utime = sy-uzeit.
  udate = sy-datum.
  username = sy-uname.
  upd_mara = 'U'.
  PERFORM cd_call_material.

  COMMIT WORK.

  UPDATE mara.

  WRITE:/ 'Material', p_matnr, 'updated'.

ENDIF.

INCLUDE fm03vcdc.
