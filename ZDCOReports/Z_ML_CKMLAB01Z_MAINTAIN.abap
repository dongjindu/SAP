REPORT z_ml_ckmlab01z_maintain .

tables: CKMLAB01Z.

DATA:
          ckmlab01_wa   LIKE ckmlab01,
          ckmlab01z_wa  LIKE ckmlab01z,
          ckmlab01z_old LIKE ckmlab01z,
          ckmlmv009_wa  LIKE ckmlmv009,
          ckmlmv010_wa  LIKE ckmlmv010.

PARAMETERS:
          p_aschem  LIKE ckmlab01z-aschema OBLIGATORY,
          p_ptyp    LIKE ckmlab01z-ptyp OBLIGATORY,
          p_bewgrp  LIKE ckmlab01z-bewartgrp,
          p_katego  LIKE ckmlab01z-kategorie.
SELECTION-SCREEN BEGIN OF BLOCK func WITH FRAME.
PARAMETERS:
          p_insert  RADIOBUTTON GROUP main DEFAULT 'X',
          p_delete  RADIOBUTTON GROUP main.
SELECTION-SCREEN END OF BLOCK func.

AT SELECTION-SCREEN.
* Prüfungen, nur beim Hinzufügen
  IF p_insert = 'X'.
* pruefen Fortschreibungsschema
    SELECT SINGLE * FROM ckmlab01 INTO ckmlab01_wa
          WHERE aschema = p_aschem.
    IF sy-subrc NE 0.
      MESSAGE e102(om) WITH p_aschem.
    ENDIF.
* pruefen Prozesstyp
    IF p_ptyp = 'B++ ' OR
       p_ptyp = 'V++ '.
      MESSAGE e492(om) WITH p_ptyp.
    ENDIF.
    SELECT SINGLE * FROM ckmlmv009 INTO ckmlmv009_wa
          WHERE ptyp = p_ptyp.
    IF sy-subrc NE 0.
      MESSAGE e468(om) WITH p_ptyp.
    ENDIF.
* pruefen Bewegungsartengruppe(initial ist zulaessig)
    IF NOT p_bewgrp IS INITIAL.
      SELECT SINGLE * FROM ckmlmv010 INTO ckmlmv010_wa
            WHERE mlbwg = p_bewgrp.
      IF sy-subrc NE 0.
        MESSAGE e102(om) WITH p_bewgrp.
      ENDIF.
    ENDIF.
* prüfen Kategorie
    IF p_katego = 'VN' OR
       p_katego = 'VP' OR
       p_katego = 'ZU'.
* ok
    ELSE.
      MESSAGE e102(om) WITH p_katego.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  MOVE: p_aschem TO ckmlab01z_wa-aschema,
        p_ptyp   TO ckmlab01z_wa-ptyp,
        p_bewgrp TO ckmlab01z_wa-bewartgrp,
        p_katego TO ckmlab01z_wa-kategorie.
  IF p_insert = 'X'.
* einfuegen
    WRITE: 'Function: insert entry'.
    ULINE.

    INSERT INTO ckmlab01z VALUES ckmlab01z_wa.
    IF sy-subrc NE 0.
      WRITE: / 'No action done !!!'.
      WRITE: /32 'ASCHEMA',
              40 'PTYP',
              45 'BEWARTGRP'.
      WRITE: / 'Entry already exists with key:',
             32 ckmlab01z_wa-aschema,
             40 ckmlab01z_wa-ptyp,
             45 ckmlab01z_wa-bewartgrp.
    ELSE.
      WRITE: /32 'ASCHEMA',
              40 'PTYP',
              45 'BEWARTGRP',
              55 'KATEGORIE'.
      WRITE: / 'entry successfully inserted:',
              32 ckmlab01z_wa-aschema,
              40 ckmlab01z_wa-ptyp,
              45 ckmlab01z_wa-bewartgrp,
              55 ckmlab01z_wa-kategorie.
    ENDIF.
  ENDIF.

  IF p_delete = 'X'.
* loeschen
    WRITE: 'Function: delete entry'.
    ULINE.
    SELECT SINGLE * FROM ckmlab01z INTO ckmlab01z_old
          WHERE aschema   = ckmlab01z_wa-aschema
          AND   ptyp      = ckmlab01z_wa-ptyp
          AND   bewartgrp = ckmlab01z_wa-bewartgrp.
    IF sy-subrc = 0.
      DELETE ckmlab01z FROM ckmlab01z_wa.
      IF sy-subrc NE 0.
        WRITE: / 'No action done !!!'.
        WRITE: / 'Delete error for key:',
               ckmlab01z_wa-aschema,
               ckmlab01z_wa-ptyp,
                 ckmlab01z_wa-bewartgrp.
      ELSE.
        WRITE: /32 'ASCHEMA',
                40 'PTYP',
                45 'BEWARTGRP',
                55 'KATEGORIE'.
        WRITE: / 'entry successfully deleted:',
              32 ckmlab01z_wa-aschema,
              40 ckmlab01z_wa-ptyp,
              45 ckmlab01z_wa-bewartgrp,
              55 ckmlab01z_old-kategorie.
       ENDIF.
    ELSE.
      WRITE: / 'No action done !!!'.
      WRITE: /32 'ASCHEMA',
              40 'PTYP',
              45 'BEWARTGRP'.
      WRITE: / 'Entry not found with key:',
             32 ckmlab01z_wa-aschema,
             40 ckmlab01z_wa-ptyp,
             45 ckmlab01z_wa-bewartgrp.
    ENDIF.
  ENDIF.
