************************************************************************
*                                                                      *
* Include RFFORI0M der Zahlungsträgerprogramme für Makrodefinitionen   *
*                                                                      *
* AUSWAHL   Aufbau einer Zeile im Selektionsbild zur Wahl der Ausgabe  *
* BLOCK     Beginn eines Blocks auf dem Selektionsbild                 *
*                                                                      *
************************************************************************



*----------------------------------------------------------------------*
* Tabelle zum Merken der Felder der Auswahl / Feldsymbol zum Füllen    *
*----------------------------------------------------------------------*
DATA:
  BEGIN OF TAB_SELFIELDS OCCURS 12,
    FIELD(8) TYPE C,
    TEXT(3)  TYPE N,
  END OF TAB_SELFIELDS.

FIELD-SYMBOLS <SELFIELD>.




*----------------------------------------------------------------------*
* Makro, das eine Zeile zur Auswahl der Ausgabe (Zahlungsträger, DTA,  *
* Avis, Begleitliste) mit Drucker und Sofortdruck aufs Selektionsbild  *
* bringt und die Felder für ein Füllen in Routine INIT merkt.          *
*----------------------------------------------------------------------*
DEFINE AUSWAHL.

  SELECTION-SCREEN:
    BEGIN OF LINE.
  PARAMETERS:
    PAR_&1 LIKE RFPDO-FORD&1.
  SELECTION-SCREEN:
    COMMENT 03(28) TEXT&1 FOR FIELD PAR_&1,
    COMMENT POS_LOW(10) TEXTPRI&2 FOR FIELD PAR_PRI&2.
  PARAMETERS:
    PAR_PRI&2 LIKE RFPDO-FORDPRI&2 VISIBLE LENGTH 11.
  SELECTION-SCREEN:
    POSITION POS_HIGH.
  PARAMETERS:
    PAR_SOF&2 LIKE RFPDO1-FORDSOF&2.
  SELECTION-SCREEN:
    COMMENT 60(18) TEXTSOF&2 FOR FIELD PAR_SOF&2,
    END OF LINE.
*---
  TAB_SELFIELDS-FIELD = 'TEXT&1'.

  CASE TAB_SELFIELDS-FIELD+4.
    WHEN 'ZDRU'. TAB_SELFIELDS-TEXT  = 101.      "Zahlungsträger drucken
    WHEN 'WDRU'. TAB_SELFIELDS-TEXT  = 103.      "Wechsel drucken
    WHEN 'XDTA'. TAB_SELFIELDS-TEXT  = 104.      "Datenträgeraustausch
    WHEN 'AVIS'. TAB_SELFIELDS-TEXT  = 105.      "Avis ausgeben
    WHEN 'BEGL'. TAB_SELFIELDS-TEXT  = 106.      "Begleitliste drucken
  ENDCASE.
  APPEND TAB_SELFIELDS.
  TAB_SELFIELDS-FIELD = 'TEXTPRI&2'.
  TAB_SELFIELDS-TEXT  = 107.                     "auf Drucker
  APPEND TAB_SELFIELDS.
  TAB_SELFIELDS-FIELD = 'TEXTSOF&2'.
  TAB_SELFIELDS-TEXT  = 108.                     "Sofortdruck
  APPEND TAB_SELFIELDS.

END-OF-DEFINITION.


*----------------------------------------------------------------------*
* Macro zur Definition eines Paramters zur Druckberechtigungsvergabe   *
*----------------------------------------------------------------------*
DEFINE SPOOL_AUTHORITY.

* selection-screen: skip 1,
*                   begin of line,
*                   comment 01(31) textauth for field par_auth.
* parameters par_auth like itcpo-tdautority.
* selection-screen end of line.

* tab_selfields-field = 'TEXTAUTH'.
* tab_selfields-text = 109.
* append tab_selfields.

 PARAMETERS PAR_AUTH LIKE ITCPO-TDAUTORITY NO-DISPLAY.

END-OF-DEFINITION.


*----------------------------------------------------------------------*
* Makro, das einen Frame mit angegebener Nummer startet und den Text   *
* zum Füllen in Routine INIT merkt.                                    *
*----------------------------------------------------------------------*
DEFINE BLOCK.

  SELECTION-SCREEN BEGIN OF BLOCK &1 WITH FRAME TITLE BLOCK00&1.

  TAB_SELFIELDS-FIELD = 'BLOCK00&1'.
  TAB_SELFIELDS-TEXT  = 90&1.                    "Blocktext
  APPEND TAB_SELFIELDS.

END-OF-DEFINITION.
