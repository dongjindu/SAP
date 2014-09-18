*----------------------------------------------------------------------*
*   Datenbank-Tabelle VEDA                                             *
*----------------------------------------------------------------------*
TABLES:  VEDA,
        *VEDA,
         VEDAVB.

DATA: BEGIN OF COMMON PART VEDACOM.

* Konstanten
DATA: VEDA_POSNR_LOW LIKE VEDA-VPOSN.

* Alter Tabellenstand beim ändern
DATA:    BEGIN OF YVEDA OCCURS 1.
        INCLUDE STRUCTURE VEDAVB.
DATA:    END OF YVEDA.

* Aktueller Tabellenstand
DATA: BEGIN OF XVEDA OCCURS 5.
        INCLUDE STRUCTURE VEDAVB.
DATA: END OF XVEDA.


DATA: SVEDA_TABIX LIKE SY-TABIX,
      SVEDA_SUBRC LIKE SY-SUBRC.

* Tabellenkey
DATA:    BEGIN OF VEDAKEY,
           MANDT LIKE VEDA-MANDT,
           VBELN LIKE VEDA-VBELN,
           VPOSN LIKE VEDA-VPOSN,
         END OF VEDAKEY.

* Tabellenstand.
DATA: VEDA_PRUEFEN.
DATA: UPD_VEDA.

*
DATA: VEDA_VBELN LIKE VEDA-VBELN.
DATA: VEDA_VPOSN LIKE VEDA-VPOSN.
DATA: VEDA_VPROF LIKE TVAK-VPROF.

* Hauptposition.
DATA: BEGIN OF HVEDA.
        INCLUDE STRUCTURE VEDAVB.
DATA: END OF HVEDA.

* Kopf
DATA: BEGIN OF KVEDA.
        INCLUDE STRUCTURE VEDAVB.
DATA: END OF KVEDA.
DATA: BEGIN OF *KVEDA.
        INCLUDE STRUCTURE VEDAVB.
DATA: END OF *KVEDA.

* Kopieren von Kopf-Vertragsdaten auf Positionsebene
DATA: VEDACOPYDIALOG TYPE C,  "X = Es soll kein Dialog geführt werden
      VEDACOPYERROR TYPE C.

DATA: BEGIN OF LISTVEDA OCCURS 0.
        INCLUDE STRUCTURE V45W_NAC.
DATA:   SORT        TYPE C,
        MESSFLD(20) TYPE C,
        MESSTXT(80) TYPE C.
DATA: END OF LISTVEDA.

DATA: *VEDA_K LIKE VEDA.

DATA: END OF COMMON PART.
