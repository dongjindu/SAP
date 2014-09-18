* INCLUDE ZRAKOPL02_SEL .

*----------------------------------------------------------------------*
*   INCLUDE T_RAKOPL02_SEL                                             *
*----------------------------------------------------------------------*
* Planungszeitraum.
SELECTION-SCREEN BEGIN OF BLOCK BL0 WITH FRAME TITLE TEXT-BL0.
* GJ der Planung.
PARAMETERS: PA_GSJHR LIKE ANLC-GJAHR OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-SS1.
*   Planung-von-Periode.
PARAMETERS: PA_VONPE LIKE RKU01G-PERAB.
SELECTION-SCREEN COMMENT 52(05) TEXT-SS2.
*   Planung-bis-Periode.
PARAMETERS: PA_BISPE LIKE RKU01G-PERBI.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK BL0.

SELECTION-SCREEN SKIP 1.

* Weitere Selektionen
SELECTION-SCREEN BEGIN OF BLOCK BL1
                 WITH FRAME
                 TITLE TEXT-BL1.
SELECT-OPTIONS:
*                 Kostenrechnungskreis.
                SO_KOKRS FOR TKA01-KOKRS,
*                 Leistungsart.
*                 SO_LSTAR FOR ANLAV-LSTAR.                "ukostl
                SO_LSTAR FOR ANIA-LSTAR.                 "ukostl
SELECTION-SCREEN END   OF BLOCK BL1.

SELECTION-SCREEN SKIP 1.

* Weitere Einstellungen zur Planung.
SELECTION-SCREEN BEGIN OF BLOCK BL2
                 WITH FRAME
                 TITLE TEXT-BL2.
PARAMETERS:
*             Planungsversion.
            PA_VERSI LIKE CCSS-VERSN DEFAULT '000' OBLIGATORY,
*             Simulationsvariante
            PA_SIMVR LIKE RBADA-SIMVAR,
*             Verteilungsschlüssel.
            PA_FCWKG LIKE RGPLN-1SPRED,
*             Belegtext bei Übergabe der Kosten an's CO.
            PA_BLTXT LIKE COBK-BLTXT,
*             Kennzeichen: Planung leistungsartenunabhängig.
            PA_LSTAU LIKE RAREP-XLSTAU DEFAULT 'X',
*             Kennzeichen: Planung leistungsartenabhängig.
            PA_LSTAR LIKE RAREP-XLSTAR,
*             Kennzeichen: Zinsen uebergeben.
            PA_UZINS LIKE RAREP-UZINS DEFAULT 'X',
*             Kennzeichen: Planung auf Kostenstelle.
*           Kennzeichen: Sonder-AfA uebergeben
            pa_safa  TYPE xfeld AS CHECKBOX,
*             pa_plkst like rarep-xkstko default 'X',           "Frank
            PA_PLKST LIKE RAREP-XKSTKO DEFAULT 'X',           "Frank
*             Kennzeichen: Planung auf Auftrag (Default).       "Frank
            PA_PLAUF LIKE RAREP-XAUFKO,                       "Frank
*           Kennzeichen: Planung auf PSP-Element.
            pa_plpsp LIKE rarep-xpspko,
*           Kennzeichen: Planung auf Immobilienobjekt.     " n. 1346996
            pa_plimm LIKE rarep-ximmko,                    " n. 1346996
*             Simulieren nur bis geplanter Abgang
            PA_XGPLA LIKE ANLA0-XGPLA DEFAULT ' '.
SELECTION-SCREEN SKIP 1.
*             Kennzeichen: Testlauf.
PARAMETERS:   PA_TESTL LIKE RAREP-TESTLAUF DEFAULT 'X',
*             Kennzeichen: Summenbericht.
              PA_SUMMB LIKE RAREP-SUMMB.

* << Start addition on 10.17 by Michelle
SELECTION-SCREEN SKIP 1.
PARAMETERS:   PA_TAB   AS CHECKBOX.
* End addition on 10.17 by Michelle >>

SELECTION-SCREEN END   OF BLOCK BL2.

*---------------------------------------------------------------alv(beg)
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-BL3.
PARAMETERS: P_VARI TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BL3.
*---------------------------------------------------------------alv(end)
