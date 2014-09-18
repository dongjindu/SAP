*----------------------------------------------------------------------*
*   INCLUDE T_RAKOPL02_DATA                                            *
*----------------------------------------------------------------------*
*TABLES: ANLAV.
*        ANLA0,
*        *ANLA0,
*        ANLZ,
*        ANLB,
*        ANLCV.
*TABLES: ANEK, ANEPV.                                       "perf
*TABLES: ANLP.                                              " n. 304202


*TABLES: TKA01,
*        TKA09,
*        TABWV,
*        T095B,
*        T093B,
*        T093D,
*        T093T,
*        T090,                                    "corni
*        T090C,                                   "corni
*        CSKT,
*        CSLT,
*        CSKU,
*        CSKS,
*        AUFKV,
*        TGSBT,
*        PROJ,
*        T001,
*        RKU01JA,
*        RKU01G,
*        RKU01_CUR,
*        RAREP,
*        ANIA,                                              "lstar
*        T093A,
*        T093C.
*
*TABLES: AUFK, PRPS, IMTP, IMPR.
*
** Report-Haeder (Titel und Bewertungsbereich).
*DATA: BEGIN OF HEAD_A,
**       Report-Titel.
*        TITLE LIKE SY-TITLE,
**       Information 1.
*        SPAC1(1)  TYPE C,
*        INFO1(50) TYPE C,
**       Information 2.
*        SPAC2(1)  TYPE C,
*        INFO2(50) TYPE C,
**       Information 3.
*        SPAC3(1)  TYPE C,
*        INFO3(50) TYPE C,
**       Information 4.
*        SPAC4(1)  TYPE C,
*        INFO4(50) TYPE C,
**       Information 5.
*        SPAC5(1)  TYPE C,
*        INFO5(50) TYPE C,
**       Information 6.
*        SPAC6(1)  TYPE C,
*        INFO6(50) TYPE C,
*      END OF HEAD_A.
*
** Report-Haeder (Perioden, Echtlauf)
*DATA: BEGIN OF HEAD_B,
**       Report-Titel.
*        TITLE LIKE SY-TITLE,
**       Information 1.
*        SPAC1(1)  TYPE C,
*        INFO1(50) TYPE C,
**       Information 2.
*        SPAC2(1)  TYPE C,
*        INFO2(50) TYPE C,
**       Information 3.
*        SPAC3(1)  TYPE C,
*        INFO3(50) TYPE C,
**       Information 4.
*        SPAC4(1)  TYPE C,
*        INFO4(50) TYPE C,
**       Information 5.
*        SPAC5(1)  TYPE C,
*        INFO5(50) TYPE C,
**       Information 6.
*        SPAC6(1)  TYPE C,
*        INFO6(50) TYPE C,
*      END OF HEAD_B.
*
**-----------------------------------------------------"ins(beg) n.
*168475
** Tabellen für die maximalen Gültigkeitsintervalle         " n. 168475
** von Kostenstellen/Innenaufträgen.                        " n. 168475
*DATA: BEGIN OF T_KOSTL_G OCCURS 100,                       " n. 168475
*        KOKRS LIKE TKA01-KOKRS,                            " n. 168475
*        KOSTL LIKE ANLP-KOSTL,                             " n. 168475
*        PERAB LIKE ANLP-PERAF,                             " n. 168475
*        PERBI LIKE ANLP-PERAF,                             " n. 168475
*      END   OF T_KOSTL_G.                                  " n. 168475
*DATA: BEGIN OF T_CAUFN_G OCCURS 100,                       " n. 168475
*        KOKRS LIKE TKA01-KOKRS,                            " n. 168475
*        CAUFN LIKE ANLP-CAUFN,                             " n. 168475
*        PERAB LIKE ANLP-PERAF,                             " n. 168475
*        PERBI LIKE ANLP-PERAF,                             " n. 168475
*      END   OF T_CAUFN_G.                                  " n. 168475
**-----------------------------------------------------"ins(end) n.
*169475
*
** Sammeltabelle für Genehmigungsjahre von InvProgrammen (RASORT09).
*DATA: BEGIN OF TAB_GNJ OCCURS 0,
*        GJAHR LIKE IMTP-GJAHR,
*      END   OF TAB_GNJ.
*
** Tabelle der Stückzahl/Leistungs-AfA-Schlüssel.     "corni
*DATA: BEGIN OF TAB_STCKAFA OCCURS 0,                 "corni
*        AFASL      LIKE T090C-AFASL,                 "corni
*      END   OF TAB_STCKAFA.                          "corni
*
** Key T095B.
*DATA: BEGIN OF KEY_T095B,
*        KTOPL LIKE T095B-KTOPL,
*        KTOGR LIKE T095B-KTOGR,
*        AFABE LIKE T095B-AFABE,
*      END OF KEY_T095B.
*
** Interne Tabelle fuer Kostensammlung pro Anlage.
*DATA: BEGIN OF KOTAB OCCURS 24,
**       Kostenart.
*        KOART    LIKE T095B-KTNAFB,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
**       Kostenstelle.                                      "ukostl
*        KOSTL    LIKE ANLAV-KOSTL,                         "ukostl
**       Innenauftrag.                                      "Frank
*        CAUFN    LIKE ANLAV-CAUFN,                         "Frank
**       Leistungsart.                                      "ukostl
*        LSTAR    LIKE ANLAV-LSTAR,                         "ukostl
**       Gesamte Kosten.
*        GKOST    LIKE RKU01G-WTGBTR,
**       Davon fix.
*        FKOST    LIKE RKU01G-WTGBTR,
*      END OF KOTAB.
*
** Tabelle fuer FB-Call 'K_COSTS_PLAN_INTERFACE_PERIOD'.
*DATA: T_RKU01JA LIKE RKU01JA OCCURS 0 WITH HEADER LINE.
** Tabelle fuer FB-Call 'K_COSTS_PLAN_INTERFACE_TOTAL'.
*DATA: T_RKU01G  LIKE RKU01G  OCCURS 0 WITH HEADER LINE.
** Periode der Altdatenübernahme.                           " n. 440555
*DATA: HLP_ALTD_BUPER  LIKE ANEP-PERAF,                     " n. 440555
*      HLP_ALTD_GJAHR  LIKE ANLC-GJAHR.                     " n. 440555
*
** Hilfsgroessen fuer FB-Call 'RA_AFABUCHEN'.
*DATA: X093D     LIKE T093D   OCCURS 1 WITH HEADER LINE.
*DATA: XANLA     LIKE ANLA.
*DATA: XANLZ     LIKE ANLZ    OCCURS 1 WITH HEADER LINE.
*DATA: XANLB     LIKE ANLB    OCCURS 1 WITH HEADER LINE.
**DATA: XANEP    LIKE ANEP    OCCURS  1 WITH HEADER LINE.       "n.
*448567
**DATA: BEGIN OF XANEP OCCURS 0.                                 "n.
*448567
**        INCLUDE       STRUCTURE ANEP.                          "n.
*448567
**DATA:   MONAT         LIKE      ANEK-MONAT,                    "n.
*448567
**      END OF XANEP.                                            "n.
*448567
*DATA: XANEA     LIKE ANEA    OCCURS 1 WITH HEADER LINE.    "perf
*DATA: XANLP     LIKE ANLP    OCCURS 1 WITH HEADER LINE.
*DATA: XANLC     LIKE ANLC    OCCURS 1 WITH HEADER LINE.
*DATA: XANFM     LIKE ANFM    OCCURS 1 WITH HEADER LINE.
** Merker für Segmente ANLAV/ANLB/ANLCV für GET ANLAV LATE. "perf
*DATA: ZANLAV    LIKE ANLAV,                                "perf
*      ZANLB     LIKE ANLB,                                 "perf
*      ZANLCV    LIKE ANLCV.                                "perf
*
*DATA:
**     GJ, in dem das Berichtsdatum liegt.
**     PA_GSJHR       LIKE T009B-BDATJ,
**     Erster/letzter Kalendertag des GJ.
*      SAV_GJBEG       LIKE SY-DATUM,
*      SAV_GJEND       LIKE SY-DATUM,
**     Erste/letzte Periode des GJ.
*      SAV_GJBEGPER    LIKE ANLP-PERAF,
*      SAV_GJENDPER    LIKE ANLP-PERAF,
**     Leitbereich zu BEREICH1, wenn BEREICH1 ein
**     paralleler Bereich im FI-AA ist.
*      SAV_PLL_LEADING_AREA LIKE T093D-AFABER,
**     Letzter Tag der Bis-Periode.
*      SAV_BISPEEND    LIKE SY-DATUM,
**     Waehrungsschluessel.
*      SAV_WAERS       LIKE T093B-WAERS,
**     Anzahl Perioden des GJ.
*      SAV_ANZPER(3)   TYPE N,
**     Alggemeines Hilfsfeld fuer Rechnungen.
*      CNT_COUNT(8)    TYPE P,
**     Hilfsfeld fuer 1. Periode wegen Typenkonsistenz in FB.
*      CON_POPER       LIKE T009B-POPER VALUE '001',
**     Bereichstyp des CO-Bereiches (siehe Festwerte Domäne BERTYP).
*      CON_BERTYP_CO   LIKE T093A-BERTYP,
**     Zugangs-BWA.                                         "anlz
*      CON_ZUGANG_BWA  LIKE TABW-BWASL,                     "anlz
**     Old-Felder.
*      OLD_OBART       LIKE ANLAV-OBART,
*      OLD_KOART       LIKE T095B-KTNAFB,
**     Flag: Kein Nachlesen auf der Datenbank beim AfA-Rechnen.
*      FLG_NO_DB(1)    TYPE C,
**     Flag: PickUp-fähige Zeile in der Liste.              "msg
*      FLG_PICKUP(1)   TYPE C,                              "msg
**     Flag: Fehelrprotokoll wird gerade ausgegeben.        "msg
*      FLG_ERRPROT(1)  TYPE C,                              "msg
**     Flag: ANEPs werden mitgegeben.
**     FLG_XANEP(1)    TYPE C,                              "perf
**     Kostenstelle 'Blank'.
*      CON_INITKOSTL   LIKE CSKS-KOSTL  VALUE '??????????',
**     Innenauftrag 'Blank'.
*      CON_INITCAUFN   LIKE AUFKV-AUFNR VALUE '????????????',
**     Hilfsueberschriften.
*      UEBS_052(20)    TYPE C,
*      UEBS_053(20)    TYPE C,
*      UEBS_201(3)     TYPE C,
*      UEBS_203(16)    TYPE C,
**     Hilfsfeld für Buchungskreis.
*      HLP_BUKRS       LIKE T093C-BUKRS,
** Deklarationen überflüssiger Variablen auskommentiert
*"no514310
**      HLP_GPLAB_BUPER LIKE ANEP-PERAF,
*"no514310
**      HLP_GPLAB_GJAHR LIKE ANLC-GJAHR,
*"no514310
**     Schalter An/Aus.
*      ON(1)           TYPE C VALUE '1',
*      OFF(1)          TYPE C VALUE '0'.

*FIELD-GROUPS: HEADER, DATEN.
*
** Key Summentabelle.
*DATA: BEGIN OF KEY_SUM,
**       Gruppenstufentyp.
*        STUFE(5) TYPE C,
**       Kostenrechnungskreis.
*        KOKRS    LIKE TKA01-KOKRS,
**       Buchungskreis.
*        BUKRS    LIKE ANLAV-BUKRS,
**       Geschaeftsbereich.
*        GSBER    LIKE ANLAV-GSBER,
**       Kostenstelle.
*        KOSTL    LIKE ANLAV-KOSTL,
**       Innenauftrag.
*        CAUFN    LIKE ANLZ-CAUFN,
**       Leistungsart.
*        LSTAR    LIKE ANLAV-LSTAR,
**       Kostenart.
*        KOART    LIKE T095B-KTNAFB,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
*      END OF KEY_SUM.
*
** Summentabelle.
*DATA: BEGIN OF SUM OCCURS 0,
**       Gruppenstufentyp.
*        STUFE(5) TYPE C,
**       Kostenrechnungskreis.
*        KOKRS    LIKE TKA01-KOKRS,
**       Buchungskreis.
*        BUKRS    LIKE ANLAV-BUKRS,
**       Geschaeftsbereich.
*        GSBER    LIKE ANLAV-GSBER,
**       Kostenstelle.
*        KOSTL    LIKE ANLAV-KOSTL,
**       Innenauftrag.
*        CAUFN    LIKE ANLZ-CAUFN,
**       Leistungsart.
*        LSTAR    LIKE ANLAV-LSTAR,
**       Kostenart.
*        KOART    LIKE T095B-KTNAFB,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
**       Gesamte Kosten.
*        GKOST    LIKE RKU01G-WTGBTR,
**       Davon fix.
*        FKOST    LIKE RKU01G-WTGBTR,
*      END OF SUM.
*
** Hilfstabelle wie SUM.
*DATA: BEGIN OF HLP_SUM OCCURS 0.
*        INCLUDE STRUCTURE SUM.
*DATA: END OF HLP_SUM.
*
** Strukturierung des Extract-Datenbestandes.
*DATA: BEGIN OF X,
**       Kostenrechnungskreis.
*        KOKRS    LIKE TKA01-KOKRS,
**       Buchungskreis.
*        BUKRS    LIKE ANLAV-BUKRS,
**       Geschaeftsbereich.
*        GSBER    LIKE ANLAV-GSBER,
**       Kostenstelle.
*        KOSTL    LIKE ANLAV-KOSTL,
**       Innenauftrag.
*        CAUFN    LIKE ANLZ-CAUFN,
**       Leistungsart.
*        LSTAR    LIKE ANLAV-LSTAR,
**       Kostenart.
*        KOART    LIKE T095B-KTNAFB,
**       Werttyp.
*        WRTTP    LIKE ANLAV-WRTTP,
**       Laufende Nummer innerhalb der Hierarchie.
*        LFDHI    LIKE ANLAV-LFDHI,
**       Laufende Nummer innerhalb der AfA-Simulationssätze (ANIA).
*        LFDNR    LIKE ANLAV-LFDNR,
**       Objektart/Objektkey.
*        OBART    LIKE ANLAV-OBART,
*        RCKEY(24),
**       Invprogramm, GJ, Position.
*        PRNAM    LIKE ANLAV-PRNAM,
*        PRGJR    LIKE ANLAV-PRGJR,
*        PRPOS    LIKE ANLAV-PRPOS,
**       PSP-Element, Projekt.
*        POSNR    LIKE ANLAV-POSNR,
*        PSPHI    LIKE ANLAV-PSPHI,
**       Auftrag.
*        EAUFN    LIKE ANLAV-EAUFN,
**       Anforderung.
*        IMAPO    LIKE ANLAV-IMAPO,
**       Anlagenhauptnummer/Inventarnummer, Anlagenunternummer.
*        ANLN0    LIKE ANLAV-ANLN0,
*        ANLN2    LIKE ANLAV-ANLN2,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
**       Bezeichnung.
*        TXT50    LIKE ANLAV-TXT50,
**       Gesamte Kosten.
*        GKOST    LIKE RKU01G-WTGBTR,
**       Davon fix.
*        FKOST    LIKE RKU01G-WTGBTR,
**       Waehrungsschluessel.
*        WAERS    LIKE T093B-WAERS,
*      END OF X.
*
** Hilfstabelle in LOOP-Verarbeitung: Summe Kosten nach Perioden.
*DATA: BEGIN OF HLP_KO OCCURS 24,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
**       Gesamte Kosten.
*        GKOST    LIKE RKU01G-WTGBTR,
**       Davon fix.
*        FKOST    LIKE RKU01G-WTGBTR,
*      END OF HLP_KO.
*
** Hilfstabelle in LOOP-Verarbeitung: Kosten zu Anlage nach Perioden.
*DATA: BEGIN OF ANL_KO OCCURS 24,
**       Periode.
*        PERAF    LIKE ANLP-PERAF,
**       Gesamte Kosten.
*        GKOST    LIKE RKU01G-WTGBTR,
**       Davon fix.
*        FKOST    LIKE RKU01G-WTGBTR,
*      END OF ANL_KO.
*
*DATA: BEGIN OF OLD_KOKRSKEY,
*        BUKRS LIKE ANLAV-BUKRS,
*        GSBER LIKE ANLAV-GSBER,
*      END   OF OLD_KOKRSKEY.
*
** HLP_AGJBEG abgeschafft, temp_gjahr aktuell laufendes
"no514310
** Geschäftsjahr
"no514310
*DATA: temp_gjahr LIKE anlcv-gjahr.
"no514310
**DATA: HLP_AGJBEG  LIKE ANLAV-DEAKT.
"no514310
*
**INSERT
***       Header.
**        X-KOKRS   X-BUKRS   X-GSBER
**        X-KOSTL   X-CAUFN
**        X-LSTAR   X-KOART
**        X-WRTTP   X-LFDHI   X-LFDNR
**        X-PRNAM   X-PRGJR   X-PRPOS
**        X-POSNR
**        X-EAUFN
**        X-IMAPO
**        X-ANLN0   X-ANLN2
**        X-PERAF
***
**        INTO HEADER.
**
**INSERT
***       Daten.
**        X-TXT50   X-GKOST   X-FKOST   X-WAERS
**        X-PSPHI   X-OBART   X-RCKEY
**        SAV_WAERS TKA01-WAERS
**
**        INTO DATEN.
