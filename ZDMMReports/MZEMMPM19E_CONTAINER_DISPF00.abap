
*---------------------------------------------------------------------*
*       Modulpool ML01SF00: Allgemeine FORM-Routinen zu SAPML01S      *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         FCODE_FORMS             Setzen der entsprechenden MODNR     *
*         LAGP_INIT               Initialisieren des LAGP Datenteils  *
*         LAGP_LESEN              Lesen eines LAGP Satzes             *
*         LAGP_LOESCHEN           Vorbereiten des Loeschens           *
*         LAGP_SORLP_HINZU        Aus T343I wird SORLP gefüllt        *
*         LAGP_REIHF_HINZU        Aus T343J wird REIHF gefüllt        *
*         ADIC_TEXT_LESEN         Liest Festwertetext zu Feld         *
*         LEIN_LESEN              Lesen eines LEIN Satzes             *
*         LQUA_INIT               Initialisieren des LQUA Datenteils  *
*         LQUA_LESEN              Lesen eines LQUA Satzes             *
*         LQUA_LOESCHEN           Loeschen eines LQUA Satzes          *
*         MLVS_LESEN              Lesen der Materialstammview MLVS    *
*         PLATZGEWICHT_ERMITTELN  Neuberechnung des Platzgewichts     *
*         PLATZGEWICHT_MESSAGE    Message wenn Gewicht neu ermittelt  *
*         STATISTIK_PLATZAUFTEILUNG  Statistik zu Lagerstamm          *
*         T300T_LESEN             Lesen eines T300T Eintrags          *
*         T301T_LESEN             Lesen eines T301T Eintrags          *
*         T302T_LESEN             Lesen eines T302T Eintrags          *
*         T303T_LESEN             Lesen eines T303T Eintrags          *
*         T307T_LESEN             Lesen eines T307T Eintrags aus LEIN *
*         T330T_LESEN             Lesen eines T330T Eintrags          *
*         T337A_LESEN             Lesen eines T337A Eintrags          *
*         T340_LESEN              Lesen eines T340  Eintrags          *
*         T340D_GEWEI_LESEN       Lesen der Gewichtseinheit pro Lgnum *
*         T341_LESEN              Lesen eines T341  Eintrags          *
*         T342_LESEN              Lesen eines T342  Eintrags          *
*         T343_LESEN              Lesen eines T343  Eintrags          *
*         T343I_LESEN             Lesen eines T343I Eintrags          *
*         T343J_LESEN             Lesen eines T343J Eintrags          *
*         TSTCT_LESEN             Lesen eines TSTCT Eintrags          *
*---------------------------------------------------------------------*
FORM FCODE_FORMS USING P_MODNR.
  PERFORM P_MODNR OF FC001             " Nichts tun
                     FC002             " LAGP Lesen fuer Hinzufuegen
                     FC003             " LAGP Lesen fuer Veraendern
                     FC004             " LAGP Lesen fuer Anzeigen
                     FC005             " Hinzufuegen LAGP
                     FC006             " Veraendern  LAGP
                     FC007             " Loeschen    LAGP
                     FC008             " Beenden Transaktion
                     FC009             " Zeigen Inventurdaten
                     FC010             " Zeigen leere Plaetze
                     FC011             " Zurueck zum Aufrufdynpro
                     FC012             " LLHM Lesen fuers Hinzufuegen
                     FC013             " LLHM Lesen fuers Veraendern
                     FC014             " LLHM Lesen fuers Anzeigen
                     FC015             " Hinzufuegen LLHM
                     FC016             " Veraendern  LLHM
                     FC017             " Loeschen    LLHM
                     FC018             " LQUA Lesen fuers Hinzufuegen
                     FC019             " LQUA Lesen fuers Veraendern
                     FC020             " LQUA Lesen fuers Anzeigen
                     FC021             " Hinzufuegen LQUA
                     FC022             " Veraendern LQUA
                     FC023             " Loeschen Lqua
                     FC024             " Anzeigen Inventurdaten/Quant
                     FC025             " Anzeigen Sonderbestandsdaten
                     FC026             " Anzeigen Quants pro Lagerplatz
                     FC027             " Anzeigen Quants zum Material
                     FC028             " Anzeigen Quants pro Lagerplatz
                     FC029             " AZ Lagerbestaende zum Material
                     FC030             " Masch. Anlegen v.Lagerplaetzen
                     FC031             " Sprung ins Anfangsmenü
                     FC032             " AZ Platzaufteilung
                     FC033             " AZ Lagerplatzstatistik
                     FC034             " Reserve
                     FC035          " Window Inventur zu Anz/Änd Quants
                     FC036             " Window Kundendaten zu   - " -
                     FC037             " Rückruf aus Call Screen
                     FC038          " Verlassen Transaktion ohne POPUP
                     FC039             " F3 mit POPUP sichern
                     FC040             " F15 mit POPUP sichern
                     FC041
                     FC042             "Check Sperrdata, LEIN sichern
                     FC043             "Anz. Quant zu LENUM (LS27)
                     FC044             "Anz. Quant zu LENUM (LS32/33)
                     FC045             "Anz. Lagerplatzdaten zur LENUM
                     FC046             "Anz. Inventurdaten zur LENUM
                     FC047             "LAGP lesen für ändern LEIN
                     FC048             "Lagereinheiten pro Platz
                     FC049             "Anz. LE's zum Platz
                     FC050             "Anz. Charge zu Quant
                     FC051             "Bestände zur Platzposition
                     FC052             "Bestände auf Hu anzeigen
                     FC053             "Tabreiter wechseln auf D0400
                     FC054             "Anzeigen Lagerquant LS23
                     FC055             "Anz. Lagereinheit LS33 od. LS28
                     FC056             "Daten-Refresh D0400
                     FC057             "Aufsteig. sortieren D4003
                     FC058             "Absteig. sortieren D4003
                     FC059             "Rücksprung D4006 -> D4003
                     FC060             "Blättern auf D4003
                     FC061             "Ein-/Ausblenden Nachzählversion
                     FC062             "Aufruf LAGP anlegen  LS01N
                     FC063             "Aufruf LAGP ändern   LS02N
                     FC064             "Aufruf LAGP anzeigen LS03N
                     FC065             "Aufruf Verbucher / Datenwechsel
                     FC066             "Blättern auf D4002
                     FC067             " LAGP Lesen fuer Hinzufuegen
                     FC068             " LAGP Lesen fuer Veraendern
                     FC069.            " LAGP Lesen fuer Anzeigen

ENDFORM.
*---------------------------------------------------------------------*
*       FORM LAGP_INIT                                                *
*---------------------------------------------------------------------*
*       Der Datenteil von LAGP wird gecleart.                         *
*---------------------------------------------------------------------*
FORM LAGP_INIT.
  *LAGP-LGNUM = LAGP-LGNUM.
  *LAGP-LGTYP = LAGP-LGTYP.
  *LAGP-LGPLA = LAGP-LGPLA.
  CLEAR LAGP.
  LAGP-LGNUM = *LAGP-LGNUM.
  LAGP-LGTYP = *LAGP-LGTYP.
  LAGP-LGPLA = *LAGP-LGPLA.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_LOESCHEN                                            *
*---------------------------------------------------------------------*
*       Es wird geprüft, ob der Platz  leer und nicht gesperrt ist.   *
*---------------------------------------------------------------------*
FORM LAGP_LOESCHEN.
  IF LAGP-ANZQU NE 0.
    MESSAGE E012.    "Es können nur leere Lagerplätze gelöscht werden
  ELSE.
    IF LAGP-SKZSA EQ CON_GESPERRT OR
       LAGP-SKZSE EQ CON_GESPERRT OR
       LAGP-SKZSI EQ CON_GESPERRT.
      MESSAGE E013.    "es können nur nicht gesperrte Plätze gelöscht..
    ENDIF.
  ENDIF.
  IF LAGP-IVIVO EQ CON_X.
    MESSAGE E031.      "bin is within inventory doc. cannot be deleted
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_SORLP_HINZU                                         *
*---------------------------------------------------------------------*
*       Beim Hinzufügen eines Platzes werden gemäß T343I sechs Stellen*
*       des Platzes in einem Sortierfeldgemerkt. Damit kann in der DB *
*       nach bestimmten Lagerplätzen selektiert werden, z.B. für eine *
*       gestreute Einlagerung. (Quereinlagerung)                      *
*---------------------------------------------------------------------*
FORM LAGP_SORLP_HINZU.
  DATA: HLP_LGPLA TYPE C,
        I  TYPE C,
        INIT_STELLE TYPE C,
        STELLE TYPE C.
  FIELD-SYMBOLS: <F>.

*........Lesen, welche Lagerplatzstellen für den Index verwendet werden.

  PERFORM T343I_LESEN USING LAGP-LGNUM LAGP-LGTYP.

*........Zuweisung der Stellen in entsprechender Reihenfolge -> SORLP...

  DO 10 TIMES VARYING STELLE FROM T343I-STE01 NEXT T343I-STE02.
    IF STELLE NE INIT_STELLE.
      STELLE = STELLE - 1.

*........Die Stellen des Platzes werden nach und nach <F> zugeordnet....
      ASSIGN LAGP-LGPLA+I(1) TO <F>.

*........Von hier aus erfolgt Zuordnung zu SORLP an die Stelle, die in
*        T343i angegeben wurde.
      WRITE <F>         TO LAGP-SORLP+STELLE(1).
    ENDIF.
    I = I + 1.                         " I= 0 bis 9
  ENDDO.
ENDFORM.

*---------------------------------------------------------------------*
*        FORM ADIC_TEXT_LESEN                                         *
*---------------------------------------------------------------------*
*        Lesen der Texte zum ADIC                                     *
*---------------------------------------------------------------------*
FORM ADIC_TEXT_LESEN USING FLG01 LANGU DNAME FDINH DDTEXT.
*--> ADIC lesen
*  perform getdd07v(rdd07dat) using flg01 langu dname
*                             changing dd07v_tab.
*  import dd07v_tab from memory.
*--> durch nachfolgenden Funktionsbaustein ersetzt
CALL FUNCTION 'DDUT_DOMVALUES_GET'
     EXPORTING
          NAME          = DNAME
          LANGU         = LANGU
          TEXTS_ONLY    = 'X'
     TABLES
          DD07V_TAB     = DD07V_TAB.
*    exceptions
*         illegal_input = 1
*         others        = 2.

*--> Text aus uebergebener Tabelle lesen
  LOOP AT DD07V_TAB.
     IF DD07V_TAB-DOMVALUE_L EQ FDINH.
       MOVE DD07V_TAB-DDTEXT TO DDTEXT.
       EXIT.
     ENDIF.
   ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*        FORM LEIN_LESEN                                              *
*---------------------------------------------------------------------*
*        Lesen der Daten zur Lagereinheit aus Tabelle LEIN            *
*---------------------------------------------------------------------*
FORM LEIN_LESEN USING VALUE(P_LENUM).
  SELECT SINGLE * FROM LEIN
    WHERE LENUM = P_LENUM.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LQUA_INIT                                                *
*---------------------------------------------------------------------*
*       Der Datenteil von LQUA wird gecleart.                         *
*---------------------------------------------------------------------*
FORM LQUA_INIT.
  *LQUA-LGNUM = LQUA-LGNUM.
  *LQUA-LQNUM = LQUA-LQNUM.
  CLEAR LQUA.
  LQUA-LGNUM = *LQUA-LGNUM.
  LQUA-LQNUM = *LQUA-LQNUM.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM LQUA LOESCHEN                                            *
*---------------------------------------------------------------------*
*       LQUA wird aus der Datenbank geloescht. Der Satz ist bereits   *
*       gesperrt und es ist sicher, dass er in der DB vorhanden ist.  *
*       Das Lagerquant       wird nur geloescht, wenn es leer ist     *
*       und nicht gesperrt ist.                                       *
*---------------------------------------------------------------------*
FORM LQUA_LOESCHEN.
  PERFORM LAGP_LESEN USING LQUA-LGNUM LQUA-LGTYP LQUA-LGPLA.
  IF LQUA-GESME NE 0.
    MESSAGE E122.  "Es können nur leere Lagerquants gelöscht werden
  ELSE.
    IF LQUA-SKZUA EQ CON_GESPERRT OR
       LQUA-SKZUE EQ CON_GESPERRT OR
       LQUA-SKZSA EQ CON_GESPERRT OR
       LQUA-SKZSE EQ CON_GESPERRT OR
       LQUA-SKZSI EQ CON_GESPERRT.
      MESSAGE E123. "Es können nur nicht gesperrte Quants gelöscht werde
    ENDIF.
    IF LAGP-KZINV NE INIT_KZINV.
      MESSAGE E118.   "Quant darf bei InventurausbuchKz nicht gelöscht..
    ENDIF.
  ENDIF.
  MESSAGE W124 WITH LQUA-LQNUM.        "Erneute Dfrg löscht Quant
ENDFORM.

*---------------------------------------------------------------------*
*       FORM PLATZGEWICHT_ERMITTELN                                   *
*---------------------------------------------------------------------*
FORM PLATZGEWICHT_ERMITTELN.

    IF LAGP-LGEWI = 0.
      LAGP-MGEWI = 0.
    ENDIF.

    IF LAGP-LGEWI > 0 AND NOT LAGP-ANZQU IS INITIAL.
      PERFORM QUANTGEWICHTE_ADDIEREN.
    ELSE.
      LAGP-MGEWI = 0.
    ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PLATZGEWICHT_MESSAGE.                                     *
*---------------------------------------------------------------------*
FORM PLATZGEWICHT_MESSAGE.
    IF LAGP-MGEWI NE SAV_MGEWI.
     CASE T340-TRTYP.
       WHEN CON_ANZEIGEN.
         LAGP-MGEWI = 0.
         MESSAGE S024.
       WHEN CON_VERAENDERN.
         MESSAGE S025.
     ENDCASE.
   ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM QUANTGEWICHTE_ADDIEREN.                                  *
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM QUANTGEWICHTE_ADDIEREN.
  CLEAR LAGP-MGEWI.
  SELECT * FROM LQUA WHERE
    LGNUM EQ LAGP-LGNUM AND
    LGTYP EQ LAGP-LGTYP AND
    LGPLA EQ LAGP-LGPLA.

    LAGP-MGEWI = LAGP-MGEWI + LQUA-MGEWI.
  ENDSELECT.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM STATISTIK_PLATZAUFTEILUNG                                *
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM STATISTIK_PLATZAUFTEILUNG.

  PERFORM T337A_LESEN.

*........Quants des Platzes in der DB lesen und Positionen als
*........vergeben kennzeichnen..........................................

* select * from lqua
*          where lgnum = lagp-lgnum
*            and lgtyp = lagp-lgtyp
*            and lgpla = lagp-lgpla.
*   perform t337a_abhaken.
* endselect.

*........Lesen der freien Platzpositionen...............................

ENDFORM.

*---------------------------------------------------------------------*
*       FORM T337A_ABHAKEN                                            *
*---------------------------------------------------------------------*
*       Nur Platzpositonen werden herausgefiltert, die nicht in       *
*       den Quants vorkommen                                          *
*---------------------------------------------------------------------*
FORM T337A_ABHAKEN.
  DO T337A-MAXQU TIMES VARYING SAV_PLPOS
                          FROM T337A-POS01  NEXT T337A-POS02
                       VARYING SAV_POSIT
                          FROM RL01S-POS01 NEXT RL01S-POS02.
    IF  SAV_PLPOS = LQUA-PLPOS.
      MOVE XFELD TO SAV_POSIT.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.




*---------------------------------------------------------------------*
*       FORM T300T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T300T-Eintrages                                   *
*---------------------------------------------------------------------*
*       VALUE(PAR_LGNUM)               Lagernummer                    *
*---------------------------------------------------------------------*
FORM T300T_LESEN USING VALUE(002_LGNUM).
  SELECT SINGLE * FROM T300T
  WHERE LGNUM = 002_LGNUM AND
        SPRAS = SY-LANGU.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T301T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T301T-Eintrages                                   *
*---------------------------------------------------------------------*
*       VALUE(PAR_LGNUM)               Lagernummer                    *
*       VALUE(PAR_LGTYP)               Lagertyp                       *
*---------------------------------------------------------------------*
FORM T301T_LESEN USING VALUE(003_LGNUM) VALUE(003_LGTYP).
  SELECT SINGLE * FROM T301T
  WHERE LGNUM = 003_LGNUM AND
        LGTYP = 003_LGTYP AND
        SPRAS = SY-LANGU.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T302T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T302T-Eintrages                                   *
*---------------------------------------------------------------------*
FORM T302T_LESEN USING VALUE(004_LGNUM)
                       VALUE(004_LGTYP)
                       VALUE(004_LGBER).
  SELECT SINGLE * FROM T302T
  WHERE LGNUM = 004_LGNUM AND
        LGTYP = 004_LGTYP AND
        LGBER = 004_LGBER AND
        SPRAS = SY-LANGU.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T303T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T303T-Eintrages                                   *
*---------------------------------------------------------------------*
FORM T303T_LESEN USING VALUE(011_LGNUM)
                       VALUE(011_LPTYP).
  SELECT SINGLE * FROM T303T
  WHERE LGNUM = 011_LGNUM AND
        LPTYP = 011_LPTYP AND
        SPRAS = SY-LANGU.
ENDFORM.

*-----------------------------------------------------------------------
*        FORM T307T_LESEN
*-----------------------------------------------------------------------
*        Lesen der Daten zum Lagereinheitentyp aus T307T
*-----------------------------------------------------------------------
FORM T307T_LESEN  USING VALUE(P_LGNUM) VALUE(P_LETYP).
  SELECT SINGLE * FROM T307T
    WHERE SPRAS = SY-LANGU
    AND   LGNUM = P_LGNUM
    AND   LETYP = P_LETYP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T330T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T330T-Eintrages                                   *
*---------------------------------------------------------------------*
FORM T330T_LESEN USING VALUE(005_LGNUM) VALUE(005_SPGRU).
  SELECT SINGLE * FROM T330T
  WHERE LGNUM = 005_LGNUM AND
        SPGRU = 005_SPGRU AND
        SPRAS = SY-LANGU.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM T331_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines T331-Eintrages                                    *
*---------------------------------------------------------------------*
FORM T331_LESEN.
  SELECT SINGLE * FROM T331
  WHERE LGNUM = LAGP-LGNUM
    AND LGTYP = LAGP-LGTYP.
  IF SY-SUBRC NE 0.
    MESSAGE E022.   "Kein Eintrag in Lagertypsteuerungstab 331 vorhanden
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T337A_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines T337A_Eintrages                                   *
*---------------------------------------------------------------------*
FORM T337A_LESEN.
  SELECT SINGLE * FROM T337A
   WHERE LGNUM = LAGP-LGNUM
     AND LGTYP = LAGP-LGTYP
     AND PLAUF = LAGP-PLAUF.
  IF SY-SUBRC <> 0.
    MESSAGE E346.         " Diese Aufteilung ist nicht vorgeshen
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM T340D_GEWEI_LESEN                                        *
*---------------------------------------------------------------------*
*       Vorschlagen der Gewichtseinheit, die pro Lagernummer def. ist *
*---------------------------------------------------------------------*
FORM T340D_GEWEI_LESEN USING P_LGNUM.
  CLEAR T340D-GEWEI.
  SELECT SINGLE * FROM T340D
         WHERE LGNUM = P_LGNUM.
  IF T340D-GEWEI    IS INITIAL.
    MESSAGE E027.              "Bitte Gewichtseinheit in LVS-Default...
  ELSE.
    MOVE T340D-GEWEI TO LAGP-GEWEI.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM T341_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines T341-Eintrages                                    *
*---------------------------------------------------------------------*
FORM T341_LESEN.
  SELECT SINGLE * FROM T341
  WHERE TCODE = 'LS03N'.
  IF SY-SUBRC NE 0.
    MESSAGE E041 WITH SY-TCODE.   "Transaktion nicht in T341 beschrieben
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T342_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines T342-Eintrages                                    *
*---------------------------------------------------------------------*
*       VALUE(PAR_TRANR)               Transportrichtung              *
*---------------------------------------------------------------------*
FORM T342_LESEN USING VALUE(007_MODPL)
                      VALUE(007_LDYNP)
                      VALUE(007_TRTYP)
                      VALUE(007_FCODE)
                      VALUE(007_VORGA)
                      VALUE(007_VORDY).
  DYNNR_PACK = 007_LDYNP.
  UNPACK DYNNR_PACK TO DYNNR.
  SELECT SINGLE * FROM T342
  WHERE MODPL = 007_MODPL    AND
        LDYNP = DYNNR        AND
        TRTYP = 007_TRTYP    AND
        FCODE = 007_FCODE    AND
        VORGA = 007_VORGA    AND
        VORDY = 007_VORDY.
  IF SY-SUBRC NE 0.
    MESSAGE E042 WITH 007_MODPL DYNNR 007_TRTYP 007_FCODE.
  ENDIF.                               "Arg nicht in T342 enthalten
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T343_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines T343-Eintrages                                    *
*---------------------------------------------------------------------*
FORM T343_LESEN USING VALUE(343_LGNUM)
                      VALUE(343_LGTYP)
                      VALUE(343_LFDNR).
  SELECT SINGLE * FROM T343
  WHERE LGNUM = 343_LGNUM    AND
        LGTYP = 343_LGTYP    AND
        LFDNR = 343_LFDNR.
  IF SY-SUBRC NE 0.
    MESSAGE E344 .
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T343I_LESEN
*---------------------------------------------------------------------*
*       Lesen eines T343I-Eintrages
*---------------------------------------------------------------------*
FORM T343I_LESEN USING VALUE(343I_LGNUM)
                       VALUE(343I_LGTYP).
  SELECT SINGLE * FROM T343I
  WHERE LGNUM = 343I_LGNUM    AND
        LGTYP = 343I_LGTYP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM T343J_LESEN
*---------------------------------------------------------------------*
*       Lesen eines T343J-Eintrages
*---------------------------------------------------------------------*
FORM T343J_LESEN USING VALUE(343J_LGNUM)
                       VALUE(343J_LGTYP).
  SELECT SINGLE * FROM T343J
  WHERE LGNUM = 343J_LGNUM    AND
        LGTYP = 343J_LGTYP.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM TSTCT_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines TSTCT-Eintrages                                   *
*---------------------------------------------------------------------*
FORM TSTCT_LESEN.
  SELECT SINGLE * FROM TSTCT
  WHERE SPRSL = SY-LANGU AND
        TCODE = SY-TCODE.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM MLVS_LESEN                                               *
*---------------------------------------------------------------------*
*       Lesen eines MLVS-Satzes                                       *
*              View auf die MAKT                                      *
*---------------------------------------------------------------------*
*       VALUE(PAR_MATNR)      Materialnummer                          *
*       VALUE(PAR_WERKS)      Werk                                    *
*       VALUE(PAR_LGNUM)      Lagernummer                             *
*---------------------------------------------------------------------*
FORM MLVS_LESEN USING VALUE(028_MATNR)
                     VALUE(028_WERKS)
                     VALUE(028_LGNUM).
  CLEAR MTCOM.
  MOVE: CON_MLVS   TO MTCOM-KENNG,
        028_MATNR  TO MTCOM-MATNR,
        028_WERKS  TO MTCOM-WERKS,
        028_LGNUM  TO MTCOM-LGNUM,
        SY-LANGU   TO MTCOM-SPRAS.

  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING
            SCHLUESSEL = MTCOM
       IMPORTING
            MATDATEN   = MLVS
            RETURN     = MTCOR
       TABLES
            SEQMAT01   = DUM_TAB.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM PRUEFUNG_LGPLA                                           *
*---------------------------------------------------------------------*
*       Plausibilitaetzpruefung Lagerplatz                            *
*                                                                     *
*       Das Sonderzeichen '/' wird fuer die Einlagerungsstrategie     *
*       als Begrenzungszeichen (fuer Lagerplatzposition) intern       *
*       verwendet. Wird '/' manuell eingegben, gibt es Probleme       *
*       die Platzposition zu entschluesseln.                          *
*                                                                     *
*       Diese Routine verhintert die manuelle '/'-Eingabe             *
*---------------------------------------------------------------------*
*       VALUE(PAR_LGPLA)      Lagerplatz                              *
*---------------------------------------------------------------------*
FORM PRUEFUNG_LGPLA USING VALUE(PAR_LGPLA).

  SEARCH PAR_LGPLA FOR './.'.
  IF SY-SUBRC = 0.
    MESSAGE E157.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAGP_REIHF_HINZU                                         *
*---------------------------------------------------------------------*
*       Beim Hinzufügen eines Platzes werden gemäß T343J sechs Stellen*
*       des Platzes in einem Reihenfolgefeld gemerkt. DAmit kann      *
*       nach bestimmten Lagerplätzen selektiert werden, z.B. für eine *
*       konzentrierte Auslagerung (Kommissionierung).                 *
*---------------------------------------------------------------------*
FORM LAGP_REIHF_HINZU.
  DATA: HLP_LGPLA TYPE C,
        I  TYPE C,
        INIT_STELLE TYPE C,
        STELLE TYPE C.
  FIELD-SYMBOLS: <F>.

*........Lesen, welche Lagerplatzstellen für den Index verwendet werden.

  PERFORM T343J_LESEN USING LAGP-LGNUM LAGP-LGTYP.

*........Zuweisung der Stellen in entsprechender Reihenfolge -> REIHF...

  DO 10 TIMES VARYING STELLE FROM T343J-STE01 NEXT T343J-STE02.
    IF STELLE NE INIT_STELLE.
      STELLE = STELLE - 1.

*........Die Stellen des Platzes werden nach und nach <F> zugeordnet....
      ASSIGN LAGP-LGPLA+I(1) TO <F>.

*........Von hier aus erfolgt Zuordnung zu REIHF an die Stelle, die in
*        T343J angegeben wurde.
      WRITE <F>         TO LAGP-REIHF+STELLE(1).
    ENDIF.
    I = I + 1.                         " I= 0 bis 9
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  D0400_DATEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM D0400_DATEN.

  reset-seite = 1.
  reset2-seite = 1.

  clear flg_d4002_firstcall.
  clear flg_d4003_firstcall.

  if lagp-lgnum is initial and
     lagp-lgtyp is initial and
     lagp-lgpla is initial.
    get parameter id 'LGN' field lagp-lgnum.
    get parameter id 'LGT' field lagp-lgtyp.
    if t340-trtyp ne con_hinzufuegen and
       t340-trtyp ne con_veraendern.
      get parameter id 'LGP' field lagp-lgpla.
    endif.
  endif.

  if not neu_lgnum is initial.
    move neu_lgnum to lagp-lgnum.
    move neu_lgtyp to lagp-lgtyp.
    move neu_lgpla to lagp-lgpla.
  endif.

  if lagp-lgnum is initial or
     lagp-lgtyp is initial or
     lagp-lgpla is initial.
    exit.
  endif.

* WM screen selection
  perform T341_LESEN.

  check bin_is_checked is initial.

  if t340-trtyp eq con_hinzufuegen.
    perform fc067.
    if not d0400_record_exist is initial.
      exit.
    endif.
  endif.
  if t340-trtyp eq con_veraendern.
    perform fc068.
    if not bin_is_wrong is initial.
      move lagp-lgpla to tmp_lgpla.
      clear lagp.
      move tmp_lgpla to lagp-lgpla.
      clear rl01s.
      clear t301t.
      clear t302t.
      clear t30at.
      clear t309t.
      clear t303t.
      clear t330t.
      refresh ilqua400.
      refresh ilinv400.
      exit.
    endif.
  endif.
  if t340-trtyp eq con_anzeigen.
    perform fc069.
    if sy-subrc ne 0.
      move lagp-lgpla to tmp_lgpla.
      clear lagp.
      move tmp_lgpla to lagp-lgpla.
      clear rl01s.
      clear t301t.
      clear t302t.
      clear t30at.
      clear t309t.
      clear t303t.
      clear t330t.
      refresh ilqua400.
      refresh ilinv400.
      exit.
    endif.
  endif.

* Warehouse number describtions
  perform T300T_LESEN_LAGP.
* Storage type describtions
  perform T301T_LESEN_LAGP.
* Text for blocking reason
  perform T330T_LESEN_LAGP.
* WM default values
  perform T340D_GEWEI_LESEN USING LAGP-LGNUM.
* Storage section names
  perform T302T_LESEN_MASK.
* WM names for storage bin types
  perform T303T_LESEN_MASK.
* Names for fire-containment sections
  perform T309T_LESEN.
* Picking area descriptions
  perform T30AT_LESEN_MASK.
* Storage type control
  perform T331_LESEN.
* Division of storage bins into sections
  if not lagp-plauf is initial.
    perform T337A_LESEN.
  else.
    clear t337A.
  endif.

  perform SAVE_DATA.
  perform PLATZGEWICHT_ERMITTELN.
  perform PLATZGEWICHT_MESSAGE.


  CALL FUNCTION 'L_LAGP_CALC_USED_CAPACITY'
     EXPORTING
          I_LGNUM = lagp-lgnum
          I_LGTYP = lagp-lgtyp
          I_LGPLA = lagp-lgpla
     IMPORTING
          E_PRPRO = rl01s-prpro
          E_PRAUS = rl01s-praus
          E_PRA01 = rl01s-pra01
          E_PRA02 = rl01s-pra02
          E_PRA03 = rl01s-pra03
          E_PRA04 = rl01s-pra04
          E_PRA05 = rl01s-pra05
          E_PRA06 = rl01s-pra06
          E_PRA07 = rl01s-pra07
          E_PRA08 = rl01s-pra08
          E_PRA09 = rl01s-pra09
          E_PRA10 = rl01s-pra10
          .

  move lagp-lgnum to merk_lgnum.
  move lagp-lgtyp to merk_lgtyp.
  move lagp-lgpla to merk_lgpla.
  move lagp-lgber to merk_lgber.
  move lagp-kober to merk_kober.
  move lagp-brand to merk_brand.
  move lagp-lptyp to merk_lptyp.
  move lagp-lgewi to merk_lgewi.
  move lagp-lkapv to merk_lkapv.
  move lagp-verif to merk_verif.
  move lagp-sorlp to merk_sorlp.
  move lagp-reihf to merk_reihf.
  move lagp-skzue to merk_skzue.
  move lagp-skzua to merk_skzua.
  move lagp-spgru to merk_spgru.

  if ( lagp-anzqu gt max_quant_on_first and
     ( func_tabstrip-activetab cs fcode_allg or
       func_tabstrip-activetab is initial ) ).
    refresh ilqua400.
    message s299 with max_quant_on_first.
    clear quant_on_first.
  else.
    quant_on_first = con_x.
  endif.

  bin_is_checked = con_x.
***  perform d4002_daten.
***  perform d4003_daten.

ENDFORM.                    " D0400_DATEN

*&---------------------------------------------------------------------*
*&      Form  T300T_LESEN_LAGP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T300T_LESEN_LAGP.

  IF LAGP-LGNUM NE INIT_LGNUM.
    PERFORM T300T_LESEN USING LAGP-LGNUM.
    IF SY-SUBRC NE 0.
      MESSAGE W002 WITH LAGP-LGNUM SY-LANGU. "Lgnum nicht in T300
    ENDIF.
  ELSE.
    CLEAR T300T.
  ENDIF.

ENDFORM.                    " T300T_LESEN_LAGP

*&---------------------------------------------------------------------*
*&      Form  T301T_LESEN_LAGP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T301T_LESEN_LAGP.

  IF LAGP-LGTYP NE INIT_LGTYP.
    PERFORM T301T_LESEN USING LAGP-LGNUM LAGP-LGTYP.
    IF SY-SUBRC NE 0.
      MESSAGE W004 WITH LAGP-LGTYP SY-LANGU.    "Lgtyp nicht in T301
    ENDIF.
  ELSE.
    CLEAR T301T.
  ENDIF.

ENDFORM.                    " T301T_LESEN_LAGP

*&---------------------------------------------------------------------*
*&      Form  T330T_LESEN_LAGP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T330T_LESEN_LAGP.

  IF LAGP-SPGRU NE INIT_SPGRU.
    PERFORM T330T_LESEN USING LAGP-LGNUM LAGP-SPGRU.
    IF SY-SUBRC NE 0.
      MESSAGE W010 WITH LAGP-SPGRU SY-LANGU.   "Bezeichnung zu Sperrgr..
    ENDIF.
  ELSE.
    CLEAR T330T.
  ENDIF.

ENDFORM.                    " T330T_LESEN_LAGP

*&---------------------------------------------------------------------*
*&      Form  T302T_LESEN_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T302T_LESEN_MASK.

  IF LAGP-LGBER NE INIT_LGBER.
    PERFORM T302T_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-LGBER.
    IF SY-SUBRC NE 0.
      MESSAGE W006 WITH LAGP-LGBER SY-LANGU.   "Bezeichnung zu Lgber
    ENDIF.
  ELSE.
    CLEAR T302T.
  ENDIF.

ENDFORM.                    " T302T_LESEN_MASK

*&---------------------------------------------------------------------*
*&      Form  T303T_LESEN_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T303T_LESEN_MASK.

  IF LAGP-LPTYP NE INIT_LPTYP.
    PERFORM T303T_LESEN USING LAGP-LGNUM LAGP-LPTYP.
    IF SY-SUBRC NE 0.
      MESSAGE W020 WITH LAGP-LPTYP SY-LANGU.   "Bezeichnung zu Lgtyp
    ENDIF.
  ELSE.
    CLEAR T303T.
  ENDIF.

ENDFORM.                    " T303T_LESEN_MASK

*&---------------------------------------------------------------------*
*&      Form  T309T_LESEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T309T_LESEN.

  IF NOT LAGP-BRAND IS INITIAL.
    SELECT SINGLE * FROM T309T
       WHERE SPRAS = SY-LANGU
         AND LGNUM = LAGP-LGNUM
         AND BRAND = LAGP-BRAND.
    IF SY-SUBRC NE 0.
      MESSAGE W255 WITH SY-LANGU LAGP-LGNUM LAGP-BRAND.
*   Brandabschnittstext & & & ist nicht vorhanden
    ENDIF.
  ELSE.
    CLEAR T309T.
  ENDIF.

ENDFORM.                    " T309T_LESEN

*&---------------------------------------------------------------------*
*&      Form  T30AT_LESEN_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM T30AT_LESEN_MASK.

  IF LAGP-KOBER NE INIT_KOBER.
    PERFORM T30AT_LESEN USING LAGP-LGNUM LAGP-LGTYP LAGP-KOBER.
    IF SY-SUBRC NE 0.
      MESSAGE W375 WITH LAGP-KOBER SY-LANGU.
*  Bezeichnung zu Kommissionierbereich & ist nicht in Sprache & gepflegt
    ENDIF.
  ELSE.
    CLEAR T30AT.
  ENDIF.

ENDFORM.                    " T30AT_LESEN_MASK

*&---------------------------------------------------------------------*
*&      Module  D4002_LOOPV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_LOOPV OUTPUT.

***  tap_tabix = tap_tabix1.

  reset-lfdps = reset-seite.

ENDMODULE.                 " D4002_LOOPV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D4002_DUMMY_HINZU  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_DUMMY_HINZU OUTPUT.

  read table ilqua400 index reset-seite.
  if sy-subrc <> 0.

    clear ilqua400.
    move lagp-lgnum to ilqua400-lgnum.
    move LAGP-LGTYP to ilqua400-lgtyp.
    move lagp-lgpla to ilqua400-lgpla.
    append ilqua400.
    d4002_dummy_index = reset-seite.
  endif.

  describe table ilqua400 lines d4002-lines.

ENDMODULE.                 " D4002_DUMMY_HINZU  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D4002_LOOP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_LOOP OUTPUT.

  reset-lfdps = d4002-current_line.
  d4002_zeilen = sy-loopc.

*........At first loop step: complete data in table ILTHU for proper
*........representation of scrollbar....................................
  if reset-lfdps = reset-seite.
    describe table ilqua400 lines d4002_lines.
    hilf_var_p = reset-seite + d4002_zeilen - 1 - d4002_lines.
    if hilf_var_p >= 0 and
       not ( hilf_var_p = 0 and reset-lfdps = d4002_dummy_index ).
      if hilf_var_p = 0.
        hilf_var_p = 1.
      endif.
      D4002_LFDPS = d4002_lines + 1.
      do hilf_var_p times.
        clear ilqua400.
        move lagp-lgnum to ilqua400-lgnum.
        move lagp-lgtyp to ilqua400-lgtyp.
        move lagp-lgpla to ilqua400-lgpla.
        append ilqua400.
        d4002_lfdps = d4002_lfdps + 1.
      enddo.
      if d4002_dummy_index = 0.
        d4002_dummy_index = d4002_lines + 1.
      endif.
    endif.

  endif.

*........lesen Arbeitsposition aus interner tabelle................
  read table ilqua400 index reset-lfdps.

  if sy-subrc ne 0.
  else.
    move-corresponding ilqua400 to lqua.
    move-corresponding ilqua400 to rl01s.

    if lqua-sobkz = sobkz_projekt.  "Special stock Q
      perform sonum_conv_int_ext(sapfl000) using lqua-sobkz
                                                 lqua-sonum
                                                 rl01s-lsonr.
    else.
      move lqua-sonum to rl01s-lsonr.
    endif.

  endif.

ENDMODULE.                 " D4002_LOOP  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D4002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_EXIT INPUT.

*........Löschen alle Dummy-Positionen aus ILQUA400(Scroll-Verarbeitung)
  if d4002_dummy_index <> 0.
    loop at ilqua400 from d4002_dummy_index.
      delete ilqua400.
    endloop.
    d4002_dummy_index = 0.
  endif.

ENDMODULE.                 " D4002_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  D4002_DUMMIES_WEG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_DUMMIES_WEG INPUT.

  check d4002_dummy_index <> 0.

  loop at ilqua400 from d4002_dummy_index.
    delete ilqua400.
  endloop.
  d4002_dummy_index = 0.

ENDMODULE.                 " D4002_DUMMIES_WEG  INPUT

*&---------------------------------------------------------------------*
*&      Module  D4002_LOOPV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_LOOPV INPUT.

  clear d4002_line_count.

ENDMODULE.                 " D4002_LOOPV  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4002_LOOP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_LOOP INPUT.

DATA: help_jahr_2 like mseg-mjahr.

  add 1 to d4002_line_count.
  move d4002-current_line to i_current_line.

  read TABLE ilqua400 index i_current_line.
  if sy-subrc eq 0.
    move rl01s-kreuz to ilqua400-kreuz.
    modify ilqua400 index i_current_line.
  endif.

  check fcode eq fcode_ausw.
  check feld eq 'LQUA-LQNUM' or
        feld eq 'LQUA-WENUM' or
        feld eq 'LQUA-LENUM' or
        feld eq 'LQUA-CHARG' or
        feld eq 'LQUA-BTANR' or
        feld eq 'LQUA-QPLOS'.

  check zeile eq d4002_line_count.
  clear fcode.

  if feld eq 'LQUA-LQNUM' and
     ( not lqua-lqnum is initial ).
    PERFORM BERECHTIGUNG_TCODE(SAPFL000) USING CON_LS23_ANZEIGEN.

    if not lqua-lqnum is initial.
      SET PARAMETER:  ID PARID_LGNUM FIELD LAGP-LGNUM,
                      ID PARID_LQNUM FIELD LQUA-LQNUM.

      if t340-trtyp eq con_veraendern.
        CALL TRANSACTION CON_LS22_ANZEIGEN AND SKIP FIRST SCREEN.
      else.
        CALL TRANSACTION CON_LS23_ANZEIGEN AND SKIP FIRST SCREEN.
      endif.
    endif.
  endif.

  if feld eq 'LQUA-WENUM' and
     ( not lqua-wenum is initial ).

     help_jahr_2 = lqua-wdatu(4).

     CALL FUNCTION 'L_MATDOC_DISPLAY'
          EXPORTING
               I_MBLNR   = lqua-wenum
               I_WERKS   = lqua-werks
               I_MJAHR   = help_jahr_2
*         EXCEPTIONS
*              NOT_FOUND = 1
*              NO_DATA   = 2
*              OTHERS    = 3
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
  endif.

  if feld eq 'LQUA-QPLOS' and
     ( not lqua-qplos is initial ).

     CALL FUNCTION 'L_INSPEC_LOT_DISPLAY'
          EXPORTING
               I_QPLOS   = lqua-qplos
*         EXCEPTIONS
*              NOT_FOUND = 1
*              NO_DATA   = 2
*              OTHERS    = 3
               .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
  endif.

  if feld eq 'LQUA-CHARG' and
     ( not lqua-charg is initial ).

     CALL FUNCTION 'L_BATCH_DISPLAY'
          EXPORTING
               I_MATNR   = lqua-matnr
               I_WERKS   = lqua-werks
               I_CHARG   = lqua-charg
*              I_LGORT   =
*         EXCEPTIONS
*              NOT_FOUND = 1
*              NO_DATA   = 2
*              OTHERS    = 3
          .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

  endif.

  if feld eq 'LQUA-LENUM' and
     ( not lqua-lenum is initial ).

    if t340-trtyp eq con_veraendern.
     CALL FUNCTION 'L_SU_CHANGE'
          EXPORTING
              I_LENUM   = lqua-lenum
*         EXCEPTIONS
*             NOT_FOUND = 1
*             NO_DATA   = 2
*             OTHERS    = 3
          .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
    else.
     CALL FUNCTION 'L_SU_DISPLAY'
          EXPORTING
              I_LENUM   = lqua-lenum
*         EXCEPTIONS
*             NOT_FOUND = 1
*             NO_DATA   = 2
*             OTHERS    = 3
          .
     IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
    endif.
  endif.

  if feld eq 'LQUA-BTANR' and
     ( not lqua-btanr is initial ).

    set parameter:  id 'LGN' field lagp-lgnum,
                    id 'TAN' field lqua-btanr.

    call transaction con_tcode_LT21 and skip first screen.

  endif.

ENDMODULE.                 " D4002_LOOP  INPUT

*&---------------------------------------------------------------------*
*&      Module  SET_ICON_D4001  OUTPUT
*&---------------------------------------------------------------------*
*       Set icon fields in dynpro D4001
*----------------------------------------------------------------------*
MODULE SET_ICON_D4001 OUTPUT.

  perform set_icon_d4001 using lagp-skzse
                               lagp-skzue
                               lagp-skzsa
                               lagp-skzua
                               lagp-skzsi
                               lagp-ivivo
                      changing rl01s-esico
                               rl01s-asico
                               rl01s-isico.

ENDMODULE.                 " SET_ICON_D4001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_icon_d4001
*&---------------------------------------------------------------------*
*       Set icon fields in dynpro D4001
*----------------------------------------------------------------------*
*      -->P_LAGP_SKZSE  text
*      -->P_LAGP_SKZUE  text
*      -->P_LAGP_SKZSA  text
*      -->P_LAGP_SKZUA  text
*      -->P_LAGP_SKZSI  text
*      -->P_LAGP_IVIVO  text
*      <--P_RL01S_ESICO  text
*      <--P_RL01S_ASICO  text
*      <--P_RL01S_ISICO  text
*----------------------------------------------------------------------*
FORM set_icon_d4001 USING    P_LAGP_SKZSE like lagp-skzse
                             P_LAGP_SKZUE like lagp-skzue
                             P_LAGP_SKZSA like lagp-skzsa
                             P_LAGP_SKZUA like lagp-skzua
                             P_LAGP_SKZSI like lagp-skzsi
                             P_LAGP_IVIVO like lagp-ivivo
                    CHANGING P_RL01S_ESICO like rl01s-esico
                             P_RL01S_ASICO like rl01s-asico
                             P_RL01S_ISICO like rl01s-isico.

  clear p_rl01s_esico.
  clear p_rl01s_asico.
  clear p_rl01s_isico.

*** Set indicator for stock placement yellow=active red=locked
  if not p_lagp_skzse is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_YELLOW'
            TEXT                  = ''
            INFO                  = TEXT-300
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ESICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if not p_lagp_skzue is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_RED'
            TEXT                  = ''
            INFO                  = TEXT-306
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ESICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*** Set indicator for stock removal yellow=active red=locked
  if not p_lagp_skzsa is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_YELLOW'
            TEXT                  = ''
            INFO                  = TEXT-301
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ASICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if not p_lagp_skzua is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_RED'
            TEXT                  = ''
            INFO                  = TEXT-307
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ASICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*** Set indicator for physical inventory yellow=planned red=active
  if not p_lagp_ivivo is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_YELLOW'
            TEXT                  = ''
            INFO                  = TEXT-302
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ISICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if not p_lagp_skzsi is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LED_RED'
            TEXT                  = ''
            INFO                  = TEXT-308
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ISICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDFORM.                    " set_icon_d4001
*&---------------------------------------------------------------------*
*&      Module  D4002_ABSCHLUSS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4002_ABSCHLUSS OUTPUT.

  reset-lfdps = 1.
  d4002-lines = d4002_lines.

ENDMODULE.                 " D4002_ABSCHLUSS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_LOOPV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_LOOPV OUTPUT.

***  tap_tabix = tap_tabix1.

  reset2-lfdps = reset2-seite.

ENDMODULE.                 " D4003_LOOPV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_DUMMY_HINZU  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_DUMMY_HINZU OUTPUT.

  read table ilinv400 index reset2-seite.
  if sy-subrc <> 0.

    clear ilinv400.
    move lagp-lgnum to ilinv400-lgnum.
    move LAGP-LGTYP to ilinv400-lgtyp.
    move lagp-lgpla to ilinv400-lgpla.
    append ilinv400.
    d4003_dummy_index = reset2-seite.
  endif.

  describe table ilinv400 lines d4003-lines.

ENDMODULE.                 " D4003_DUMMY_HINZU  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_LOOP  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_LOOP OUTPUT.

  reset2-lfdps = d4003-current_line.
  d4003_zeilen = sy-loopc.

*........At first loop step: complete data in table ILINV400 for proper
*........representation of scrollbar....................................
  if reset2-lfdps = reset2-seite.
    describe table ilinv400 lines d4003_lines.
    hilf_var_p = reset2-seite + d4003_zeilen - 1 - d4003_lines.
    if hilf_var_p >= 0 and
       not ( hilf_var_p = 0 and reset2-lfdps = d4003_dummy_index ).
      if hilf_var_p = 0.
        hilf_var_p = 1.
      endif.
      D4003_LFDPS = d4003_lines + 1.
      do hilf_var_p times.
        clear ilinv400.
        move lagp-lgnum to ilinv400-lgnum.
        move lagp-lgtyp to ilinv400-lgtyp.
        move lagp-lgpla to ilinv400-lgpla.
        append ilinv400.
        d4003_lfdps = d4003_lfdps + 1.
      enddo.
      if d4003_dummy_index = 0.
        d4003_dummy_index = d4003_lines + 1.
      endif.
    endif.

  endif.

*........lesen Arbeitsposition aus interner tabelle................
  read table ilinv400 index reset2-lfdps.

  if sy-subrc ne 0.
  else.
    move-corresponding ilinv400 to linv.
    move-corresponding ilinv400 to rl01s.
    perform set_text_d4003_2.
    perform set_icon_d4003_2 using linv-istat
                                   linv-gesme
*>>>>>>>>>>BEGIN DELETION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>>
***                                   linv-menga
*<<<<<<<<<<END DELETION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>>>>>>>>>>BEGIN INSERTION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>
                                   linv-menge
*<<<<<<<<<<END INSERTION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                          changing rl01s-imico.
  endif.

ENDMODULE.                 " D4003_LOOP  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_ABSCHLUSS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_ABSCHLUSS OUTPUT.

  reset2-lfdps = 1.
  d4003-lines = d4003_lines.

ENDMODULE.                 " D4003_ABSCHLUSS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_EXIT INPUT.

*........Löschen alle Dummy-Positionen aus ILINV400(Scroll-Verarbeitung)
  if d4003_dummy_index <> 0.
    loop at ilinv400 from d4003_dummy_index.
      delete ilinv400.
    endloop.
    d4003_dummy_index = 0.
  endif.

ENDMODULE.                 " D4003_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_DUMMIES_WEG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_DUMMIES_WEG INPUT.

  check d4003_dummy_index <> 0.

  loop at ilinv400 from d4003_dummy_index.
    delete ilinv400.
  endloop.
  d4003_dummy_index = 0.

ENDMODULE.                 " D4003_DUMMIES_WEG  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_LOOPV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D4003_LOOPV INPUT.

  clear d4003_line_count.

ENDMODULE.                 " D4003_LOOPV  INPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_LOOP  INPUT
*&---------------------------------------------------------------------*
*       Call screen D4006 by double-click on field LINV-IVNUM
*----------------------------------------------------------------------*
MODULE D4003_LOOP INPUT.

  add 1 to d4003_line_count.
  move d4003-current_line to i_current_line.

  check fcode eq fcode_ausw.
  check feld eq 'LINV-IVNUM'.
  check zeile eq d4003_line_count.
  clear fcode.

  READ TABLE ilinv400 INDEX i_current_line.
  MOVE-CORRESPONDING ilinv400 TO LINV.

  IF ILINV400-MATNR IS INITIAL.
    CLEAR MLVS.
  ELSE.
*........Der Materialkurztext wird hinzugelesen........................
    PERFORM MATERIAL_LESEN USING ILINV400-MATNR
                                 ILINV400-WERKS
                                 ILINV400-LGNUM.
  ENDIF.

  CALL SCREEN CON_QUANT_DYNPRO STARTING AT 01 02
                               ENDING AT   80 16.

ENDMODULE.                 " D4003_LOOP  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ICON_D4003  OUTPUT
*&---------------------------------------------------------------------*
*     Set icon fields in dynpro D4003
*----------------------------------------------------------------------*
MODULE SET_ICON_D4003 OUTPUT.

  perform set_icon_d4001 using lagp-skzse
                               lagp-skzue
                               lagp-skzsa
                               lagp-skzua
                               lagp-skzsi
                               lagp-ivivo
                      changing rl01s-esico
                               rl01s-asico
                               rl01s-isico.

ENDMODULE.                 " SET_ICON_D4003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ICON_D4005  OUTPUT
*&---------------------------------------------------------------------*
*       Set icon fields in dynpro D4005
*----------------------------------------------------------------------*
MODULE SET_ICON_D4005 OUTPUT.

  perform set_icon_d4005 using lagp-kzler
                               lagp-kzvol
                               lagp-kzdyn
                      changing rl01s-lkico
                               rl01s-vkico
                               rl01s-dpico.

ENDMODULE.                 " SET_ICON_D4005  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_icon_d4005
*&---------------------------------------------------------------------*
*       Set icon fields in dynpro D4005
*----------------------------------------------------------------------*
*      -->P_LAGP_KZLER  text
*      -->P_LAGP_KZVOL  text
*      -->P_LAGP_KZDYN  text
*      <--P_RL01S_LKICO  text
*      <--P_RL01S_VKICO  text
*      <--P_RL01S_DPICO  text
*----------------------------------------------------------------------*
FORM set_icon_d4005 USING    P_LAGP_KZLER like lagp-kzler
                             P_LAGP_KZVOL like lagp-kzvol
                             P_LAGP_KZDYN like lagp-kzdyn
                    CHANGING P_RL01S_LKICO like rl01s-lkico
                             P_RL01S_VKICO like rl01s-vkico
                             P_RL01S_DPICO like rl01s-dpico.

  clear p_rl01s_lkico.
  clear p_rl01s_vkico.
  clear p_rl01s_dpico.

  if not p_lagp_kzler is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-303
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_LKICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if not p_lagp_kzvol is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-304
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_VKICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

  if not p_lagp_kzdyn is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-305
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_DPICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDFORM.                    " set_icon_d4005
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT_D4003  OUTPUT
*&---------------------------------------------------------------------*
*       Get text from domain and move to output field
*----------------------------------------------------------------------*
MODULE SET_TEXT_D4003 OUTPUT.

data: text_struc like dd07v.
data: dummy_value(10) type c.

  clear rl01s-ivatx.

  check not lagp-kzinv is initial.

  dummy_value = lagp-kzinv.

  CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
     EXPORTING
          NAME          = 'LVS_KZINV'
          VALUE         = DUMMY_VALUE
          LANGU         = SY-LANGU
          TEXTS_ONLY    = 'X'
     IMPORTING
          DD07V_WA      = text_struc
     EXCEPTIONS
          NOT_FOUND     = 1
          ILLEGAL_INPUT = 2
          OTHERS        = 3
          .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  move text_struc-ddtext to rl01s-ivatx.

ENDMODULE.                 " SET_TEXT_D4003  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_LESEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ILINV400_MATNR  text
*      -->P_ILINV400_WERKS  text
*      -->P_ILINV400_LGNUM  text
*----------------------------------------------------------------------*
FORM MATERIAL_LESEN USING    value(P_MATNR)
                             value(P_WERKS)
                             value(P_LGNUM).

  CLEAR MTCOM.
  MOVE: CON_MLVS   TO MTCOM-KENNG,
        P_MATNR    TO MTCOM-MATNR,
        P_WERKS    TO MTCOM-WERKS,
        P_LGNUM    TO MTCOM-LGNUM,
        SY-LANGU   TO MTCOM-SPRAS.

  CALL FUNCTION 'MATERIAL_READ'
       EXPORTING SCHLUESSEL = MTCOM
       IMPORTING MATDATEN   = MLVS
                 RETURN     = MTCOR
       TABLES    SEQMAT01   = DUM_TAB.

ENDFORM.                    " MATERIAL_LESEN
*&---------------------------------------------------------------------*
*&      Form  ilinv400_fuellen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ilinv400_fuellen.

data: merk_ivnum like linv-ivnum,
      merk_ivpos like LINV-IVPOS,
      merk_nanum like linv-nanum.

  refresh ilinvtemp.
  refresh ilinvxx.

  SELECT * FROM LINV
       WHERE LGNUM = LAGP-LGNUM
         AND LGTYP = LAGP-LGTYP
         AND LGPLA = LAGP-LGPLA.

    MOVE-CORRESPONDING LINV TO ilinvtemp.
    MOVE-CORRESPONDING LINV TO ilinv400.
    APPEND ilinvtemp.                 "For internal use (see below)
    APPEND ilinv400.                  "For display use in table control
  ENDSELECT.

  if not d4003_kz_zahl is initial.
*** take no account of recount numbers; only highest will be displayed
    refresh ilinv400.
    sort ilinvtemp by lgnum ivnum ivpos nanum descending.

    loop at ilinvtemp.
      if sy-tabix eq 1.               "Initialize temp. values
        move ilinvtemp-ivnum to merk_ivnum.
        move ilinvtemp-ivpos to merk_ivpos.
        move ilinvtemp-nanum to merk_nanum.
      endif.
      if ilinvtemp-ivnum = merk_ivnum and
         ilinvtemp-ivpos = merk_ivpos.
        if ilinvtemp-nanum = merk_nanum.
          move-corresponding ilinvtemp to ilinvxx.
          append ilinvxx.
        else.
          refresh ilinvxx.
          move-corresponding ilinvtemp to ilinvxx.
          append ilinvxx.
        endif.
      else.
        loop at ilinvxx.
          move-corresponding ilinvxx to ilinv400.
          append ilinv400.
        endloop.
        refresh ilinvxx.
        move-corresponding ilinvtemp to ilinvxx.
        append ilinvxx.
      endif.
      move ilinvtemp-ivnum to merk_ivnum.
      move ilinvtemp-ivpos to merk_ivpos.
      move ilinvtemp-nanum to merk_nanum.
    endloop.
    if not ilinvxx is initial.
      loop at ilinvxx.
        move-corresponding ilinvxx to ilinv400.
        append ilinv400.
      endloop.
    endif.
  endif.

ENDFORM.                    " ilinv400_fuellen
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SAVE_DATA.

  SAV_LGEWI = LAGP-LGEWI.
  SAV_MGEWI = LAGP-MGEWI.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  D4002_DATEN
*&---------------------------------------------------------------------*
*       Read stock data for dnpro D4002
*----------------------------------------------------------------------*
FORM D4002_DATEN.

  refresh ilqua400.

  if not d0400_record_exist is initial.
    exit.
  endif.

  SELECT * FROM LQUA
       WHERE LGNUM = LAGP-LGNUM
         AND LGTYP = LAGP-LGTYP
         AND LGPLA = LAGP-LGPLA.

    MOVE-CORRESPONDING LQUA TO ilqua400.
    APPEND ilqua400.
  ENDSELECT.

  describe table ilqua400 lines d4002-lines.

ENDFORM.                    " D4002_DATEN
*&---------------------------------------------------------------------*
*&      Form  D4003_DATEN
*&---------------------------------------------------------------------*
*       Read inventory data for dynpro d4003
*----------------------------------------------------------------------*
FORM D4003_DATEN.

  refresh ilinv400.
  perform ilinv400_fuellen.

  describe table ilinv400 lines d4003-lines.

ENDFORM.                    " D4003_DATEN
*&---------------------------------------------------------------------*
*&      Form  LOCK_SAVE_BUTTON_D0400
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOCK_SAVE_BUTTON_D0400.

  perform pfstatus_setzen_d0400.

ENDFORM.                    " LOCK_SAVE_BUTTON_D0400
*&---------------------------------------------------------------------*
*&      Form  PFSTATUS_SETZEN_D0400
*&---------------------------------------------------------------------*
*       Display or cancal function specific FCODEs
*----------------------------------------------------------------------*
FORM PFSTATUS_SETZEN_D0400.

data: begin of fcode_tab occurs 6,
         fcode like rsmpe-func,
      end of fcode_tab.

  refresh fcode_tab.

  if t340-trtyp eq con_anzeigen.
*mspark
    move 'ES01' to fcode_tab-fcode.
    append fcode_tab.
    move 'ES02' to fcode_tab-fcode.
    append fcode_tab.
*mspark
    move 'ES03' to fcode_tab-fcode.
    append fcode_tab.
    move 'BU' to fcode_tab-fcode.
    append fcode_tab.
    move 'LO' to fcode_tab-fcode.
    append fcode_tab.
*** if called from another Transaktion create and update aren't possible
    if not sy-calld is initial.
      move 'ES01' to fcode_tab-fcode.
      append fcode_tab.
      move 'ES02' to fcode_tab-fcode.
      append fcode_tab.
    endif.
  endif.
  if t340-trtyp eq con_hinzufuegen.
    move 'ES01' to fcode_tab-fcode.
    append fcode_tab.
    move 'LO' to fcode_tab-fcode.
    append fcode_tab.
    if not d0400_record_exist is initial.
      move 'BU' to fcode_tab-fcode.
      append fcode_tab.
    endif.
*** if key data is not checked - save isn't allowed
    if bin_is_checked is initial.
      move 'BU' to fcode_tab-fcode.
      append fcode_tab.
    endif.
  endif.
  if t340-trtyp eq con_veraendern.
    move 'ES02' to fcode_tab-fcode.
    append fcode_tab.
*** if key data is not checked - save isn't allowed
    if bin_is_checked is initial.
      move 'BU' to fcode_tab-fcode.
      append fcode_tab.
      move 'LO' to fcode_tab-fcode.
      append fcode_tab.
    endif.
  endif.

*mspark
  set pf-status '4000' excluding fcode_tab.
*mspark
ENDFORM.                    " PFSTATUS_SETZEN_D0400
*&---------------------------------------------------------------------*
*&      Form  set_text_d4003_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_text_d4003_2.

data: text_struc like dd07v.
data: dummy_value(10) type c.

  check not linv-istat is initial.

  dummy_value = linv-istat.

  CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
     EXPORTING
          NAME          = 'LVS_ISTAT'
          VALUE         = DUMMY_VALUE
          LANGU         = SY-LANGU
          TEXTS_ONLY    = 'X'
     IMPORTING
          DD07V_WA      = text_struc
     EXCEPTIONS
          NOT_FOUND     = 1
          ILLEGAL_INPUT = 2
          OTHERS        = 3
          .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  move text_struc-ddtext to rl01s-ivstx.
ENDFORM.                    " set_text_d4003_2
*&---------------------------------------------------------------------*
*&      Form  set_icon_d4003_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LINV_ISTAT  text
*      -->P_LINV_GESME  text
*      -->P_LINV_MENGE  text
*      <--P_RL01S_IMICO  text
*----------------------------------------------------------------------*
FORM set_icon_d4003_2 USING    P_LINV_ISTAT like linv-istat
                               P_LINV_GESME like linv-gesme
*>>>>>>>>>>BEGIN DELETION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>>
***                               P_LINV_MENGA like linv-menga
*<<<<<<<<<<END DELETION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>>>>>>>>>>BEGIN INSERTION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>
                               P_LINV_MENGE like linv-menge
*<<<<<<<<<<END INSERTION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<
                      CHANGING P_RL01S_IMICO like rl01s-imico.

  clear p_rl01s_imico.

  check p_linv_istat ne con_nichtgezaehlt.

*>>>>>>>>>>BEGIN DELETION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>>
***  if p_linv_menga eq p_linv_gesme.
*<<<<<<<<<<END DELETION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>>>>>>>>>>BEGIN INSERTION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>
  if p_linv_menge eq p_linv_gesme.
*<<<<<<<<<<END INSERTION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_EQUAL_GREEN'
            TEXT                  = ''
            INFO                  = TEXT-309
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_IMICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*>>>>>>>>>>BEGIN DELETION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>>
***  if p_linv_menga lt p_linv_gesme.
*<<<<<<<<<<END DELETION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>>>>>>>>>>BEGIN INSERTION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>
  if p_linv_menge lt p_linv_gesme.
*<<<<<<<<<<END INSERTION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_LESS_RED'
            TEXT                  = ''
            INFO                  = TEXT-310
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_IMICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*>>>>>>>>>>BEGIN DELETION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>>
***  if p_linv_menga gt p_linv_gesme.
*<<<<<<<<<<END DELETION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*>>>>>>>>>>BEGIN INSERTION HP_305506>>>>>>>>>>>>>>>>>>>>>>>>>>
  if p_linv_menge gt p_linv_gesme.
*<<<<<<<<<<END INSERTION HP_305506<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_GREATER_RED'
            TEXT                  = ''
            INFO                  = TEXT-311
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_IMICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDFORM.                    " set_icon_d4003_2
*&---------------------------------------------------------------------*
*&      Module  SET_IO_D4001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_IO_D4001 OUTPUT.

  LOOP AT SCREEN.
    if t340-trtyp ne con_anzeigen and
       bin_is_checked is initial.
      screen-input = con_off.
    endif.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " SET_IO_D4001  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_icon_d4002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LQUA_SKZSE  text
*      -->P_LQUA_SKZSA  text
*      -->P_LQUA_SKZSI  text
*      <--P_RL01S_ESICO  text
*      <--P_RL01S_ASICO  text
*      <--P_RL01S_ISICO  text
*----------------------------------------------------------------------*
FORM set_icon_d4002 USING    P_LQUA_SKZSE like lqua-skzse
                             P_LQUA_SKZSA like lqua-skzsa
                             P_LQUA_SKZSI like lqua-skzsi
                    CHANGING P_RL01S_ESICO like rl01s-esico
                             P_RL01S_ASICO like rl01s-asico
                             P_RL01S_ISICO like rl01s-isico.

  clear p_rl01s_esico.
  clear p_rl01s_asico.
  clear p_rl01s_isico.

*** Set indicator for active stock placement
  if not p_lqua_skzse is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-300
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ESICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*** Set indicator for active stock removal
  if not p_lqua_skzsa is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-301
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ASICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

*** Set indicator for active physical inventory
  if not p_lqua_skzsi is initial.
    CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = 'ICON_OKAY'
            TEXT                  = ''
            INFO                  = TEXT-308
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = P_RL01S_ISICO
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.

ENDFORM.                    " set_icon_d4002
*&---------------------------------------------------------------------*
*&      Module  FADE_OUT_D4002  OUTPUT
*&---------------------------------------------------------------------*
*       Fade out/in complete columns from a table control
*----------------------------------------------------------------------*
MODULE FADE_OUT_D4002 OUTPUT.

data: inttab type cxtab_column.
data: anzahl_hu type i.

  clear anzahl_hu.

  loop at ilqua400 where not kzhuq is initial.
    anzahl_hu = 1.
  endloop.

  if anzahl_hu = 0.
    loop at d4002-cols into inttab.
      IF INTTAB-SCREEN-NAME EQ 'LQUA-KZHUQ'.
        inttab-invisible = 1.
        modify d4002-cols from inttab.
      ENDIF.
    ENDLOOP.
  else.
    loop at d4002-cols into inttab.
      IF INTTAB-SCREEN-NAME EQ 'LQUA-KZHUQ'.
        clear inttab-invisible.
        modify d4002-cols from inttab.
      ENDIF.
    ENDLOOP.
  endif.

ENDMODULE.                 " FADE_OUT_D4002  OUTPUT
