************************************************************************
* Program name : ZEMMPM19E_CONTAINER_DISP
* Created by   : Min-su Park
* Created on   : 2003.10.07.
* Pattern      :
* Description  : Container Content display(Graphical)
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.10.07.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

REPORT rls10030 NO STANDARD PAGE HEADING MESSAGE-ID l1.

*........Lagerspiegel mit Lagerdauern..................................

TABLES: lagp,                          " Lagerplätze
        lqua,                          " Lagerquants
        rlist,                         " Felder für LVS-Listen
        rl01s,                         " LVS Stammdatenverwaltung
        rl030.                         " Join von LAGP und LQUA

**--- insert by stlim (2004/05/06)
TABLES : makt.
**--- end of insert

TABLES: tmcnv.                         " Konvertierung MATNR
*........Konstanten....................................................

CONSTANTS: status(8) TYPE c VALUE 'ALVSTAND',
           titel(3)  TYPE c VALUE '001'.

*........Allgemeine LVS-Konstanten.....................................

INCLUDE zmllvskon.
*INCLUDE  MLLVSKON.

*........Hilfsvariablen................................................

DATA: sav_sy_repid LIKE sy-repid,
      dauer        LIKE rl01s-dauer,
      sav_index    LIKE sy-tabix,
      misch_platz  LIKE lagp-lgpla.

DATA: lv_tmcnv     TYPE c.


*........Interne Tabellen..............................................

DATA: BEGIN OF itab OCCURS 0.
        INCLUDE STRUCTURE rl030.       " LAGP- u. LQUA-Felder
DATA: cont_reg_numb1 LIKE leci_tra_dyn-cont_reg_numb1,
      dauer LIKE dauer,
      virgo LIKE lqua-virgo,
      lsonr LIKE rl01s-lsonr,
**--- insert by stlim (2004/05/06)
      maktx LIKE makt-maktx.
**--- end of insert
DATA: END OF itab.


*........Datendeklarationen für den ALV................................

TYPE-POOLS: slis.

DATA: xfield      TYPE slis_t_fieldcat_alv,
      afield      TYPE slis_fieldcat_alv,
      xheader     TYPE slis_t_listheader WITH HEADER LINE,
      xevents     TYPE slis_t_event,
      ls_event    TYPE slis_alv_event,
      gt_extab    TYPE slis_t_extab WITH HEADER LINE,
      gt_sp_group TYPE slis_t_sp_group_alv,
      variant     LIKE disvariant,
      c_variant   LIKE disvariant,
      layout      TYPE slis_layout_alv,
      a_save      TYPE c VALUE 'A',
      rs_selfield TYPE slis_selfield.

*........Select-Einstellungen...........................................

SET TITLEBAR titel.
SELECTION-SCREEN  BEGIN OF BLOCK xxx WITH FRAME TITLE text-100.


SELECT-OPTIONS:  kzinv FOR lagp-kzinv.

PARAMETERS:      pmitb LIKE rlist-pmitb.
SELECT-OPTIONS:  bestq FOR lqua-bestq.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-080.
PARAMETERS:   dauvo LIKE rlist-dauvo.
SELECTION-SCREEN POSITION 58.
PARAMETERS:   daubi LIKE rlist-daubi  DEFAULT '999999'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK xxx.

SELECTION-SCREEN SKIP 1.
PARAMETERS:    p_vari LIKE disvariant-variant.

*---------------------------------------------------------------------*
*        AT SELECTION SCREEN ON VALUE-REQUEST                         *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

*---------------------------------------------------------------------*
*        AT SELECTION SCREEN                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM check_variant_existence.

*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR itab. REFRESH itab.

  SELECT SINGLE * FROM tmcnv WHERE convid = 'MATCONV'.

  IF sy-subrc = 0.
    IF ( tmcnv-lmatnr < 10 ) OR ( NOT tmcnv-maske IS INITIAL ).
      lv_tmcnv = con_true.
    ELSE.
      lv_tmcnv = con_false.
    ENDIF.
  ELSE.
    lv_tmcnv = con_false.
  ENDIF.

*---------------------------------------------------------------------*
*        GET LAGP                                                     *
*---------------------------------------------------------------------*
GET lagp.

*........Nur Plätze mit bestimmten Kennzeichen Inventur................

  CHECK lagp-kzinv IN kzinv.

*........Nur Plätze mit Bestand?.......................................

  IF NOT  pmitb  IS INITIAL.
    CHECK pmitb NE lagp-kzler.         " Nur belegte Plätze
  ELSE.
    IF lagp-kzler = con_x.             " Alle Plätze
*.......Leere Plätze in interne Tabelle ITAB schreiben.................
      CLEAR itab.
      IF lv_tmcnv = con_false.
        MOVE text-002 TO itab-matnr.
      ELSE.
        MOVE space TO itab-matnr.
      ENDIF.
      MOVE-CORRESPONDING lagp TO itab.
      APPEND itab.
    ENDIF.
  ENDIF.

*---------------------------------------------------------------------*
*        GET LQUA                                                     *
*---------------------------------------------------------------------*
GET lqua.

  dauer   =  sy-datlo - lqua-edatu.

  IF dauvo IS INITIAL.
    CHECK dauer <= daubi.                                 "#EC PORTABLE
  ELSE.
    CHECK dauer BETWEEN dauvo AND daubi.                  "#EC PORTABLE
  ENDIF.

  IF lqua-edatu IS INITIAL OR
     lqua-gesme < 0.
    dauer = space.
  ENDIF.

*........Platzposition in MISCH_PLATZ einmischen.......................

  IF lqua-plpos NE space.
    CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
         EXPORTING
              lgpla   = lqua-lgpla
              plpos   = lqua-plpos
         IMPORTING
              o_lgpla = misch_platz.
  ELSE.
    misch_platz = lqua-lgpla.
  ENDIF.
*.......Plätze mit Bestand in interne Tabelle ITAB schreiben...........
  CLEAR itab.
  MOVE-CORRESPONDING lqua TO itab.
  MOVE-CORRESPONDING lagp TO itab.

*mspark(20031205)
*   SELECT SINGLE cont_reg_numb1
*    INTO  itab-cont_reg_numb1  "Container No.
*    FROM ztmm_container
*    WHERE lgpla = lagp-lgpla AND   "Storage bin
*          kzler = space.    "Indicator whether storage bin is empty
*mspark(20031205)

  TABLES : ztmm_container.

  SELECT * FROM ztmm_container
          WHERE lgpla EQ lagp-lgpla
            AND kzler EQ space
       ORDER BY pass_date DESCENDING.
    MOVE : ztmm_container-cont_reg_numb1
                          TO itab-cont_reg_numb1.
    EXIT.
  ENDSELECT.

*........doppelte Felder einzeln einfügen..............................
  MOVE: lqua-lgpla  TO itab-lqua_lgpla,
        misch_platz TO itab-lgpla,
        lqua-lgnum  TO itab-lqua_lgnum,
        lqua-lgtyp  TO itab-lqua_lgtyp,
        lqua-skzua  TO itab-lqua_skzua,
        lqua-skzue  TO itab-lqua_skzue,
        lqua-skzsa  TO itab-lqua_skzsa,
        lqua-skzse  TO itab-lqua_skzse,
        lqua-skzsi  TO itab-lqua_skzsi,
        lqua-spgru  TO itab-lqua_spgru,
        lqua-bdatu  TO itab-lqua_bdatu,
        lqua-bzeit  TO itab-lqua_bzeit,
        lqua-btanr  TO itab-lqua_btanr,
        lqua-btaps  TO itab-lqua_btaps,
        lqua-mgewi  TO itab-lqua_mgewi,
        lqua-gewei  TO itab-lqua_gewei,
        lqua-ivnum  TO itab-lqua_ivnum,
        lqua-ivpos  TO itab-lqua_ivpos,
        lqua-kober  TO itab-lqua_kober,
        lqua-idatu  TO itab-lqua_idatu.
*........zusätzliche Felder ergänzen...................................
  WRITE dauer TO itab-dauer RIGHT-JUSTIFIED.

  IF lqua-sobkz = sobkz_projekt.
*........Konvertierung bei Sonderbestand Q.............................
    PERFORM sonum_conv_int_ext(sapfl000) USING lqua-sobkz
                                               lqua-sonum
                                               rl01s-lsonr.
    WRITE rl01s-lsonr TO itab-lsonr.
  ELSE.
    WRITE lqua-sonum TO itab-lsonr.
  ENDIF.

**--- insert by stlim (2004/05/06)
  CLEAR : makt.
  SELECT SINGLE maktx INTO itab-maktx
                      FROM makt
                     WHERE matnr EQ itab-matnr
                       AND spras EQ sy-langu.
**--- end of insert

  APPEND itab.

*---------------------------------------------------------------------*
*        END-OF-SELECTION                                             *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM listkopf_aufbauen.

  PERFORM special_group USING gt_sp_group[].

  PERFORM events_definieren USING xevents[].

  PERFORM layout_definieren USING layout.

  PERFORM feldkatalog_aufbauen.

  PERFORM variant_init.

  PERFORM listviewer_aufrufen.

***********************************************************************
*    FORM-Routinen                                                    *
***********************************************************************


*---------------------------------------------------------------------*
*      Form  FELDKATALOG_AUFBAUEN                                     *
*---------------------------------------------------------------------*
*      Feldkatalog für ALV aufbauen                                   *
*---------------------------------------------------------------------*
FORM feldkatalog_aufbauen.

  REFRESH xfield.

*---------------------------------------------------------------------*
*        1. per Default eingeblendete Felder                          *
*---------------------------------------------------------------------*

*........Lagertyp......................................................
  CLEAR afield.
  afield-fieldname = 'LGTYP'.
  afield-ref_tabname = 'LAGP'.
  APPEND afield TO xfield.
*........Lagerplatz....................................................
  CLEAR afield.
  afield-fieldname = 'LGPLA'.
  afield-ref_tabname = 'LAGP'.
  afield-hotspot = 'X'.
  APPEND afield TO xfield.
*........Materialnummer................................................
  CLEAR afield.
  afield-fieldname = 'MATNR'.
  afield-ref_tabname = 'LQUA'.
  afield-hotspot = 'X'.
  APPEND afield TO xfield.
*........Werk..........................................................
  CLEAR afield.
  afield-fieldname = 'WERKS'.
  afield-ref_tabname = 'LQUA'.
  APPEND afield TO xfield.
*........Chargennummer.................................................
  CLEAR afield.
  afield-fieldname = 'CHARG'.
  afield-ref_tabname = 'LQUA'.
  afield-hotspot = 'X'.
  APPEND afield TO xfield.
*........Bestandsqualifikation im LVS..................................
  CLEAR afield.
  afield-fieldname = 'BESTQ'.
  afield-ref_tabname = 'LQUA'.
  APPEND afield TO xfield.
*........Sonderbestandskennzeichen.....................................
  CLEAR afield.
  afield-fieldname = 'SOBKZ'.
  afield-ref_tabname = 'LQUA'.
  APPEND afield TO xfield.
*........Sonderbestandsnummer..........................................
  CLEAR afield.
  afield-fieldname = 'LSONR'.
  afield-ref_tabname = 'RL01S'.
  afield-lzero = 'X'.
  APPEND afield TO xfield.
*........Dauer.........................................................
  CLEAR afield.
  afield-fieldname = 'DAUER'.
  afield-ref_tabname = 'RL01S'.
  afield-seltext_s = text-003.         " Dauer
  afield-seltext_m = text-003.
  afield-seltext_l = text-003.
  afield-rollname = 'RL03TDAUER'.
  afield-outputlen = 6.
  APPEND afield TO xfield.

*mspark(20031205)
  CLEAR afield.
  afield-fieldname = 'CONT_REG_NUMB1'.
*  afield-ref_tabname = 'LECI_TRA_DYN'.
  afield-seltext_s = 'CONT.NO'.
  afield-seltext_m = 'CONT.NO'.
  afield-seltext_l = 'CONT.NO'.
  afield-rollname  = 'CONT.NO'.
  afield-outputlen = 20.
  APPEND afield TO xfield.
*mspark(20031205)

**--- insert by stlim (2004/05/06)
  CLEAR afield.
  afield-fieldname = 'MAKTX'.
*  afield-ref_tabname = 'LECI_TRA_DYN'.
  afield-seltext_s = 'Mat''l Description'.
  afield-seltext_m = 'Mat''l Description'.
  afield-seltext_l = 'Mat''l Description'.
  afield-rollname  = 'Mat''l Description'.
  afield-outputlen = 40.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
**--- end of insert

*---------------------------------------------------------------------*
*        2. potentiell einblendbare Felder (LAGP)                     *
*---------------------------------------------------------------------*

*........Lagerbereich..................................................
  CLEAR afield.
  afield-fieldname = 'LGBER'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Lagerplatztyp.................................................
  CLEAR afield.
  afield-fieldname = 'LPTYP'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Aufteilung des Lagerplatzes...................................
  CLEAR afield.
  afield-fieldname = 'PLAUF'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: für Auslagerung (User)......................
  CLEAR afield.
  afield-fieldname = 'SKZUA'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: für Einlagerung (User)......................
  CLEAR afield.
  afield-fieldname = 'SKZUE'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: durch laufende Auslagerung (System).........
  CLEAR afield.
  afield-fieldname = 'SKZSA'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: durch laufende Einlagerung (System).........
  CLEAR afield.
  afield-fieldname = 'SKZSE'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: durch laufende Inventur (System)............
  CLEAR afield.
  afield-fieldname = 'SKZSI'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sperrgrund....................................................
  CLEAR afield.
  afield-fieldname = 'SPGRU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Anzahl der Quants auf Lagerplatz..............................
  CLEAR afield.
  afield-fieldname = 'ANZQU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Maximale Anzahl der Quants auf dem Lagerplatz.................
  CLEAR afield.
  afield-fieldname = 'MAXQU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Anzahl der Lagereinheiten auf dem Lagerplatz..................
  CLEAR afield.
  afield-fieldname = 'ANZLE'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Maximale Anzahl der Lagereinheiten auf dem Lagerplatz.........
  CLEAR afield.
  afield-fieldname = 'MAXLE'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Kennzeichen: Dynamischer Lagerplatz...........................
  CLEAR afield.
  afield-fieldname = 'KZDYN'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Tragfähigkeit des Lagerplatzes................................
  CLEAR afield.
  afield-fieldname = 'LGEWI'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  afield-qfieldname = 'GEWEI'.
  APPEND afield TO xfield.
*........Gewichtseinheit...............................................
  CLEAR afield.
  afield-fieldname = 'GEWEI'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Gewicht der Materialien auf dem Lagerplatz....................
  CLEAR afield.
  afield-fieldname = 'MGEWI'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  afield-qfieldname = 'GEWEI'.
  APPEND afield TO xfield.
*........Datum der letzten Bewegung....................................
  CLEAR afield.
  afield-fieldname = 'BDATU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Uhrzeit der letzten Bewegung..................................
  CLEAR afield.
  afield-fieldname = 'BZEIT'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Transportauftragsnummer der letzten Bewegung..................
  CLEAR afield.
  afield-fieldname = 'BTANR'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Position im Transportauftrag der letzten Bewegung.............
  CLEAR afield.
  afield-fieldname = 'BTAPS'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Datum der letzten Räumung.....................................
  CLEAR afield.
  afield-fieldname = 'RDATU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Uhrzeit der letzten Räumung...................................
  CLEAR afield.
  afield-fieldname = 'RZEIT'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Inventurart...................................................
  CLEAR afield.
  afield-fieldname = 'KZINV'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Datum der letzten Inventurart auf dem Lagerplatz..............
  CLEAR afield.
  afield-fieldname = 'IDATU'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Nummer des Inventuraufnahmebelegs.............................
  CLEAR afield.
  afield-fieldname = 'IVNUM'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Positionsnummer im Inventuraufnahmebeleg......................
  CLEAR afield.
  afield-fieldname = 'IVPOS'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Inventur in Vorbereitung......................................
  CLEAR afield.
  afield-fieldname = 'IVIVO'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Kennzeichen, ob der Platz leer ist............................
  CLEAR afield.
  afield-fieldname = 'KZLER'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Kennzeichen, ob der Platz voll ist............................
  CLEAR afield.
  afield-fieldname = 'KZVOL'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Sortierfeld Lagerplatz (Quereinlagerung)......................
  CLEAR afield.
  afield-fieldname = 'SORLP'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Brandabschnitt................................................
  CLEAR afield.
  afield-fieldname = 'BRAND'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Name des letzten Änderers.....................................
  CLEAR afield.
  afield-fieldname = 'UNAME'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Datum der letzten Änderung....................................
  CLEAR afield.
  afield-fieldname = 'LAEDT'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Gesamtkapazität des Lagerplatzes..............................
  CLEAR afield.
  afield-fieldname = 'LKAPV'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Freie Kapazität auf dem Lagerplatz............................
  CLEAR afield.
  afield-fieldname = 'RKAPV'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Kommissionierbereich..........................................
  CLEAR afield.
  afield-fieldname = 'KOBER'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Reihenfolgefeld Lagerplatz (Sortierfeld für TA-Positionen)....
  CLEAR afield.
  afield-fieldname = 'REIHF'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.
*........Verifikationsfeld bei mobiler Datenerfassung..................
  CLEAR afield.
  afield-fieldname = 'VERIF'.
  afield-ref_tabname = 'LAGP'.
  afield-no_out = 'X'.
  afield-sp_group = 'P'.
  APPEND afield TO xfield.

*---------------------------------------------------------------------*
*        3. potentiell einblendbare Felder (LQUA)                     *
*---------------------------------------------------------------------*

*........Lagerquant....................................................
  CLEAR afield.
  afield-fieldname = 'LQNUM'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lagertyp......................................................
  CLEAR afield.
  afield-fieldname = 'LQUA_LGTYP'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lagerplatz....................................................
  CLEAR afield.
  afield-fieldname = 'LQUA_LGPLA'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Position auf dem Lagerplatz...................................
  CLEAR afield.
  afield-fieldname = 'PLPOS'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: für Einlagerung (User)......................
  CLEAR afield.
  afield-fieldname = 'LQUA_SKZUE'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: für Auslagerung (User)......................
  CLEAR afield.
  afield-fieldname = 'LQUA_SKZUA'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: für laufende Einlagerung (System)...........
  CLEAR afield.
  afield-fieldname = 'LQUA_SKZSE'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: durch laufende Auslagerung (System).........
  CLEAR afield.
  afield-fieldname = 'LQUA_SKZSA'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrkennzeichen: durch laufende Inventur (System)............
  CLEAR afield.
  afield-fieldname = 'LQUA_SKZSI'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Sperrgrund....................................................
  CLEAR afield.
  afield-fieldname = 'LQUA_SPGRU'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Zeugnisnummer.................................................
  CLEAR afield.
  afield-fieldname = 'ZEUGN'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum der letzten Bewegung....................................
  CLEAR afield.
  afield-fieldname = 'LQUA_BDATU'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Uhrzeit der letzten Bewegung..................................
  CLEAR afield.
  afield-fieldname = 'LQUA_BZEIT'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Transportauftragsnummer der letzten Bewegung..................
  CLEAR afield.
  afield-fieldname = 'LQUA_BTANR'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Position im Transportauftrag der letzten Bewegung.............
  CLEAR afield.
  afield-fieldname = 'LQUA_BTAPS'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum der letzten Einlagerung.................................
  CLEAR afield.
  afield-fieldname = 'EDATU'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Uhrzeit der letzten Einlagerung...............................
  CLEAR afield.
  afield-fieldname = 'EZEIT'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum der letzten Auslagerung.................................
  CLEAR afield.
  afield-fieldname = 'ADATU'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Uhrzeit der letzten Auslagerung...............................
  CLEAR afield.
  afield-fieldname = 'AZEIT'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum der letzten Zulagerung..................................
  CLEAR afield.
  afield-fieldname = 'ZDATU'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum des Wareneingangs.......................................
  CLEAR afield.
  afield-fieldname = 'WDATU'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Wareneingangsnummer...........................................
  CLEAR afield.
  afield-fieldname = 'WENUM'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Wareneingangsposition.........................................
  CLEAR afield.
  afield-fieldname = 'WEPOS'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lieferungsnummer..............................................
  CLEAR afield.
  afield-fieldname = 'VBELN'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lieferungsposition.........................................
  CLEAR afield.
  afield-fieldname = 'POSNR'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lagereinheitentyp.............................................
  CLEAR afield.
  afield-fieldname = 'LETYP'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Basismengeneinheit............................................
  CLEAR afield.
  afield-fieldname = 'MEINS'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Gesamtmenge...................................................
  CLEAR afield.
  afield-fieldname = 'GESME'.
  afield-ref_tabname = 'LQUA'.
  afield-qfieldname = 'MEINS'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Verfügbarer Bestand...........................................
  CLEAR afield.
  afield-fieldname = 'VERME'.
  afield-ref_tabname = 'LQUA'.
  afield-qfieldname = 'MEINS'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Einzulagernder Bestand........................................
  CLEAR afield.
  afield-fieldname = 'EINME'.
  afield-ref_tabname = 'LQUA'.
  afield-qfieldname = 'MEINS'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Auszulagernde Menge...........................................
  CLEAR afield.
  afield-fieldname = 'AUSME'.
  afield-ref_tabname = 'LQUA'.
  afield-qfieldname = 'MEINS'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Gewicht des Materials.........................................
  CLEAR afield.
  afield-fieldname = 'LQUA_MGEWI'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-qfieldname = 'LQUA_GEWEI'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Gewichtseinheit...............................................
  CLEAR afield.
  afield-fieldname = 'LQUA_GEWEI'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Transportbedarfsnummer........................................
  CLEAR afield.
  afield-fieldname = 'TBNUM'.
  afield-ref_tabname = 'LQUA'.
  afield-hotspot = 'X'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Nummer des Inventuraufnahmebelegs.............................
  CLEAR afield.
  afield-fieldname = 'LQUA_IVNUM'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Positionsnummer im Inventuraufnahmebeleg......................
  CLEAR afield.
  afield-fieldname = 'LQUA_IVPOS'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Bedarfstyp....................................................
  CLEAR afield.
  afield-fieldname = 'BETYP'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Bedarfsnummer.................................................
  CLEAR afield.
  afield-fieldname = 'BENUM'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lagereinheitennummer..........................................
  CLEAR afield.
  afield-fieldname = 'LENUM'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-hotspot = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Prüflosnummer.................................................
  CLEAR afield.
  afield-fieldname = 'QPLOS'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Verfallsdatum oder Mindesthaltbarkeitsdatum...................
  CLEAR afield.
  afield-fieldname = 'VFDAT'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Kapazitätsverbrauch des Lagerquants...........................
  CLEAR afield.
  afield-fieldname = 'QKAPV'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Kommissionierbereich..........................................
  CLEAR afield.
  afield-fieldname = 'LQUA_KOBER'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Lagerort......................................................
  CLEAR afield.
  afield-fieldname = 'LGORT'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Quant enthält noch keine WE-Daten.............................
  CLEAR afield.
  afield-fieldname = 'VIRGO'.
  afield-ref_tabname = 'LQUA'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
*........Datum der letzten Inventur des Quants..........................
  CLEAR afield.
  afield-fieldname = 'LQUA_IDATU'.
  afield-ref_tabname = 'RL030'.
  afield-no_out = 'X'.
  afield-sp_group = 'Q'.
  APPEND afield TO xfield.
ENDFORM.                               " FELDKATALOG_AUFBAUEN
*---------------------------------------------------------------------*
*      Form  LISTVIEWER_AUFRUFEN                                      *
*---------------------------------------------------------------------*
*      ALV aufrufen und Daten anzeigen                                *
*---------------------------------------------------------------------*
FORM listviewer_aufrufen.

  sav_sy_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program       = sav_sy_repid
            i_callback_pf_status_set = 'SET_PF_STATUS'
*           i_callback_user_command  =
*           i_structure_name         =
            is_layout                = layout
            it_fieldcat              = xfield
            it_excluding             = gt_extab[]
            it_special_groups        = gt_sp_group[]
*           IT_SORT                  =
*           IT_FILTER                =
*           IS_SEL_HIDE              =
*           i_default                = ' '
            i_default                = 'X'
            i_save                   = a_save
            is_variant               = variant
            it_events                = xevents[]
*           IT_EVENT_EXIT            =
*           IS_PRINT                 =
*           I_SCREEN_START_COLUMN    = 0
*           I_SCREEN_START_LINE      = 0
*           I_SCREEN_END_COLUMN      = 0
*           I_SCREEN_END_LINE        = 0
*      iMPORTING
*           E_EXIT_CAUSED_BY_CALLER  =
       TABLES
            t_outtab                 = itab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

  IF sy-subrc NE 0.
    IF NOT sy-msgno IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno.
    ELSE.
      MESSAGE e706 WITH 'REUSE_ALV_DISPLAY_LIST'.
*    Interner Fehler ist aufgetreten (FB &)
    ENDIF.
  ENDIF.
ENDFORM.                               " LISTVIEWER_AUFRUFEN
*---------------------------------------------------------------------*
*      Form  LISTKOPF_AUFBAUEN                                        *
*---------------------------------------------------------------------*
*      Informationen für den Listkopf aufbauen                        *
*---------------------------------------------------------------------*
FORM listkopf_aufbauen.

  REFRESH xheader.
  CLEAR xheader.

*........Selektionsinfo................................................
  xheader-typ  = 'S'.
  xheader-key  = text-008.
  xheader-info = s1_lgnum.
  APPEND xheader.

ENDFORM.                               " LISTKOPF_AUFBAUEN
*---------------------------------------------------------------------*
*      Form  TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*      Kopfinformationen ausgeben                                     *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = xheader[].

ENDFORM.                               " TOP_OF_PAGE
*---------------------------------------------------------------------*
*      Form  EVENTS_DEFINIEREN                                        *
*---------------------------------------------------------------------*
*      Auszuführende Events definieren                                *
*---------------------------------------------------------------------*
FORM events_definieren USING xevents TYPE slis_t_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = xevents.

*........Ereignis TOP-OF-PAGE hinzufügen...............................
  READ TABLE xevents WITH KEY name = slis_ev_top_of_page
                          INTO ls_event.
  IF sy-subrc = 0.
    MOVE slis_ev_top_of_page TO ls_event-form.
    APPEND ls_event TO xevents.
  ENDIF.

*........Ereignis AT-USER-COMMAND hinzufügen...........................
  READ TABLE xevents WITH KEY name = slis_ev_user_command
                          INTO ls_event.
  IF sy-subrc = 0.
    MOVE slis_ev_user_command TO ls_event-form.
    APPEND ls_event TO xevents.
  ENDIF.

ENDFORM.                               " EVENTS_DEFINIEREN
*---------------------------------------------------------------------*
*      Form  SET_PF_STATUS                                            *
*---------------------------------------------------------------------*
*      Eigenen PF-Status setzen                                       *
*---------------------------------------------------------------------*
FORM set_pf_status USING extab TYPE slis_t_extab.

  SET TITLEBAR titel.
  SET PF-STATUS status EXCLUDING extab.

ENDFORM.                               " SET_PF_STATUS
*---------------------------------------------------------------------*
*      Form  USER_COMMAND                                             *
*---------------------------------------------------------------------*
*      Eigene User-Commands bearbeiten                                *
*---------------------------------------------------------------------*
FORM user_command  USING r_ucomm LIKE sy-ucomm
                         rs_selfield TYPE slis_selfield.

  sav_index = rs_selfield-tabindex.
  IF sav_index LE 0.
    MESSAGE e004(0k).
*   Bitte positionieren Sie den Cursor auf eine Zeile des Listkörpers
  ELSE.
    READ TABLE itab INDEX sav_index.
    CASE r_ucomm.
      WHEN '&IC1'.
        CASE rs_selfield-sel_tab_field.
*........Lagerplatz anzeigen (Platz)...................................
          WHEN 'ITAB-LGPLA'.
*            CALL FUNCTION 'L_BIN_DISPLAY'
*                 EXPORTING
*                      I_LGNUM = ITAB-LGNUM
*                      I_LGTYP = ITAB-LGTYP
*                      I_LGPLA = ITAB-LGPLA.
*            mspark
            PERFORM storage_bin USING itab-lgnum
                                      itab-lgtyp
                                      itab-lgpla.
*            mspark
*........Lagerplatz anzeigen (Quant)...................................
          WHEN 'ITAB-LQUA_LGPLA'.
*            CALL FUNCTION 'L_BIN_DISPLAY'
*                 EXPORTING
*                      I_LGNUM = ITAB-LQUA_LGNUM
*                      I_LGTYP = ITAB-LQUA_LGTYP
*                      I_LGPLA = ITAB-LQUA_LGPLA.
*            mspark
            PERFORM storage_bin USING itab-lgnum
                                      itab-lgtyp
                                      itab-lgpla.
*            mspark
*........Lagerquant anzeigen...........................................
          WHEN 'ITAB-MATNR' OR 'ITAB-LQNUM'.
            CALL FUNCTION 'L_QUANT_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lgnum
                      i_lqnum = itab-lqnum.
*........Lagereinheit anzeigen.........................................
          WHEN 'ITAB-LENUM'.
            CALL FUNCTION 'L_SU_DISPLAY'
                 EXPORTING
                      i_lenum = itab-lenum.
*........Charge anzeigen...............................................
          WHEN 'ITAB-CHARG'.
            CALL FUNCTION 'L_BATCH_DISPLAY'
                 EXPORTING
                      i_matnr = itab-matnr
                      i_werks = itab-werks
                      i_charg = itab-charg.
*........Transportbedarf anzeigen......................................
          WHEN 'ITAB-TBNUM'.
            CALL FUNCTION 'L_TR_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lgnum
                      i_tbnum = itab-tbnum.
*........Transportauftrag anzeigen (Platz).............................
          WHEN 'ITAB-BTANR'.
            CALL FUNCTION 'L_TO_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lgnum
                      i_tanum = itab-btanr.
*........Transportauftrag anzeigen (Quant).............................
          WHEN 'ITAB-LQUA_BTANR'.
            CALL FUNCTION 'L_TO_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lqua_lgnum
                      i_tanum = itab-lqua_btanr.
*........Inventurbeleg anzeigen (Platz)................................
          WHEN 'ITAB-IVNUM'.
            CALL FUNCTION 'L_INV_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lgnum
                      i_ivnum = itab-ivnum.
*........Inventurbeleg anzeigen (Quant)................................
          WHEN 'ITAB-LQUA_IVNUM'.
            CALL FUNCTION 'L_INV_DISPLAY'
                 EXPORTING
                      i_lgnum = itab-lqua_lgnum
                      i_ivnum = itab-lqua_ivnum.
        ENDCASE.
    ENDCASE.
  ENDIF.

ENDFORM.                               " TOP_OF_PAGE
*---------------------------------------------------------------------*
*      Form  SPECIAL_GROUP                                            *
*---------------------------------------------------------------------*
*      Feldgruppen definieren                                         *
*---------------------------------------------------------------------*
*     -->LT_SP_GROUP[]  Feldgruppentexte                            *
*---------------------------------------------------------------------*
FORM special_group USING lt_sp_group TYPE slis_t_sp_group_alv.
  DATA: ls_sp_group TYPE slis_sp_group_alv.

  CLEAR ls_sp_group.
  ls_sp_group-sp_group = 'P'.
  ls_sp_group-text = text-004.
  APPEND ls_sp_group TO lt_sp_group.

  CLEAR ls_sp_group.
  ls_sp_group-sp_group = 'Q'.
  ls_sp_group-text = text-005.
  APPEND ls_sp_group TO lt_sp_group.

ENDFORM.                               " SPECIAL_GROUP
*---------------------------------------------------------------------*
*      Form  VARIANT_INIT                                             *
*---------------------------------------------------------------------*
*      Variante Initialisieren                                        *
*---------------------------------------------------------------------*
FORM variant_init.

  CLEAR variant.
  variant-report = sy-repid.

  IF NOT p_vari IS INITIAL.
    MOVE p_vari TO variant-variant.
  ENDIF.

ENDFORM.                               " VARIANT_INIT
*---------------------------------------------------------------------*
*      Form  LAYOUT_DEFINIEREN                                        *
*---------------------------------------------------------------------*
*      Layout der Liste definieren                                    *
*---------------------------------------------------------------------*
FORM layout_definieren USING p_layout TYPE slis_layout_alv.

  p_layout-group_change_edit = 'X'.
  p_layout-detail_popup = 'X'.

ENDFORM.                               " LAYOUT_DEFINIEREN
*---------------------------------------------------------------------*
*      Form  F4_FOR_VARIANT                                           *
*---------------------------------------------------------------------*
*      F4-Hilfe für Anzeigevariante                                   *
*---------------------------------------------------------------------*
FORM f4_for_variant.

  DATA: i_variant LIKE disvariant,
        e_variant LIKE disvariant.

  i_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = i_variant
            i_save              = a_save
*           i_tabname_header    =
*           i_tabname_item      =
*           it_default_fieldcat =
       IMPORTING
*           e_exit              =
            es_variant          = e_variant
       EXCEPTIONS
            not_found = 2.

  IF sy-subrc = 2.
    MESSAGE s205(0k).
*    Keine Auswahl vorhanden!
  ELSE.
    p_vari = e_variant-variant.
  ENDIF.
ENDFORM.                               " F4_FOR_VARIANT
*---------------------------------------------------------------------*
*      Form  CHECK_VARIANT_EXISTENCE                                  *
*---------------------------------------------------------------------*
*      Prüfung, ob selektierte Variante vorhanden                     *
*---------------------------------------------------------------------*
FORM check_variant_existence.

  IF NOT p_vari IS INITIAL.
    MOVE: p_vari  TO c_variant-variant,
          sy-repid TO c_variant-report.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              i_save     = a_save
         CHANGING
              cs_variant = c_variant.
*        exceptions
*             wrong_input   = 1
*             not_found     = 2
*             program_error = 3
*             others        = 4.
  ENDIF.

ENDFORM.                               " CHECK_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*&      Form  STORAGE_BIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_LGNUM  text
*      -->P_ITAB_LGTYP  text
*      -->P_ITAB_LGPLA  text
*----------------------------------------------------------------------*
FORM storage_bin USING    itab_lgnum
                          itab_lgtyp
                          itab_lgpla.
  DATA: hlp_lgpla       LIKE lagp-lgpla.
*........Wurden alle notwendigen Daten übergeben?......................
  IF ( itab_lgnum IS INITIAL ) OR
     ( itab_lgtyp IS INITIAL ) OR
     ( itab_lgpla IS INITIAL ).
    MESSAGE e159(l1).
*   Es wurden keine Daten selektiert (neu selektieren)
  ENDIF.

*........Prüfen, ob es sich um einen Platz mit Platzposition handelt...

  CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
       EXPORTING
            lgnum     = itab_lgnum
            lgtyp     = itab_lgtyp
            lgpla     = itab_lgpla
       IMPORTING
            o_lgpla   = hlp_lgpla
       EXCEPTIONS
            not_found = 1.
  IF sy-subrc = 1.
*........Platzposition nicht vorhanden.................................
    hlp_lgpla = itab_lgpla.
  ENDIF.

*........Prüfen, ob Platz (noch) vorhanden.............................
  SELECT SINGLE * FROM lagp WHERE lgnum = itab_lgnum
                              AND lgtyp = itab_lgtyp
                              AND lgpla = hlp_lgpla.
  IF sy-subrc NE 0.
    MESSAGE e040(l0) WITH itab_lgnum itab_lgtyp itab_lgpla.
*   Lagerplatz & & & kann nicht angezeigt werden
  ELSE.
    SET PARAMETER: ID 'LGN' FIELD itab_lgnum,
                   ID 'LGT' FIELD itab_lgtyp,
                   ID 'LGP' FIELD hlp_lgpla.

*........Aufruf der Transaktion LS03N (Lagerplatz anzeigen)............
*   CALL TRANSACTION CON_LAGP_ANZEIGEN AND SKIP FIRST SCREEN.
    CALL TRANSACTION 'ZWME02'.
  ENDIF.


ENDFORM.                    " STORAGE_BIN
