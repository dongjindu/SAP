PROGRAM SAPML01S MESSAGE-ID L1.
*-----------------------------------------------------------------------
*        Tabellen
*-----------------------------------------------------------------------
TABLES: LECI_TRA_DYN.
TABLES:  LAGP, *LAGP, LAGPV,           " Lagerplaetze
         LEIN, LEINV,                  " Lagereinheitennummern
         LQUA, *LQUA, LQUAV,           " LVS-Bestaende
         LINV,                         " Inventurdaten
         MARM,  MTCOR,
         RL01S, MTCOM.                 " E/A Felder

TABLES:  TSTC,                         " Transaktionen
         TSTCT,                        " Transaktionstexte
         T002,                         " Sprachenschluessel
         T300,                         " Lagernummern
         T300T,                        " Lagernummerbezeichnungen
         T301,                         " Lagertypen
         T301T,                        " Lagertypbezeichnugnen
         T302,                         " Lagerbereiche
         T302T,                        " Lagerbereichbezeichnungen
         T303,                         " Lagerplatztypen
         T303T,                        " Lagerplatztypbezeichnungen
         T307T,                        " Lagereinheitstyp-Text
         T309T,                        " Brandschutzzonenbezeichnungen
         T320,                         " Werke/Lagerorte
         T330,                         " Sperrgruende
         T330T,                        " Sperrgrundbezeichnungen
         T331,                         " Lagertypsteuerung
         T337A,                        " Platzaufteilung
         T337Z,                        " Platzaufteilung
         T340,                         " LVS Transaktionen
         T340D,                        " LVS Defaultwerte
         T341,                         " Transaktionsfeldauswahl
         T342, *T342,                  " Dynprofolgesteuerung
         T342T,                        " Titlebars
         T343,                         " Lagerstruktur
         T343I,                        " Indextabelle Lagerplatz (SORLP)
         T343J,                        " Indextabelle Lagerplatz (REIHF)
         T30AT,                        " Kommibereichsbezeichnung
         T30A,                         " Kommissionierbereiche
         MLVS,
         MLGN.

TABLES: DYNPFLD.                       " Eingabehilfe auf Feldern

*-----------------------------------------------------------------------
*        COMMON DATA
*-----------------------------------------------------------------------
*ATA:    BEGIN OF COMMON PART.
*ATA:    END OF COMMON PART.

*
*-----------------------------------------------------------------------
*       Datenfelder fuer den Modulpool SAPML01S und zugehoerige
*       Modulpools (INCLUDE)
*
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*       Teil 3 :  Einzelfelder  ( variabel )
*       Teil 4 :  Konstanten
*       Teil 5 :  Field-Symbols
*
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*-----------------------------------------------------------------------

INCLUDE MLLVSTAB.                      " Tabellen fürs ganze LVS

DATA:  BEGIN OF MEI OCCURS 100.        " Mengeneinheiten
        INCLUDE STRUCTURE MARM.
DATA:  END OF MEI.


DATA:  BEGIN OF DUM_TAB OCCURS 1,
         DUMDUM(1) TYPE C,
       END OF DUM_TAB.

*........Domänentext zu LE-Status einlesen..............................
DATA: BEGIN OF DD07V_TAB OCCURS 1.
        INCLUDE STRUCTURE DD07V.
DATA: END OF DD07V_TAB.

*........interne Tabellen...............................................
data: begin of ilqua400 occurs 10.  "For display use in table control
        include structure lqua.
data: kreuz like rl01s-kreuz,       "Line selected
      end of ilqua400.

data: begin of ilinv400 occurs 10.  "For display use in table control
        include structure linv.
data: kreuz like rl01s-kreuz,       "Line selected
      end of ilinv400.
data: begin of ilinvtemp occurs 10. "Internal use
        include structure linv.
data: end of ilinvtemp.
data: begin of ilinvxx occurs 5.    "Internal use
        include structure linv.
data: end of ilinvxx.

*
*
*-----------------------------------------------------------------------
*       Teil 2 :  Strukturen    ( BEGIN OF ... )
*-----------------------------------------------------------------------

INCLUDE MLLVSSTR.


DATA: BEGIN OF GETFIELDS OCCURS 0.     "Eingabehilfe auf Dynprofelder
        INCLUDE STRUCTURE DYNPFLD.
DATA: END OF GETFIELDS.

DATA: BEGIN OF UPDFIELDS OCCURS 0.     "Eingabehilfe auf Dynprofelder
        INCLUDE STRUCTURE DYNPFLD.
DATA: END OF UPDFIELDS.

*
*-----------------------------------------------------------------------
*       Teil 3 :  Einzelfelder
*-----------------------------------------------------------------------

INCLUDE MLLVSFLD.

DATA:    INVENTURTEXT(20)      TYPE C,
         PFSTATUS(4)           TYPE C,
         PFSTATUS_WIND(4)      TYPE C      VALUE 'WIND',
         SAV_TCODE             LIKE SY-TCODE,           "-> Ex-/Import
         SAV_TRTYP             LIKE T340-TRTYP.         "-> Ex-/Import

*........Felder für LE-Nummer-Status....................................
DATA:    CON_DNAME       LIKE DD07V-DOMNAME VALUE 'LEIN_STATU',
         DDTEXT          LIKE DD07V-DDTEXT.

data:    ilqua_tabix           like          sy-tabix,
         ilqua_tabix1          like          sy-tabix,
         d4002_dummy_index     like          sy-tabix,
         d4002_lines           like          sy-tabix,
         d4002_lfdps           like          sy-tabix,
         d4003_dummy_index     like          sy-tabix,
         d4003_lines           like          sy-tabix,
         d4003_lfdps           like          sy-tabix.
data:    d4002_zeilen(2)     type p   value 16,
         d4003_zeilen(2)     type p   value 11.

data:  begin of reset,
       lfdps like sy-tabix,
       seite like sy-tabix,
       maxps like sy-tabix,
       end of reset.
data:  begin of reset2,
       lfdps like sy-tabix,
       seite like sy-tabix,
       maxps like sy-tabix,
       end of reset2.

data: hilf_var_p type p.
data: hilf_subrc like sy-subrc.
data: i_current_line          type i.
data: con_tab_ilqua400(8) type c   value 'ILQUA400',
      con_tab_ilinv400(8) type c   value 'ILINV400',
      flg_keine_dynpros   type c.
data: merk_lgnum like lagp-lgnum,
      merk_lgtyp like lagp-lgtyp,
      merk_lgpla like lagp-lgpla,
      merk_lgber like lagp-lgber,
      merk_kober like lagp-kober,
      merk_brand like lagp-brand,
      merk_lptyp like lagp-lptyp,
      merk_lgewi like lagp-lgewi,
      merk_lkapv like lagp-lkapv,
      merk_verif like lagp-verif,
      merk_sorlp like lagp-sorlp,
      merk_reihf like lagp-reihf,
      merk_skzue like lagp-skzue,
      merk_skzua like lagp-skzua,
      merk_spgru like lagp-spgru,
      neu_lgnum  like lagp-lgnum,
      neu_lgtyp  like lagp-lgtyp,
      neu_lgpla  like lagp-lgpla,
      tmp_lgnum  like lagp-lgnum,
      tmp_lgtyp  like lagp-lgtyp,
      tmp_lgpla  like lagp-lgpla.
data: con_quant_dynpro(5) type c   value 'D4006'.
data: d4003_kz_zahl(1)    type c   value 'X'.
data: quant_on_first(1)   type c   value ' '.
data: max_quant_on_first  type i   value 50.
data: d0400_record_exist(1) type c   value ' '.
data: d4002_line_count    type i.
data: d4003_line_count    type i.
data: bin_is_checked(1)   type c   value ' '.
data: bin_is_wrong(1)     type c   value ' '.
data: bin_is_changed(1)   type c   value ' '.

*-----------------------------------------------------------------------
*       Teil 4 :  Konstanten
*-----------------------------------------------------------------------
INCLUDE MLLVSKON.

*........allgemeine Felder.............................................

constants:
         CON_DA                   TYPE P   VALUE 1,
         CON_ANZLP_DEFAULT(8)     TYPE C   VALUE '32',
         CON_MAPPE_DEFAULT(12)    TYPE C   VALUE 'RLLS0500    ',
         XFELD                    TYPE C   VALUE 'X'        .

*........Feldauswahl...................................................

constants:
         CON_MODPL(4)             TYPE C   VALUE 'L01S',
         CON_SPERR                TYPE C   VALUE 'L',
         CON_ST(2)                TYPE C   VALUE 'ST'.

*........Funktionscodes.................................................

constants:
         FCODE_ANDB(4)            TYPE C   VALUE 'ANDB'                ,
         FCODE_ANDL(4)            TYPE C   VALUE 'ANDL'                ,
         FCODE_BACK(4)            TYPE C   VALUE 'BACK'                ,
         FCODE_BEST(4)            TYPE C   VALUE 'BEST'                ,
         FCODE_BU(4)              TYPE C   VALUE 'BU  '                ,
         FCODE_EN(4)              TYPE C   VALUE 'EN  '                ,
         FCODE_ESC(4)             TYPE C   VALUE 'ESC '                ,
         FCODE_RET(4)             TYPE C   VALUE 'RET '                ,
         FCODE_HI(4)              TYPE C   VALUE 'HI  '                ,
         FCODE_LO(4)              TYPE C   VALUE 'LO  '                ,
         FCODE_LAGP(4)            TYPE C   VALUE 'LAGP'                ,
         FCODE_STRA(4)            TYPE C   VALUE 'STRA'                ,
         FCODE_STRB(4)            TYPE C   VALUE 'STRB'                ,
         FCODE_KN(4)              TYPE C   VALUE 'KN  '                ,
         FCODE_PL(4)              TYPE C   VALUE 'PL  '                ,
         FCODE_SPLA(4)            TYPE C   VALUE 'SPLA'                ,
         FCODE_SWED(4)            TYPE C   VALUE 'SWED'                ,
         FCODE_SMEN(4)            TYPE C   VALUE 'SMEN'                ,
         FCODE_SCHA(4)            TYPE C   VALUE 'SCHA'                ,
         FCODE_UMEI(4)            TYPE C   VALUE 'UMEI'                ,
         FCODE_ULVS(4)            TYPE C   VALUE 'ULVS'                ,
         FCODE_UAUS(4)            TYPE C   VALUE 'UAUS'                ,
         FCODE_UBST(4)            TYPE C   VALUE 'UBST'                ,
         FCODE_FIRST(4)   TYPE C   VALUE 'P--',  "Allg: First Page
         FCODE_PREV(4)    TYPE C   VALUE 'P-',   "Allg: Up Page
         FCODE_NEXT(4)    TYPE C   VALUE 'P+',   "Allg: Down Page
         FCODE_LAST(4)    TYPE C   VALUE 'P++',  "Allg: Last Page
         FCODE_ALLG(4)    TYPE C   VALUE 'ALLG', "D0400: Allg.Platzdaten
         FCODE_INVE(4)    TYPE C   VALUE 'INVE', "D0400: Inventurdaten
         FCODE_AUFT(4)    TYPE C   VALUE 'AUFT', "D0400: Platzaufteilung
         FCODE_STAT(4)    TYPE C   VALUE 'STAT', "D0400: Statistikdaten
         FCODE_OK4006(4)  TYPE C   VALUE 'OK',   "D4006: Inventurdetail
         FCODE_ESC4006(4) TYPE C   VALUE 'ESC',  "D4006: Inventurdetail
         FCODE_AUSW(4)    TYPE C   VALUE 'AUSW', "D0400: Hotspot
         FCODE_FP(4)      TYPE C   VALUE 'FP',   "D4002: First Page
         FCODE_UP(4)      TYPE C   VALUE 'UP',   "D4002: Up Page
         FCODE_DP(4)      TYPE C   VALUE 'DP',   "D4002: Down Page
         FCODE_LP(4)      TYPE C   VALUE 'LP',   "D4002: Last Page
         FCODE_FPI(4)     TYPE C   VALUE 'FPI',  "D4003: First Page
         FCODE_UPI(4)     TYPE C   VALUE 'UPI',  "D4003: Up Page
         FCODE_DPI(4)     TYPE C   VALUE 'DPI',  "D4003: Down Page
         FCODE_LPI(4)     TYPE C   VALUE 'LPI',  "D4003: Last Page
         FCODE_BES2(4)    TYPE C   VALUE 'BES2'. "D4004: Bestandsdaten

*........Dynpronummern..................................................

constants:
         CON_INVENTURDYNPRO(4)    TYPE C   VALUE '0102'                ,
         CON_INVENTURDYNPRO2(4)   TYPE C   VALUE '0206'                ,
         CON_SONDERBESTAND(4)     TYPE C   VALUE '0204'                .

constants:
         DYNPRO_4_PLATZ      LIKE SY-DYNNR VALUE '4001',
         DYNPRO_4_BESTAND    LIKE SY-DYNNR VALUE '4002',
         DYNPRO_4_INVENTUR   LIKE SY-DYNNR VALUE '4003',
         DYNPRO_4_AUFTEILUNG LIKE SY-DYNNR VALUE '4004',
         DYNPRO_4_STATISTIK  LIKE SY-DYNNR VALUE '4005'.

*........Transaktionscodes.............................................

constants:
         CON_TRCODE1    LIKE SY-TCODE      VALUE 'LS25',
         CON_MENU       LIKE SY-TCODE      VALUE 'LLVS',
         CON_TCODE_LS02 LIKE SY-TCODE      VALUE 'LS02',
         CON_TCODE_LS03 LIKE SY-TCODE      VALUE 'LS03',
         CON_TCODE_LI05 LIKE SY-TCODE      VALUE 'LI05',
         CON_TCODE_LS23 LIKE SY-TCODE      VALUE 'LS23',
         CON_TCODE_LS27 LIKE SY-TCODE      VALUE 'LS27',
         CON_TCODE_LS28 LIKE SY-TCODE      VALUE 'LS28',
         CON_TCODE_LS32 LIKE SY-TCODE      VALUE 'LS32',
         CON_TCODE_LF15 LIKE SY-TCODE      VALUE 'LF15',
         CON_TCODE_MSC3 LIKE SY-TCODE      VALUE 'MSC3',
         CON_TCODE_O02C LIKE SY-TCODE      VALUE 'O02C',
         CON_TCODE_MSC3N like sy-tcode     value 'MSC3N',
         CON_TCODE_LS01N LIKE SY-TCODE      VALUE 'LS01N',
         CON_TCODE_LS02N LIKE SY-TCODE      VALUE 'LS02N',
         CON_TCODE_LS03N LIKE SY-TCODE      VALUE 'LS03N',
         CON_TCODE_LT21 LIKE SY-TCODE      VALUE 'LT21'.

*........Statusfelder...................................................

constants:
         CON_GELOESCHT            TYPE C   VALUE 'X'                   ,
         CON_GESPERRT             TYPE C   VALUE 'X'                   ,
         CON_NICHT_INVENTIERT(2)  TYPE C   VALUE '  '                  ,
         CON_NICHTDA              TYPE P   VALUE 0                     ,
         CON_NICHTGELOESCHT       TYPE C   VALUE ' '                   ,
         CON_NICHTGESPERRT        TYPE C   VALUE ' '                   ,
         CON_PFSTATUS(1)          TYPE C   VALUE 'A',
         CON_L(1)                          VALUE 'L',
         CON_A(1)                          VALUE 'A'.

*........Initialfelder..................................................

DATA: INIT_FCODE LIKE T342-FCODE,
      INIT_KZINV LIKE LAGP-KZINV,
      INIT_LGBER LIKE LAGP-LGBER,
      INIT_LENUM LIKE LEIN-LENUM,
      INIT_LETYP LIKE LEIN-LETYP,
      INIT_LGNUM LIKE LAGP-LGNUM,
      INIT_LGTYP LIKE LAGP-LGTYP,
      INIT_LPTYP LIKE LAGP-LPTYP,
      INIT_MEINS LIKE MLVS-MEINS,
      INIT_NANUM LIKE LINV-NANUM,
      INIT_PTYPT LIKE T303T-PTYPT,
      INIT_SONUM LIKE LQUA-SONUM,
      INIT_SPGRU LIKE LAGP-SPGRU,
      INIT_KOBER LIKE LAGP-KOBER,
      INIT_X     TYPE C         .

*........Hilfsfelder ...................................................

DATA: SAV_PLPOS  LIKE LQUA-PLPOS ,
      SAV_POSIT  LIKE RL01S-POS01,
      SAV_LGEWI  LIKE LAGP-LGEWI,
      SAV_MGEWI  LIKE LAGP-MGEWI,
      SAV_REPID  LIKE SY-REPID,
      HLP_LGNUM  LIKE LEIN-LGNUM,
      HLP_LGPLA  LIKE LEIN-LGPLA,
      HLP_LGTYP  LIKE LEIN-LGTYP.

DATA: HLP_FIELDNAME(11) TYPE C,
      HLP_PLPOS(2)      TYPE C.

*........sonstige Hilfsfelder..........................................

DATA: SUBSCREEN               LIKE SY-DYNNR.
DATA: FLG_D0400_FIRSTCALL     TYPE C.        "Erstaufruf von Dynpro 400
DATA: FLG_D4002_FIRSTCALL     TYPE C.        "Erstaufruf von Dynpro 4002
DATA: FLG_D4003_FIRSTCALL     TYPE C.        "Erstaufruf von Dynpro 4003


*----------------------------------------------------------------------*
*     ALV-Felder                                                       *
*----------------------------------------------------------------------*

TYPE-POOLS: SLIS.

DATA: C_VARIANT LIKE DISVARIANT,       " Report-Variante für FB
      E_VARIANT LIKE DISVARIANT,       " Export-Variante für FB
      I_VARIANT LIKE DISVARIANT.       " Import-Variante für FB

*........Report, der bei LS24 aufgerufen wird..........................
DATA: CON_RLLS2400 LIKE SY-REPID VALUE 'RLLS2400',
      CON_RLLS2600 LIKE SY-REPID VALUE 'RLLS2600',
      CON_RLLS0400 LIKE SY-REPID VALUE 'RLLS0400'.
*-----------------------------------------------------------------------
*       Teil 5 :  Field-Symbols
*-----------------------------------------------------------------------

FIELD-SYMBOLS: <FELDNAME> ,
               <FIELDNAME>.

CONTROLS D4001         TYPE TABLEVIEW USING SCREEN 4001.
CONTROLS D4002         TYPE TABLEVIEW USING SCREEN 4002.
CONTROLS D4003         TYPE TABLEVIEW USING SCREEN 4003.
CONTROLS D4004         TYPE TABLEVIEW USING SCREEN 4004.
CONTROLS D4005         TYPE TABLEVIEW USING SCREEN 4005.

CONTROLS FUNC_TABSTRIP TYPE TABSTRIP.
