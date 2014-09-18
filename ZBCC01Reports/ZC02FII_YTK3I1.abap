*----------------------------------------------------------------------*
*   INCLUDE YTK3I1                                                     *
*----------------------------------------------------------------------*
*-----------------------------------------------------------------------
*        Tabellen
*-----------------------------------------------------------------------
TABLES:  bgr00,                        " Mappenvorsatz
         bbkpf,                        " Belegkopf + Tcode
         bbseg,                        " Belegsegment.
         bbtax,                        " Belegsteuern.
         bwith,                        " Quellensteuer
         bselk,                        " Selektionsdaten Kopf
         bselp,                        " Selektionsdaten Position
         lfa1,                         " 구매처마스터 (일반섹
         lfbw.                         " 구매처레코드 (원천세

TABLES:  tbsl.                         " Buchungsschl?sel
TABLES:  t041a.                        " Ausgleichsvorg?ge
TABLES:  t100.                         " Nachrichten
TABLES:  t003.                         " 문서유?

TABLES : *bgr00,
         *bbkpf,
         *bbseg,
         *bbtax,
         *bwith.

DATA:   BEGIN OF ftpost OCCURS 100.
        INCLUDE STRUCTURE ftpost.
DATA:   END OF ftpost.

DATA:   BEGIN OF ftclear OCCURS 20.
        INCLUDE STRUCTURE ftclear.
DATA:   END OF ftclear.

DATA:   BEGIN OF fttax OCCURS 0.
        INCLUDE STRUCTURE fttax.
DATA:   END OF fttax.

DATA:   BEGIN OF xblntab  OCCURS 2.
        INCLUDE STRUCTURE blntab.
DATA:   END OF xblntab.


DATA:    BEGIN OF save_ftclear.
        INCLUDE STRUCTURE ftclear.
DATA:    END OF save_ftclear.

*------- Tabelle T_BBKPF enth?t Belegkopf + Tcode  --------------------
DATA:    t_bbkpf LIKE bbkpf OCCURS 1.

*------- Tabelle T_BBSEG enth?t Belegsegment --------------------------
DATA:    t_bbseg LIKE bbseg_di OCCURS 50.

*------- Tabelle T_BBTAX enth?t Steuerdaten ---------------------------
DATA:    t_bbtax LIKE bbtax OCCURS 50.

*------- Tabelle T_BWITH enth?t Quellensteuerdaten --------------------
DATA:    t_bwith LIKE bwith_di OCCURS 50.

*------- Tabelle FFILE enth?t alle Datens?ze -------------------------
DATA:    BEGIN OF tfile OCCURS 0,
           rec(2600)  TYPE c,
         END OF tfile.

DATA:    BEGIN OF efile OCCURS 100,
           rec(2600)  TYPE c,
         END OF efile.
DATA:    BEGIN OF ertab OCCURS 5,
           rec(2600)  TYPE c,
         END OF ertab.

*------- Feld-Informationen aus NAM
*ETAB --------------------------------
DATA:    BEGIN OF nametab OCCURS 120.
        INCLUDE STRUCTURE dntab.
DATA:    END OF nametab.

*------- Tabelle XT001 ------------
*-------------------------------------
DATA:    BEGIN OF xt001 OCCURS 5.
        INCLUDE STRUCTURE t001.
DATA:    END OF xt001.

*------- Tabelle XTBSL ------------
*-------------------------------------
DATA:    BEGIN OF xtbsl OCCURS 10.
        INCLUDE STRUCTURE tbsl.
DATA:    END OF xtbsl.


*------- Tabelle XT041A -----------
*-------------------------------------
DATA:    BEGIN OF xt041a OCCURS 5,
           auglv        LIKE t041a-auglv,
         END OF xt041a.

*eject
*---------------------------------------------------------------------*
*        Strukturen
*---------------------------------------------------------------------*
*------- Initialstrukturen --------------------------------------------
DATA:    BEGIN OF i_bbkpf.
        INCLUDE STRUCTURE bbkpf.       " Belegkopf
DATA:    END OF i_bbkpf.

DATA:    BEGIN OF i_bbseg.
        INCLUDE STRUCTURE bbseg.       " Belegsegment
DATA:    END OF i_bbseg.

DATA:    BEGIN OF i_bbtax.
        INCLUDE STRUCTURE bbtax.       " Belegsteuern
DATA:    END OF i_bbtax.

DATA:    BEGIN OF i_bselk.
        INCLUDE STRUCTURE bselk.       " Selektionsdaten Kopf
DATA:    END OF i_bselk.

DATA:    BEGIN OF i_bselp.
        INCLUDE STRUCTURE bselp.       " Selektionsdaten Position
DATA:    END OF i_bselp.

DATA:    BEGIN OF i_bwith.
        INCLUDE STRUCTURE bwith.       " Quellensteuer
DATA:    END OF i_bwith.

*------- Hilfsstrukturen f? Direct
* Input ------------------------------
DATA:    BEGIN OF wa_bbseg_di.
        INCLUDE STRUCTURE bbseg_di.
DATA:    END OF wa_bbseg_di.

DATA:    BEGIN OF wa_bwith_di.
        INCLUDE STRUCTURE bwith_di.
DATA:    END OF wa_bwith_di.

*------- Replace special codes with space -----------------------------
DATA:    BEGIN OF trans OCCURS 0,
           x     TYPE x VALUE '00',
           c_00  TYPE c VALUE ' ',
           soh   TYPE x VALUE '01',
           c_01  TYPE c VALUE ' ',
           stx   TYPE x VALUE '02',
           c_02  TYPE c VALUE ' ',
           etx   TYPE x VALUE '03',
           c_03  TYPE c VALUE ' ',
           eot   TYPE x VALUE '04',
           c_04  TYPE c VALUE ' ',
           enq   TYPE x VALUE '05',
           c_05  TYPE c VALUE ' ',
           ack   TYPE x VALUE '06',
           c_06  TYPE c VALUE ' ',
           bel   TYPE x VALUE '07',
           c_07  TYPE c VALUE ' ',
           bs    TYPE x VALUE '08',
           c_08  TYPE c VALUE ' ',
           ht    TYPE x VALUE '09',
           c_09  TYPE c VALUE ' ',
           lf    TYPE x VALUE '0A',
           c_0a  TYPE c VALUE ' ',
           vt    TYPE x VALUE '0B',
           c_0b  TYPE c VALUE ' ',
           ff    TYPE x VALUE '0C',
           c_0c  TYPE c VALUE ' ',
           cr    TYPE x VALUE '0D',
           c_0d  TYPE c VALUE ' ',
           so    TYPE x VALUE '0E',
           c_0e  TYPE c VALUE ' ',
           si    TYPE x VALUE '0F',
           c_0f  TYPE c VALUE ' ',
           dle   TYPE x VALUE '10',
           c_10  TYPE c VALUE ' ',
           dc1   TYPE x VALUE '11',
           c_11  TYPE c VALUE ' ',
           dc2   TYPE x VALUE '12',
           c_12  TYPE c VALUE ' ',
           dc3   TYPE x VALUE '13',
           c_13  TYPE c VALUE ' ',
           dc4   TYPE x VALUE '14',
           c_14  TYPE c VALUE ' ',
           nak   TYPE x VALUE '15',
           c_15  TYPE c VALUE ' ',
           syn   TYPE x VALUE '16',
           c_16  TYPE c VALUE ' ',
           etb   TYPE x VALUE '17',
           c_17  TYPE c VALUE ' ',
           can   TYPE x VALUE '18',
           c_18  TYPE c VALUE ' ',
           em    TYPE x VALUE '19',                     "#EC NO_M_RISC3
           c_19  TYPE c VALUE ' ',
           sub   TYPE x VALUE '1A',
           c_1a  TYPE c VALUE ' ',
           esc   TYPE x VALUE '1B',
           c_1b  TYPE c VALUE ' ',
           fs    TYPE x VALUE '1C',
           c_1c  TYPE c VALUE ' ',
           gs    TYPE x VALUE '1D',
           c_1d  TYPE c VALUE ' ',
           rs    TYPE x VALUE '1E',
           c_1e  TYPE c VALUE ' ',
           us    TYPE x VALUE '1F',
           c_1f  TYPE c VALUE ' ',
         END OF trans.


*------- Workarea zum Lesen der BI-
*S?ze -------------------------------
*------- wa, ertab, tfile und efile muessen mindestens so lang sein
*------- wie die laengste Batchinput-Struktur BBSEG + kundeneigene
*------- Felder im Include CI_COBL_BI.
DATA:    BEGIN OF wa,
           char1(2600)  TYPE c,
         END OF wa.

*eject
*---------------------------------------------------------------------*
*        Einzelfelder
*---------------------------------------------------------------------*
DATA:    beleg_count(6) TYPE c,        " Anz. Belege je Mappe
         beleg_break(6) TYPE c,        " Anz. Belege je Mappe
         bukrs          LIKE bbseg-newbk,   " Buchungskreis
         bbkpf_ok(1)    TYPE c,        " Belegkopf ?ergeben
         bbseg_count(3) TYPE n,        " Anz. BSEGS pro Beleg
         bbseg_tax(1)   TYPE c.        " Steuer ?er BBSEG eingegeb

DATA:    char(40)       TYPE c,        " Char. Hilfsfeld
         char1(1)       TYPE c,        " Char. Hilfsfeld
         char2(40)      TYPE c,        " Char. Hilfsfeld
         tfile_fill(1)  TYPE c,        " X=TFILE schon gef?lt
         tfsave_fill(1)  TYPE c,       " X=TFSAVE schon gef?lt
         commit_count(4) TYPE n,       " Z?ler f? Commit
         all_commit LIKE tbist-aktnum. " Anzahl der Belege bis zum
" letzten COMMIT

DATA:    dyn_name(12)   TYPE c.        " Dynproname

DATA:    fcode(5)       TYPE c.        " Funktionscode
*        FUNCTION       LIKE  RFIPI-FUNCT.  " B= BDC, C= Call Trans
" D-DIRECT INPUT
DATA:    group_count(6) TYPE c,        " Anzahl Mappen
         group_open(1)  TYPE c.        " X=Mappe schon ge?fnet

DATA:    ln_bbseg(8)    TYPE p,        " L?ge des BBSEG
         ln_bbkpf(8)    TYPE p,        " L?ge des BBKPF
         ln_bselk(8)    TYPE p,        " L?ge des BSELK
         ln_bselp(8)    TYPE p.        " L?ge des BSELP

DATA:    mode           LIKE  rfpdo-allgazmd.
DATA:    msgvn          LIKE sy-msgv1, " Hilfsfeld Message-Variable
         msgid          LIKE sy-msgid,
         msgty          LIKE sy-msgty,
         msgno          LIKE sy-msgno,
         msgv1          LIKE sy-msgv1,
         msgv2          LIKE sy-msgv2,
         msgv3          LIKE sy-msgv3,
         msgv4          LIKE sy-msgv4.

DATA:    n(2)           TYPE n,        " Hilfsfeld num.
         nodata(1)      TYPE c,        " Keine BI-Daten f? Feld
         nodata_old     LIKE nodata.   " NODATA gemerkt

DATA:    prefix_p       LIKE tcurp-prefix_p, "price-based rate prefix
        prefix_m       LIKE tcurp-prefix_p. "quantity-based rate prefix

DATA:    refe1(8)       TYPE p.        " Hilfsfeld gepackt

DATA:    satz2_count(6) TYPE c,        " Anz. S?ze(Typ2) je Trans.
         satz2_cnt_akt  LIKE satz2_count,   " Anz. S?ze(Typ2) - 1
         save_tbnam     LIKE bbseg-tbnam,   " gemerkter Tabellenname
         save_bgr00     LIKE bgr00,    " gemerkter BGR00
         subrc          LIKE sy-subrc, " Subrc
         count          TYPE i.        " Anz. Belege

DATA:    tabix(2)       TYPE n,        " Tabelleninex
         tbist_aktiv(1) TYPE c,        " Restart aktiv?
         text(200)      TYPE c,        " Messagetext
         text1(40)      TYPE c,        " Messagetext
         text2(40)      TYPE c,        " Messagetext
         text3(40)      TYPE c,        " Messagetext
         tfill_ftpost   TYPE i,        " Anz. Eintr?e in FTPOST
         tfill_t_bbseg  TYPE i,        " Anz. Eintr?e in T_BBSEG
         tfill_t_bwith  TYPE i,        " Anz. Eintr?e in T_BWITH
         tfill_tfile    TYPE i,        " Anz. Eintr?e in TFILE
         tfill_ertab    TYPE i,        " Anz. Eintr?e in ERTAB
         tfill_ftc(3)   TYPE n,        " Anz. Eintr?e in FTC
         tfill_ftk(3)   TYPE n,        " Anz. Eintr?e in FTK
         tfill_ftz(3)   TYPE n,        " Anz. Eintr?e in FTZ
         tfill_041a(1)  TYPE n.        " Anz. Eintr?e in XT041A


DATA:    wert(60)       TYPE c,        " Hilfsfeld Feldinhalt
         wt_count       TYPE i.        " Z?ler Quellensteuer

DATA:    xbdcc          LIKE rfipi-xbdcc,
  " X=BDC bei Error in CallTra
         xeof(1)        TYPE c,        " X=End of File erreicht
         xmess_bbkpf-sende(1) TYPE c,  " Message gesendet f? BBKPF
         xmess_bbseg-sende(1) TYPE c,  " Message gesendet f? BBSEG
         xmess_bbtax-sende(1) TYPE c,  " Message gesendet f? BBTAX
*        XMWST          LIKE BKPF-XMWST,    " Steuer rechnen
         xnewg(1)       TYPE c,        " X=Neue Mappe
         xftclear(1).                  " Append FTCLEAR durchfuehren?

* DATAs wichtig f? Wiederaufsetzbarkeit
DATA: aktnum LIKE tbist-aktnum.
DATA: startnum LIKE tbist-aktnum.      " erster zu bearbeitender Satz
DATA: numerror LIKE tbist-numerror.
DATA: olderror LIKE tbist-numerror.    " Anzahl Fehler aus dem
DATA: lasterrnum LIKE tbist-lastnum.   "Letzte Fehlernummer
DATA: nostart LIKE tbist-nostarting
              VALUE 'X'. " Start-Infos schreiben ?
DATA: jobid LIKE tbtco-jobname.
DATA: jobid_ext LIKE tbtco-jobname.

CONSTANTS:   pack_size TYPE i VALUE '250'.
TABLES: terrd,
        tfsave.

*-------------------------------------
*        Konstanten und Field-Symbols
*----------------------------------
*-------------------------------------
DATA:    c_nodata(1)    TYPE c VALUE '/',   " Default f? NODATA
         xon                   VALUE 'X'.   " Flag eingeschaltet

DATA:    fmf1ges(1)     TYPE x VALUE '20'.  " Beide Flags aus: Input.
DATA:    fmb1num(1)     TYPE x VALUE '10'.  "       "

DATA:    max_commit(4)  TYPE n.        " Max. Belege je Commit

DATA:    rep_name_a(8)  TYPE c VALUE 'SAPMF05A'. " Mpool SAPMF05A
DATA:    rep_name_c(8)  TYPE c VALUE 'SAPLFCPD'. " Mpool SAPLFCPD
DATA:    rep_name_k(8)  TYPE c VALUE 'SAPLKACB'. " Mpool SAPLKACB

FIELD-SYMBOLS: <f1>.

*..
TABLES : skb1, "G/L 계정마스터 (회사코드)
         ska1. "G/L 계정마스터레코드 (계정과목표)


DATA: BEGIN OF bbkpf_tab OCCURS 0,
        tabname    LIKE  dd02l-tabname,
        fieldname  LIKE  dd03d-fieldname,
      END OF bbkpf_tab.

DATA : bbseg_tab LIKE bbkpf_tab OCCURS 0 WITH HEADER LINE,
       bwith_tab LIKE bbkpf_tab OCCURS 0 WITH HEADER LINE,
       bbtax_tab LIKE bbkpf_tab OCCURS 0 WITH HEADER LINE. "# JI.PARK

DATA : function    LIKE rfpdo-rfbifunct VALUE 'C',
       anz_mode    LIKE rfpdo-allgazmd  VALUE 'E', "# JI.PARK
       update      LIKE rfpdo-allgvbmd  VALUE 'S'. "# JI.PARK

DATA : BEGIN OF etab OCCURS 0.
        INCLUDE STRUCTURE itab.
DATA :   text  LIKE t100-text,
         err_check(5) TYPE c,
       END OF etab.

FIELD-SYMBOLS : <fn>.

DATA : fname(30),
       s_cnt TYPE i,
       e_cnt TYPE i,
       newko(10) TYPE n,
       mwskz_flag.

DATA : loop_lines  LIKE  sy-loopc.
DATA : v_blart LIKE bbkpf-blart.
DATA v_cnt TYPE i. "count
