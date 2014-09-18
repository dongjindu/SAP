*&---------------------------------------------------------------------*
*& Report  ZRFFMCHECK_STRUCTURE                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*& bereitgestellt zur Aufklärung der großen Belegbündel an Tübingen    *
*& durch Berater (Markus Herhold, Lutz Rohloff)                        *
*&---------------------------------------------------------------------*
* Programm prüft pro Finanzkreis/ Haushaltsjahr, die Anzahl der
* selektierten Belegbündel, die eine vorgegebene Größe (Parameter)
* übersteigen und gibt einen Startbeleg je Bündel aus
* zur weiteren Analyse eines einzelnen Bündel kann ZRFFMCHECK_BUNDLE
* verwendet werden

REPORT  zrffmcheck_structure.

*-Typepools------------------------------------------------------------*
TYPE-POOLS:
fmfi.

*-Includes-------------------------------------------------------------*
INCLUDE:
ifmficon,
ififmcon_bool,
ififmcon_appl,
fm_rffms200_data.

*-Tabellen-------------------------------------------------------------*
TABLES:
  bkpf.

*- Datendeklaration ---------------------------------------------------*
DATA:
  g_t_fi_keys     TYPE fmpa_t_fi_header_type       WITH HEADER LINE,
  g_t_header      LIKE g_t_fi_header  OCCURS 0     WITH HEADER LINE,
  g_t_items       LIKE g_t_fi_items   OCCURS 0     WITH HEADER LINE,
  g_bukrs         LIKE t001-bukrs,

BEGIN OF g_t_big_bundles OCCURS 0,
  bukrs   LIKE  bkpf-bukrs,
  belnr   LIKE  bkpf-belnr,
  gjahr   LIKE  bkpf-gjahr,
  size    LIKE  sy-tfill,
END OF g_t_big_bundles.

*- Parameter und Select-Options (S200) --------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK block01 WITH FRAME TITLE text-010.
PARAMETERS:
*- Buchungskreis
  p_fikrs LIKE fmifiit-fikrs   OBLIGATORY MEMORY ID fik,

*- Geschäftsjahr
  p_gjahr LIKE bkpf-gjahr   OBLIGATORY MEMORY ID gjr.

SELECT-OPTIONS: so_belnr FOR bkpf-belnr NO-DISPLAY.
SELECTION-SCREEN SKIP 1.
PARAMETERS:

*- Größe der auszugebenden Belegbündel
p_size LIKE sy-tfill OBLIGATORY.

SELECTION-SCREEN END OF BLOCK block01.

************************************************************************
START-OF-SELECTION.
************************************************************************
*------Message-Handler initialisieren
  call function 'MESSAGES_INITIALIZE'.

*----- KEYs der umzusetzenden FM Belege einlesen
  CALL FUNCTION 'FM_PAYTR_READ_DOCUMENT_KEYS_FM'
       EXPORTING
            i_bukrs   = g_bukrs
            i_fikrs   = p_fikrs
            i_gjahr   = p_gjahr
            i_hhm     = con_on
       TABLES
            t_fi_keys = g_t_fi_keys
            t_belnr   = so_belnr.

  LOOP AT g_t_fi_keys WHERE dlflg IS initial.

*----- Belegweise Initialisieren
    REFRESH: g_t_header,
             g_t_items.

*----- Fi Belege lesen
    CALL FUNCTION 'FM_PAYTR_READ_DOCUMENTS_FI'
         TABLES
              t_fi_keys         = g_t_fi_keys
              t_header          = g_t_header
              t_items           = g_t_items
         EXCEPTIONS
              fi_header_missing = 1.

*----- ist das Belegbündel größer als die Prüfgröße
    DESCRIBE TABLE g_t_header LINES sy-tfill.
    CHECK sy-tfill >= p_size.
    g_t_big_bundles-size = sy-tfill.
    MOVE-CORRESPONDING g_t_fi_keys TO g_t_big_bundles.
    APPEND g_t_big_bundles.
  ENDLOOP.

  DESCRIBE TABLE g_t_big_bundles LINES sy-tfill.

*----- Gesamtzahl Bündel ab x Belege:
  WRITE: text-010, p_size, text-011, sy-tfill.
  ULINE.

*----- Startbelege: (Größe)
  WRITE:/ text-020.
  LOOP AT g_t_big_bundles.
    WRITE:/ g_t_big_bundles-bukrs,
             g_t_big_bundles-belnr,
             g_t_big_bundles-gjahr,
             g_t_big_bundles-size.
  ENDLOOP.
