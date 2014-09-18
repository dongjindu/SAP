*&---------------------------------------------------------------------*
*& Report  ZRFFMCHECK_PARALLEL_SELECTION                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zrffmcheck_parallel_selection.

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
* FI-Einzelposten
  bseg,
* FI-Köpfe
  bkpf,
* Indextabelle Inter-Company
  bvor,
* OP Kreditoren
  bsik,
* OP Debitoren
  bsid,
* FM-Einzelposten
  fmifiit,
* Selektionsbutton
  sscrfields.

*- Datendeklaration ---------------------------------------------------*
DATA:
  g_t_fi_keys    TYPE fmpa_t_fi_header_type       WITH HEADER LINE,
  g_t_header     LIKE g_t_fi_header  OCCURS 0     WITH HEADER LINE,
  g_t_items      LIKE g_t_fi_items   OCCURS 0     WITH HEADER LINE,
  g_count        TYPE i,
  g_fikrs        LIKE t001-fikrs,

 BEGIN OF g_t_check_docs OCCURS 0,
    bukrs LIKE bseg-bukrs,
    belnr LIKE bseg-belnr,
    gjahr LIKE bseg-gjahr,
 END OF  g_t_check_docs.

*- Parameter und Select-Options (S200) --------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.       "Selektionsbutton HHM
SELECTION-SCREEN FUNCTION KEY 2.       "Selektionsbutton FI

SELECTION-SCREEN BEGIN OF BLOCK block01 WITH FRAME TITLE text-010.
PARAMETERS:
*- Finanzkreis
  p_fikrs   LIKE fmifiit-fikrs MEMORY ID fik MODIF ID hhm,
  p_hhm     TYPE c  NO-DISPLAY,
*- Buchungskreis
  p_bukrs LIKE bkpf-bukrs   MEMORY ID buk MODIF ID fi.

*- Geschäftsjahr
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) yeartext FOR FIELD p_gjahr.
PARAMETERS  p_gjahr LIKE bkpf-gjahr   OBLIGATORY MEMORY ID gjr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK block02 WITH FRAME TITLE text-020.
SELECT-OPTIONS: s_belnr FOR bseg-belnr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK block02.
SELECTION-SCREEN END OF BLOCK block01.

SELECTION-SCREEN BEGIN OF BLOCK block03 WITH FRAME TITLE text-030.
PARAMETERS:
*- Buchungskreis
  p_bukrs1 LIKE bkpf-bukrs  OBLIGATORY MEMORY ID buk,

*- Geschäftsjahr
  p_gjahr1 LIKE bkpf-gjahr  OBLIGATORY MEMORY ID gjr.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK block04 WITH FRAME TITLE text-020.
SELECT-OPTIONS: s_belnr1 FOR bseg-belnr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK block04.
SELECTION-SCREEN END OF BLOCK block03.

***********************************************************************
INITIALIZATION.
***********************************************************************
*- Selektionsbutton Finanzkreis
  sscrfields-functxt_01 = text-040.
*- Selektionsbutton Buchungskreis
  sscrfields-functxt_02 = text-050.

************************************************************************
AT SELECTION-SCREEN OUTPUT.
************************************************************************

* Selektion Finanzkreis oder Buchungskreis einblenden
  IF p_hhm = con_off.
    LOOP AT SCREEN.
      IF screen-group1 = 'HHM'.
        screen-active = '0'.
      ENDIF.
      IF screen-group1 = 'FI'.
        screen-active = '1'.
        yeartext = text-041.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'HHM'.
        screen-active = '1'.
        yeartext = text-051.
      ENDIF.
      IF screen-group1 = 'FI'.
        screen-active = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

************************************************************************
AT SELECTION-SCREEN.
************************************************************************
  IF sscrfields-ucomm = 'FC01'.
    p_hhm = con_on.
  ELSEIF sscrfields-ucomm = 'FC02'.
    p_hhm = con_off.
  ENDIF.

*---Eingabeprüfung für Finanz- bzw. Buchungskreis
  PERFORM check_input(rffms200)  USING p_hhm
                                       p_bukrs
                                       p_fikrs.


************************************************************************
START-OF-SELECTION.
************************************************************************

*----- Buchungskreis lesen
  IF p_hhm IS INITIAL.
    CALL FUNCTION 'FMFK_GET_FIKRS_FROM_BUKRS'
      EXPORTING
        i_bukrs = p_bukrs
      IMPORTING
        e_fikrs = g_fikrs.
  ELSE.
    g_fikrs = p_fikrs.
  ENDIF.

*----- KEYs der umzusetzenden FM Belege einelesen
  CALL FUNCTION 'FM_PAYTR_READ_DOCUMENT_KEYS_FM'
    EXPORTING
      i_bukrs   = p_bukrs
      i_fikrs   = g_fikrs
      i_gjahr   = p_gjahr
      i_hhm     = con_on
    TABLES
      t_fi_keys = g_t_fi_keys
      t_belnr   = s_belnr.

  LOOP AT s_belnr1.
    g_t_check_docs-bukrs = p_bukrs1.
    g_t_check_docs-gjahr = p_gjahr1.
    g_t_check_docs-belnr = s_belnr1-low.
    APPEND g_t_check_docs.
  ENDLOOP.

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

    LOOP AT g_t_check_docs.
      READ TABLE g_t_header WITH KEY bukrs = g_t_check_docs-bukrs
                                     belnr = g_t_check_docs-belnr
                                     gjahr = g_t_check_docs-gjahr.
      IF sy-subrc = 0.
        g_count = g_count + 1.
        WRITE: 'Ausgleichsbündel ',g_count.
        PERFORM open_write(saplfmpa) TABLES
                                         g_t_header.


      ENDIF.
      IF g_count = 2.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
