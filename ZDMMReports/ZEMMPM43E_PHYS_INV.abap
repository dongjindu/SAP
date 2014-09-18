************************************************************************
* Program Name      : ZEMMPM43E_PHYS_INV
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.08.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K909346
* Addl Documentation:
* Description       : Physical Inventory - Cycle Count
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.08.     Sung-Tae Lim     UD1K909346     Initial Coding
*
*
************************************************************************


REPORT rlinv050 USING DATABASE s1l
                MESSAGE-ID l4 NO STANDARD PAGE HEADING.
*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments PA8 20020823
INITIALIZATION.
  DATA: mgv_matnr_prog LIKE rsvar-report,
        mgv_matnr_selopt_tab LIKE rsldbdfs OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS <mgv_matnr_selopt_conv> TYPE STANDARD TABLE.
  mgv_matnr_prog = sy-repid.
  mgv_matnr_selopt_tab-name = 'MATNR' .
  APPEND mgv_matnr_selopt_tab.
  CALL FUNCTION 'MGV_SELOP_AFTER_INITIALIZATION'
       EXPORTING
            program        = mgv_matnr_prog
       TABLES
            selop          = mgv_matnr_selopt_tab
       EXCEPTIONS
            no_programname = 1
            OTHERS         = 2.

START-OF-SELECTION.
  LOOP AT mgv_matnr_selopt_tab.
    CONCATENATE mgv_matnr_selopt_tab-name'[]' INTO
    mgv_matnr_selopt_tab-name.
    ASSIGN (mgv_matnr_selopt_tab-name) TO <mgv_matnr_selopt_conv>.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'MGV_SELOP_AFTER_START_OF_SEL'
           EXPORTING
                selopt_name = mgv_matnr_selopt_tab-name
           TABLES
                range       = <mgv_matnr_selopt_conv>.
    ENDIF.
  ENDLOOP.
*{SEL-OPT End}
*----------------------------------------------------------------------*
*    Report RLINV050: Report zur Selektion von Lagerplätzen zur        *
*                     Cycle Counting Inventur                          *
*    This report reads all quants per storage type. Each will be       *
*    stored in an internal table and an IM-report submitted getting    *
*    the table via export/import to/from memory.  It checks if a       *
*    materials are allowed for cycle counting and calculates the new   *
*    inventory dates. These are compared to the timespan for which     *
*    you want to create an inventory list and a list output            *
*    is generated.                                                     *
*                                                                      *
*    Checks:                                                           *
*             New indicator for inventory: CC for Cycle Counting       *
*    In IM:   In Storage view of material master: CC-Indicator maint.  *
*             Table 159C (accessable via Cust) has to be maintained    *
*----------------------------------------------------------------------*
*                                                                      *
*       Zeitpunkte:                                                    *
*         GET LAGP                                                     *
*         GET LQUA                                                     *
*         GET LAGP LATE                                                *
*         START-OF-SELECTION                                           *
*         END-OF-SELECTION                                             *
*         TOP-OF-PAGE                                                  *
*         USER-COMMAND                                                 *
*                                                                      *
*       Rou-teenies:                                                   *
*                                                                      *
*         AUSGABE                 Output preparation                   *
*         AUSTAB_CREATE           Creating selected bin-table after IM *
*         AUSGABE_STATISTIK       Statistical data                     *
*         AUSGABE_ZEILE           Line output                          *
*         AT_SELECTION_SCREEN
*         BELEG_OEFFNEN           CTU Preparation                      *
*         BELEG_SCHLIESSEN        CTU Preparation                      *
*         ERROR_LGTYP_FILL        Falsely selected storage types       *
*         FEHLER_ANZEIGEN         Error handling                       *
*         INVENTUR_ANLEGEN        CTU Preparation & CTU                *
*         INVENTUR_PLATZ          CTU Preparation & CTU                *
*         LQUA_INV_PRUEFEN        Check for inventory ability          *
*         LQUAB_LESEN             Read summed info per bin             *
*         MEUES_DYNPRO            Triggering new dynpro                *
*         MIXED_BIN_CHECK         Concatenate several tabs -> 1 austab *
*         PAI                     Sy-UCOMM relevant check              *
*         PARAMETER_KONSISTENZ    Check number of bins / inv.document  *
*         RANGE_FILL              Newly rebuilt S1_LGTYP               *
*         SELEKTION_IM            XWMIM export to IM, check and return *
*         SELEKTION_IM_AUSTAB     Creation of Output table  after IM   *
*         SORT_AUSTAB             Different sorts available            *
*         SORT_PREPARE            Prepare sort per selected column     *
*         STORAGE_TYPE_CHECK      Check of selected storage types      *
*         SU_BLOCK_STORAGE_SPECIAL Check der Summensätze               *
*         T300T_LESEN             Read Whse number text                *
*         T301T_LESEN             Read stor.type text                  *
*         T331_LESEN              Read view storage type control       *
*         TAB_MARK                Mark AUSTAB-Record                   *
*         TAB_MODIFY              Modify AUSTAB-Record                 *
*         TAB_READ                Read Austab-Record                   *
*         TOP_OF_PAGE             Titels and statistics call           *
*----------------------------------------------------------------------*
*..........DATA DEFINITION ............................................

  TABLES: lagp,
          lqua, lquab,
          link,
          lmess,
          t300t,                         " Lagernummerbezeichnungen
          t301t,                         " Lagertypbezeichnungen
          t331,                          " Lagertypsteuerung
          t331b,                         " Lagertypsteuerung Blocklager
          t340d.                         " Lagernummerndefaultwerte


**--- insert by stlim (2004/04/08)
  TABLES : marc,
           makt.
**--- end of insert


*-----------------------------------------------------------------------
*       Teil 1 :  Tabellen      ( BEGIN OF ... OCCURS )
*-----------------------------------------------------------------------

*........Interne Tabellen.

*........Reihenfolge: Platz - Kreuz - Kzpmi   wichitg für Ausgabe....
  DATA:   BEGIN OF tab OCCURS 100,
             lgtyp LIKE lagp-lgtyp,
             lgpla LIKE lagp-lgpla,
             idatu LIKE lagp-idatu,
             werks LIKE lqua-werks,
             matnr LIKE lqua-matnr,
             sperr TYPE c,               "ind. blocked due to transfer
             kzmmq TYPE c,               "ind. bin with several quants

**--- insert by stlim (2004/04/12)
             gesme LIKE lqua-gesme,
             meins LIKE lqua-meins,
             lgnum LIKE lagp-lgnum,
             lqnum LIKE lqua-lqnum,
             abcin LIKE marc-abcin,
             plpos LIKE lqua-plpos,
**--- end of insert

          END OF tab.
*........Output-Sructure...............................................
  DATA:   BEGIN OF austab OCCURS 100,
             lgtyp LIKE lagp-lgtyp,
             lgpla LIKE lagp-lgpla,
             kreuz TYPE c,               "ind. to be invented
             kzpmi TYPE c,               "ind. Plus Minus Initial
             idatu LIKE lagp-idatu,
             werks LIKE lqua-werks,
             matnr LIKE lqua-matnr,
             charg LIKE lqua-charg,
             flg_aktiv  TYPE c,          "ind.
             nidat LIKE lagp-idatu,      "New Inventory date
             ttext(25)   TYPE c,         "Textfield
             sperr TYPE c,               "ind. blocked due to transfer
             kzmmq TYPE c,               "ind. bin with several quants
             iverz(1)   TYPE c,
             linno LIKE sy-linno,
             pagno LIKE sy-pagno,

**--- insert by stlim (2004/04/12)
             gesme LIKE lqua-gesme,
             meins LIKE lqua-meins,
             maktx LIKE makt-maktx,
             lgnum LIKE lagp-lgnum,
             lqnum LIKE lqua-lqnum,
             abcin LIKE marc-abcin,
             plpos LIKE lqua-plpos,
**--- end of insert

          END OF austab.
*.........IM-structure................................................
  DATA:   BEGIN OF xwmim OCCURS 100,
             werks LIKE lqua-werks,
             matnr LIKE lqua-matnr,
             idatu LIKE lagp-idatu,
             nidat LIKE lagp-idatu,
             kzpmi TYPE c,
          END OF xwmim.

  DATA:   BEGIN OF wmdat,
             v_dat LIKE lagp-idatu,
             b_dat LIKE lagp-idatu,
          END OF wmdat.

  DATA:   BEGIN OF bdc_daten OCCURS 0.
          INCLUDE STRUCTURE bdcdata.
  DATA:   END OF bdc_daten.

  DATA: BEGIN OF mess OCCURS 0.
          INCLUDE STRUCTURE lmess.
  DATA: END OF mess.

  RANGES:   ilgtyp              FOR  t301-lgtyp.
  DATA:     error_lgtyp         LIKE t301t  OCCURS 20.
  DATA:     it301t              LIKE t301t OCCURS 20.
  DATA:     it331               LIKE t331  OCCURS 20.
  DATA:     lin TYPE p.
  DATA       sav_bulk_lgtyp    LIKE t331-lgtyp.


*........Allgemeine  Konstanten ........................................

  INCLUDE zemmpm43e_phys_inv_incl.
*INCLUDE MLLVSKON.

  DATA:   con_platz(1)            TYPE c    VALUE 'P',
          con_question(1)         TYPE c    VALUE '?',
          con_negativ(1)          TYPE c    VALUE '-',
          con_kl_x(1)             TYPE c    VALUE 'x',
          im_report               LIKE sy-repid  VALUE 'RM07ICN1',
          kzinv_cc                LIKE lagp-kzinv   VALUE  'CC',
          max_anplb(2)            TYPE p    VALUE 100,
          pfstat_listbild(4)      TYPE c    VALUE 'NORM',
          pfstat_fehl(4)          TYPE c    VALUE 'FEHL',
            sav_sort(3)           TYPE c,
          titel_sors(3)           TYPE c    VALUE '004',     "KREUZ
          titel_sort(3)           TYPE c    VALUE '005',     "TTEXT
         titel_sord(3)           TYPE c    VALUE '001',     "NIDAT/IDATU
         titel_sorp(3)           TYPE c    VALUE '002',     "LGTYP/LGPLA
          titel_sorq(3)           TYPE c    VALUE '003',     "MATNR

**--- insert by stlim (2004/04/12)
          titel_sory(3) TYPE c VALUE '998',     " quantiry
          titel_sorz(3) TYPE c VALUE '999',     " description
**--- end of insert

          titel_fehl(3)           TYPE c    VALUE '009',
          tcode_lagp_anzeigen     LIKE sy-tcode  VALUE 'LS03N',
          tcode_inventur_anlegen  LIKE sy-tcode  VALUE 'LI01 ',

*........Konstanten für Batch-Input....................................

          dynpro_anforderung      LIKE sy-dynnr  VALUE '0100',
          dynpro_erfassung        LIKE sy-dynnr  VALUE '0102',
          max_lagp_dynpro(2)      TYPE p         VALUE 10,
          modpl_inv               LIKE sy-repid  VALUE 'SAPML04I',

*.........Funktionscodes..............................................*

          fcode_alle_mark(4)     TYPE c    VALUE 'MRKA',
          fcode_alle_demark(4)   TYPE c    VALUE 'MRKL',
          fcode_anz_lagp(4)      TYPE c    VALUE 'LAGP',
          fcode_back(4)          TYPE c    VALUE 'BAKK',
          fcode_error(4)         TYPE c    VALUE 'ERRO',
          fcode_beenden(4)       TYPE c    VALUE 'ERET',
          fcode_buchen(4)        TYPE c    VALUE 'BU  ',
          fcode_abbrechen(4)     TYPE c    VALUE 'EESC',
          fcode_anlegen(4)       TYPE c    VALUE 'ANLE',
          fcode_aktivieren(4)    TYPE c    VALUE 'AK  ',
          fcode_loeschen(4)      TYPE c    VALUE 'DELE',
          fcode_neues_bild(4)    TYPE c    VALUE '    ',
          fcode_sauf(4)          TYPE c    VALUE 'SAUF',
          fcode_soab(4)          TYPE c    VALUE 'SOAB',
          fcode_sorp(4)          TYPE c    VALUE 'SORP',
          fcode_sord(4)          TYPE c    VALUE 'SORD',
          fcode_sorq(4)          TYPE c    VALUE 'SORQ',

*.........Markiercodes................................................*

          mark_input(1)          TYPE c    VALUE '1',
          mark_on(1)             TYPE c    VALUE '2',
          mark_off(1)            TYPE c    VALUE '3',
          mark_switch(1)         TYPE c    VALUE '4',

*........Flags und Status...............................................

          flg_beleg_offen         TYPE c,
          flg_top                 TYPE c,
          flg_trans               TYPE c,"blocked by sth.
          flg_out                 TYPE c,"Output yes or no
          flg_lqua_gesperrt       TYPE c,
          flg_platz_nicht_buchbar TYPE c,
          flg_was_bin_ich         TYPE c,

*........Interne Hilfsfelder............................................

          antwort(1)              TYPE c,
          cursorfield(20)         TYPE c,
          cnt_fehler              TYPE i,
          cnt_lagp                TYPE i,
          cnt_lagpp               TYPE i,"total bins
          cnt_leere               TYPE i,"empty bins
          cnt_ok                  TYPE i,"IM-selected
          cnt_nok                 TYPE i,"IM-rejected
          cnt_irrel               TYPE i,"irrelevant to CycleCounting
          cnt_inven               TYPE i,"already inv. or about to be
          cnt_lqua                TYPE i,
          cnt_lagp_beleg          TYPE i,
          cnt_lagp_dynpro         TYPE i,
          hlp_anzqu(4)            TYPE c,
          hlp_gesme               LIKE rl04i-mekln,
          hlp_plobs               LIKE rl04i-plobs,
          hlp_text(300)           TYPE c,
          sav_curow               LIKE      sy-curow,
          sav_lisel               LIKE      sy-lisel,
          sav_lgnum               LIKE      lagp-lgnum,
          sav_lgtyp               LIKE      lagp-lgtyp,
          sav1_lgtyp              LIKE      lagp-lgtyp,
          sav_lgpla               LIKE      lagp-lgpla,
          sav_pfstat              LIKE      sy-pfkey,
          sav_subrc               LIKE      sy-subrc,
          sav_tabix               LIKE      sy-tabix,
          sav_titel(3)            TYPE c,
          sav_was_bin_ich(1)      TYPE c,
          tab_tabix               LIKE      sy-tabix,
          tit_kreuz               TYPE c,
          tit_lgtyp               LIKE      lagp-lgtyp,
          tit_lgpla               LIKE      lagp-lgpla,
          tit_ttext(20)           TYPE c,
          tit_matnr               LIKE      lqua-matnr,
          tit_idatu(10)           TYPE c,
          tit_nidat(10)           TYPE c,

**--- insert by stlim (2004/04/12)
          tit_gesme(10)           TYPE c,
          tit_maktx(20)           TYPE c,
          tit_abcin(03)           TYPE c,
**--- end of insert

*........Inital-Felder..................................................

          init_bestq              LIKE     lqua-bestq,
          init_sobkz              LIKE     lqua-sobkz,
          init_ausme              LIKE     lqua-ausme,
          init_einme              LIKE     lqua-einme,
          init_ivivo              LIKE     lagp-ivivo,
          init_nidat              LIKE     lagp-idatu,
          init_kzinv              LIKE     lagp-kzinv,
          init_skzsa              LIKE     lagp-skzsa,
          init_skzse              LIKE     lagp-skzse,
          init_skzsi              LIKE     lqua-skzsi,
          init_skzue              LIKE     lagp-skzue,
          init_skzua              LIKE     lagp-skzua,
          init_x                  LIKE     lagp-ivivo.

*........Field-Groups..................................................


*........Reportspezifische Parameter und Select-Options.................
  SELECTION-SCREEN  BEGIN OF BLOCK xxx WITH FRAME TITLE text-010.
  SELECT-OPTIONS:
*........Nur bestimmte Materialnummern.................................
               matnr FOR lqua-matnr.
*........Selektionszeitraum ...........................................
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT  1(30) text-030.
  SELECTION-SCREEN POSITION 33.
  PARAMETERS: v_dat LIKE lagp-idatu  DEFAULT sy-datlo.
  SELECTION-SCREEN COMMENT 52(03) text-031.
  SELECTION-SCREEN POSITION 58.
  PARAMETERS: b_dat LIKE lagp-idatu  DEFAULT sy-datlo.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS:
*........General date for inv. documents to be created ................
               pdatu LIKE link-pdatu DEFAULT sy-datlo,
*........Binding inv. documents with a reference ......................
               irnum LIKE link-irnum,
*........Maximum number of bins per inv document ......................
               anplb LIKE rl04i-anplb,
*........Activate selected bins directly...............................
               liakt LIKE rl04i-liakt,
*........Take date of last placement into stock as inv. date...........
               p_edatu LIKE rl04i-edatu,
*........Guy who is supposed to do the count..........................
               p_uname LIKE link-uname.


**--- insert by stlim (2004/04/08)
  SELECTION-SCREEN ULINE.
  PARAMETERS : p_werks LIKE marc-werks OBLIGATORY DEFAULT 'P001'.
  SELECT-OPTIONS : s_abcin FOR marc-abcin DEFAULT 'A'.
**--- end of insert


  SELECTION-SCREEN END OF BLOCK xxx.


*---------------------------------------------------------------------*
*          INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.
  b_dat = b_dat + '07'.

*..........AT SELECTION-SCREEN ........................................
AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

* .........START-OF-SELECTION .........................................
START-OF-SELECTION.
*{   INSERT         PA8K018651                                        1
* MATNR EXT-LISTEN

**--- blocked by stlim (2004/04/12)
*  NEW-PAGE LINE-SIZE 105.
**--- end of block

**--- insert by stlim (2004/04/12)
  NEW-PAGE LINE-SIZE 132.
**--- end of insert

*}   INSERT
  flg_top = con_true.
  flg_lqua_gesperrt = con_false.

*---------------------------------------------------------------------*
*          GET LAGP
*---------------------------------------------------------------------*
GET lagp FIELDS lgnum lgtyp lgpla ivivo skzsi skzsa skzse idatu
                skzue skzua anzqu.
  CLEAR tab.
  cnt_lagpp = cnt_lagpp + 1.
*.........No bins which are aready in an inv. Document..................

  IF lagp-ivivo <> init_ivivo OR
     lagp-skzsi <> init_skzsi.
    cnt_inven = cnt_inven + 1.
    CHECK 1 = 2.
  ENDIF.
*........No blocking indicators on bins allowed ........................

  IF lagp-skzse <> init_skzse OR
     lagp-skzsa <> init_skzsa OR
     lagp-skzue <> init_skzue OR
     lagp-skzua <> init_skzua.
*   CNT_PSPER = CNT_PSPER + 1.
    tab-sperr = con_x.
  ENDIF.
*........No empty bins .................................................
  IF lagp-anzqu = 0.
    cnt_leere = cnt_leere + 1.
  ENDIF.


*---------------------------------------------------------------------*
*         GET LQUA
*---------------------------------------------------------------------*
GET lqua FIELDS lgnum lgtyp lgpla matnr werks ivnum einme ausme
                skzsi skzse skzsa skzua skzue gesme bestq sobkz
                sonum charg edatu

**--- insert by stlim (2004/04/12)
                meins lqnum plpos wdatu idatu edatu.
**--- end of insert

  CLEAR xwmim.

*........here, 'cause the clear tab should not destroy the block.ind...
  IF lagp-anzqu > 1.
**--- blocked by stlim (2004/04/14)
*    tab-kzmmq = con_x.
**--- end of block
**--- insert by stlim (2004/04/14)
    tab-kzmmq = con_x.
**--- end of insert
  ENDIF.

  flg_lqua_gesperrt = con_false.
*........only range of materials......................................
  CHECK select-options.


**--- insert by stlim (2004/04/08)
*--- check plant & ABC indicator
  CHECK p_werks EQ lqua-werks.

  CLEAR : marc.
  SELECT SINGLE werks matnr
                abcin       INTO (marc-werks, marc-matnr, marc-abcin)
                            FROM marc
                           WHERE matnr EQ lqua-matnr
                             AND werks EQ lqua-werks
                             AND abcin IN s_abcin.
  CHECK sy-subrc EQ 0.
**--- end of insert


  PERFORM lqua_inv_pruefen.
*........Check if transport is due ...................................*
*........Storage unit management specialiies.........................
  PERFORM su_block_storage_special.
*........everything ok. then appending to internal treatment..........
  IF flg_lqua_gesperrt =  con_false.
    MOVE: lagp-lgtyp       TO tab-lgtyp,
          lagp-lgpla       TO tab-lgpla,
          lqua-werks       TO tab-werks,
          lqua-matnr       TO tab-matnr.

**--- insert by stlim (2004/04/12)
    MOVE : lqua-gesme      TO tab-gesme,
           lqua-meins      TO tab-meins.
    MOVE : lqua-lgnum      TO tab-lgnum,
           lqua-lqnum      TO tab-lqnum.
    MOVE : marc-abcin      TO tab-abcin.
    MOVE : lqua-lgtyp      TO tab-lgtyp,
           lqua-plpos      TO tab-plpos.
**--- end of insert

    MOVE: lqua-werks        TO xwmim-werks,
          lqua-matnr        TO xwmim-matnr.
*.......accepting IDATU only when quant has an IVNUM..................
*       this means: quant was invented on bin
    IF NOT lqua-ivnum IS INITIAL.
**--- blocked by stlim (2004/04/14)
*      MOVE: lagp-idatu TO xwmim-idatu,
*            lagp-idatu TO tab-idatu.
**--- end of block
**--- insert by stlim (2004/04/14)
      MOVE: lqua-idatu TO xwmim-idatu,
            lqua-idatu TO tab-idatu.
**--- end of insert
    ENDIF.
*.......if there is no inventory data: take date of quant creation....
    IF xwmim-idatu IS INITIAL OR
       xwmim-idatu EQ space.
      IF p_edatu = con_x.
        MOVE: lqua-edatu TO xwmim-idatu,
              lqua-edatu TO tab-idatu.
      ENDIF.
    ENDIF.
*........Before outputting cnt_ok is subtracted, so don't worry here...

**--- insert by stlim (2004/04/14)
    IF lqua-plpos NE space.
      CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
           EXPORTING
                lgpla   = lqua-lgpla
                plpos   = lqua-plpos
           IMPORTING
                o_lgpla = tab-lgpla.
    ENDIF.
**--- end of insert

    APPEND tab.
    CLEAR tab.
*........several identical recordds are ignored. Only one collected...
    COLLECT xwmim.
  ENDIF.
*--------------------------------------------------------------------*
*            END-OF-SELECTION                                        *
*--------------------------------------------------------------------*
END-OF-SELECTION.

  SET PF-STATUS pfstat_listbild.
  SORT tab.
  SET TITLEBAR titel_sorq.
  sav_titel = titel_sorq.
  PERFORM parameter_konsistenz.        "Check for number bins/inv.doc.
  PERFORM selektion_im.                "und Ausgabe der zuückgemeldeten
*........Only output or activation immediately.........................
  IF liakt = con_x.
**--- blocked by stlim (2004/04/14)
*    PERFORM inventur_anlegen.
**--- end of block
**--- insert by stlim (2004/04/14)
    PERFORM create_inventory_document.
**--- end of insert
  ELSE.
    PERFORM sort_austab USING titel_sorq fcode_sauf.  "sort by material
    PERFORM ausgabe.
  ENDIF.
  IF flg_out = con_false.
    MESSAGE s136.
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.
*--------------------------------------------------------------------*
*            TOP-OF-PAGE                                             *
*--------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM top_of_page.

*--------------------------------------------------------------------*
*            TOP-OF-PAGE DURING LINE-SELECTION                       *
*--------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

*--------------------------------------------------------------------*
*            AT USER-COMMAND                                         *
*--------------------------------------------------------------------*
AT USER-COMMAND.

*........ Further processing due to keyed in command..................
  PERFORM pai.
  CASE sy-ucomm.

*---------------------------------------------------------------------*
*       Demark all austab's                                           *
*---------------------------------------------------------------------*
    WHEN fcode_alle_demark.

      LOOP AT austab
      WHERE kreuz =  con_x
      AND iverz   <> con_x.
        PERFORM tab_mark USING mark_off sy-tabix.
      ENDLOOP.
*---------------------------------------------------------------------*
*       Mark all austab's                                             *
*---------------------------------------------------------------------*
    WHEN fcode_alle_mark.

      LOOP AT austab
      WHERE kreuz =  space
      AND iverz   <> con_x.
        PERFORM tab_mark USING mark_on sy-tabix.
      ENDLOOP.

*---------------------------------------------------------------------*
*       Verarbeitung von PF9 Markieren                                *
*---------------------------------------------------------------------*
    WHEN fcode_markieren.
      PERFORM tab_read USING tab_tabix.

      IF sy-subrc <> 0 OR austab-iverz = con_x.
        IF austab-iverz = con_x.
          MESSAGE s175 WITH austab-lgpla.
        ENDIF.
        EXIT.
      ENDIF.
      PERFORM tab_mark USING mark_switch tab_tabix.

*---------------------------------------------------------------------*
*       Löschen Lagerplätze, für die die Belege erzeugt wurden        *
*---------------------------------------------------------------------*
    WHEN fcode_loeschen.
*
*     loop at austab where kreuz = con_stern.
      DELETE austab WHERE kreuz = con_stern.
*     endloop.
      PERFORM ausgabe.
      IF cnt_lagp = 0.
        MESSAGE s137.
        IF sy-calld <> space. LEAVE. ENDIF.
        LEAVE TO TRANSACTION sy-tcode.
      ENDIF.
      sy-lsind = 0.

*---------------------------------------------------------------------*
*       Verarbeitung von PF3 Zurück                                   *
*---------------------------------------------------------------------*
    WHEN fcode_back.

      IF sy-pfkey <> pfstat_fehl.
        IF sy-calld <> space. LEAVE. ENDIF.
        LEAVE TO TRANSACTION sy-tcode.
      ELSE.
        SET PF-STATUS sav_pfstat.
        SET TITLEBAR sav_titel.
        PERFORM ausgabe.
        sy-lsind = 0.
      ENDIF.
*---------------------------------------------------------------------*
*       Processing of ERRO: list the falsely selected storage types   *
*---------------------------------------------------------------------*
    WHEN fcode_error.
      SET PF-STATUS pfstat_fehl.
      sav_pfstat = pfstat_listbild.
*      sav_titel  = titel_sord.
      SKIP.
      PERFORM ausgabe_statistik.
      SKIP.
      DESCRIBE TABLE error_lgtyp LINES lin.
      IF lin > 0.

        WRITE: / text-057.

        WRITE: /1(31) sy-uline.
        LOOP AT error_lgtyp INTO t301t.
          WRITE: /1 sy-vline,
                  2 t301t-lgtyp COLOR COL_NORMAL,
                  5 sy-vline,
                  6 t301t-ltypt COLOR COL_NORMAL,
                  31 sy-vline.
        ENDLOOP.
        WRITE: /1(31) sy-uline.
      ENDIF.
      sy-lsind = 0.
*---------------------------------------------------------------------*
*       Verarbeitung von PF15 Beenden                                 *
*---------------------------------------------------------------------*
    WHEN fcode_beenden.
      LEAVE.
      LEAVE TO TRANSACTION sy-tcode.

*---------------------------------------------------------------------*
*       Verarbeitung von PF12 Abbrechen                               *
*---------------------------------------------------------------------*
    WHEN fcode_abbrechen.
      IF sy-calld <> space. LEAVE. ENDIF.
      LEAVE TO TRANSACTION sy-tcode.

*---------------------------------------------------------------------*
*       Sortieren absteigend  / Sort descending                       *
*---------------------------------------------------------------------*
    WHEN fcode_soab.
      PERFORM sort_prepare USING fcode_soab.

*---------------------------------------------------------------------*
*       Sortieren aufteigend  / Sort ascending                        *
*---------------------------------------------------------------------*
    WHEN fcode_sauf.
      PERFORM sort_prepare USING fcode_sauf.

*---------------------------------------------------------------------*
*         Anzeigen Lagerplatz                                         *
*---------------------------------------------------------------------*
    WHEN fcode_anz_lagp.

      SET PARAMETER:  ID parid_lgnum FIELD s1_lgnum,
                      ID parid_lgtyp FIELD austab-lgtyp,
                      ID parid_lgpla FIELD austab-lgpla.

      CALL TRANSACTION tcode_lagp_anzeigen AND SKIP FIRST SCREEN.

*---------------------------------------------------------------------*
*       Anlegen Inventurbeleg                                         *
*---------------------------------------------------------------------*
    WHEN fcode_anlegen.
*     perform pai.
      CLEAR liakt.
      flg_top = con_false.
      PERFORM inventur_anlegen.

*---------------------------------------------------------------------*
*       Aktivieren Inventurbeleg                                      *
*---------------------------------------------------------------------*
    WHEN fcode_aktivieren.
*     perform pai.
      liakt = con_x.
      flg_top = con_false.

**--- blocked by stlim (2004/04/13)
*      PERFORM inventur_anlegen.
**--- end of block

**--- insert by stlim (2004/04/13)
      PERFORM create_inventory_document.
**--- end of insert

  ENDCASE.
  CLEAR flg_was_bin_ich.

***********************************************************************
*        Begin of Routeenies     "AAA
***********************************************************************

*---------------------------------------------------------------------*
*       FORM AUSGABE                                                  *
*---------------------------------------------------------------------*
*       Loop over table tab                                           *
*       OUTPUT Only:  kreuz = 'X' or kreuz = ' ' AND Kzpmi = '+'.
*       meaning: only selected now or due in future.
*---------------------------------------------------------------------*
FORM ausgabe.
*{   INSERT         KA5K019240                                        1
* MATNR EXT-LISTEN
  DATA rxm0_length0 TYPE i.
  DATA rxm0_len80   TYPE i.
  DESCRIBE FIELD austab-matnr OUTPUT-LENGTH rxm0_length0.
  rxm0_len80 = 80 + rxm0_length0 - 18.

*}   INSERT
  DATA: flg_intens TYPE c.
  flg_out = con_false.
  CLEAR sav1_lgtyp.

  LOOP AT austab.                      "AÖA
*.........when sort by bin then with new-page, not for date-&mat-sort..
    AT NEW lgtyp.
      IF sav_titel = titel_sorp.       "nur bei Lagerplatz new-page
        NEW-PAGE.
      ENDIF.
    ENDAT.

*.......There is a successful selection ? ...........................
    flg_out = con_true.
*.......Output - complication........................................
    IF flg_intens = 0.
      FORMAT INTENSIFIED ON.
      flg_intens = 1.
    ELSE.
      FORMAT INTENSIFIED OFF.
      flg_intens = 0.
    ENDIF.
    tab_tabix = sy-tabix.

*{   REPLACE        KA5K019240                                        2
*\    WRITE /(80) SPACE COLOR COL_NORMAL.
* MATNR EXT-LISTEN
    WRITE AT /(rxm0_len80) space COLOR COL_NORMAL.
*}   REPLACE
    WRITE: /1 sy-vline.
    IF austab-iverz = con_x OR
      austab-kreuz = con_stern.
      WRITE:   2 austab-kreuz.
    ELSE.
      WRITE:   2 austab-kreuz AS CHECKBOX INPUT.
    ENDIF.
    WRITE: 3 sy-vline.
    PERFORM ausgabe_zeile.
*........Check counter after deleting processed bins -> sy-ucomm
    cnt_lagp = cnt_lagp + 1.
  ENDLOOP.

*{   REPLACE        KA5K019240                                        3
*\  WRITE: /(80) SY-ULINE.
* MATNR EXT-LISTEN

**--- blocked by stlim (2004/04/12)
*  WRITE: AT /(rxm0_len80) sy-uline.
**--- end of block

**--- insert by stlim (2004/04/12)
  WRITE : /(132) sy-uline.
**--- end of insert

*}   REPLACE
* endif.
  CLEAR austab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM AUSGABE_STATISTIK                                        *
*---------------------------------------------------------------------*
*       AUSGABE_STATISTIK                                              *
*---------------------------------------------------------------------*
*      cnt_lagpp     Lagerplätze gesamt    / total # of bins
*      cnt_ok        vorgeschlagene Quants / quants selected for count
*      cnt_leere     Leerplätze            / empty bins, not relevant
*      cnt_nok       Quants mit Sperre     / due later or blocked quants
*      cnt_inven     Plätze bereits in Inv / bins already in invent.doc.
*---------------------------------------------------------------------*
FORM ausgabe_statistik.

  cnt_irrel =  cnt_lagpp - ( cnt_inven + cnt_leere + cnt_nok + cnt_ok ).
*........1.line.............
  WRITE: /1(60) sy-uline.
  WRITE: /1 sy-vline,
          2  text-065  COLOR COL_NORMAL INTENSIFIED ON,
          19 sy-vline,
*         20 cnt_ok    color col_normal intensified off,
          31 sy-vline,
          32 text-062  COLOR COL_NORMAL INTENSIFIED ON,
          49 sy-vline,
          50 cnt_lagpp COLOR COL_NORMAL INTENSIFIED OFF,
          60 sy-vline.
*........2.line.............
  WRITE: /1(60) sy-uline.
  WRITE: /1 sy-vline,
*         2  text-063  color col_normal,
          2  text-061  COLOR COL_NORMAL INTENSIFIED ON,
          19 sy-vline,
*         20 cnt_inven color col_normal,
          20 cnt_ok    COLOR COL_NORMAL INTENSIFIED OFF,
          31 sy-vline,
          32 text-064  COLOR COL_NORMAL,
          49 sy-vline,
          50 cnt_leere COLOR COL_NORMAL,
          60 sy-vline.
*........3.line.............
* WRITE: /1(60) SY-ULINE.
  WRITE: /1 sy-vline,
*         2  text-065  color col_normal,
          2  text-066  COLOR COL_NORMAL,
          19 sy-vline,
*         20 cnt_irrel color col_normal,
          20 cnt_nok   COLOR COL_NORMAL,
          31 sy-vline,
*         32 text-066  color col_normal,
          32 text-063  COLOR COL_NORMAL,
          49 sy-vline,
*         50 cnt_nok   color col_normal,
          50 cnt_inven COLOR COL_NORMAL,
          60 sy-vline.
  WRITE: /1(60) sy-uline.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM AUSGABE_ZEILE                                            *
*---------------------------------------------------------------------*
*       Ausgabe der einzelnen Zeilen der Liste, summierte Kombinationen
*       von WERKS innerhalb einer Lagernummer und eines  Lagertyps
*---------------------------------------------------------------------*
FORM ausgabe_zeile.

  IF austab-kreuz = con_x AND austab-kzpmi = space
    AND austab-sperr = space.
    MOVE text-040 TO austab-ttext.     "vorgesehen
  ENDIF.

  IF austab-kreuz = con_x AND austab-kzpmi = con_negativ
     AND austab-sperr = space.
    MOVE text-041 TO austab-ttext.     "überfällig
  ENDIF.
*.......Nidat <> 0 means: this one came back from IM but is blocked....
*       this is to make sure IM didn't just kill the record and we
*       didn't notice it.
  IF austab-kreuz = space AND austab-kzpmi = space
    AND austab-sperr = con_x AND austab-nidat <> init_nidat.
    MOVE text-042 TO austab-ttext.     "vorgesehen aber mit Sperre
  ENDIF.
*.......here the existence of kzpmi negativ means it came from IM......
  IF austab-kreuz = space AND austab-kzpmi = con_negativ
     AND austab-sperr = con_x.
    MOVE text-043 TO austab-ttext.     "überfällig, aber mit Sperre
  ENDIF.

  WRITE:
*{   REPLACE        KA5K003221                                        1
*\       4  AUSTAB-LGTYP COLOR COL_NORMAL,
*\       7    SY-VLINE,
*\       8  AUSTAB-LGPLA COLOR COL_NORMAL,
*\       18   SY-VLINE,
*\       19 AUSTAB-MATNR COLOR COL_NORMAL,
*\       37   SY-VLINE,
*\       38 AUSTAB-TTEXT COLOR COL_NORMAL,
*\       58   SY-VLINE,
*\       59 AUSTAB-IDATU DD/MM/YYYY COLOR COL_NORMAL,
*\       69   SY-VLINE,
*\       70 AUSTAB-NIDAT DD/MM/YYYY COLOR COL_NORMAL,
*\       80   SY-VLINE.
* MATNR EXT-LISTEN
         4 austab-lgtyp COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP,
           austab-lgpla COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP,

**--- blocked by stlim (2004/04/12)
*           austab-matnr COLOR COL_NORMAL NO-GAP,
**--- end of block

**--- insert by stlim (2004/04/12)
           (18) austab-matnr COLOR COL_NORMAL NO-GAP,
           sy-vline COLOR COL_NORMAL NO-GAP,
           (30) austab-maktx COLOR COL_NORMAL NO-GAP,
**--- end of insert

           sy-vline NO-GAP,
        AT (20) austab-ttext COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP,
           austab-idatu DD/MM/YYYY COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP,
           austab-nidat DD/MM/YYYY COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP.

**--- insert by stlim (2004/04/12)
  WRITE :  (12) austab-gesme UNIT austab-meins COLOR COL_NORMAL NO-GAP,
           sy-vline NO-GAP,
           austab-meins COLOR COL_NORMAL NO-GAP,
*           sy-vline COLOR COL_NORMAL NO-GAP,
*           (30) austab-maktx COLOR COL_NORMAL NO-GAP,
           sy-vline COLOR COL_NORMAL NO-GAP,
           (03) austab-abcin COLOR COL_NORMAL NO-GAP CENTERED,
           sy-vline NO-GAP.
**--- end of insert

*}   REPLACE
*      77 AUSTAB-KZPMI,
*      78   SY-VLINE.

  HIDE: s1_lgnum, s1_lgtyp-low, austab-lgtyp , austab-lgpla,
                  tab_tabix,    austab-werks, austab-matnr.

**--- insert by stlim (2004/04/12)
  HIDE : austab-gesme, austab-maktx.
**--- end of insert

  austab-linno = sy-linno.
  austab-pagno = sy-pagno.

  MODIFY austab.
ENDFORM.

*---------------------------------------------------------------------*
*       AUSTAB_CREATE                                                 *
*---------------------------------------------------------------------*
*       creation of new output file with selected data after IM-select*
*
*       small adding: if there is a mixed bin with only KZPMI = +, then
*       the bin will not be listed, but also not beeing counted
*---------------------------------------------------------------------*
FORM austab_create USING flag.

*.........Setting blocking indicator or selection indicator..........
  IF austab-sperr = con_x OR flag = con_x.
    cnt_nok = cnt_nok + 1.
  ELSE.
    austab-kreuz = con_x.
    cnt_ok = cnt_ok + 1.
  ENDIF.

  MOVE xwmim-nidat TO austab-nidat.
  MOVE xwmim-kzpmi TO austab-kzpmi.
  APPEND austab.
  CLEAR flag.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM at_selection_screen.

  IF b_dat < v_dat.
    MESSAGE e171.                      "VonDatum > BisDatum
  ENDIF.
  IF v_dat < sy-datum.
    MESSAGE e180.           "date in the past not allowed because quants
  ENDIF.                    "are then overdue and listed anyway
  PERFORM t300t_lesen USING s1_lgnum.
  PERFORM storage_type_check.

ENDFORM.                               " AT_SELECTION_SCREEN
*--------------------------------------------------------------------*
*       FORM BELEG_OEFFNEN                                           *
*--------------------------------------------------------------------*
*       Öffnen eines Inventurbeleges                                 *
*--------------------------------------------------------------------*
FORM beleg_oeffnen.

  REFRESH bdc_daten.

*........Senden Anforderbild...........................................

  CLEAR bdc_daten.
  bdc_daten-program  = modpl_inv.
  bdc_daten-dynpro   = dynpro_anforderung.
  bdc_daten-dynbegin = con_x.
  APPEND bdc_daten.

*........Lagernummer...................................................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'LINK-LGNUM'.
  bdc_daten-fval = s1_lgnum.
  APPEND bdc_daten.

*........Lagertyp......................................................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'LINK-LGTYP'.
  bdc_daten-fval = austab-lgtyp.
  APPEND bdc_daten.

*........Plandatum.....................................................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'LINK-PDATU'.
  CLEAR bdc_daten-fval.
  WRITE pdatu TO bdc_daten-fval(10).
  APPEND bdc_daten.

*........Referenznummer................................................
  IF NOT irnum IS INITIAL.
    CLEAR bdc_daten.
    bdc_daten-fnam = 'LINK-IRNUM'.
    bdc_daten-fval = irnum.
    APPEND bdc_daten.
  ENDIF.

*........Inventory type .......................................
  CLEAR bdc_daten.
  bdc_daten-fnam = 'RL04I-KZINV'.
  bdc_daten-fval = kzinv_cc.
  APPEND bdc_daten.

*........Inventory type................................................
  IF NOT p_uname IS INITIAL.
    CLEAR bdc_daten.
    bdc_daten-fnam = 'LINK-UNAME'.
    bdc_daten-fval =  p_uname.
    APPEND bdc_daten.
  ENDIF.
*........OK-Code für die Folgebildsteuerung.............................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'BDC_OKCODE'.
  CLEAR bdc_daten-fval.
  APPEND bdc_daten.

*........Senden Erfassungsbild.........................................

  CLEAR bdc_daten.
  bdc_daten-program  = modpl_inv.
  bdc_daten-dynpro   = dynpro_erfassung.
  bdc_daten-dynbegin = con_x.
  APPEND bdc_daten.

*........Fortschreiben Programmflußsteuerung............................

  flg_beleg_offen = con_true.
  cnt_lagp_beleg  = 0.
  cnt_lagp_dynpro = 0.

ENDFORM.

*--------------------------------------------------------------------*
*       FORM BELEG_SCHLIESSEN                                        *
*--------------------------------------------------------------------*
*       Schließen eines Inventurbeleges                              *
*--------------------------------------------------------------------*
FORM beleg_schliessen.

*........OK-Code fürs Buchen............................................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'BDC_OKCODE'.
  IF liakt = con_x.
    bdc_daten-fval = fcode_aktivieren.
  ELSE.
    bdc_daten-fval = fcode_buchen.
  ENDIF.
  APPEND bdc_daten.

*........Call Transaction...............................................

  CALL TRANSACTION tcode_inventur_anlegen
                   USING  bdc_daten
                   MODE   anz_nichts
                   UPDATE upd_synchron.

  IF sy-subrc <> 0.
    cnt_fehler = cnt_fehler + 1.
    mess-msgid = sy-msgid.
    mess-msgno = sy-msgno.
    mess-msgv1 = sy-msgv1.
    mess-msgv2 = sy-msgv2.
    mess-msgv3 = sy-msgv3.
    mess-msgv4 = sy-msgv4.
    APPEND mess.
    LOOP AT austab WHERE flg_aktiv = con_true.
      austab-kreuz = con_question.
      austab-flg_aktiv = con_false.
      MODIFY austab.
      READ LINE austab-linno OF PAGE austab-pagno.
      sy-lisel(1) = con_question.
      MODIFY LINE austab-linno OF PAGE austab-pagno
             FIELD FORMAT austab-kreuz INPUT.
    ENDLOOP.
  ELSE.
    LOOP AT austab WHERE flg_aktiv = con_true.
      austab-kreuz = con_stern.
      austab-flg_aktiv = con_false.
      MODIFY austab.
      READ LINE austab-linno OF PAGE austab-pagno.
      sy-lisel(1) = con_stern.
      MODIFY LINE austab-linno OF PAGE austab-pagno.
    ENDLOOP.
  ENDIF.
  READ TABLE austab INDEX sav_tabix.

*........Fortschreiben Programmflußsteuerung............................

  flg_beleg_offen = con_false.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ERROR_LGTYP_FILL
*&---------------------------------------------------------------------*
*       Builds a table with falsely (no CC types) selected st.types
*----------------------------------------------------------------------*
FORM error_lgtyp_fill.
*.......Read texts from internal table................................*
  LOOP AT it301t INTO t301t WHERE spras = sy-langu
                              AND lgnum = s1_lgnum
                              AND lgtyp = t331-lgtyp.
    APPEND t301t TO error_lgtyp.

  ENDLOOP.

ENDFORM.                               " ERROR_LGTYP_FILL

*---------------------------------------------------------------------*
*        FORM FEHLER_ANZEIGEN                                         *
*---------------------------------------------------------------------*
*        Ausgabe eines Fehlerprotokolls.                              *
*---------------------------------------------------------------------*
FORM fehler_anzeigen.

  sav_pfstat = sy-pfkey.
  SET PF-STATUS pfstat_fehl.
  SET TITLEBAR titel_fehl.
  CLEAR sav1_lgtyp.

  DETAIL.

*........Schleife über alle Fehler......................................

  LOOP AT mess.

    MOVE-CORRESPONDING mess TO lmess.

    CALL FUNCTION 'L_MESSAGE_AUFBEREITEN'
         EXPORTING
              i_mess = lmess
         IMPORTING
              o_text = hlp_text.

    WRITE: / hlp_text.

  ENDLOOP.

  CLEAR sav1_lgtyp. " sonst wird bei erneuter Selektion kein
  " Beleg geöffnet

ENDFORM.

*--------------------------------------------------------------------*
*       FORM INVENTUR_ANLEGEN                                        *
*--------------------------------------------------------------------*
*       Anlegen der Batch-Input-Mappe für das Erzeugen der           *
*       Inventurbelege.                                              *
*--------------------------------------------------------------------*
FORM inventur_anlegen.
  sav_sort = sav_titel.                " remember the actual sorting
  SORT austab BY lgtyp lgpla.
  REFRESH mess.
  cnt_fehler = 0.

  cnt_lagp_beleg = 0.
  flg_beleg_offen = con_false.

  LOOP AT austab WHERE kreuz = con_x.
    sav_tabix = sy-tabix.
    PERFORM inventur_platz.
    austab-flg_aktiv = con_true.
    MODIFY austab INDEX sav_tabix.
  ENDLOOP.

  IF flg_beleg_offen = con_true.
    PERFORM beleg_schliessen.
  ENDIF.

  IF cnt_fehler > 0.
    PERFORM fehler_anzeigen.
  ELSE.
    PERFORM sort_austab USING sav_titel fcode_sauf.
    PERFORM ausgabe.
    sy-lsind = 0.
  ENDIF.

ENDFORM.

*--------------------------------------------------------------------*
*       FORM INVENTUR_PLATZ                                          *
*--------------------------------------------------------------------*
*       Füllen des Dynpros für einen Lagerplatz                      *
*--------------------------------------------------------------------*
FORM inventur_platz.

*........new inventory document for new storge type ..................*
  IF austab-lgtyp <> sav1_lgtyp.
    IF flg_beleg_offen = con_true.
      PERFORM beleg_schliessen.
    ENDIF.
    PERFORM beleg_oeffnen.
  ENDIF.
  sav1_lgtyp = austab-lgtyp.
*........new inventory document (max.no. of entries per inv.doc.reached)
  IF cnt_lagp_beleg >= anplb.
    PERFORM beleg_schliessen.
    PERFORM beleg_oeffnen.
  ENDIF.

*........new dynpro every 10 doc. entries (due to CTU on LI01).........*
  IF cnt_lagp_dynpro >= max_lagp_dynpro.
    PERFORM neues_dynpro.
  ENDIF.

  cnt_lagp_dynpro = cnt_lagp_dynpro + 1.
  cnt_lagp_beleg  = cnt_lagp_beleg  + 1.

*........Lagerplatz....................................................

  CLEAR bdc_daten.
  CASE cnt_lagp_dynpro.
    WHEN  1. bdc_daten-fnam = 'LINP-LGPLA(01)'.
    WHEN  2. bdc_daten-fnam = 'LINP-LGPLA(02)'.
    WHEN  3. bdc_daten-fnam = 'LINP-LGPLA(03)'.
    WHEN  4. bdc_daten-fnam = 'LINP-LGPLA(04)'.
    WHEN  5. bdc_daten-fnam = 'LINP-LGPLA(05)'.
    WHEN  6. bdc_daten-fnam = 'LINP-LGPLA(06)'.
    WHEN  7. bdc_daten-fnam = 'LINP-LGPLA(07)'.
    WHEN  8. bdc_daten-fnam = 'LINP-LGPLA(08)'.
    WHEN  9. bdc_daten-fnam = 'LINP-LGPLA(09)'.
    WHEN 10. bdc_daten-fnam = 'LINP-LGPLA(10)'.
  ENDCASE.
  bdc_daten-fval = austab-lgpla.
  APPEND bdc_daten.

ENDFORM.

*--------------------------------------------------------------------*
*       FORM LQUA_INV_PRUEFEN                                        *
*--------------------------------------------------------------------*
*       Prüfung, ob die Quants inventurfähig sind                    *
*--------------------------------------------------------------------*
FORM lqua_inv_pruefen.

  IF lqua-ausme NE init_ausme OR
     lqua-einme NE init_einme OR
**--- blocked by stlim (2004/04/14)
*    LQUA-SKZSI NE INIT_SKZSI OR
**--- end of block
**--- insert by stlim (2004/04/14)
     lqua-skzsi NE init_skzsi OR
**--- end of insert
     lqua-skzse NE init_skzse OR
     lqua-skzsa NE init_skzsa OR
     lqua-skzua NE init_skzua OR
     lqua-skzue NE init_skzue OR
     lqua-gesme < 0.
*   FLG_LQUA_GESPERRT = CON_TRUE.
*   CNT_QSPER = CNT_QSPER + 1.
    tab-sperr = con_x.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM LQUAB_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen eines Summeninfos                                       *
*---------------------------------------------------------------------*
*       INPUT:  LQUA                   Quantinformation               *
*       OUTPUT: LQUAB                                                 *
*---------------------------------------------------------------------*
FORM lquab_lesen.

  CLEAR sy-subrc.
  IF NOT t331b-summe IS INITIAL.
    IF lqua-lgnum <> lquab-lgnum OR
      lqua-matnr <> lquab-matnr OR
      lqua-werks <> lquab-werks OR
      lqua-lgtyp <> lquab-lgtyp OR
      lqua-lgpla <> lquab-lgpla.
      SELECT * FROM lquab
        WHERE lgnum = lqua-lgnum
        AND matnr = lqua-matnr
        AND werks = lqua-werks
        AND bestq = lqua-bestq
        AND sobkz = lqua-sobkz
        AND sonum = lqua-sonum
        AND lgtyp = lqua-lgtyp
        AND lgpla = lqua-lgpla.
        EXIT.
      ENDSELECT.
    ENDIF.
  ENDIF.
  IF sy-subrc <> 0 AND lqua-gesme > 0.
    MESSAGE e176 WITH lqua-lgpla.
  ENDIF.
ENDFORM.


*--------------------------------------------------------------------*
*       FORM MIXED_BIN_CHECK                                         *
*--------------------------------------------------------------------*
*       Special treatment for bins with several quants.              *
*       All quants are checked if they are blocked or/and            *
*       selected by IM for CC. INformation is concatenated in the    *
*       AUSTAB and appendend in AUSTAB_CREATE                        *
*--------------------------------------------------------------------*
FORM mixed_bin_check.                  "mÖB
  DATA: flg_loop LIKE sy-subrc.
  DATA:  cnt_i   TYPE i.
  CLEAR cnt_i.
  CLEAR flg_trans.
  CLEAR austab.
  CLEAR flg_loop.

  MOVE: tab-lgtyp TO  austab-lgtyp.
  MOVE: tab-lgpla TO  austab-lgpla.
  MOVE: tab-idatu TO  austab-idatu.
  MOVE: tab-werks TO  austab-werks.
  MOVE: tab-matnr TO  austab-matnr.
  MOVE: tab-sperr TO  austab-sperr.
  MOVE: tab-kzmmq TO  austab-kzmmq.
  CLEAR austab-matnr.
  MOVE text-067 TO austab-matnr.       "Mischbelegung
* while flg_loop = 0.                                     "8/96
*   read table tab with key austab-lgpla binary search.   "8/96
*   flg_loop = sy-subrc.                                  "8/96
  LOOP AT tab WHERE lgtyp = austab-lgtyp
                AND lgpla = austab-lgpla.
    IF tab-sperr = con_x.
      MOVE: tab-sperr TO  austab-sperr.                     "8/96
      flg_trans = con_x.
    ENDIF.
  ENDLOOP.                                                  "8/96
*   clear tab-lgpla.                                      "8/96
*   modify tab index sy-tabix.                            "8/96
* endwhile.                                               "8/96

ENDFORM.
*--------------------------------------------------------------------*
*       FORM NEUES_DYNPRO                                            *
*--------------------------------------------------------------------*
*       Abschicken eines Erfassungsdynrpros, öffnen eines neuen      *
*--------------------------------------------------------------------*
FORM neues_dynpro.

*........OK-Code neues Bild............................................

  CLEAR bdc_daten.
  bdc_daten-fnam = 'BDC_OKCODE'.
  bdc_daten-fval = fcode_neues_bild.
  APPEND bdc_daten.

*........Senden Erfassungsbild.........................................

  CLEAR bdc_daten.
  bdc_daten-program  = modpl_inv.
  bdc_daten-dynpro   = dynpro_erfassung.
  bdc_daten-dynbegin = con_x.
  APPEND bdc_daten.

*........Fortschreiben Programmflußsteuerung............................

  cnt_lagp_dynpro = 0.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM PAI                                                      *
*---------------------------------------------------------------------*
*       Neue Version, die immer über die ganze Liste geht.            *
*---------------------------------------------------------------------*
FORM pai.

  CHECK sy-lsind = 1.

*........Retten eventuell benötigter Daten..............................

  sav_lisel       = sy-lisel.
  sav_lgnum       = s1_lgnum.
  sav_lgtyp       = austab-lgtyp.
  sav_lgpla       = austab-lgpla.
  sav_tabix       = tab_tabix.
  sav_was_bin_ich = flg_was_bin_ich.

*........Schleife über TAB ............................................


  LOOP AT austab
  WHERE iverz <> con_x
    AND kreuz <> con_stern.

    PERFORM tab_mark USING mark_input sy-tabix.

  ENDLOOP.

*........Zurückschreiben der geretteten Daten...........................

  sy-lisel        = sav_lisel.
  s1_lgnum        = sav_lgnum.
  austab-lgtyp    = sav_lgtyp.
  austab-lgpla    = sav_lgpla.
  tab_tabix       = sav_tabix.
  flg_was_bin_ich = sav_was_bin_ich.

ENDFORM.
*--------------------------------------------------------------------*
*       FORM PARAMETER_KONSISTENZ                                    *
*--------------------------------------------------------------------*
*       Konsistenzprüfung der Eingabeparameter                       *
*--------------------------------------------------------------------*
FORM parameter_konsistenz.

*........Anzahl Plätze pro Beleg.......................................

  IF anplb IS INITIAL.
    SELECT SINGLE * FROM t340d
     WHERE lgnum = s1_lgnum.
    IF sy-subrc = 0.
      anplb = t340d-minps.
    ENDIF.
    IF anplb IS INITIAL.
      anplb = max_anplb.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RANGE_FILL
*&---------------------------------------------------------------------*
*       Builds the slection ranges anew -> S1_LGTYP
*----------------------------------------------------------------------*
FORM range_fill.
  MOVE:    t331-lgtyp TO s1_lgtyp-low,
          'I'   TO  s1_lgtyp-sign,
          'EQ'  TO  s1_lgtyp-option.
  APPEND s1_lgtyp.

ENDFORM.                               " RANGE_FILL
*--------------------------------------------------------------------*
*       FORM SELEKTION_IM                                            *
*--------------------------------------------------------------------*
*       New inventory  dates are determined in IM and brought back   *
*       to this report where the dates are compared to the           *
*       selection criteria above (itage) and choosen or rejected     *
*       for inventory accordingly.                                   *
*--------------------------------------------------------------------*
FORM selektion_im.

  IF xwmim[] IS INITIAL.
    MESSAGE s136.
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

  MOVE: v_dat TO wmdat-v_dat,
        b_dat TO wmdat-b_dat.
*........Export .......................................................
  EXPORT xwmim TO MEMORY ID 'WM_IM_CYCLE'.
  EXPORT wmdat TO MEMORY ID 'WM_IM_DATE'.
  IF sy-subrc NE 0.
    MESSAGE e172.
  ENDIF.
  REFRESH xwmim.
*........Submit ......................................................
  SUBMIT (im_report) AND RETURN.
  IF sy-subrc <> 0.
    MESSAGE e174.
  ENDIF.
*........Import ......................................................
  IMPORT xwmim FROM MEMORY ID 'IM_WM_CYCLE'.


  IF sy-subrc <> 0.
    MESSAGE e173.
  ENDIF.
*........Compare XWMIM with TAB and create AUSTAB (rel. bins)........
  PERFORM selektion_im_austab.

ENDFORM.

*--------------------------------------------------------------------*
*       FORM SELEKTION_IM_AUSTAB.                                    *
*--------------------------------------------------------------------*
*       New inventory  dates are determined in IM and brought back   *
*       to this report where the dates are compared to the           *
*       selection criteria above (itage) and choosen or rejected     *
*       for inventory accordingly.                                   *
*--------------------------------------------------------------------*
FORM selektion_im_austab.
  DATA:  init_lgpla LIKE lagp-lgpla.

  LOOP AT xwmim.                       "sösel
*........Suggested for Inventory by CC ...............................
    LOOP AT tab WHERE werks = xwmim-werks
                  AND matnr = xwmim-matnr
                  AND idatu = xwmim-idatu.

      CHECK tab-lgpla <> init_lgpla.
*........due in future, not relevant right now.........................
      IF xwmim-kzpmi = con_plus AND tab-kzmmq = space.
        cnt_nok = cnt_nok + 1.
*         DELETE TAB.
      ENDIF.

*........only relevant being selected..................................
      IF xwmim-kzpmi = space OR xwmim-kzpmi = con_negativ.
        CLEAR austab.

**--- blocked by stlim (2004/04/14)
*        IF tab-kzmmq <> con_x.
**--- end of block
        MOVE: tab-lgtyp TO  austab-lgtyp.
        MOVE: tab-lgpla TO  austab-lgpla.
        MOVE: tab-idatu TO  austab-idatu.
        MOVE: tab-werks TO  austab-werks.
        MOVE: tab-matnr TO  austab-matnr.
        MOVE: tab-sperr TO  austab-sperr.
        MOVE: tab-kzmmq TO  austab-kzmmq.

**--- insert by stlim (2004/04/12)
        MOVE : tab-gesme TO austab-gesme,
               tab-meins TO austab-meins.
        CLEAR : makt.
        SELECT SINGLE maktx INTO austab-maktx
                            FROM makt
                           WHERE spras EQ sy-langu
                             AND matnr EQ tab-matnr.
        MOVE : tab-lgnum TO austab-lgnum,
               tab-lqnum TO austab-lqnum,
               tab-plpos TO austab-plpos.
        MOVE : tab-abcin TO austab-abcin.
**--- end of insert

        CLEAR flg_trans.
        PERFORM austab_create USING flg_trans.
**--- blocked by stlim (2004/04/14)
*      ELSE.
*        PERFORM mixed_bin_check.
*        PERFORM austab_create USING flg_trans.
*      ENDIF.
**--- end of block
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT austab.

**--- blocked by stlim (2004/04/12)
*  DELETE ADJACENT DUPLICATES FROM austab COMPARING lgtyp lgpla.
**--- end of block

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SORT_AUSTAB
*&---------------------------------------------------------------------*
*       Sort AUSTAB according to what you want
*----------------------------------------------------------------------*
FORM sort_austab USING p_sort p_ucomm.
  SET TITLEBAR p_sort.

  CASE p_ucomm.
    WHEN fcode_soab.   "descending
      CASE p_sort.
        WHEN titel_sors. SORT austab DESCENDING BY kreuz lgtyp lgpla.
        WHEN titel_sort. SORT austab DESCENDING BY ttext lgtyp lgpla.
        WHEN titel_sord.
          IF cursorfield = 'AUSTAB-IDATU' OR cursorfield = 'TIT_IDATU'.
            SORT austab DESCENDING BY idatu lgtyp lgpla.
          ELSE.
            SORT austab DESCENDING BY nidat lgtyp lgpla.
          ENDIF.
**--- blocked by stlim (2004/04/12)
*        WHEN titel_sorq. SORT austab DESCENDING BY matnr lgtyp lgpla.
**--- end of block

**--- insert by stlim (2004/04/12)
        WHEN titel_sorq. SORT austab DESCENDING BY lgtyp lgpla matnr.
        WHEN titel_sory. SORT austab DESCENDING BY gesme lgtyp lgpla.
        WHEN titel_sorz. SORT austab DESCENDING BY maktx lgtyp lgpla.
**--- end of insert

        WHEN titel_sorp. SORT austab DESCENDING BY lgtyp lgpla.
        WHEN OTHERS.     SORT austab DESCENDING BY lgtyp lgpla.
      ENDCASE.
    WHEN fcode_sauf.  "ascending.
      CASE p_sort.
        WHEN titel_sors. SORT austab ASCENDING BY kreuz lgtyp lgpla.
        WHEN titel_sort. SORT austab ASCENDING BY ttext lgtyp lgpla.
        WHEN titel_sord.
          IF cursorfield = 'AUSTAB-IDATU' OR cursorfield = 'TIT_IDATU'.
            SORT austab ASCENDING BY idatu lgtyp lgpla.
          ELSE.
            SORT austab ASCENDING BY nidat lgtyp lgpla.
          ENDIF.
**--- blocked by stlim (2004/04/12)
*        WHEN titel_sorq. SORT austab ASCENDING BY matnr lgtyp lgpla.
**--- end of block

**--- insert by stlim (2004/04/12)
        WHEN titel_sorq. SORT austab ASCENDING BY lgtyp lgpla matnr.
        WHEN titel_sory. SORT austab DESCENDING BY gesme lgtyp lgpla.
        WHEN titel_sorz. SORT austab DESCENDING BY maktx lgtyp lgpla.
**--- end of insert

        WHEN titel_sorp. SORT austab ASCENDING BY lgtyp lgpla.
        WHEN OTHERS.     SORT austab ASCENDING BY lgtyp lgpla.
      ENDCASE.
  ENDCASE.
  sav_titel = p_sort.
ENDFORM.                               " SORT_AUSTAB
*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
*       Prepare the column select sorted by
*----------------------------------------------------------------------*
FORM sort_prepare USING p_fcode.
  SET PF-STATUS pfstat_listbild.
  GET CURSOR FIELD cursorfield.
  CASE cursorfield.
    WHEN 'AUSTAB-KREUZ'.
      PERFORM sort_austab USING titel_sors p_fcode.
    WHEN 'AUSTAB-TTEXT'.
      PERFORM sort_austab USING titel_sort p_fcode.
    WHEN 'AUSTAB-MATNR'.
      PERFORM sort_austab USING titel_sorq p_fcode.
    WHEN 'AUSTAB-LGTYP'.                                    "50 oder 56
      PERFORM sort_austab USING titel_sorp p_fcode.
    WHEN 'AUSTAB-LGPLA'.
      PERFORM sort_austab USING titel_sorp p_fcode.
    WHEN 'AUSTAB-IDATU'.
      PERFORM sort_austab USING titel_sord p_fcode.
    WHEN 'AUSTAB-NIDAT'.
      PERFORM sort_austab USING titel_sord p_fcode.
    WHEN 'TIT_KREUZ'.
      PERFORM sort_austab USING titel_sors p_fcode.
    WHEN 'TIT_TTEXT'.
      PERFORM sort_austab USING titel_sort p_fcode.
    WHEN 'TIT_MATNR'.
      PERFORM sort_austab USING titel_sorq p_fcode.
    WHEN 'TIT_LGTYP'.                                       "50 oder 56
      PERFORM sort_austab USING titel_sorp p_fcode.
    WHEN 'TIT_LGPLA'.
      PERFORM sort_austab USING titel_sorp p_fcode.
    WHEN 'TIT_NIDAT'.
      PERFORM sort_austab USING titel_sord p_fcode.
    WHEN 'TIT_IDATU'.
      PERFORM sort_austab USING titel_sord p_fcode.

**--- insert by stlim (2004/04/12)
    WHEN 'AUSTAB-GESME' OR 'TIT_GESME'.
      PERFORM sort_austab USING titel_sory p_fcode.
    WHEN 'AUSTAB-MAKTX' OR 'TIT_MAKTX'.
      PERFORM sort_austab USING titel_sorz p_fcode.
**--- end of insert

    WHEN OTHERS.
      MESSAGE e168.   "place cursor on a column
  ENDCASE.
  PERFORM ausgabe.
  sy-lsind = 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  STORAGE_TYPE_CHECK
*&---------------------------------------------------------------------*
*       This routine:
*       - checks all the selected storage types of table S1_LGTYP
*         - for existence
*         - if CC is the inventory method of choice
*       - rebuilds the internal table S1_LGTYP for better performance
*       - fills an internal table with the wrong types for later output
*----------------------------------------------------------------------*
FORM storage_type_check.

*........temporarily used for storing away original selection..........
  LOOP AT s1_lgtyp.
    ilgtyp = s1_lgtyp.
    APPEND ilgtyp.
  ENDLOOP.

  REFRESH s1_lgtyp. CLEAR s1_lgtyp.
*........Read existing storage types ..................................
  SELECT * FROM t301t INTO TABLE it301t
    WHERE spras   = sy-langu
      AND lgnum   = s1_lgnum
      AND lgtyp IN  ilgtyp.

*........If no entry found => new selection ..........................
  DESCRIBE TABLE it301t LINES lin.
  IF lin = 0.
    MESSAGE e138.      " Keine Daten vorhanden (neu selektieren)
  ENDIF.

*........Read storage type masters ....................................
  SELECT * FROM t331 INTO TABLE it331
      WHERE lgnum = s1_lgnum
        AND lgtyp IN  ilgtyp.

*........Check, if CC is used in the storage types selected............
  LOOP AT it331 INTO t331.
    IF t331-invcc = con_x.
      PERFORM range_fill.              " for log.db selection
    ELSE.
      PERFORM error_lgtyp_fill.        " for error protocol
    ENDIF.
  ENDLOOP.

*........If no entry found => new selection ..........................
  DESCRIBE TABLE s1_lgtyp  LINES lin.
  IF lin = 0.
    MESSAGE e169.      " Lagertypen nicht für CC vorgesehen
  ENDIF.


ENDFORM.                               " LGTYP_CHECK
*&---------------------------------------------------------------------*
*&      Form  SU_BLOCK_STORAGE_SPECIAL
*&---------------------------------------------------------------------*
*       In SU managed block storage there are some minor differences
*----------------------------------------------------------------------*
FORM su_block_storage_special.
*.......Read from internal table if necessary.........................
  IF lagp-lgtyp <> sav_bulk_lgtyp.
    LOOP AT it331 INTO t331
      WHERE  lgnum = s1_lgnum
        AND  lgtyp = lagp-lgtyp.
    ENDLOOP.
    IF t331-stein = con_stein_b.
      SELECT SINGLE * FROM t331b
             WHERE lgnum = s1_lgnum
               AND lgtyp = lagp-lgtyp.
    ENDIF.
  ENDIF.
  sav_bulk_lgtyp = lagp-lgtyp.

*........Do the work then.............................................
  IF t331-stein = con_stein_b AND t331-lenvw = con_x.
    PERFORM lquab_lesen.
    IF lquab-aofta > 0.
*     CNT_QSPER = CNT_QSPER + 1.
      flg_lqua_gesperrt = con_true.
    ENDIF.
  ENDIF.

ENDFORM.                               " SU_BLOCK_STORAGE_SPECIAL
*---------------------------------------------------------------------*
*       FORM TAB_MARK                                                 *
*---------------------------------------------------------------------*
*       Markieren eines TAB                                           *
*---------------------------------------------------------------------*
*       INPUT:     TAB        zu modifizierender Eintrag              *
*                  P_MARK     Angabe, wie markiert werden soll        *
*                  P_TABIX    Index des Eintrags, der verändert       *
*                             werden soll                             *
*---------------------------------------------------------------------*
FORM tab_mark USING value(p_mark) value(p_tabix).

  READ LINE austab-linno OF PAGE austab-pagno.

  CASE p_mark.

*........Markierung ändern..............................................

    WHEN mark_switch.
      IF austab-kreuz <> con_x.
        p_mark = mark_on.
      ELSE.
        p_mark = mark_off.
      ENDIF.

*........Markierung laut Benutzereingabe in SY-LISEL(1).................

    WHEN mark_input.
      CHECK austab-kreuz   <> con_question OR
            sy-lisel+1(1) = con_x         OR
            sy-lisel+1(1) = con_kl_x.
      IF sy-lisel+1(1) = con_x OR sy-lisel+1(1) = con_kl_x.
        p_mark = mark_on.
      ELSE.
        p_mark = mark_off.
      ENDIF.

  ENDCASE.

  CASE p_mark.

*........Entkreuzen.....................................................

    WHEN mark_off.
      CLEAR: austab-kreuz, sy-lisel+1(1).

*........Ankreuzen......................................................

    WHEN mark_on.
      austab-kreuz = con_x.
      sy-lisel+1(1) = con_x.

  ENDCASE.

  MODIFY LINE austab-linno OF PAGE austab-pagno
         FIELD FORMAT austab-kreuz INPUT.
  PERFORM tab_modify USING p_tabix.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM TAB_MODIFY                                               *
*---------------------------------------------------------------------*
*       Verändern eines Eintrags in Tabelle TAB                       *
*---------------------------------------------------------------------*
*       INPUT:     TAB        zu modifizierender Eintrag              *
*                  P_TABIX    Index des Eintrags, der verändert       *
*                             werden soll                             *
*---------------------------------------------------------------------*
FORM tab_modify USING value(p_tabix).
  MODIFY austab INDEX p_tabix.
  IF sy-subrc NE 0.
    MESSAGE a111.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM TAB_READ                                                 *
*---------------------------------------------------------------------*
*       Lesen eines Eintrags in Tabelle TAB                           *
*---------------------------------------------------------------------*
*       INPUT:     P_TABIX    Index des Eintrags, der gelesen werden  *
*                             soll                                    *
*                                                                     *
*       OUTPUT:    TAB        gelesener Eintrag                       *
*---------------------------------------------------------------------*
FORM tab_read USING value(p_tabix).
  READ TABLE austab INDEX p_tabix.
  IF sy-subrc NE 0.
    MESSAGE a111.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       Ausgeben Seitenkopf                                           *
*---------------------------------------------------------------------*
FORM top_of_page.

*{   INSERT         KA5K003221                                        1
* MATNR EXT-LISTEN
  DATA rxm0_length0 TYPE i.
  DATA rxm0_l1      TYPE i.
  DATA rxm0_l37     TYPE i.
  DATA rxm0_l38     TYPE i.
  DATA rxm0_l58     TYPE i.
  DATA rxm0_l59     TYPE i.
  DATA rxm0_l69     TYPE i.
  DATA rxm0_l70     TYPE i.
  DATA rxm0_len80   TYPE i.
  DESCRIBE FIELD austab-matnr OUTPUT-LENGTH rxm0_length0.

**--- blocked by stlim (2004/04/12)
*  rxm0_l1 = rxm0_length0 - 18.
**--- end of block

**--- blocked by stlim (2004/04/14)
*  rxm0_l37 = 37 + rxm0_l1.
*  rxm0_l38 = 38 + rxm0_l1.
*  rxm0_l58 = 58 + rxm0_l1.
*  rxm0_l59 = 59 + rxm0_l1.
*  rxm0_l69 = 69 + rxm0_l1.
*  rxm0_l70 = 70 + rxm0_l1.
*  rxm0_len80 = 80 + rxm0_l1.
**--- end of block

**--- insert by stlim (2004/04/14)
  rxm0_l37 = 68 + rxm0_l1.
  rxm0_l38 = 69 + rxm0_l1.
  rxm0_l58 = 89 + rxm0_l1.
  rxm0_l59 = 90 + rxm0_l1.
  rxm0_l69 = 100 + rxm0_l1.
  rxm0_l70 = 101 + rxm0_l1.
  rxm0_len80 = 111 + rxm0_l1.
**--- end of insert


*}   INSERT
  CASE sy-pfkey.
*........when error handling..........................................*
    WHEN pfstat_fehl.
      WRITE: / text-008 COLOR COL_NORMAL.
      SKIP.
    WHEN OTHERS.
*........Whse number and storage type data............................*

**--- blocked by stlim (2004/04/14)
*      CHECK cnt_ok NE space.
**--- end of block

      WRITE: /2 text-006    COLOR COL_BACKGROUND INTENSIFIED,
             14 s1_lgnum    COLOR COL_BACKGROUND INTENSIFIED OFF,
                t300t-lnumt COLOR COL_BACKGROUND INTENSIFIED OFF.
*            45 text-007    color col_background intensified,
*               s1_lgtyp-low color col_background intensified off,
*               t301t-ltypt color col_background intensified off.
*.........statistical data............................................*
      IF flg_top = con_true.
*     flg_top = con_false.
        SKIP.
        PERFORM ausgabe_statistik.
      ENDIF.
*........List-Header .................................................*
      MOVE:
            text-055 TO tit_kreuz,
            text-052 TO tit_ttext,
            text-056 TO tit_lgtyp,
            text-050 TO tit_lgpla,
            text-051 TO tit_matnr,
            text-053 TO tit_idatu,
            text-054 TO tit_nidat.
      SKIP.
*{   REPLACE        KA5K003221                                        2
*\      WRITE : /(80) SY-ULINE.
*\      WRITE: /(80) SPACE COLOR COL_HEADING.
*\      WRITE:  1 SY-VLINE,
*\              2 TIT_KREUZ COLOR COL_HEADING,
*\              3 SY-VLINE,
*\              4 TIT_LGTYP COLOR COL_HEADING,
*\              7 SY-VLINE,
*\              8 TIT_LGPLA COLOR COL_HEADING,
*\             18 SY-VLINE,
*\             19 TIT_MATNR COLOR COL_HEADING,
*\             37 SY-VLINE,
*\             38 TIT_TTEXT COLOR COL_HEADING,
*\             58 SY-VLINE,
*\             59 TIT_IDATU COLOR COL_HEADING,
*\             69 SY-VLINE,
*\             70 TIT_NIDAT COLOR COL_HEADING,
*\             80 SY-VLINE.
*\      WRITE : /(80) SY-ULINE.
* MATNR EXT-LISTEN

**--- blocked by stlim (2004/04/12)
*      WRITE : AT /(rxm0_len80) sy-uline.
*      WRITE:  AT /(rxm0_len80) space COLOR COL_HEADING.
**--- end of block

**--- insert by stlim (2004/04/12)
      WRITE : /(132) sy-uline.
      WRITE : /(132) space COLOR COL_HEADING.
**--- end of insert

**--- insert by stlim (2004/04/12)
      MOVE : text-555 TO tit_gesme,
             text-557 TO tit_maktx.
      MOVE : text-558 TO tit_abcin.
**--- end of insert

      WRITE:  1 sy-vline NO-GAP,
              2 tit_kreuz COLOR COL_HEADING NO-GAP,
              3 sy-vline NO-GAP,
              4 tit_lgtyp COLOR COL_HEADING NO-GAP,
              7 sy-vline  NO-GAP,
              8 tit_lgpla COLOR COL_HEADING NO-GAP,
             18 sy-vline NO-GAP,

**--- blocked by stlim (2004/04/14)
*             19 tit_matnr COLOR COL_HEADING NO-GAP,
**--- end of block

**--- insert by stlim (2004/04/14)
             19(18) tit_matnr COLOR COL_HEADING NO-GAP.
      WRITE:        sy-vline NO-GAP,
               (30) tit_maktx COLOR COL_HEADING NO-GAP,
                    sy-vline NO-GAP,
**--- end of insert

      AT rxm0_l37 sy-vline NO-GAP,
      AT rxm0_l38 tit_ttext COLOR COL_HEADING NO-GAP,
      AT rxm0_l58 sy-vline NO-GAP,
      AT rxm0_l59 tit_idatu COLOR COL_HEADING NO-GAP,
      AT rxm0_l69 sy-vline NO-GAP,
      AT rxm0_l70 tit_nidat COLOR COL_HEADING NO-GAP,
      AT rxm0_len80 sy-vline NO-GAP.

**--- insert by stlim (2004/04/14)
      WRITE : (12) tit_gesme COLOR COL_HEADING NO-GAP,
                   sy-vline NO-GAP,
                   text-556 COLOR COL_HEADING NO-GAP,
                   sy-vline NO-GAP,
*              (30) tit_maktx COLOR COL_HEADING NO-GAP,
*                   sy-vline NO-GAP,
              (03) tit_abcin COLOR COL_HEADING NO-GAP,
                   sy-vline NO-GAP.
**--- end of insert

**--- blocked by stlim (2004/04/12)
*      WRITE : AT /(rxm0_len80) sy-uline.
**--- end of block

**--- insert by stlim (2004/04/12)
      WRITE : /(132) sy-uline.
**--- end of insert

*}   REPLACE
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM T300T_LESEN                                              *
*---------------------------------------------------------------------*
*       Lesen der Lagernummerbezeichnung.                             *
*---------------------------------------------------------------------*
*       INPUT:     P_LGNUM        Lagernummer                         *
*                                                                     *
*       OUTPUT:    T300T          passender Tabelleneintrag           *
*---------------------------------------------------------------------*
FORM t300t_lesen USING value(p_lgnum).

  SELECT SINGLE * FROM t300t
  WHERE spras = sy-langu
    AND lgnum = p_lgnum.

  IF sy-subrc NE 0.
    MESSAGE s002 WITH p_lgnum sy-langu.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_inventory_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_inventory_document.
*---
  DATA: lt_lagp LIKE rlagp OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua LIKE rlqua OCCURS 0 WITH HEADER LINE.
  DATA: lv_plpos                LIKE      lqua-plpos.

*---
  DATA: lv_repid TYPE sy-repid,
        lv_first TYPE c,
        lv_hndle TYPE balloghndl,
        lv_title TYPE balnrext.

  lv_repid = sy-repid.
  lv_title = text-068.
  sav_sort = sav_titel.                " remember the actual sorting

  SORT austab BY lgtyp lgpla.

* Create Application Log
  CALL FUNCTION 'L_APP_LOG_CREATE'
       EXPORTING
            iv_extnr = lv_title
            iv_alusr = sy-uname                          "#EC DOM_EQUAL
            iv_alprg = lv_repid
       IMPORTING
            ev_hndle = lv_hndle.

* Process selected items
  cnt_lagp_beleg = 0.
  lv_first = con_true.

  LOOP AT austab WHERE kreuz = con_x.

    sav_tabix = sy-tabix.

    IF lv_first = con_true.
      MOVE austab-lgtyp TO sav1_lgtyp.
      lv_first = con_false.
    ENDIF.

    IF austab-lgtyp NE sav1_lgtyp.

      PERFORM create_inv_doc TABLES lt_lagp
                                    lt_lqua
                              USING s1_lgnum
                                    sav1_lgtyp
                                    lv_hndle.

      sav1_lgtyp = austab-lgtyp.

    ENDIF.

    MOVE: austab-lgnum TO lt_lagp-lgnum,
          austab-lgtyp TO lt_lagp-lgtyp.

    CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
         EXPORTING
              lgnum     = austab-lgnum
              lgtyp     = austab-lgtyp
              lgpla     = austab-lgpla
         IMPORTING
              o_lgpla   = lt_lagp-lgpla
              o_plpos   = lv_plpos
         EXCEPTIONS
              not_found = 1
              overflow  = 2
              OTHERS    = 3.
    CASE sy-subrc.
      WHEN 1.
        MOVE austab-lgpla TO lt_lagp-lgpla.
      WHEN 2 OR 3.
        MESSAGE ID     sy-msgid
                TYPE   sy-msgty
                NUMBER sy-msgno
                WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

    COLLECT lt_lagp.

    MOVE: austab-lgnum TO lt_lqua-lgnum,
          austab-lqnum TO lt_lqua-lqnum.
    APPEND lt_lqua.

    austab-flg_aktiv = con_true.
    MODIFY austab INDEX sav_tabix.
  ENDLOOP.

*.Create inventory document for the rest................................
  IF NOT lt_lagp[] IS INITIAL.

    PERFORM create_inv_doc TABLES lt_lagp
                                  lt_lqua
                            USING s1_lgnum
                                  sav1_lgtyp
                                  lv_hndle.
  ENDIF.


* Display Application Log...............................................
  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
       EXCEPTIONS
            OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE ID     sy-msgid
            TYPE   'S'
            NUMBER sy-msgno
            WITH   sy-msgv1
                   sy-msgv2
                   sy-msgv3
                   sy-msgv4.
  ENDIF.

* Update internal table
  LOOP AT austab WHERE flg_aktiv = con_true.
  ENDLOOP.

* Back to list processing
  PERFORM sort_austab USING sav_titel fcode_sauf.
  PERFORM ausgabe.
  sy-lsind = 0.
ENDFORM.                    " create_inventory_document

*&---------------------------------------------------------------------*
*&      Form  create_inv_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_LAGP  text
*      -->P_LT_LQUA  text
*      -->P_S1_LGNUM  text
*      -->P_SAV1_LGTYP  text
*      -->P_LV_HNDLE  text
*----------------------------------------------------------------------*
FORM create_inv_doc TABLES   ct_lagp STRUCTURE rlagp
                             ct_lqua STRUCTURE rlqua
                    USING    value(iv_lgnum)
                             value(iv_lgtyp)
                             value(iv_hndle).
*---
  DATA lt_linv LIKE linv_vb OCCURS 0 WITH HEADER LINE.


  CALL FUNCTION 'L_INV_DOC_CREATE'
       EXPORTING
            iv_lgnum = iv_lgnum
            iv_lgtyp = iv_lgtyp
            iv_kzinv = kzinv_cc
            iv_irnum = irnum
            iv_liakt = liakt
            iv_uname = p_uname
            iv_pdatu = v_dat
            iv_anplb = anplb
            iv_hndle = iv_hndle
       TABLES
            it_lagp  = ct_lagp
            it_lqua  = ct_lqua
            et_linv  = lt_linv.


  CLEAR ct_lagp.
  REFRESH ct_lagp.

  CLEAR ct_lqua.
  REFRESH ct_lqua.

* Modify output table checkboxes
  LOOP AT austab WHERE flg_aktiv = con_true.
    READ TABLE lt_linv WITH KEY lgnum = austab-lgnum
                                lqnum = austab-lqnum.
    IF sy-subrc = 0.
*   Inventory Document has been created
      austab-kreuz = con_stern.
      austab-flg_aktiv = con_false.
      MODIFY austab.
      READ LINE austab-linno OF PAGE austab-pagno.
      sy-lisel(1) = con_stern.
      MODIFY LINE austab-linno OF PAGE austab-pagno.
    ELSE.
*   Inventory Document has not been created
      austab-kreuz = con_question.
      austab-flg_aktiv = con_false.
      MODIFY austab.
      READ LINE austab-linno OF PAGE austab-pagno.
      sy-lisel(1) = con_question.
      MODIFY LINE austab-linno OF PAGE austab-pagno
             FIELD FORMAT austab-kreuz INPUT.
    ENDIF.
  ENDLOOP.

  READ TABLE austab INDEX sav_tabix.
ENDFORM.                    " create_inv_doc
