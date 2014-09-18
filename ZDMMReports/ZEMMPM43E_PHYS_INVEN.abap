************************************************************************
* Program Name      : ZEMMPM43E_PHYS_INVEN
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.15.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K909346
* Addl Documentation:
* Description       : Physical Inventory - Cycle Count
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.15.     Sung-Tae Lim     UD1K909346     Initial Coding
*
*
************************************************************************

REPORT rlinv060 USING DATABASE s1l
                MESSAGE-ID l4
                NO STANDARD PAGE HEADING.
*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments KA5 20010927
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
*    Report RLINV060: Cycle Counting by Quant                          *
*                                                                      *
*    This report reads all quants that correspond with the chosen      *
*    selection criteria.                                               *
*    The quants are then submitted to an IM-report checking whether    *
*    the material is allowed for Cycle Counting and calculating the    *
*    new inventory date.                                               *
*    This is compared with the timespan for which inventory documents  *
*    shall be created and the list is shown on the screen.             *
*                                                                      *
*    In IM:   In Storage view of material master: CC-Indicator maint.  *
*             Table 159C (accessable via Cust) has to be maintained    *
*----------------------------------------------------------------------*
*..........DATA DEFINITION ............................................

  TABLES: lagp,                          " Lagerplätze
          lqua,                          " Quants
          lquab,                         " Summenquants (Blocklager)
          link,                          " Inventurbelegkopf
          t300t,                         " Lagernummerbezeichnungen
          t301t,                         " Lagertypbezeichnungen
          t331,                          " Lagertypsteuerung
          t331b,                         " Lagertypsteuerung Blocklager
          t340d.                         " Lagernummerndefaultwerte

**--- insert by stlim (2004/04/08)
  TABLES : marc,
           makt,
           linp.
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
             lqnum LIKE lqua-lqnum,                           "new
             lgnum LIKE lqua-lgnum,                           "new
             plpos LIKE lqua-plpos,                           "new

**--- insert by stlim (2004/04/12)
             gesme LIKE lqua-gesme,
             meins LIKE lqua-meins,
             abcin LIKE marc-abcin,
**--- end of insert

          END OF tab.
*........Output-Structure...............................................
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
             lqnum LIKE lqua-lqnum,                           "new
             lgnum LIKE lqua-lgnum,                           "new
             plpos LIKE lqua-plpos,                           "new

**--- insert by stlim (2004/04/12)
             gesme LIKE lqua-gesme,
             meins LIKE lqua-meins,
             maktx LIKE makt-maktx,
             abcin LIKE marc-abcin,
**--- end of insert

          END OF austab.
*.........IM-Structure................................................
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


  DATA: lt_lagp LIKE rlagp OCCURS 0 WITH HEADER LINE.
  DATA: lt_lqua LIKE rlqua OCCURS 0 WITH HEADER LINE.


  RANGES:   ilgtyp              FOR  t301-lgtyp.

  DATA:     error_lgtyp         LIKE t301t  OCCURS 20.
  DATA:     it301t              LIKE t301t OCCURS 20.
  DATA:     it331               LIKE t331  OCCURS 20.
  DATA:     lin                 TYPE p.
  DATA      sav_bulk_lgtyp      LIKE t331-lgtyp.

*........Allgemeine  Konstanten ........................................

  INCLUDE zemmpm43e_phys_inven_incl.
*include mllvskon.

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

          lv_lgpla                LIKE      lagp-lgpla,
          lv_plpos                LIKE      lqua-plpos,
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

**--- blocked by stlim (2004/04/14)
*  SELECT-OPTIONS:
**........Nur bestimmte Materialnummern.................................
*               matnr FOR lqua-matnr NO INTERVALS
*                                    OBLIGATORY.
**--- end of block

**--- insert by stlim (2004/04/14)
  SELECT-OPTIONS : matnr FOR lqua-matnr.
**--- end of block

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
  SELECT-OPTIONS : s_abcin FOR marc-abcin DEFAULT 'A',
                   s_istat FOR linp-istat.
**--- end of insert

  SELECTION-SCREEN END OF BLOCK xxx.


*---------------------------------------------------------------------*
*          INITIALIZATION
*---------------------------------------------------------------------*
INITIALIZATION.

  b_dat = b_dat + 7.

*..........AT SELECTION-SCREEN ........................................
AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

* .........START-OF-SELECTION .........................................
START-OF-SELECTION.
  flg_top = con_true.
  flg_lqua_gesperrt = con_false.

*.Programmfortschritt anzeigen.........................................
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 0
            text       = text-020.


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
                sonum charg lqnum plpos wdatu idatu edatu

**--- insert by stlim (2004/04/12)
                meins ivnum ivpos.
**--- end of insert

  CLEAR xwmim.

*........here, 'cause the clear tab should not destroy the block.ind...
  IF lagp-anzqu > 1.
    tab-kzmmq = con_x.
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
*--- check status of inventory
  IF lqua-ivnum NE space.
    CLEAR : linp.
    SELECT SINGLE ivnum INTO linp-ivnum
                        FROM linp
                       WHERE ivnum EQ lqua-ivnum
                         AND ivpos EQ lqua-ivpos
                         AND istat IN s_istat.
    CHECK sy-subrc EQ 0.
  ENDIF.
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
    MOVE : marc-abcin      TO tab-abcin.
**--- end of insert

    MOVE: lqua-werks        TO xwmim-werks,
          lqua-matnr        TO xwmim-matnr.
*.......accepting IDATU only when quant has an IVNUM..................
    IF NOT lqua-ivnum IS INITIAL.
      MOVE: lqua-idatu TO xwmim-idatu, " Date of inv. by quant
            lqua-idatu TO tab-idatu.
    ENDIF.
*.......if there is no inventory data: take date of quant creation....
    IF xwmim-idatu IS INITIAL OR
       xwmim-idatu EQ space.
      IF p_edatu = con_x.
        MOVE: lqua-edatu TO xwmim-idatu,
              lqua-edatu TO tab-idatu.
      ENDIF.
    ENDIF.

    MOVE: lqua-lgnum TO tab-lgnum,
          lqua-lgtyp TO tab-lgtyp,
          lqua-lqnum TO tab-lqnum,
          lqua-plpos TO tab-plpos.

*........Mix bin with bin position......................................

    IF lqua-plpos NE space.
      CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
           EXPORTING
                lgpla   = lqua-lgpla
                plpos   = lqua-plpos
           IMPORTING
                o_lgpla = tab-lgpla.
    ENDIF.

*........Before outputting cnt_ok is subtracted, so don't worry here...

    APPEND tab.
    CLEAR tab.
*........several identical records are ignored. Only one collected...
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
    PERFORM inventur_anlegen.
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
          MESSAGE s175.
        ENDIF.
        EXIT.
      ENDIF.
      PERFORM tab_mark USING mark_switch tab_tabix.

*---------------------------------------------------------------------*
*       Löschen Lagerplätze, für die die Belege erzeugt wurden        *
*---------------------------------------------------------------------*
    WHEN fcode_loeschen.
      DELETE austab WHERE kreuz = con_stern.
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

      CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
           EXPORTING
                lgnum     = s1_lgnum
                lgtyp     = austab-lgtyp
                lgpla     = austab-lgpla
           IMPORTING
                o_lgpla   = lv_lgpla
                o_plpos   = lv_plpos
           EXCEPTIONS
                not_found = 1
                overflow  = 2
                OTHERS    = 3.

      CASE sy-subrc.
        WHEN 1.
          MOVE austab-lgpla TO lv_lgpla.
        WHEN 2 OR 3.
          MESSAGE ID     sy-msgid
                  TYPE   sy-msgty
                  NUMBER sy-msgno
                  WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDCASE.


      SET PARAMETER:  ID parid_lgnum FIELD s1_lgnum,
                      ID parid_lgtyp FIELD austab-lgtyp,
                      ID parid_lgpla FIELD lv_lgpla.

      CALL TRANSACTION tcode_lagp_anzeigen AND SKIP FIRST SCREEN.

*---------------------------------------------------------------------*
*       Anlegen Inventurbeleg                                         *
*---------------------------------------------------------------------*
    WHEN fcode_anlegen.
*.....Inv. by quant is always acivated immediately.....................
      liakt = con_x.
      flg_top = con_false.
      PERFORM inventur_anlegen.

*---------------------------------------------------------------------*
*       Aktivieren Inventurbeleg                                      *
*---------------------------------------------------------------------*
    WHEN fcode_aktivieren.
      liakt = con_x.
      flg_top = con_false.
      PERFORM inventur_anlegen.

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
  DATA: flg_intens TYPE c.
  flg_out = con_false.

* sort austab by lgpla plpos.

  LOOP AT austab.
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

**--- blocked by stlim (2004/04/14)
*    WRITE /(80) space COLOR COL_NORMAL.
**--- end of block

**--- insert of stlim (2004/04/14)
    WRITE /(132) space COLOR COL_NORMAL.
**--- end of insert

    WRITE: /1 sy-vline.
    IF austab-iverz = con_x OR
      austab-kreuz = con_stern.                             " or
*      austab-sperr = con_x.
      WRITE:   2 austab-kreuz.
    ELSE.
      WRITE:   2 austab-kreuz AS CHECKBOX INPUT.
    ENDIF.
    WRITE: 3 sy-vline.
    PERFORM ausgabe_zeile.
*........Check counter after deleting processed bins -> sy-ucomm
    cnt_lagp = cnt_lagp + 1.
  ENDLOOP.

**--- blocked by stlim (2004/04/14)
*  WRITE: /(80) sy-uline.
**--- end of block

**--- insert of stlim (2004/04/14)
  WRITE: /(132) sy-uline.
**--- end of insert

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
          31 sy-vline,
          32 text-062  COLOR COL_NORMAL INTENSIFIED ON,
          49 sy-vline,
          50 cnt_lagpp COLOR COL_NORMAL INTENSIFIED OFF,
          60 sy-vline.
*........2.line.............
  WRITE: /1(60) sy-uline.
  WRITE: /1 sy-vline,
          2  text-061  COLOR COL_NORMAL INTENSIFIED ON,
          19 sy-vline,
          20 cnt_ok    COLOR COL_NORMAL INTENSIFIED OFF,
          31 sy-vline,
          32 text-064  COLOR COL_NORMAL,
          49 sy-vline,
          50 cnt_leere COLOR COL_NORMAL,
          60 sy-vline.
*........3.line.............
  WRITE: /1 sy-vline,
          2  text-066  COLOR COL_NORMAL,
          19 sy-vline,
          20 cnt_nok   COLOR COL_NORMAL,
          31 sy-vline,
          32 text-063  COLOR COL_NORMAL,
          49 sy-vline,
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
       4  austab-lgtyp COLOR COL_NORMAL,
       7    sy-vline,
       8  austab-lgpla COLOR COL_NORMAL,
       18   sy-vline,
       19 austab-matnr COLOR COL_NORMAL,
       37   sy-vline,

**--- blocked by stlim (2004/04/14)
*       38 austab-ttext COLOR COL_NORMAL,
*       58   sy-vline,
*       59 austab-idatu DD/MM/YYYY COLOR COL_NORMAL,
*       69   sy-vline,
*       70 austab-nidat DD/MM/YYYY COLOR COL_NORMAL,
*       80   sy-vline.
**--- end of block

**--- insert of stlim (2004/04/14)
      38(30) austab-maktx COLOR COL_NORMAL,
      68   sy-vline,
      69(12) austab-gesme UNIT austab-meins COLOR COL_NORMAL,
      81   sy-vline,
      82 austab-meins COLOR COL_NORMAL,
      85   sy-vline,
      86 austab-ttext COLOR COL_NORMAL,
     106   sy-vline,
     107 austab-idatu DD/MM/YYYY COLOR COL_NORMAL,
     117   sy-vline,
     118 austab-nidat DD/MM/YYYY COLOR COL_NORMAL,
     128   sy-vline,
     129(03) austab-abcin COLOR COL_NORMAL CENTERED,
     132   sy-vline.
**--- end of insert

  HIDE: s1_lgnum, s1_lgtyp-low, austab-lgtyp , austab-lgpla,
                  tab_tabix,    austab-werks, austab-matnr.

**--- insert of stlim (2004/04/14)
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



*--------------------------------------------------------------------*
*       FORM INVENTUR_ANLEGEN                                        *
*--------------------------------------------------------------------*
*       Anlegen der Inventurbelege                                   *
*--------------------------------------------------------------------*
FORM inventur_anlegen.

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


ENDFORM.
*--------------------------------------------------------------------*
*       FORM LQUA_INV_PRUEFEN                                        *
*--------------------------------------------------------------------*
*       Prüfung, ob die Quants inventurfähig sind                    *
*--------------------------------------------------------------------*
FORM lqua_inv_pruefen.

  IF lqua-ausme NE init_ausme OR
     lqua-einme NE init_einme OR
     lqua-skzsi NE init_skzsi OR                            "new
     lqua-skzse NE init_skzse OR
     lqua-skzsa NE init_skzsa OR
     lqua-skzua NE init_skzua OR
     lqua-skzue NE init_skzue OR
     lqua-gesme < 0.
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
    MESSAGE e176.
  ENDIF.
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

  LOOP AT xwmim.
*........Suggested for Inventory by CC ...............................
    LOOP AT tab WHERE werks = xwmim-werks
                  AND matnr = xwmim-matnr
                  AND idatu = xwmim-idatu.

      CHECK tab-lgpla <> init_lgpla.
*........due in future, not relevant right now.........................
      IF xwmim-kzpmi = con_plus AND tab-kzmmq = space.
        cnt_nok = cnt_nok + 1.
      ENDIF.

*........only relevant being selected..................................
      IF xwmim-kzpmi = space OR xwmim-kzpmi = con_negativ.
        CLEAR austab.

*        if tab-kzmmq <> con_x.
        MOVE: tab-lgtyp TO  austab-lgtyp.
        MOVE: tab-lgpla TO  austab-lgpla.
        MOVE: tab-idatu TO  austab-idatu.
        MOVE: tab-werks TO  austab-werks.
        MOVE: tab-matnr TO  austab-matnr.
        MOVE: tab-sperr TO  austab-sperr.
        MOVE: tab-kzmmq TO  austab-kzmmq.
        MOVE: tab-lgnum TO  austab-lgnum.
        MOVE: tab-lqnum TO  austab-lqnum.
        MOVE: tab-plpos TO  austab-plpos.

**--- insert by stlim (2004/04/12)
        MOVE : tab-gesme TO austab-gesme,
               tab-meins TO austab-meins.
        CLEAR : makt.
        SELECT SINGLE maktx INTO austab-maktx
                            FROM makt
                           WHERE spras EQ sy-langu
                             AND matnr EQ tab-matnr.
        MOVE : tab-abcin TO austab-abcin.
**--- end of insert

        CLEAR flg_trans.
        PERFORM austab_create USING flg_trans.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  SORT austab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SORT_AUSTAB
*&---------------------------------------------------------------------*
*       Sort AUSTAB according to what you want
*----------------------------------------------------------------------*
FORM sort_austab USING p_sort p_ucomm.
  SET TITLEBAR p_sort.

  CASE p_ucomm.
    WHEN fcode_soab.                   "descending
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
    WHEN fcode_sauf.                   "ascending.
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
    WHEN OTHERS.
      MESSAGE e168.                    "place cursor on a column
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

  CASE sy-pfkey.
*........when error handling..........................................*
    WHEN pfstat_fehl.
      WRITE: / text-008 COLOR COL_NORMAL.
      SKIP.
    WHEN OTHERS.
*........Whse number and storage type data............................*
*     check cnt_ok ne space. "del
      WRITE: /2 text-006    COLOR COL_BACKGROUND INTENSIFIED,
             14 s1_lgnum    COLOR COL_BACKGROUND INTENSIFIED OFF,
                t300t-lnumt COLOR COL_BACKGROUND INTENSIFIED OFF.
*.........statistical data............................................*
      IF flg_top = con_true.
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

**--- blocked by stlim (2004/04/14)
*      WRITE : /(80) sy-uline.
*      WRITE: /(80) space COLOR COL_HEADING.
**--- end of block

**--- insert by stlim (2004/04/14)
      MOVE : text-555 TO tit_gesme,
             text-557 TO tit_maktx.
      MOVE : text-558 TO tit_abcin.

      WRITE : /(132) sy-uline.
      WRITE: /(132) space COLOR COL_HEADING.
**--- end of insert

      WRITE:  1 sy-vline,
              2 tit_kreuz COLOR COL_HEADING,
              3 sy-vline,
              4 tit_lgtyp COLOR COL_HEADING,
              7 sy-vline,
              8 tit_lgpla COLOR COL_HEADING,
             18 sy-vline,
             19 tit_matnr COLOR COL_HEADING,
             37 sy-vline,

**--- blocked by stlim (2004/04/14)
*             38 tit_ttext COLOR COL_HEADING,
*             58 sy-vline,
*             59 tit_idatu COLOR COL_HEADING,
*             69 sy-vline,
*             70 tit_nidat COLOR COL_HEADING,
*             80 sy-vline.
*      WRITE : /(80) sy-uline.
**--- end of block

**--- insert by stlim (2004/04/14)
             38 tit_maktx COLOR COL_HEADING,
             68 sy-vline,
             69 tit_gesme COLOR COL_HEADING,
             81 sy-vline,
             82 text-556 COLOR COL_HEADING,
             85 sy-vline,
             86 tit_ttext COLOR COL_HEADING,
            106 sy-vline,
            107 tit_idatu COLOR COL_HEADING,
            117 sy-vline,
            118 tit_nidat COLOR COL_HEADING,
            128 sy-vline,
            129 tit_abcin COLOR COL_HEADING,
            132 sy-vline.
      WRITE : /(132) sy-uline.
**--- end of insert

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
*&      Form  CREATE_INV_DOC
*&---------------------------------------------------------------------*
*       Create Inventory Document via Function Module
*----------------------------------------------------------------------*
*      -->CT_LAGP   Storage Bins
*      -->CT_LQUA   Quants
*      -->IV_LGNUM  Warehouse Number
*      -->IV_LGTYP  Storage Type
*      -->IV_HNDLE  Handle (Log Identifier)
*----------------------------------------------------------------------*
FORM create_inv_doc TABLES ct_lagp STRUCTURE rlagp
                           ct_lqua STRUCTURE rlqua
                    USING  value(iv_lgnum)
                           value(iv_lgtyp)
                           value(iv_hndle).

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

ENDFORM.                               " CREATE_INV_DOC
