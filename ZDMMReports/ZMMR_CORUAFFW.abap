REPORT coruaffw MESSAGE-ID ru
                NO STANDARD PAGE HEADING.
* Copied STD Program CORUAFFW ( COGI TCODE) to ZPROGRAM
* to make it to work in background ( Reference:- Help Desk 59P374122)
***********************************************************
* Date      Developer    Request     Description
* 12/20/06  Manju        UD1K930055  Initial Coding
*
************************************************************

*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments PA8 20041020
INITIALIZATION.
  DATA: mgv_matnr_prog LIKE rsvar-report,
        mgv_matnr_selopt_tab like rsldbdfs occurs 0 with header line.
  FIELD-SYMBOLS <mgv_matnr_selopt_conv> TYPE STANDARD TABLE.
  mgv_matnr_prog = sy-repid.
  mgv_matnr_selopt_tab-name = 'S_BAUGR' .
  append mgv_matnr_selopt_tab.
  mgv_matnr_selopt_tab-name = 'S_MATNR' .
  append mgv_matnr_selopt_tab.
  call function 'MGV_SELOP_AFTER_INITIALIZATION'
       EXPORTING
            PROGRAM        = mgv_matnr_prog
       TABLES
            SELOP          = mgv_matnr_selopt_tab
       EXCEPTIONS
            NO_PROGRAMNAME = 1
            OTHERS         = 2.

START-OF-SELECTION.
  LOOP AT mgv_matnr_selopt_tab.
    CONCATENATE mgv_matnr_selopt_tab-name'[]' INTO
    mgv_matnr_selopt_tab-name.
    ASSIGN (mgv_matnr_selopt_tab-name) TO <mgv_matnr_selopt_conv>.
    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'MGV_SELOP_AFTER_START_OF_SEL'
           EXPORTING
                SELOPT_NAME = mgv_matnr_selopt_tab-name
           TABLES
                RANGE       = <mgv_matnr_selopt_conv>.
    ENDIF.
  ENDLOOP.
*{SEL-OPT End}

  TYPE-POOLS: slis.

*{   INSERT         PA8K037066                                        1
* DI backflush : IS-A-PPC
  CONSTANTS:
    con_new_conf LIKE affw-flg_orig VALUE '1'.
  DATA:
    ls_affw TYPE affw.
  TYPES:
    BEGIN OF ppc_rskey,
      matnr TYPE matnr,
      werks TYPE werks_d,
      lgort TYPE lgort_d,
      charg TYPE charg_d,
      prvbe TYPE prvbe,
      bwart TYPE bwart,
      shkzg TYPE shkzg,
      sobkz TYPE sobkz,
      kdauf TYPE kdauf,
      kdpos TYPE kdpos,
    END OF ppc_rskey.
  DATA:
   gt_locked TYPE HASHED TABLE OF ppc_rskey WITH UNIQUE KEY matnr werks
*{   INSERT         UD1K921615                                        1
                   lgort charg prvbe bwart shkzg sobkz kdauf kdpos.
* note 835810
  types:   ttyp_resb TYPE TABLE OF resb,
           ttyp_affw TYPE TABLE OF affw.
  TYPES:
    BEGIN OF ty_affwresb,
      weblnr  TYPE affw-weblnr,
      weblpos TYPE affw-weblpos,
      rsnum   TYPE affw-rsnum,
      rspos   TYPE affw-rspos,
      c_matnr TYPE affw-matnr,
      c_werks TYPE affw-werks,
      c_lgort TYPE affw-lgort,
      c_charg TYPE affw-charg,
      c_sobkz TYPE affw-sobkz,
      c_bwart TYPE affw-bwart,
      c_erfme TYPE affw-erfme,
      r_matnr TYPE affw-matnr,
      r_werks TYPE affw-werks,
      r_lgort TYPE affw-lgort,
      r_charg TYPE affw-charg,
      r_sobkz TYPE affw-sobkz,
      r_bwart TYPE affw-bwart,
      r_erfme TYPE affw-erfme,
    END OF ty_affwresb,
    ty_tab_affwresb TYPE TABLE OF ty_affwresb.

*}   INSERT
*{   DELETE         UD1K921615                                        4
*\                 lgort charg prvbe bwart shkzg sobkz kdauf kdpos.
*}   DELETE
*}   INSERT
  TABLES: affw,
          affwb,
          afpo,
          aufk,
          cowb_comp,
          imkpf,
          makt,
          mseg,
          mtcom,
          prps,
          resb,
          rc27x,
          tafwd,
          t001l,
          t156n.



*---------------------------------------------------------------------*
*        Selektionsbild                                               *
*---------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK affw WITH FRAME TITLE text-007.
  SELECT-OPTIONS:
    s_werks FOR affw-werks MEMORY ID wrk,
    s_lgort FOR affw-lgort,
    s_matnr FOR affw-matnr MATCHCODE OBJECT mat1,
    s_dispo FOR affw-dispo,
    s_bwart FOR affw-bwart,
    s_belnr FOR affw-weblnr no-display.

  SELECTION-SCREEN BEGIN OF BLOCK para WITH FRAME TITLE text-009.
  SELECT-OPTIONS:
*     s_psp_af ist pspnr für die Selektion der AFFW-sätze
    s_psp_af FOR prps-posid MATCHCODE OBJECT prpm no-display.
  SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(30) text-001.
  SELECTION-SCREEN POSITION 31.
  PARAMETERS:
    p_kdauf LIKE affw-kdauf no-display,
    p_kdpos LIKE affw-kdpos no-display.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) text-020. "Fehler: Id / Nummer
  SELECTION-SCREEN POSITION 31.
  PARAMETERS:
    p_msgid LIKE t100-arbgb.
  SELECTION-SCREEN COMMENT 52(1) text-021. "/
  PARAMETERS:
    p_msgno LIKE t100-msgnr.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) text-022. "Fehlerdatum von
  SELECTION-SCREEN POSITION 31.
  PARAMETERS:
    p_datuv LIKE affw-fwdat,
    p_uzeiv LIKE affw-fwzet.
  SELECTION-SCREEN COMMENT 51(5) text-023.                    "bis
  PARAMETERS:
    p_datub LIKE affw-fwdat,
    p_uzeib LIKE affw-fwzet.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(30) text-024. "Buchungsdatum von
  SELECTION-SCREEN POSITION 31.
  PARAMETERS:
    p_datpv LIKE affw-budat.
  SELECTION-SCREEN COMMENT 51(5) text-023.                    "bis
  PARAMETERS:
    p_datpb LIKE affw-budat.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK para.

  SELECTION-SCREEN BEGIN OF BLOCK baugr WITH FRAME TITLE text-016.
  SELECT-OPTIONS:
    s_autyp FOR affw-autyp no-display,
    s_aufnr FOR affw-aufnr MATCHCODE OBJECT orde no-display,
    s_baugr FOR afpo-matnr MATCHCODE OBJECT mat1 no-display,
    s_verid FOR afpo-verid no-display,
    s_bwerk FOR afpo-pwerk no-display,
    s_fevor FOR affw-fevor no-display,
*     s_psp_or für die Selektion der auf s_psp_or kontierten Aufträge
    s_psp_or FOR prps-posid MATCHCODE OBJECT prpm no-display.
  SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(30) text-001.
  SELECTION-SCREEN POSITION 31.
  PARAMETERS:
    p_bkauf LIKE afpo-kdauf no-display,
    p_bkpos LIKE afpo-kdpos no-display.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK baugr.
  PARAMETERS:
    p_maxrow(4) TYPE n,
    r_single RADIOBUTTON GROUP rad1,
    r_cumul  RADIOBUTTON GROUP rad1 DEFAULT 'X'.
  PARAMETERS:
    p_disply AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK affw.

* nur für Aufruf Serienfert.
  SELECT-OPTIONS l_prtnr FOR affw-prtnr NO-DISPLAY.

* control mini-app behaviour
  PARAMETERS: p_mini TYPE flag DEFAULT ' ' NO-DISPLAY.


*---------------------------------------------------------------------*
*        Ranges                                                       *
*---------------------------------------------------------------------*
  RANGES: r_rsnum FOR affw-rsnum.
  RANGES: psp_affw_range FOR prps-pspnr. "Für AFFW-Selektion
  RANGES: psp_order_range FOR prps-pspnr."Für Auftragsselektion

  DATA: parallel LIKE rc27x-flg_sel VALUE space,
        pakumul  LIKE rc27x-flg_sel VALUE space.

  DATA: BEGIN OF affw_tab OCCURS 0.
          INCLUDE STRUCTURE affw.
  DATA: END   OF affw_tab.
  DATA: BEGIN OF affw_old OCCURS 0.
          INCLUDE STRUCTURE affw.
  DATA: END   OF affw_old.
* Tabelle der Beelgnummern von abhängigen Positionen (WE-/WA im A.Netz)
  DATA: BEGIN OF refblnr_tab OCCURS 0,
          blnr  LIKE affw-refbln,
          kzbew LIKE affw-kzbew,
          indx  LIKE sy-tabix,
        END   OF refblnr_tab.
* Tabelle der gesperrten Reservierungsköpfe (SHARED)
  DATA: BEGIN OF enq_rsnum OCCURS 0,
          rsnum LIKE resb-rsnum,
          count LIKE sy-tabix,
        END   OF enq_rsnum.
* Tabelle der gesperrten Einzelreservierungen (EXKLUSIV)
  DATA: BEGIN OF enq_resb OCCURS 0,
          rsnum LIKE resb-rsnum,
          rspos LIKE resb-rspos,
          rsart LIKE resb-rsart,
          count LIKE sy-tabix,
        END   OF enq_resb.
  DATA: BEGIN OF resb_pre_tab OCCURS 0,
          rsnum LIKE resb-rsnum,
          rspos LIKE resb-rspos,
          rsart LIKE resb-rsart,
        END   OF resb_pre_tab.
  DATA: BEGIN OF pre03_tab OCCURS 0.
          INCLUDE STRUCTURE pre03.
  DATA: END   OF pre03_tab.
  DATA: BEGIN OF affwb_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF affwb_tab.
DATA ls_affwb_tab LIKE affwb.                                        "AL
  DATA: BEGIN OF affwb_task OCCURS 0.    "Fehlersätze pro parall. Task
          INCLUDE STRUCTURE affwb.
  DATA: END   OF affwb_task.

  DATA: BEGIN OF affwb_out_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF affwb_out_tab.

  DATA: BEGIN OF WA_affwb_out_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF WA_affwb_out_tab.


  DATA: BEGIN OF split_tab OCCURS 0.
          INCLUDE STRUCTURE affw.
  DATA:   splmg LIKE affwb-erfmg,
          ind_kum LIKE sy-tabix,
        END   OF split_tab.
  DATA: BEGIN OF dummy_tab OCCURS 0,
          dummy,
        END   OF dummy_tab.
  DATA: BEGIN OF tafwd_tab OCCURS 5.
          INCLUDE STRUCTURE tafwd.
  DATA: END   OF tafwd_tab.
  DATA: BEGIN OF tabix_tab OCCURS 0,
          ind_kum LIKE sy-tabix,
          ind LIKE sy-tabix,
        END   OF tabix_tab.
*     Tabelle der Auftragsdaten für die WB-übersicht
  DATA: gt_order_tab TYPE STANDARD TABLE OF cowb_order WITH HEADER LINE.
*     Tabelle der Materialbewegungs-Positionen
  DATA: cowb_comp_tab TYPE STANDARD TABLE OF cowb_comp WITH HEADER LINE.
*     Tabelle der Inputdaten Warenbewegung
  DATA: imseg_tab TYPE STANDARD TABLE OF imseg WITH HEADER LINE.
*     Tabelle der Reservierungen
  DATA: resb_tab_ges TYPE STANDARD TABLE OF resb WITH HEADER LINE.
*     SS-Tabelle der Reservierungen
  DATA: BEGIN OF resb_tab OCCURS 0.
          INCLUDE STRUCTURE resb_affw.
  DATA: END   OF resb_tab.
*     Materialnummmern für Sicherheitsabfrage
  DATA: BEGIN OF matnr_tab OCCURS 0,
          matnr LIKE affw-matnr,
          antwo(1),
        END   OF matnr_tab.
* ---   Select-Options und Parameter   ---
  DATA: BEGIN OF param_tab OCCURS 0.
          INCLUDE STRUCTURE rsparams.
  DATA: END   OF param_tab.
* ---   CUA-Tabelle excl. Funktionen   ---
  DATA: BEGIN OF cuafc_tab OCCURS 0.
          INCLUDE STRUCTURE cuafcode.
  DATA: END   OF cuafc_tab.
*     Daten fuer die Sortfunktionen
  DATA: BEGIN OF sortx OCCURS 0.
          INCLUDE STRUCTURE mdsort.
  DATA: END OF sortx.
* Tabelle für Scroll-Bar
  DATA: BEGIN OF scroll_tab OCCURS 0,
          dummy,
        END OF scroll_tab.
* globale Sperrtabelle der Fehlersätze
  DATA: BEGIN OF enq_glob OCCURS 0,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF enq_glob.
* Input für Sperren der Fehlersätze
  DATA: BEGIN OF enq_loc OCCURS 0,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF enq_loc.
  DATA tmp_enq_loc LIKE enq_loc OCCURS 0 WITH HEADER LINE.          "AL
* Tabelle der bisher größten vergebenen Position je AFFW-Beleg
  DATA: BEGIN OF weblpos_max OCCURS 0,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF weblpos_max.
* Tabelle der Schlüssel der zu stornierenden Materialdokumente
  DATA: BEGIN OF mseg_key OCCURS 0,
          mblnr LIKE mseg-mblnr,
          mjahr LIKE mseg-mjahr,
          zeile LIKE mseg-zeile,
        END   OF mseg_key.
* Tabelle der Bewertung zum Zeitpunkt der retrograden Entn.
  DATA: BEGIN OF bfwrt_tab OCCURS 0,
          mblnr LIKE mseg-mblnr,
          mjahr LIKE mseg-mjahr,
          zeile LIKE mseg-zeile,
          sakto LIKE mseg-sakto,
          dmbtr LIKE mseg-dmbtr,
          exbwr LIKE mseg-exbwr,
          bstmg LIKE mseg-bstmg,
          bwart LIKE mseg-bwart,         "Original-Bewegungsart
          menge LIKE mseg-menge,         "Menge in Lager-ME
          meins LIKE mseg-meins,         "Lager-ME
        END   OF bfwrt_tab.
  DATA gt_field_for_sort_tab LIKE mcs01 OCCURS 5 WITH HEADER LINE.
* Schlüssel des AFFW-Satzes
  DATA: BEGIN OF affw_key,
          mandt   LIKE affw-mandt,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF affw_key.
*     Key der Reservierung
  DATA: BEGIN OF resb_key,
          mandt LIKE resb-mandt,
          rsnum LIKE resb-rsnum,
          rspos LIKE resb-rspos,
          rsart LIKE resb-rsart,
        END   OF resb_key.
*     Steuerstruktur Warenbewegungsbearbeitung
  DATA: cowb_ctrl LIKE cowb_ctrl.
  DATA: BEGIN OF line1,
          vline_left LIKE sy-vline,
          fill01(1),
          flg_sel LIKE affwb-flg_sel,
          fill02(1),
          matnr   LIKE affwb-matnr,
          fill03(1),
          werks   LIKE affwb-werks,
          fill04(1),
          lgort   LIKE affwb-lgort,
          fill05(1),
          charg   LIKE affwb-charg,
          fill06(4),
          bwart   LIKE affwb-bwart,
          fill07(1),
          erfmg(17),
          fill08(1),
          erfme   LIKE affwb-erfme,
          fill09(1),
          kzear   LIKE affwb-kzear,
          fill10(3),
          msgid(2),
          fill11(1),
          msgno   LIKE affwb-msgno,
          fill12(1),
          fwdat(10),
          fill13(1),
          fwzet(8),
          fill14(1),
          dispo   LIKE affwb-dispo,
          fill15(1),
          vline_right LIKE sy-vline,
        END   OF line1.
  DATA: BEGIN OF line3,
          vline_left LIKE sy-vline,
          fill01(1),
          flg_sel LIKE affwb-flg_sel,
          fill02(1),
          matnr   LIKE affwb-matnr,
          fill03(1),
          werks   LIKE affwb-werks,
          fill04(1),
          lgort   LIKE affwb-lgort,
          fill05(1),
          charg   LIKE affwb-charg,
          fill06(4),
          bwart   LIKE affwb-bwart,
          fill07(1),
          erfmg(17),
          fill08(1),
          erfme   LIKE affwb-erfme,
          fill09(1),
          msgid(2),
          fill10(1),
          msgno   LIKE affwb-msgno,
          fill11(1),
          blcount(4) TYPE n,
          fill12(1),
          vline_right LIKE sy-vline,
        END   OF line3.

* Name einer asynchronen Task
  DATA: tasknam(7)  TYPE n VALUE 0.      "Zähler als Namen der Task

  DATA: acti_pr       TYPE i,
        antwort,
        cntr          LIKE sy-tabix,
        cursor_old    LIKE sy-tabix,
        b_aend        LIKE rc27x-flg_sel,
        b_flg_sel     LIKE rc27x-flg_sel,
        b_vbkz        LIKE affwb-vbkz,
        feldpos       LIKE sy-fdpos,
        flg_corp      LIKE rc27x-flg_sel,
        flg_network   LIKE rc27x-flg_sel,
        flg_sort_b    LIKE rc27x-flg_sel,
        flg_start     LIKE rc27x-flg_sel,
        flg_refresh   LIKE rc27x-flg_sel,
        msg_text(132) TYPE c,
        n_locked      LIKE sy-tabix,
        nk_numbrc     LIKE inri-returncode, "Retcode int. Nummernvergabe
        ok_code       LIKE sy-ucomm,
        program       LIKE rsvar-report,
        scroll_cursor LIKE sy-tabix,
        sellin        LIKE sy-tabix,
        subrc         LIKE sy-subrc,
        s_pname       LIKE rs38m-programm,
        tabix         LIKE sy-tabix,
        tabix_do      LIKE sy-tabix,
        tabix_enq     LIKE sy-tabix,
        tabix_kum     LIKE sy-tabix,
        tabix_para    LIKE sy-tabix,
*{   INSERT         PA8K037066                                        2
        unit          LIKE affw-erfme,
*}   INSERT
        weblnr_new    LIKE affw-weblnr,
        xallp         LIKE rc27x-flg_sel."Kennz. abh. Positionen
* Konstanten
  CONSTANTS:
        kenng_makt  LIKE mtcom-kenng     VALUE 'MAKT',
        kzbew_f                          VALUE 'F',
        nk_range    LIKE inri-nrrangenr  VALUE '01',
        nk_object   LIKE nriv-object     VALUE 'AFFW_NR',
        minus       LIKE scal-indicator  VALUE '-', "Vorzeich neg. Wert
        msgty_i                          VALUE 'I',
        plus                             VALUE '+',
        rueck_space LIKE afru-rueck      VALUE '0000000000',
        screen_0100 LIKE sy-dynnr        VALUE '0100',
        screen_0200 LIKE sy-dynnr        VALUE '0200',
        screen_0300 LIKE sy-dynnr        VALUE '0300',
        screen_0400 LIKE sy-dynnr        VALUE '0400',
        screen_0500 LIKE sy-dynnr        VALUE '0500',
        abbruch                          VALUE 'A',
        vbkz_bu                          VALUE 'B',
        vbkz_del                         VALUE 'D',
        vbkz_dll                         VALUE 'L',
        vbkz_ins                         VALUE 'I',
        vbkz_shw                         VALUE 'S',
        vbkz_upd                         VALUE 'U',
        yja                              VALUE 'J',
        yblank                           VALUE ' ',
        ynein                            VALUE 'N',
        yx                               VALUE 'X',
        shkzg_haben   LIKE resb-shkzg    VALUE 'H',
        shkzg_soll    LIKE resb-shkzg    VALUE 'S',
        mhd_necessary LIKE sy-subrc      VALUE '1',
        hsd_necessary LIKE sy-subrc      VALUE '2'.

* Auftragstypen
  CONSTANTS:
        BEGIN OF auftyp,
          corp LIKE t490-autyp VALUE '05',   "Serienauftrag
          fert LIKE t490-autyp VALUE '10',   "Fertigungsauftrag
          netw LIKE t490-autyp VALUE '20',   "Netzplan
          inst LIKE t490-autyp VALUE '30',   "Instandhaltungsauftrag
          bord LIKE t490-autyp VALUE '40',   "Batchrezept
        END   OF auftyp.
* Transaktionscodes
  CONSTANTS:
        tc_co03    LIKE sy-tcode         VALUE 'CO03',
        tc_cor3    LIKE sy-tcode         VALUE 'COR3',      "3.1H FR
        tc_co14    LIKE sy-tcode         VALUE 'CO14',
        tc_cort    LIKE sy-tcode         VALUE 'CORT',      "3.1H FR
        tc_cn28    LIKE sy-tcode         VALUE 'CN28',
        tc_iw33    LIKE sy-tcode         VALUE 'IW33',
        tc_iw43    LIKE sy-tcode         VALUE 'IW43',
        tc_md04    LIKE sy-tcode         VALUE 'MD04',
        tc_mmbe    LIKE sy-tcode         VALUE 'MMBE',
        tc_mm03    LIKE sy-tcode         VALUE 'MM03',
        tc_net_inf LIKE sy-tcode         VALUE 'CN41'.
* Funktionscodes
  CONSTANTS:
        fc_amak    LIKE t185-fcode       VALUE 'AMAK',
        fc_amal    LIKE t185-fcode       VALUE 'AMAL',
        fc_auft    LIKE t185-fcode       VALUE 'AUFT',
        fc_back    LIKE t185-fcode       VALUE 'BCK ',
        fc_bacs    LIKE t185-fcode       VALUE 'BACS',
        fc_bu      LIKE t185-fcode       VALUE 'BU  ',
        fc_buba    LIKE t185-fcode       VALUE 'BUBA',
        fc_buda    LIKE t185-fcode       VALUE 'BUDA',
        fc_char    LIKE t185-fcode       VALUE 'CHAR',
        fc_date    LIKE t185-fcode       VALUE 'DATE',
        fc_del     LIKE t185-fcode       VALUE 'DEL ',
        fc_end     LIKE t185-fcode       VALUE 'END ',
        fc_esc     LIKE t185-fcode       VALUE 'ESC ',
        fc_fail    LIKE t185-fcode       VALUE 'FAIL',
        fc_frsh    LIKE t185-fcode       VALUE 'FRSH',
        fc_korr    LIKE t185-fcode       VALUE 'KORR',
        fc_lort    LIKE t185-fcode       VALUE 'LORT',
        fc_mark    LIKE t185-fcode       VALUE 'MARK',
        fc_matn    LIKE t185-fcode       VALUE 'MATN',
        fc_md04    LIKE t185-fcode       VALUE 'MD04',
        fc_minus   LIKE t185-fcode       VALUE 'P-  ',
        fc_minusminus LIKE t185-fcode    VALUE 'P-- ',
        fc_mmbe    LIKE t185-fcode       VALUE 'MMBE',
        fc_plus    LIKE t185-fcode       VALUE 'P+  ',
        fc_plusplus LIKE t185-fcode      VALUE 'P++ ',
        fc_ruck    LIKE t185-fcode       VALUE 'RUCK',
        fc_rw      LIKE t185-fcode       VALUE '%RW ',
        fc_sngl    LIKE t185-fcode       VALUE 'SNGL',
        fc_sort    LIKE t185-fcode       VALUE 'SORT',
        fc_splt    LIKE t185-fcode       VALUE 'SPLT',
        fc_summ    LIKE t185-fcode       VALUE 'SUMM',
        fc_verw    LIKE t185-fcode       VALUE 'VERW',
        fc_weit    LIKE t185-fcode       VALUE 'WEIT'.
  DATA g_sort_handle LIKE abap-memory_id.

*eject
START-OF-SELECTION.
  CLEAR imkpf.
  imkpf-bldat = sy-datlo.
  imkpf-budat = sy-datlo.
* Aufruf aus START_OF_SELECTION
  flg_start = yx.
* Fehlersätze aus Tabelle einlesen
  PERFORM get_entries.

* Kz nur Netzpläne
  flg_network = yx.
* Reset Anzahl gesperrter Fehlersätze
  cntr = 0.
* Daten selektieren und bereitstellen
  LOOP AT affw_tab.
    IF affw_tab-autyp <> auftyp-netw.
      CLEAR flg_network.
    ENDIF.
    PERFORM select_entries USING subrc.
    CHECK subrc = 0.
    IF affw_tab-refbln IS INITIAL    AND
       NOT affw_tab-autyp IS INITIAL AND
       NOT affw_tab-werks IS INITIAL.
      AUTHORITY-CHECK OBJECT 'C_AFFW_TWK'
           ID 'AUTYP' FIELD affw_tab-autyp
           ID 'WERKS' FIELD affw_tab-werks.
      subrc = sy-subrc.
      IF sy-subrc <> 0.
        cntr = cntr + 1.
      ENDIF.
    ENDIF.
    IF NOT affw_tab-dnotw IS INITIAL.
      CLEAR affw_tab-dnotw.
      MODIFY affw_tab TRANSPORTING dnotw.
    ENDIF.
    CLEAR affwb_tab.
    MOVE-CORRESPONDING affw_tab TO affwb_tab.
    IF subrc = 0 AND
       p_disply IS INITIAL.
      CLEAR affwb_tab-vbkz.
    ELSE.
      affwb_tab-vbkz = vbkz_shw.
    ENDIF.
    APPEND affwb_tab.
    IF NOT affwb_tab-refbln IS INITIAL.
*     Belegnummern von abhängigen Warenbewegungen sammeln
      refblnr_tab-blnr = affwb_tab-refbln.
      APPEND refblnr_tab.
    ENDIF.
*   Pre-Read Materialkurztext
    READ TABLE pre03_tab WITH KEY affwb_tab-matnr BINARY SEARCH.
    IF sy-subrc <> 0.
*     Materialnummer für Pre-Read in Tabelle aufnehmen
      pre03_tab-matnr = affw_tab-matnr.
      INSERT pre03_tab INDEX sy-tabix.
    ENDIF.
*   Pre-Read Reservierung
    IF NOT affwb_tab-rsnum IS INITIAL AND
       NOT affwb_tab-rspos IS INITIAL.
      READ TABLE resb_pre_tab WITH KEY rsnum = affwb_tab-rsnum
                                       rspos = affwb_tab-rspos
                                       rsart = affwb_tab-rsart
                                       BINARY SEARCH.
      IF sy-subrc <> 0.
*       Reservierung für  Pre-Read in Tabelle aufnehmen
        resb_pre_tab-rsnum = affwb_tab-rsnum.
        resb_pre_tab-rspos = affwb_tab-rspos.
        resb_pre_tab-rsart = affwb_tab-rsart.
        INSERT resb_pre_tab INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Wenn keine Fehlersätze selektiert wurden, Programm beenden
  IF affwb_tab[] IS INITIAL.
    MESSAGE s268.
  ENDIF.
  CHECK NOT affwb_tab[] IS INITIAL.
* Abhängige Warenbewegungen dürfen nur gemeinsam verarbeitet werden
  IF NOT refblnr_tab[] IS INITIAL.
    SORT refblnr_tab BY blnr.
    DELETE ADJACENT DUPLICATES FROM refblnr_tab COMPARING blnr.
*
    DELETE affw_old  WHERE NOT refbln IS initial.
    DELETE affwb_tab WHERE NOT refbln IS initial.
    SELECT * FROM affw INTO TABLE affw_tab
                       FOR ALL ENTRIES IN refblnr_tab
                       WHERE weblnr = refblnr_tab-blnr.
    REFRESH refblnr_tab.
*   Daten selektieren und bereitstellen
    LOOP AT affw_tab.
      subrc = 0.
      IF NOT affw_tab-autyp IS INITIAL AND
         NOT affw_tab-werks IS INITIAL.
        AUTHORITY-CHECK OBJECT 'C_AFFW_TWK'
             ID 'AUTYP' FIELD affw_tab-autyp
             ID 'WERKS' FIELD affw_tab-werks.
        subrc = sy-subrc.
        IF sy-subrc <> 0.
          cntr = cntr + 1.
        ENDIF.
      ENDIF.
      CLEAR affwb_tab.
      MOVE-CORRESPONDING affw_tab TO affwb_tab.
      IF subrc = 0 AND
         p_disply IS INITIAL.
        CLEAR affwb_tab-vbkz.
      ELSE.
        affwb_tab-vbkz = vbkz_shw.
      ENDIF.
      APPEND affwb_tab.
      APPEND affw_tab TO affw_old.
      IF NOT affwb_tab-refbln IS INITIAL.
*       Belegnummern von abhängigen Warenbewegungen sammeln
        refblnr_tab-blnr = affwb_tab-refbln.
        APPEND refblnr_tab.
      ENDIF.
*     Pre-Read Materialkurztext
      READ TABLE pre03_tab WITH KEY affwb_tab-matnr BINARY SEARCH.
      IF sy-subrc <> 0.
*       Materialnummer für Pre-Read in Tabelle aufnehmen
        pre03_tab-matnr = affw_tab-matnr.
        INSERT pre03_tab INDEX sy-tabix.
      ENDIF.
*     Pre-Read Reservierung
      IF NOT affwb_tab-rsnum IS INITIAL AND
         NOT affwb_tab-rspos IS INITIAL.
        READ TABLE resb_pre_tab WITH KEY rsnum = affwb_tab-rsnum
                                         rspos = affwb_tab-rspos
                                         rsart = affwb_tab-rsart
                                         BINARY SEARCH.
        IF sy-subrc <> 0.
*         Reservierung für  Pre-Read in Tabelle aufnehmen
          resb_pre_tab-rsnum = affwb_tab-rsnum.
          resb_pre_tab-rspos = affwb_tab-rspos.
          resb_pre_tab-rsart = affwb_tab-rsart.
          INSERT resb_pre_tab INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT affw_old BY weblnr weblpos.
  ENDIF.
  IF cntr > 0.
*   für mind. 1 Fehlersatz keine Berechtigung, daher nicht bearbeitbar
    MESSAGE s432 WITH cntr.
  ENDIF.
* Fehlersätze sind vorhanden
  IF NOT resb_pre_tab[] IS INITIAL.
*   Pre-Read Reservierungen
    SELECT * FROM resb INTO TABLE resb_tab_ges
                       FOR ALL ENTRIES IN resb_pre_tab
                       WHERE rsnum = resb_pre_tab-rsnum
                       AND   rspos = resb_pre_tab-rspos
                       AND   rsart = resb_pre_tab-rsart.
*{   DELETE         UD1K921615                                        2
*\  ENDIF.
*}   DELETE
*{   INSERT         UD1K921615                                        3
* note 835810 - check the reservations
    data: ls_resb like resb,
          l_lock(1) type c.
    loop at resb_tab_ges into ls_resb where bdmng < 0.
      exit.
    endloop.
    if sy-subrc eq 0.
* do the crossover check
      perform affwresb tables resb_tab_ges[]
                              affw_tab[]
                       changing l_lock.
      IF not l_lock is initial and NOT resb_pre_tab[] IS INITIAL.
*   Pre-Read Reservierungen
        SELECT * FROM resb INTO TABLE resb_tab_ges
                           FOR ALL ENTRIES IN resb_pre_tab
                           WHERE rsnum = resb_pre_tab-rsnum
                           AND   rspos = resb_pre_tab-rspos
                           AND   rsart = resb_pre_tab-rsart.
      ENDIF.
    endif.
  ENDIF.

  if sy-batch eq 'X' .
    wa_affwb_out_tab-flg_sel = 'X'.
    modify affwb_tab from wa_affwb_out_tab transporting flg_sel
           where flg_sel eq space .
  endif.

*}   INSERT
  REFRESH: resb_pre_tab,
           enq_resb,
           enq_rsnum.
  SORT resb_tab_ges BY rsnum rspos rsart.
  IF NOT pre03_tab[] IS INITIAL.
*   Prefetch der Materialkurztexte
    CALL FUNCTION 'MATERIAL_PRE_READ_MAKT'
         EXPORTING
              kzrfb  = space
         TABLES
              ipre03 = pre03_tab.
  ENDIF.
* Sortierung vorbereiten
  PERFORM init_sorttab.
* FCodes im Anzeigemodus ausblenden
  IF NOT p_disply IS INITIAL.
    cuafc_tab-fcode = fc_bu.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_buba.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_buda.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_char.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_del.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_korr.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_lort.
    APPEND cuafc_tab.
    cuafc_tab-fcode = fc_splt.
    APPEND cuafc_tab.
  ENDIF.
  IF p_mini IS INITIAL.
    IF r_single IS INITIAL.
*     Liste der fehlerhaften Warenbewegungen ausgeben, kumuliert
      PERFORM list_affwb_blocked_entries.
    ELSE.
*     Liste der fehlerhaften Warenbewegungen ausgeben, Einzelsätze
      PERFORM list_affwb_entries.
    ENDIF.

    if sy-batch eq 'X' .
      PERFORM post_changes.
    endif.

*   Überhaupt Einträge gefunden ?
    DESCRIBE TABLE affwb_tab LINES sy-tabix.
    IF sy-tabix IS INITIAL.
      IF cntr > 0.
*       mind. 1 Fehlersatz ist gesperrt und wird nicht angezeigt
        MESSAGE i432 WITH cntr.
      ELSE.
        MESSAGE i198.
      ENDIF.
    ENDIF.
  ELSE.
    DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.
    DATA: l_variant        TYPE disvariant.
*    l_variant-variant = p_layout.
    l_variant-report = sy-repid.


*--> build up fieldcatalog
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
         EXPORTING
              i_structure_name = 'AFFWB'
         CHANGING
              ct_fieldcat      = lt_fieldcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              i_structure_name = 'AFFWB'
              i_grid_title     = text-015
              it_fieldcat      = lt_fieldcat[]
              i_save           = 'A'
              is_variant       = l_variant
         TABLES
              t_outtab         = affwb_tab.


  ENDIF.

TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

AT LINE-SELECTION.

  CLEAR flg_start.
  PERFORM select_entry.

AT USER-COMMAND.

* Aufruf aus USER-COMMAND
  CLEAR flg_start.
* Kundeneigene Berechtigungsprüfung für Funktionscodes
  DATA l_error TYPE xflag.
  PERFORM aufruf_badi_authority_check USING affwb_tab[]
                                            sy-ucomm
                                      CHANGING l_error.
  IF NOT l_error IS INITIAL.
*   Auf Kundenwunsch zurück zum Listbild
    EXIT.
  ENDIF.
  CASE sy-ucomm.
    WHEN fc_amak.
      PERFORM mark_all.
    WHEN fc_amal.
      PERFORM del_mark_all.
    WHEN fc_auft.
      PERFORM show_order.
    WHEN fc_back.
      PERFORM leave_transaction.
    WHEN fc_bu.
      PERFORM post_changes.
      flg_start = yx.
      IF r_single IS INITIAL.
        PERFORM modif_screen_blocked.
      ELSE.
        PERFORM modif_screen_single.
      ENDIF.
    WHEN fc_buba.
      PERFORM post_background.
    WHEN fc_buda.
      PERFORM send_win_budat.
    WHEN fc_char.
      PERFORM change_charge.
    WHEN fc_date.
      PERFORM change_budat.
    WHEN fc_del.
      PERFORM del_entry.
    WHEN fc_end.
      PERFORM leave_transaction.
    WHEN fc_esc.
      PERFORM leave_transaction.
    WHEN fc_fail.
      PERFORM show_failure.
    WHEN fc_frsh.
      PERFORM refresh.
    WHEN fc_korr.
      PERFORM change_entries.
    WHEN fc_lort.
      PERFORM change_lgort.
    WHEN fc_mark.
      PERFORM locate_entry USING space.
      IF tabix <> 0.
        IF r_single IS INITIAL.
          READ TABLE affwb_out_tab INDEX tabix.
          IF affwb_out_tab-vbkz <> vbkz_bu AND
             affwb_out_tab-vbkz <> vbkz_shw.
            affwb_out_tab-flg_sel = yx.
            MODIFY affwb_out_tab INDEX tabix.
            MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yx.
            LOOP AT affwb_tab.
              CHECK affwb_tab-vbkz <> vbkz_bu AND
                    affwb_tab-vbkz <> vbkz_shw.
              affwb_tab-flg_sel = yx.
              MODIFY affwb_tab.
            ENDLOOP.
          ENDIF.
        ELSE.
          READ TABLE affwb_tab INDEX tabix.
          IF affwb_tab-vbkz <> vbkz_bu AND
             affwb_tab-vbkz <> vbkz_shw.
            affwb_tab-flg_sel = yx.
            MODIFY affwb_tab INDEX tabix.
            MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yx.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN fc_matn.
      PERFORM show_material.
    WHEN fc_md04.
      PERFORM show_stock_requ_list.
    WHEN fc_mmbe.
      PERFORM show_stock_list.
    WHEN fc_ruck.
      PERFORM show_conf.
    WHEN fc_sngl.
      PERFORM change_mode.
    WHEN fc_sort.
      PERFORM sort_entries.
    WHEN fc_splt.
      PERFORM split_entries.
    WHEN fc_summ.
      PERFORM change_mode.
    WHEN fc_verw.
      PERFORM show_admin.
  ENDCASE.

*----------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                               *
*----------------------------------------------------------------------*
*       Überschrift ausgeben                                           *
*----------------------------------------------------------------------*
FORM top_of_page.

  CLEAR sellin.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED.
  WRITE: /1      sy-datlo  COLOR COL_HEADING INVERSE,
          15     sy-title  COLOR COL_HEADING INVERSE.
  HIDE sellin.
  SKIP 1.
  WRITE / sy-uline.
  HIDE sellin.
  FORMAT COLOR COL_HEADING INTENSIFIED.
*{   INSERT         PA8K037066                                        1
* MATNR EXT-LISTEN
  DATA: RXM0_CUT TYPE I, RXM0_XTEXT(105), RXM0_ZTEXT(127).
  DESCRIBE FIELD AFFWB-MATNR OUTPUT-LENGTH RXM0_CUT.
  RXM0_CUT = 19 + RXM0_CUT - 18.
  DEFINE __DYNTEXT. " &1 = Text-
    RXM0_XTEXT = &1. RXM0_ZTEXT(19) = RXM0_XTEXT.
    RXM0_ZTEXT+RXM0_CUT = RXM0_XTEXT+19. WRITE AT /5 RXM0_ZTEXT.
  END-OF-DEFINITION.
*}   INSERT
  IF r_single IS INITIAL.
*{   REPLACE        PA8K037066                                        2
*\    WRITE: /5 text-013.
* MATNR EXT-LISTEN
    __DYNTEXT TEXT-013.
*}   REPLACE
  ELSE.
*{   REPLACE        PA8K037066                                        3
*\    WRITE: /5 text-010.
* MATNR EXT-LISTEN
    __DYNTEXT TEXT-010.
*}   REPLACE
  ENDIF.
  HIDE sellin.
  WRITE  1   sy-vline.
  POSITION   sy-linsz.
  WRITE      sy-vline.
  HIDE sellin.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  IF r_single IS INITIAL.
*{   REPLACE        PA8K037066                                        4
*\    WRITE: /5 text-014.
* MATNR EXT-LISTEN
    __DYNTEXT TEXT-014.
*}   REPLACE
  ELSE.
    IF flg_network IS INITIAL.
*{   REPLACE        PA8K037066                                        5
*\      WRITE: /5 text-011.
* MATNR EXT-LISTEN
      __DYNTEXT TEXT-011.
*}   REPLACE
    ELSE.
*{   REPLACE        PA8K037066                                        6
*\      WRITE: /5 text-012.
* MATNR EXT-LISTEN
      __DYNTEXT TEXT-012.
*}   REPLACE
    ENDIF.
  ENDIF.
  WRITE  1   sy-vline.
  POSITION   sy-linsz.
  WRITE      sy-vline.
  HIDE sellin.
  WRITE / sy-uline.
  HIDE sellin.
  sellin = 1.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM ADD_REFERED_POS                                           *
*----------------------------------------------------------------------*
*       Abhängige Positionen sperren und wenn erfolgreich in           *
*       Indextabelle TABIX_TAB aufnehmen                               *
*----------------------------------------------------------------------*
FORM add_refered_pos.

  DATA: tmp_tabix     LIKE sy-tabix,
        tmp_verschieb LIKE sy-tabix.

* mehrfache Einträge zur gleichen Belegnummer entfernen
  SORT refblnr_tab BY blnr indx.
  DELETE ADJACENT DUPLICATES FROM refblnr_tab COMPARING blnr.
  CLEAR tabix_tab.
  LOOP AT refblnr_tab.
*   abh. Positionen je Belegnummer sperren und nachlesen
    REFRESH enq_loc.
    LOOP AT affwb_tab WHERE weblnr = refblnr_tab-blnr
                      AND   vbkz   <> vbkz_del.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ENDLOOP.
*   Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
*   der Eintrag in Tabelle ENQ_LOC erhalten
    PERFORM sperren_affw.
    IF enq_loc[] IS INITIAL.
      IF r_single IS INITIAL.
*       bei kumulierter Anzeige zugehörige WE-Position bestimmen
        LOOP AT affwb_tab INTO affwb WHERE weblnr = refblnr_tab-blnr
                                     AND   NOT kzbew IS INITIAL
                                     AND   vbkz   <> vbkz_bu
                                     AND   vbkz   <> vbkz_shw.
          EXIT.
        ENDLOOP.
*       Index für Eintrag in der kumulierten Tabelle suchen
        LOOP AT affwb_out_tab WHERE werks = affwb-werks
                              AND   lgort = affwb-lgort
                              AND   matnr = affwb-matnr
                              AND   charg = affwb-charg
                              AND   bwart = affwb-bwart
                              AND   msgid = affwb-msgid
                              AND   msgno = affwb-msgno
                              AND   erfme = affwb-erfme.
          tabix_tab-ind_kum = sy-tabix.
          EXIT.
        ENDLOOP.
      ENDIF.
*     alle abhängigen Positionen konnten gesperrt werden, deshalb
*     alle Positionen für Detail-Warenbewegungs-Übersicht vorsehen
      LOOP AT affwb_tab WHERE weblnr = refblnr_tab-blnr
                        AND   vbkz   <> vbkz_bu
                        AND   vbkz   <> vbkz_shw.
        tabix_tab-ind = sy-tabix.
        IF NOT refblnr_tab-indx IS INITIAL.
          tmp_tabix = refblnr_tab-indx + tmp_verschieb.
          INSERT tabix_tab INDEX tmp_tabix.
          tmp_verschieb = tmp_verschieb + 1.
        ELSE.
          APPEND tabix_tab.
        ENDIF.
      ENDLOOP.
    ELSE.
*     mind. 1 abhängige Position konnte nicht gesperrt werden, deshalb
*     darf keine der abhängigen Positionen bearbeitet werden
    ENDIF.
  ENDLOOP.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM REFRESH                                                   *
*----------------------------------------------------------------------*
*       Fehlersätze zu Selektionsparametern einlesen                   *
*----------------------------------------------------------------------*
FORM refresh.

  IF p_disply IS INITIAL.
    REFRESH enq_loc.
    cntr = 0.
    DO.
      cntr = cntr + 1.
      READ LINE cntr.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CHECK sellin = 1.
      IF r_single IS INITIAL.
*       Blockanzeige
        line3 = sy-lisel.
        CHECK NOT line3-flg_sel IS INITIAL.
        LOOP AT affwb_tab WHERE werks = affwb-werks
                          AND   lgort = affwb-lgort
                          AND   matnr = affwb-matnr
                          AND   charg = affwb-charg
                          AND   bwart = affwb-bwart
                          AND   erfme = affwb-erfme
                          AND   msgid = affwb-msgid
                          AND   msgno = affwb-msgno.
          CHECK affwb_tab-vbkz IS INITIAL.
          IF affwb_tab-refbln IS INITIAL.
            affwb_tab-vbkz = vbkz_upd.
            MODIFY affwb_tab TRANSPORTING vbkz.
            enq_loc-weblnr  = affwb_tab-weblnr.
            enq_loc-weblpos = affwb_tab-weblpos.
            APPEND enq_loc.
          ELSE.
*           Nummer des Referenzbelegs sammeln
            refblnr_tab-blnr = affwb_tab-refbln.
            APPEND refblnr_tab.
          ENDIF.
        ENDLOOP.
      ELSE.
*       Einzelsatzanzeige
        line1 = sy-lisel.
        CHECK NOT line1-flg_sel IS INITIAL.
*       Key der Belegposition merken
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = affwb-weblnr.
        affw_key-weblpos = affwb-weblpos.
        READ TABLE affwb_tab WITH KEY affw_key.
        CHECK sy-subrc = 0.
        CHECK affwb_tab-vbkz IS INITIAL.
        IF affwb_tab-refbln IS INITIAL.
          affwb_tab-vbkz = vbkz_upd.
          MODIFY affwb_tab INDEX sy-tabix TRANSPORTING vbkz.
          enq_loc-weblnr  = affwb_tab-weblnr.
          enq_loc-weblpos = affwb_tab-weblpos.
          APPEND enq_loc.
        ELSE.
*         Nummer des Referenzbelegs sammeln
          refblnr_tab-blnr = affwb_tab-refbln.
          APPEND refblnr_tab.
        ENDIF.
      ENDIF.
    ENDDO.
*   AFFW-Satz sperren und nachlesen
*   Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
*   der Eintrag in Tabelle ENQ_LOC erhalten
    PERFORM sperren_affw.
*   für nicht sperrbare AFFW-Sätze Verbuchungskennzeichen zurücknehmen
    LOOP AT enq_loc.
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = enq_loc-weblnr.
      affw_key-weblpos = enq_loc-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CLEAR affwb_tab-vbkz.
      MODIFY affwb_tab INDEX sy-tabix.
    ENDLOOP.
*
    FREE imseg_tab.
    CLEAR affwb_tab.
    LOOP AT affwb_tab.
      CHECK affwb_tab-vbkz = vbkz_ins OR
            affwb_tab-vbkz = vbkz_upd.
      EXIT.
    ENDLOOP.
    IF affwb_tab-vbkz = vbkz_ins OR
       affwb_tab-vbkz = vbkz_upd.
*     Popup mit Sicherheitsabfrage beim Stornieren
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
           EXPORTING
                diagnosetext1 = text-003
                textline1     = text-008
                titel         = text-002
           IMPORTING
                answer        = antwort.
      CASE antwort.
        WHEN yja.
          PERFORM post_changes.
      ENDCASE.
    ENDIF.
  ENDIF.
* Select-Options und Parameter vom Erstaufruf abholen
  program = sy-repid.
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
       EXPORTING
            curr_report     = program
       TABLES
            selection_table = param_tab
       EXCEPTIONS
            not_found       = 1
            no_report       = 2
            OTHERS          = 3.
  CHECK sy-subrc = 0.
* alle Sperren zurücknehmen
  CALL FUNCTION 'DEQUEUE_ALL'
       EXCEPTIONS
            OTHERS = 1.

* aktuelles Programm neu aufrufen
  SUBMIT (program) WITH SELECTION-TABLE param_tab.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM GET_ENTRIES                                               *
*----------------------------------------------------------------------*
*       Fehlersätze zu Selektionsparametern einlesen                   *
*----------------------------------------------------------------------*
FORM get_entries.

  DATA: tmp_tabix LIKE sy-tabix.
* Umstellen externe --> interne PSP-Nummer
  IF NOT s_psp_af[]  IS INITIAL OR
     NOT s_psp_or[] IS INITIAL.
    PERFORM convert_psp_to_intern TABLES s_psp_af
                                         s_psp_or
                                         psp_affw_range
                                         psp_order_range.
  ENDIF.

* Wenn Selektionskriterien zur Baugruppe vorhanden, dann Vorselektion
  IF NOT s_baugr[] IS INITIAL OR
     NOT s_verid[] IS INITIAL OR
     NOT s_bwerk[] IS INITIAL OR
     NOT psp_order_range[] IS INITIAL OR
     NOT p_bkauf   IS INITIAL.
*   sind Serienaufträge in der Selektion ?
    IF auftyp-corp IN s_autyp.
*     Aufträge (Kostensammler) der Serienfertigung auswählen
      CALL FUNCTION 'RM_SELECT_RSNUM'
           EXPORTING
                i_kdauf       = p_bkauf
                i_kdpos       = p_bkpos
           TABLES
                i_werks_range = s_bwerk
                i_matnr_range = s_baugr
                i_verid_range = s_verid
                i_pspel_range = psp_order_range
                e_rsnum_range = r_rsnum.
    ENDIF.
*{   INSERT         PA8K037066                                        1
* Fall: Nachbearbeitungssätze zur "neuen" Rückmeldung
    CALL FUNCTION 'PPC1PP_RES_HEADER_READ'
         EXPORTING
              IF_KDAUF       = p_bkauf
              IF_KDPOS       = p_bkpos
         TABLES
              IT_WERKS_RANGE = s_bwerk
              IT_MATNR_RANGE = s_baugr
              IT_VERID_RANGE = s_verid
              IT_PSPEL_RANGE = psp_order_range
              ET_RSNUM_RANGE = r_rsnum.
*}   INSERT
    IF p_bkpos IS INITIAL.
      SELECT afpo~aufnr aufk~auart aufk~autyp
             FROM afpo INNER JOIN aufk ON afpo~aufnr = aufk~aufnr
             INTO CORRESPONDING FIELDS OF TABLE gt_order_tab
             WHERE afpo~matnr IN s_baugr
               AND afpo~verid IN s_verid
               AND afpo~pwerk IN s_bwerk
               AND afpo~projn IN psp_order_range
               AND afpo~kdauf =  p_bkauf
               AND afpo~posnr = '0001'.
    ELSE.
      SELECT afpo~aufnr aufk~auart aufk~autyp
             FROM afpo INNER JOIN aufk ON afpo~aufnr = aufk~aufnr
             INTO CORRESPONDING FIELDS OF TABLE gt_order_tab
             WHERE afpo~matnr IN s_baugr
               AND afpo~verid IN s_verid
               AND afpo~pwerk IN s_bwerk
               AND afpo~projn IN s_psp_or
               AND afpo~kdauf =  p_bkauf
               AND afpo~kdpos =  p_bkpos
               AND afpo~posnr = '0001'.
    ENDIF.
    CLEAR s_aufnr.
    s_aufnr-option = 'EQ'.
    s_aufnr-sign   = 'I'.
    LOOP AT gt_order_tab.
      s_aufnr-low = gt_order_tab-aufnr.
      APPEND s_aufnr.
    ENDLOOP.
*   wenn die Selektion nach Baugruppe kein Ergebnis bringt,
*   dann soll kein Select auf die AFFW erfolgen, wenn keine
*   weiteren Selektionskriterien angegeben sind
    CHECK NOT gt_order_tab[] IS INITIAL OR
          NOT r_rsnum[] IS INITIAL.
  ENDIF.

* Dummy-Fehlermeldungen einlesen
  SELECT * FROM tafwd INTO TABLE tafwd_tab.
  SORT tafwd_tab.

* Sätze mit Dummy-Fehlern dürfen nicht selektiert werden. Sie werden
* nur über den Hintergrundjob mit dem Programm CORUAFW0 bearbeitet,
* Dabei werden sie wie fehlerfreie Sätze behandelt.
* Inaktive Sätze aus AFFW dürfen ebenfalls nicht selektiert werden
* Sie gehören zu fehlerhaften oder vorgemerkten Rückmeldungen
* alle potentiellen Fehlersätze lesen
*  SELECT * FROM affw INTO TABLE affw_tab UP TO p_maxrow ROWS
*                                WHERE weblnr <> space
*                                AND   wablnr = space
*                                AND   ( inact = space OR
*                                        inact IS NULL )
*                                AND   weblnr IN s_belnr
*                                AND   werks  IN s_werks
*                                AND   matnr  IN s_matnr
*                                AND   lgort  IN s_lgort
*                                AND   dispo  IN s_dispo
*                                AND   fevor  IN s_fevor
*                                AND   ps_psp_pnr IN psp_affw_range
*                                AND   msgno <> space.

  SELECT * FROM affw as a INTO TABLE affw_tab UP TO p_maxrow ROWS
                                WHERE weblnr <> space
                                AND   wablnr = space
                                AND   ( inact = space OR
                                        inact IS NULL )
                                AND   weblnr IN s_belnr
                                AND   werks  IN s_werks
                                AND   matnr  IN s_matnr
                                AND   lgort  IN s_lgort
                                AND   dispo  IN s_dispo
                                AND   fevor  IN s_fevor
                                AND   ps_psp_pnr IN psp_affw_range
                                AND   msgno <> space
                                and exists ( select * from mard
                                             where matnr = a~matnr and
                                                   lgort = a~lgort and
                                                   werks = a~werks and
                                                   labst > 0 )
                                order by matnr  FWDAT   .


*  SELECT
*  a~WEBLNR  a~WEBLPOS a~ERSDA a~ERNAM a~LAEDA
*          a~AENAM a~MATNR a~WERKS a~LGORT a~CHARG
*a~BWART a~SOBKZ a~KZVBR a~KDAUF a~KDPOS a~KDEIN
*a~SHKZG a~WAERS a~ERFMG a~ERFME a~ELIKZ a~WEMPF
*a~ABLAD a~AUFNR a~RSNUM a~RSPOS a~KZEAR a~KZBEW
*a~LGNUM a~LGTYP a~LGPLA a~RUECK a~RMZHL a~AUFPL
*a~APLZL a~AUFPS a~MSGID a~MSGNO a~MSGTY a~MSGV1
*a~MSGV2 a~MSGV3 a~MSGV4 a~WABLNR a~AUTYP a~LIFNR
*a~INSMK a~FEVOR a~DISPO a~FWDAT a~BUDAT a~BLDAT
*a~SMBLN a~SMBLP a~SJAHR a~RSART a~MHDAT a~DNOTW
*a~MJAHR a~MBLPO a~PS_PSP_PNR a~ERZET a~FWZET
*a~PRVBE a~PRTNR  a~REFBLN a~INACT a~BEMOT a~PARBU
*a~GRUND a~BWTAR a~FLG_ORIG
*INTO CORRESPONDING FIELDS OF TABLE affw_tab UP TO p_maxrow ROWS
*   INTO TABLE affw_tab UP TO p_maxrow ROWS
*               FROM affw as a inner join mard as b on
*                     a~matnr = b~matnr and
*                     a~werks = b~werks and
*                     a~LGort = b~lgort
*                                WHERE a~weblnr <> space
*                                AND   a~wablnr = space
*                                AND   ( a~inact = space OR
*                                        a~inact IS NULL )
*                                AND   a~weblnr IN s_belnr
*                                AND   a~werks  IN s_werks
*                                AND   a~matnr  IN s_matnr
*                                AND   a~lgort  IN s_lgort
*                                AND   a~dispo  IN s_dispo
*                                AND   a~fevor  IN s_fevor
*                                AND   a~ps_psp_pnr IN psp_affw_range
*                                AND   a~msgno <> space
*                                and   b~labst > 0.

* Dummy-Fehlersätze eliminieren (ausgenommen Serienfertigung)
* (bei der Selektion der AFFWs besteht keine Möglichkeit, die in
*  der TAFWD definierten Fehler auszuschliessen!)
  IF NOT tafwd_tab[] IS INITIAL.
    SORT affw_tab BY autyp msgid msgno.
* Sätze aus der Serienfertigung (niedrigster Autyp) überspringen
    LOOP AT affw_tab WHERE autyp > auftyp-corp.
      EXIT.
    ENDLOOP.
    tmp_tabix = sy-tabix.
    LOOP AT affw_tab FROM tmp_tabix.
      READ TABLE tafwd_tab WITH KEY arbgb = affw_tab-msgid
                                    msgnr = affw_tab-msgno
                            BINARY SEARCH
                            TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DELETE affw_tab.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF NOT s_aufnr[] IS INITIAL.
    DELETE affw_tab WHERE NOT aufnr IN s_aufnr.
  ENDIF.
  IF NOT r_rsnum[] IS INITIAL.
    DELETE affw_tab WHERE NOT rsnum IN r_rsnum.
  ENDIF.

* unvollständige Reservierungen der Serienfertigung vervollständigen
  LOOP AT affw_tab WHERE autyp = auftyp-corp
                   AND   rspos IS INITIAL.
    APPEND affw_tab TO affw_old.
    DELETE affw_tab.
  ENDLOOP.
  IF NOT affw_old[] IS INITIAL.
    CALL FUNCTION 'RM_FILL_RSPOS_IN_AFFW'
         TABLES
              t_affw = affw_old
         EXCEPTIONS
              OTHERS = 1.
    LOOP AT affw_old.
      APPEND affw_old TO affw_tab.
    ENDLOOP.
  ENDIF.
  REFRESH affw_old.
  CLEAR   affw_old.
  affw_old[] = affw_tab[].
  SORT affw_old BY weblnr weblpos.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM LEAVE_TRANSACTION                                         *
*----------------------------------------------------------------------*
*       Transaktion beenden                                            *
*----------------------------------------------------------------------*
FORM leave_transaction.

* Parameter des Selektionsbildes an Transaktion übergeben
  SET PARAMETER ID 'WRK' FIELD s_werks-low.

  IF p_disply IS INITIAL.
    REFRESH enq_loc.
    cntr = 0.
    DO.
      cntr = cntr + 1.
      READ LINE cntr.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CHECK sellin = 1.
      IF r_single IS INITIAL.
*       Blockanzeige
        line3 = sy-lisel.
        CHECK NOT line3-flg_sel IS INITIAL.
        LOOP AT affwb_tab WHERE werks = affwb-werks
                          AND   lgort = affwb-lgort
                          AND   matnr = affwb-matnr
                          AND   charg = affwb-charg
                          AND   bwart = affwb-bwart
                          AND   erfme = affwb-erfme
                          AND   msgid = affwb-msgid
                          AND   msgno = affwb-msgno.
          CHECK affwb_tab-vbkz IS INITIAL.
          IF affwb_tab-refbln IS INITIAL.
            affwb_tab-vbkz = vbkz_upd.
            MODIFY affwb_tab TRANSPORTING vbkz.
            enq_loc-weblnr  = affwb_tab-weblnr.
            enq_loc-weblpos = affwb_tab-weblpos.
            APPEND enq_loc.
          ELSE.
*           Nummer des Referenzbelegs sammeln
            refblnr_tab-blnr = affwb_tab-refbln.
            APPEND refblnr_tab.
          ENDIF.
        ENDLOOP.
      ELSE.
*       Einzelsatzanzeige
        line1 = sy-lisel.
        CHECK NOT line1-flg_sel IS INITIAL.
*       Key der Belegposition merken
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = affwb-weblnr.
        affw_key-weblpos = affwb-weblpos.
        READ TABLE affwb_tab WITH KEY affw_key.
        CHECK sy-subrc = 0.
        CHECK affwb_tab-vbkz IS INITIAL.
        IF affwb_tab-refbln IS INITIAL.
          affwb_tab-vbkz = vbkz_upd.
          MODIFY affwb_tab INDEX sy-tabix TRANSPORTING vbkz.
          enq_loc-weblnr  = affwb_tab-weblnr.
          enq_loc-weblpos = affwb_tab-weblpos.
          APPEND enq_loc.
        ELSE.
*         Nummer des Referenzbelegs sammeln
          refblnr_tab-blnr = affwb_tab-refbln.
          APPEND refblnr_tab.
        ENDIF.
      ENDIF.
    ENDDO.
*   AFFW-Satz sperren und nachlesen
*   Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
*   der Eintrag in Tabelle ENQ_LOC erhalten
    PERFORM sperren_affw.
*   für nicht sperrbare AFFW-Sätze Verbuchungskennzeichen zurücknehmen
    LOOP AT enq_loc.
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = enq_loc-weblnr.
      affw_key-weblpos = enq_loc-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CLEAR affwb_tab-vbkz.
      MODIFY affwb_tab INDEX sy-tabix.
    ENDLOOP.
*
    FREE imseg_tab.
    CLEAR affwb_tab.
    LOOP AT affwb_tab.
      CHECK affwb_tab-vbkz = vbkz_ins OR
            affwb_tab-vbkz = vbkz_upd OR
            affwb_tab-vbkz = vbkz_del.
      EXIT.
    ENDLOOP.
    IF affwb_tab-vbkz = vbkz_ins OR
       affwb_tab-vbkz = vbkz_upd OR
       affwb_tab-vbkz = vbkz_del.
*     Popup mit Sicherheitsabfrage beim Stornieren
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
           EXPORTING
                diagnosetext1 = text-003
                textline1     = text-008
                titel         = text-002
           IMPORTING
                answer        = antwort.
      CASE antwort.
        WHEN yja.
          PERFORM post_changes.
          flg_start = yx.
          IF r_single IS INITIAL.
            PERFORM modif_screen_blocked.
          ELSE.
            PERFORM modif_screen_single.
          ENDIF.
        WHEN ynein.
          IF sy-binpt IS INITIAL AND sy-calld EQ yx.
            LEAVE PROGRAM.
          ELSE.
            LEAVE TO TRANSACTION sy-tcode.
          ENDIF.
      ENDCASE.
    ELSE.
      IF sy-binpt IS INITIAL AND sy-calld EQ yx.
       if fc_back  eq 'BCK'.
          LEAVE TO SCREEN 0.
         else.
         LEAVE PROGRAM.
        endif.
      ELSE.
        LEAVE TO TRANSACTION sy-tcode.
      ENDIF.
    ENDIF.
  ELSE.
    IF sy-binpt IS INITIAL AND sy-calld EQ yx.
      LEAVE PROGRAM.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM LIST_AFFWB_BLOCKED_ENTRIES                                *
*----------------------------------------------------------------------*
*       Liste der fehlerhaften Warenbewegungen ausgeben, kumuliert     *
*----------------------------------------------------------------------*
FORM list_affwb_blocked_entries.
*{   INSERT         PA8K037066                                        1
* MATNR EXT_LISTEN
  DATA: RXM0_48 TYPE I,
        RXM0_63 TYPE I,
        RXM0_74 TYPE I,
        RXM0_77 TYPE I,
        RXM0_86 TYPE I.
  DESCRIBE FIELD AFFWB-MATNR OUTPUT-LENGTH RXM0_48.
  RXM0_48 = RXM0_48 - 18.
  RXM0_63 = 63 + RXM0_48.
  RXM0_74 = 74 + RXM0_48.
  RXM0_77 = 77 + RXM0_48.
  RXM0_86 = 86 + RXM0_48.
  RXM0_48 = 48 + RXM0_48.
*
*}   INSERT

*{   REPLACE        PA8K037066                                        2
*\  NEW-PAGE LINE-SIZE 86.
* MATNR EXT-LISTEN
  NEW-PAGE LINE-SIZE RXM0_86.
*}   REPLACE
* CUA-Status und -Titel setzen
  SET TITLEBAR '003'.
  SET PF-STATUS 'BLOCK' EXCLUDING cuafc_tab.
* Kumulierte Ausgabetabelle aufbauen
  IF flg_start = yx OR
*    Neuaufbau nach Bearbeitung aus kumulierter Sicht
     NOT flg_refresh IS INITIAL OR
     NOT s_pname IS INITIAL.
*   Neuaufbau nur nach Start bzw. Sichern oder bei ext. Sort.
    PERFORM create_affwb_out_tab.
  ELSE.
*   Aktualisiern
    PERFORM update_affwb_out_tab.
  ENDIF.

  IF s_pname IS INITIAL.
*   Erstsortierung nach Erfassungsdatum
*   nur nach Start bzw. Sichern
    IF flg_start = yx.
      SORT affwb_out_tab BY ersda erzet werks lgort matnr charg
                            bwart erfme msgid msgno weblnr weblpos.
    ENDIF.
  ELSE.
*   externe Sortierung
*   PERFORM DO_SORT IN PROGRAM (S_PNAME) TABLES AFFWB_OUT_TAB.
    CALL FUNCTION 'CNSG_SORT_TABLE'
         TABLES
              t_table  = affwb_out_tab
         CHANGING
              c_handle = g_sort_handle.
  ENDIF.
* Zweizeilige Ausgabe
  RESERVE 2 LINES.

  if sy-batch eq 'X' .
    wa_affwb_out_tab-flg_sel = 'X'.
    modify affwb_out_tab from wa_affwb_out_tab transporting flg_sel
           where flg_sel eq space .
  endif.
* kumulierte Daten ausgeben
  LOOP AT affwb_out_tab.
    affwb = affwb_out_tab.
    CASE affwb-vbkz.
      WHEN vbkz_ins.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_upd.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_del.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WHEN vbkz_dll.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WHEN vbkz_bu.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_shw.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
      WHEN OTHERS.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
    ENDCASE.
    CLEAR subrc.
    IF NOT affwb-autyp IS INITIAL AND
       NOT affwb-werks IS INITIAL.
      AUTHORITY-CHECK OBJECT 'C_AFFW_TWK'
           ID 'AUTYP' FIELD affwb-autyp
           ID 'WERKS' FIELD affwb-werks.
      subrc = sy-subrc.
    ENDIF.
    IF NOT subrc IS INITIAL OR
      affwb-vbkz = vbkz_bu  OR
      affwb-vbkz = vbkz_del OR
      affwb-vbkz = vbkz_dll OR
      affwb-vbkz = vbkz_shw.
      WRITE: /03 affwb-flg_sel AS CHECKBOX     "nicht bearbeitbar
                               INPUT OFF.
    ELSE.
      WRITE: /03 affwb-flg_sel AS CHECKBOX     "bearbeitbar
                               INPUT.
    ENDIF.
    line3-blcount = affwb-blcount.
    WRITE:   affwb-matnr,
             affwb-werks,
             affwb-lgort,
             affwb-charg,
*{   REPLACE        PA8K037066                                        3
*\          48 affwb-bwart,
* MATNR EXT-LISTEN
          AT RXM0_48 AFFWB-BWART,
*}   REPLACE
             affwb-erfmg UNIT affwb-erfme,
             affwb-erfme,
*{   REPLACE        PA8K037066                                        4
*\          74 affwb-msgid,
*\          77 affwb-msgno,
* MATNR EXT-LISTEN
          AT RXM0_74 AFFWB-MSGID,
          AT RXM0_77 AFFWB-MSGNO,
*}   REPLACE
             line3-blcount NO-ZERO.
    HIDE:    affwb-matnr,
             affwb-werks,
             affwb-lgort,
             affwb-charg,
             affwb-bwart,
             affwb-erfme,
             affwb-msgid,
             affwb-msgno.
    sellin = 1.
    HIDE: sellin.
    WRITE  1   sy-vline.
    POSITION   sy-linsz.
    WRITE      sy-vline.
*   Schlüssel für Lesebaustein füllen
    mtcom-kenng = kenng_makt.
    mtcom-spras = sy-langu.
    mtcom-matnr = affwb-matnr.
*   Materialstamm lesen
    CALL FUNCTION 'MATERIAL_READ'
         EXPORTING
              schluessel         = mtcom
         IMPORTING
              matdaten           = makt
         TABLES
              seqmat01           = dummy_tab
         EXCEPTIONS
              material_not_found = 04
              plant_not_found    = 08
              account_not_found  = 12.
    CASE affwb-vbkz.
      WHEN vbkz_ins.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      WHEN vbkz_upd.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      WHEN vbkz_del.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      WHEN vbkz_dll.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      WHEN OTHERS.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDCASE.
    WRITE: /  makt-maktx UNDER affwb-matnr,
*{   REPLACE        PA8K037066                                        5
*\           63 affwb-ersda NO-ZERO,
         at rxm0_63 affwb-ersda NO-ZERO,
*}   REPLACE
              affwb-fwdat UNDER affwb-msgid NO-ZERO.
    sellin = 2.
    HIDE sellin.
    WRITE  1   sy-vline.
    POSITION   sy-linsz.
    WRITE      sy-vline.
*    write sy-lisel.
  ENDLOOP.
  CLEAR sellin.
  WRITE sy-uline.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CREATE_AFFWB_OUT_TAB                                      *
*----------------------------------------------------------------------*
*       Ausgabetabelle für verdichtete Anzeige aufbauen                *
*----------------------------------------------------------------------*
FORM create_affwb_out_tab.

  REFRESH affwb_out_tab.
  SORT affwb_tab BY werks lgort matnr charg bwart erfme msgid msgno.
* 1. mögliche Warenbewegung bereitstellen
  LOOP AT affwb_tab WHERE refbln IS INITIAL
                    OR    NOT kzbew IS INITIAL.
*   bei abhängigen Warenbewegungen zum Auftragsnetz nur WE-Positionen
*   auf Liste anzeigen
    EXIT.
  ENDLOOP.
  CHECK sy-subrc = 0.
  affwb_out_tab = affwb_tab.
  CLEAR: affwb_out_tab-erfmg,
         affwb_out_tab-fwdat,
         affwb_out_tab-fwzet,
         affwb_out_tab-ersda,
         affwb_out_tab-erzet.
  affwb_out_tab-blcount = 0.
  CLEAR: b_aend,
         b_flg_sel,
         b_vbkz.
  LOOP AT affwb_tab.
*   bei abhängigen Warenbewegungen zum Auftragsnetz nur WE-Positionen
*   auf Liste anzeigen
    CHECK affwb_tab-refbln IS INITIAL OR
          NOT affwb_tab-kzbew IS INITIAL.
    IF affwb_out_tab-matnr = affwb_tab-matnr AND
       affwb_out_tab-werks = affwb_tab-werks AND
       affwb_out_tab-lgort = affwb_tab-lgort AND
       affwb_out_tab-charg = affwb_tab-charg AND
       affwb_out_tab-bwart = affwb_tab-bwart AND
       affwb_out_tab-erfme = affwb_tab-erfme AND
       affwb_out_tab-msgid = affwb_tab-msgid AND
       affwb_out_tab-msgno = affwb_tab-msgno.
*     kumulieren
      affwb_out_tab-erfmg = affwb_out_tab-erfmg + affwb_tab-erfmg.
      IF affwb_out_tab-fwdat IS INITIAL OR
         affwb_out_tab-fwdat > affwb_tab-fwdat OR
         ( affwb_out_tab-fwdat = affwb_tab-fwdat AND
           affwb_out_tab-fwzet > affwb_tab-fwzet ).
        affwb_out_tab-fwdat = affwb_tab-fwdat.
        affwb_out_tab-fwzet = affwb_tab-fwzet.
      ENDIF.
      IF affwb_out_tab-ersda IS INITIAL OR
         affwb_out_tab-ersda > affwb_tab-ersda OR
         ( affwb_out_tab-ersda = affwb_tab-ersda AND
           affwb_out_tab-erzet > affwb_tab-erzet ).
        affwb_out_tab-ersda = affwb_tab-ersda.
        affwb_out_tab-erzet = affwb_tab-erzet.
      ENDIF.
      affwb_out_tab-blcount = affwb_out_tab-blcount + 1.
      IF affwb_tab-vbkz <> vbkz_bu AND
         affwb_tab-vbkz <> vbkz_shw.
        b_aend = yx.
      ENDIF.
      IF b_flg_sel IS INITIAL.
        b_flg_sel = affwb_tab-flg_sel.
      ENDIF.
      IF b_vbkz <> vbkz_ins            AND
         b_vbkz <> vbkz_upd            AND
         NOT affwb_tab-vbkz IS INITIAL AND
         affwb_tab-vbkz <> vbkz_bu     AND
         affwb_tab-vbkz <> vbkz_shw.
        b_vbkz = affwb_tab-vbkz.
      ENDIF.
    ELSE.
      affwb_out_tab-flg_sel = b_flg_sel.
      IF b_aend IS INITIAL.
        affwb_out_tab-vbkz = vbkz_shw.
      ELSE.
        affwb_out_tab-vbkz = b_vbkz.
      ENDIF.
*     kumulierte Daten sammeln
      APPEND affwb_out_tab.
*     neuer Eintrag
      affwb_out_tab = affwb_tab.
      affwb_out_tab-blcount = 1.
      IF affwb_tab-vbkz = vbkz_bu OR
         affwb_tab-vbkz = vbkz_shw.
        CLEAR: b_aend,
               b_vbkz.
      ELSE.
        b_aend = yx.
        b_vbkz = affwb_tab-vbkz.
      ENDIF.
      b_flg_sel = affwb_tab-flg_sel.
    ENDIF.
  ENDLOOP.
* kumulierte Daten sammeln
  affwb_out_tab-flg_sel = b_flg_sel.
  IF b_aend IS INITIAL.
    affwb_out_tab-vbkz = vbkz_shw.
  ELSE.
    affwb_out_tab-vbkz = b_vbkz.
  ENDIF.
  APPEND affwb_out_tab.

ENDFORM.
*eject
*---------------------------------------------------------------------*
*       FORM UPDATE_AFFWB_OUT_TAB                                     *
*---------------------------------------------------------------------*
*       Ausgabetabelle für verdichtete Anzeige aktualisieren          *
*---------------------------------------------------------------------*
FORM update_affwb_out_tab.

* AFFWB-Tablle (Einzelsätze) zu jeweils EINEM kumuliertem Satz
  DATA: BEGIN OF tmp_affwb_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF tmp_affwb_tab.
* Variablen
  DATA: tmp_index_kum  LIKE sy-tabix,
        tmp_index_offs LIKE sy-tabix,
        tmp_next_index LIKE sy-tabix,
        tmp_offset     LIKE sy-tabix,
        tmp_counter    LIKE sy-tabix,
        tmp_countermax LIKE sy-tabix.

  SORT tabix_tab BY ind_kum.
  DESCRIBE TABLE tabix_tab LINES tmp_countermax.
  tmp_counter = 0.
  tmp_offset  = 0.
  READ TABLE tabix_tab INDEX 1.
  tmp_next_index = tabix_tab-ind_kum.

* W H I L E  - S C H L E I F E   über TABIX_TAB-Einträge
  WHILE tmp_counter < tmp_countermax.
    REFRESH tmp_affwb_tab.
    tmp_index_kum = tmp_next_index.
*   AFFWB-Tabelle zu EINEM kumuliertem Satz aufbauen
    LOOP AT tabix_tab WHERE ind_kum >= tmp_index_kum.
      IF tmp_index_kum = tabix_tab-ind_kum.
        tmp_counter = tmp_counter + 1.
        READ TABLE affwb_tab INDEX tabix_tab-ind.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING affwb_tab TO tmp_affwb_tab.
          APPEND tmp_affwb_tab.
        ENDIF.
      ELSE.
        tmp_next_index = tabix_tab-ind_kum.
        EXIT.
      ENDIF.
    ENDLOOP.

*   Aktuellen AFFWB_OUT_TAB Satz löschen
    READ TABLE affwb_out_tab INDEX tmp_index_kum.
    IF sy-subrc = 0.
      DELETE affwb_out_tab INDEX tmp_index_kum.
    ENDIF.

    SORT tmp_affwb_tab BY werks lgort matnr charg bwart
                                      erfme msgid msgno.
*   1. mögliche Warenbewegung bereitstellen
    LOOP AT tmp_affwb_tab WHERE refbln IS INITIAL
                          OR    NOT kzbew IS INITIAL.
*     bei abhängigen Warenbewegungen zum Auftragsnetz nur WE-Positionen
*     auf Liste anzeigen
      EXIT.
    ENDLOOP.

    affwb_out_tab = tmp_affwb_tab.
    CLEAR: affwb_out_tab-erfmg,
         affwb_out_tab-fwdat,
         affwb_out_tab-fwzet,
         affwb_out_tab-ersda,
         affwb_out_tab-erzet.
    affwb_out_tab-blcount = 0.
    CLEAR: b_aend,
           b_flg_sel,
           b_vbkz.
    LOOP AT tmp_affwb_tab.
*     bei abhängigen Warenbewegungen zum Auftragsnetz nur WE-Positionen
*     auf Liste anzeigen
      CHECK tmp_affwb_tab-refbln IS INITIAL OR
            NOT tmp_affwb_tab-kzbew IS INITIAL.
      IF affwb_out_tab-matnr = tmp_affwb_tab-matnr AND
         affwb_out_tab-werks = tmp_affwb_tab-werks AND
         affwb_out_tab-lgort = tmp_affwb_tab-lgort AND
         affwb_out_tab-charg = tmp_affwb_tab-charg AND
         affwb_out_tab-bwart = tmp_affwb_tab-bwart AND
         affwb_out_tab-erfme = tmp_affwb_tab-erfme AND
         affwb_out_tab-msgid = tmp_affwb_tab-msgid AND
         affwb_out_tab-msgno = tmp_affwb_tab-msgno.
*       kumulieren
        affwb_out_tab-erfmg = affwb_out_tab-erfmg
                                      + tmp_affwb_tab-erfmg.
        IF affwb_out_tab-fwdat IS INITIAL OR
           affwb_out_tab-fwdat > tmp_affwb_tab-fwdat OR
           ( affwb_out_tab-fwdat = tmp_affwb_tab-fwdat AND
             affwb_out_tab-fwzet > tmp_affwb_tab-fwzet ).
          affwb_out_tab-fwdat = tmp_affwb_tab-fwdat.
          affwb_out_tab-fwzet = tmp_affwb_tab-fwzet.
        ENDIF.
        IF affwb_out_tab-ersda IS INITIAL OR
           affwb_out_tab-ersda > tmp_affwb_tab-ersda OR
           ( affwb_out_tab-ersda = tmp_affwb_tab-ersda AND
             affwb_out_tab-erzet > tmp_affwb_tab-erzet ).
          affwb_out_tab-ersda = tmp_affwb_tab-ersda.
          affwb_out_tab-erzet = tmp_affwb_tab-erzet.
        ENDIF.
        affwb_out_tab-blcount = affwb_out_tab-blcount + 1.
        IF tmp_affwb_tab-vbkz <> vbkz_bu AND
           tmp_affwb_tab-vbkz <> vbkz_shw.
          b_aend = yx.
        ENDIF.
        IF b_flg_sel IS INITIAL.
          b_flg_sel = tmp_affwb_tab-flg_sel.
        ENDIF.
        IF b_vbkz <> vbkz_ins            AND
           b_vbkz <> vbkz_upd            AND
           NOT tmp_affwb_tab-vbkz IS INITIAL AND
           tmp_affwb_tab-vbkz <> vbkz_bu     AND
           tmp_affwb_tab-vbkz <> vbkz_shw.
          b_vbkz = tmp_affwb_tab-vbkz.
        ENDIF.
      ELSE.
        affwb_out_tab-flg_sel = b_flg_sel.
        IF b_aend IS INITIAL.
          affwb_out_tab-vbkz = vbkz_shw.
        ELSE.
          affwb_out_tab-vbkz = b_vbkz.
        ENDIF.
*       kumulierte Daten sammeln
        tmp_index_offs = tmp_index_kum + tmp_offset.
        INSERT affwb_out_tab INDEX tmp_index_offs.
        tmp_offset = tmp_offset + 1.
*       neuen Eintrag merken
        affwb_out_tab = tmp_affwb_tab.
        affwb_out_tab-blcount = 1.
        IF tmp_affwb_tab-vbkz = vbkz_bu OR
           tmp_affwb_tab-vbkz = vbkz_shw.
          CLEAR: b_aend,
                 b_vbkz.
        ELSE.
          b_aend = yx.
          b_vbkz = tmp_affwb_tab-vbkz.
        ENDIF.
        b_flg_sel = tmp_affwb_tab-flg_sel.
      ENDIF.
    ENDLOOP.
*   kumulierte Daten sammeln
    affwb_out_tab-flg_sel = b_flg_sel.
    IF b_aend IS INITIAL.
      affwb_out_tab-vbkz = vbkz_shw.
    ELSE.
      affwb_out_tab-vbkz = b_vbkz.
    ENDIF.
    tmp_index_offs = tmp_index_kum + tmp_offset.
    INSERT affwb_out_tab INDEX tmp_index_offs.
  ENDWHILE.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM LIST_AFFWB_ENTRIES                                        *
*----------------------------------------------------------------------*
*       Liste der fehlerhaften Warenbewegungen ausgeben                *
*----------------------------------------------------------------------*
FORM list_affwb_entries.
*{   INSERT         PA8K037066                                        1
* MATNR EXT-LISTEN
  DATA: RXM0_48 TYPE I,
        RXM0_80 TYPE I,
        RXM0_83 TYPE I,
        RXM0_111 TYPE I.
  DESCRIBE FIELD AFFWB-MATNR OUTPUT-LENGTH RXM0_48.
  RXM0_48 = RXM0_48 - 18.
  RXM0_80 = 80 + RXM0_48.
  RXM0_83 = 83 + RXM0_48.
  RXM0_111 = 111 + RXM0_48.
  RXM0_48 = 48 + RXM0_48.
*}   INSERT

*{   REPLACE        PA8K037066                                        2
*\  NEW-PAGE LINE-SIZE 111.
* MATNR EXT-LISTEN
  NEW-PAGE LINE-SIZE RXM0_111.
*}   REPLACE
* CUA-Status und -Titel setzen
  SET TITLEBAR '001'.
  SET PF-STATUS 'LIST' EXCLUDING cuafc_tab.
* Tabelle sortieren
  IF s_pname IS INITIAL.
*   Erstsortierung nach Erfassungsdatum
*   nur nach Start bzw. nach Sichern
    IF flg_start = yx.
      SORT affwb_tab BY ersda erzet werks lgort matnr charg
                        bwart erfme msgid msgno weblnr weblpos.
    ENDIF.
  ELSE.
*   externe Sortierung
*   PERFORM DO_SORT IN PROGRAM (S_PNAME) TABLES AFFWB_TAB.
    CALL FUNCTION 'CNSG_SORT_TABLE'
         TABLES
              t_table  = affwb_tab
         CHANGING
              c_handle = g_sort_handle.
  ENDIF.
* Zweizeilige Ausgabe
  RESERVE 2 LINES.
  LOOP AT affwb_tab.
*   bei abhängigen Warenbewegungen zum Auftragsnetz nur WE-Positionen
*   auf Liste anzeigen
    CHECK affwb_tab-refbln IS INITIAL OR
          NOT affwb_tab-kzbew IS INITIAL.
    affwb = affwb_tab.
    CASE affwb-vbkz.
      WHEN vbkz_ins.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_upd.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_del.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WHEN vbkz_dll.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED.
      WHEN vbkz_bu.
        FORMAT COLOR COL_POSITIVE INTENSIFIED.
      WHEN vbkz_shw.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
      WHEN OTHERS.
        FORMAT COLOR COL_NORMAL INTENSIFIED.
    ENDCASE.
    CLEAR subrc.
    IF NOT affwb-autyp IS INITIAL AND
       NOT affwb-werks IS INITIAL.
      AUTHORITY-CHECK OBJECT 'C_AFFW_TWK'
           ID 'AUTYP' FIELD affwb-autyp
           ID 'WERKS' FIELD affwb-werks.
      subrc = sy-subrc.
    ENDIF.
    IF NOT subrc IS INITIAL OR
      affwb-vbkz = vbkz_bu  OR
      affwb-vbkz = vbkz_del OR
      affwb-vbkz = vbkz_dll OR
      affwb-vbkz = vbkz_shw.
      WRITE: /03 affwb-flg_sel AS CHECKBOX   "nicht bearbeitbar
                               INPUT OFF.
    ELSE.
      WRITE: /03 affwb-flg_sel AS CHECKBOX   "bearbeitbar
                               INPUT.
    ENDIF.
    WRITE:   affwb-matnr,
             affwb-werks,
             affwb-lgort,
             affwb-charg,
*{   REPLACE        PA8K037066                                        3
*\          48 affwb-bwart,
*\             affwb-erfmg UNIT affwb_tab-erfme,
*\             affwb-erfme,
*\             affwb-kzear,
*\          80 affwb-msgid,
*\          83 affwb-msgno,
* MATNR EXT-LISTEN
          AT RXM0_48 AFFWB-BWART,
             AFFWB-ERFMG UNIT AFFWB_TAB-ERFME,
             AFFWB-ERFME,
             AFFWB-KZEAR,
          AT RXM0_80 AFFWB-MSGID,
          AT RXM0_83 AFFWB-MSGNO,
*}   REPLACE
             affwb-fwdat NO-ZERO,
             affwb-fwzet NO-ZERO,
             affwb-dispo.
    HIDE:    affwb-matnr,
             affwb-werks,
             affwb-lgort,
             affwb-charg,
             affwb-bwart,
             affwb-erfme,
             affwb-msgid,
             affwb-msgno.

    sellin = 1.
    HIDE: sellin, affwb-weblnr, affwb-weblpos.
    WRITE  1   sy-vline.
    POSITION   sy-linsz.
    WRITE      sy-vline.
*   Schlüssel für Lesebaustein füllen
    mtcom-kenng = kenng_makt.
    mtcom-spras = sy-langu.
    mtcom-matnr = affwb-matnr.
*   Materialstamm lesen
    CALL FUNCTION 'MATERIAL_READ'
         EXPORTING
              schluessel         = mtcom
         IMPORTING
              matdaten           = makt
         TABLES
              seqmat01           = dummy_tab
         EXCEPTIONS
              material_not_found = 04
              plant_not_found    = 08
              account_not_found  = 12.
    CASE affwb-vbkz.
      WHEN vbkz_ins.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      WHEN vbkz_upd.
        FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
      WHEN vbkz_del.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      WHEN vbkz_dll.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
      WHEN OTHERS.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDCASE.
    WRITE: / makt-maktx          UNDER affwb-matnr,
             affwb-kdauf         UNDER affwb_tab-bwart,
             affwb-kdpos NO-ZERO,
             affwb-aufnr NO-ZERO UNDER affwb-erfme,
             affwb-autyp         UNDER affwb-msgno,
             affwb-ersda NO-ZERO UNDER affwb-fwdat,
             affwb-erzet NO-ZERO UNDER affwb-fwzet,
             affwb-fevor         UNDER affwb-dispo.
    sellin = 2.
    HIDE sellin.
    WRITE  1   sy-vline.
    POSITION   sy-linsz.
    WRITE      sy-vline.
    CLEAR sellin.
  ENDLOOP.
  WRITE sy-uline.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CHANGE_BUDAT                                              *
*----------------------------------------------------------------------*
*       Buchungsdatum ersetzen                                         *
*----------------------------------------------------------------------*
FORM change_budat.

  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      IF affwb_tab-refbln IS INITIAL.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ELSE.
        refblnr_tab-blnr = affwb_tab-refbln.

      ENDIF.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CHECK NOT tabix_tab[] IS INITIAL.
*
  cntr = 0.
  REFRESH: enq_loc, tmp_enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
* markierte Sätze sperren
    enq_loc-weblnr  = affwb_tab-weblnr.
    enq_loc-weblpos = affwb_tab-weblpos.
    APPEND enq_loc.
* abhängige WaBe ? wenn ja, diese ebenfalls sperren
    IF NOT affwb_tab-refbln  IS INITIAL.
      LOOP AT  affwb_tab INTO ls_affwb_tab
              WHERE weblnr = affwb_tab-refbln
              AND weblpos NE affwb_tab-weblpos.
        enq_loc-weblnr  = ls_affwb_tab-weblnr.
        enq_loc-weblpos = ls_affwb_tab-weblpos.
        APPEND enq_loc.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* zu einer nicht-sperrbaren Position abhängige WaBe auch als
* nicht-sperrbar kennzeichnen
  APPEND LINES OF enq_loc TO tmp_enq_loc.
  LOOP AT tmp_enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = tmp_enq_loc-weblnr
                      AND   weblpos = tmp_enq_loc-weblpos.
      CLEAR enq_loc.
      enq_loc-weblnr  = ls_affwb_tab-weblnr.
      enq_loc-weblpos = ls_affwb_tab-weblpos.
      APPEND enq_loc.
    ENDLOOP.
  ENDLOOP.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  CHECK NOT tabix_tab[] IS INITIAL.
  affwb-budat = sy-datlo.
  affwb-bldat = sy-datlo.
  CALL SCREEN screen_0100 STARTING AT 20 3.
  CHECK ok_code <> fc_rw.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    affwb_tab-budat = affwb-budat.
    affwb_tab-bldat = affwb-bldat.
    affwb_tab-mhdat = affwb-mhdat.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz  = vbkz_upd.
    ENDIF.
    MODIFY affwb_tab INDEX tabix_tab-ind.
* bei abhängigen Warenbewegungen ebenfalls Daten übernehmen.
    IF NOT affwb_tab-refbln IS INITIAL.
      ls_affwb_tab = affwb_tab.
      LOOP AT  affwb_tab  WHERE weblnr = ls_affwb_tab-refbln
                          AND weblpos NE ls_affwb_tab-weblpos.
        affwb_tab-budat = affwb-budat.
        affwb_tab-bldat = affwb-bldat.
        affwb_tab-mhdat = affwb-mhdat.
        IF affwb_tab-vbkz <> vbkz_ins.
          affwb_tab-vbkz  = vbkz_upd.
        ENDIF.
        MODIFY affwb_tab.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  SET PF-STATUS 'LIST' EXCLUDING cuafc_tab.
  SET TITLEBAR '001'.
  IF r_single IS INITIAL.
    PERFORM modif_screen_blocked.
  ELSE.
    PERFORM modif_screen_single.
  ENDIF.
  REFRESH tabix_tab.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CHANGE_CHARGE                                             *
*----------------------------------------------------------------------*
*       Charge ersetzen                                                *
*----------------------------------------------------------------------*
FORM change_charge.

  DATA: ls_marc LIKE marc.

  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
*       Nicht bei Storno-Zeile, da Beleg-Storno
        CHECK affwb_tab-sjahr IS INITIAL.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz <> vbkz_bu AND
            affwb_tab-vbkz <> vbkz_shw.
*     Nicht bei Storno-Zeile, da Beleg-Storno
      CHECK affwb_tab-sjahr IS INITIAL.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
*           Nicht bei Storno-Zeile, da Beleg-Storno
            CHECK affwb_tab-sjahr IS INITIAL.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
*         Nicht bei Storno-Zeile, da Beleg-Storno
          CHECK affwb_tab-sjahr IS INITIAL.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CHECK NOT tabix_tab[] IS INITIAL.
*
  cntr = 0.
  REFRESH enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nicht verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      cntr = cntr + 1.
      DELETE tabix_tab.
    ENDIF.
  ENDLOOP.
  IF cntr > 0.
    MESSAGE i507 WITH cntr.
    CHECK NOT tabix_tab[] IS INITIAL.
  ENDIF.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  CHECK NOT tabix_tab[] IS INITIAL.
  CALL SCREEN screen_0300 STARTING AT 20 3.
  CHECK ok_code <> fc_rw.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    IF NOT affw-charg IS INITIAL.
*     auf Chargenpflicht prüfen
      CALL FUNCTION 'MARC_SINGLE_READ'
           EXPORTING
                matnr             = affwb_tab-matnr
                werks             = affwb_tab-werks
           IMPORTING
                wmarc             = ls_marc
           EXCEPTIONS
                lock_on_marc      = 1
                lock_system_error = 2
                wrong_call        = 3
                not_found         = 4.
      IF sy-subrc IS INITIAL AND
         NOT ls_marc-xchpf IS INITIAL.
*       Charge zuweisen
        affwb_tab-charg = affw-charg.
      ENDIF.
    ENDIF.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz  = vbkz_upd.
    ENDIF.
    MODIFY affwb_tab INDEX tabix_tab-ind.
  ENDLOOP.
* Neuaufbau der kumulierten Sicht
  flg_refresh = yx.
  IF r_single IS INITIAL.
    PERFORM modif_screen_blocked.
  ELSE.
    PERFORM modif_screen_single.
  ENDIF.
  REFRESH tabix_tab.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CHANGE_LGORT                                              *
*----------------------------------------------------------------------*
*       Lagerort ersetzen                                              *
*----------------------------------------------------------------------*
FORM change_lgort.

  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
*       Nicht bei Storno-Zeile, da Beleg-Storno
        CHECK affwb_tab-sjahr IS INITIAL.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz <> vbkz_bu AND
            affwb_tab-vbkz <> vbkz_shw.
*     Nicht bei Storno-Zeile, da Beleg-Storno
      CHECK affwb_tab-sjahr IS INITIAL.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
*           Nicht bei Storno-Zeile, da Beleg-Storno
            CHECK affwb_tab-sjahr IS INITIAL.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
*         Nicht bei Storno-Zeile, da Beleg-Storno
          CHECK affwb_tab-sjahr IS INITIAL.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CHECK NOT tabix_tab[] IS INITIAL.
*
  cntr = 0.
  REFRESH enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nicht verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      cntr = cntr + 1.
      DELETE tabix_tab.
    ENDIF.
  ENDLOOP.
  IF cntr > 0.
    MESSAGE i507 WITH cntr.
    CHECK NOT tabix_tab[] IS INITIAL.
  ENDIF.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  CHECK NOT tabix_tab[] IS INITIAL.
* Fehlersätze konnten erfolgreich gesperrt werden
  CALL SCREEN screen_0200 STARTING AT 20 3.
  CHECK ok_code <> fc_rw.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    affwb_tab-lgort = affw-lgort.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz = vbkz_upd.
    ENDIF.
    MODIFY affwb_tab INDEX tabix_tab-ind.
  ENDLOOP.
* Neuaufbau der kumulierten Sicht
  flg_refresh = yx.
  IF r_single IS INITIAL.
    PERFORM modif_screen_blocked.
  ELSE.
    PERFORM modif_screen_single.
  ENDIF.
  REFRESH tabix_tab.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SEND_WIN_BUDAT                                            *
*----------------------------------------------------------------------*
*       Window zur Pflege des Beleg- und Buchungsdatums senden         *
*----------------------------------------------------------------------*
FORM send_win_budat.

  DATA: tmp_budat LIKE affwb-budat,
        tmp_bldat LIKE affwb-bldat,
        tmp_mhdat LIKE affwb-mhdat.

  CHECK NOT r_single IS INITIAL.
  REFRESH refblnr_tab.
  CLEAR tabix_tab.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
*   Funktion nur bei Einzelsatzanzeige aktiv
    line1 = sy-lisel.
    CHECK NOT line1-flg_sel IS INITIAL.
*   vollständiger Eintrag in Tabelle lesen
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = affwb-weblnr.
    affw_key-weblpos = affwb-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz <> vbkz_bu AND
          affwb_tab-vbkz <> vbkz_shw.
    tabix_tab-ind = sy-tabix.
    EXIT.
  ENDDO.
  IF tabix_tab-ind IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    READ TABLE affwb_tab INDEX tabix.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz <> vbkz_bu AND
          affwb_tab-vbkz <> vbkz_shw.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  READ TABLE affwb_tab INDEX tabix_tab-ind.
  CHECK sy-subrc = 0.
  affwb = affwb_tab.
  REFRESH enq_loc.
* Fehlersätze sperren
  IF affwb_tab-refbln IS INITIAL.
    IF affwb_tab-vbkz IS INITIAL.
*     Pos. unabhängig von anderen Positionen: Fehlersatz sperren
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ENDIF.
  ELSE.
*   Position ist abhängig von anderen Positionen
    refblnr_tab-blnr = affwb_tab-refbln.
    LOOP AT affwb_tab WHERE weblnr = refblnr_tab-blnr.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ENDLOOP.
  ENDIF.

* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* Fehlersatz erfolgreich gesperrt: Tabelle ENQ_TAB ist leer
  CHECK enq_loc[] IS INITIAL.
*
  tmp_budat = affwb-budat.
  tmp_bldat = affwb-bldat.
  tmp_mhdat = affwb-mhdat.
  CALL SCREEN screen_0100 STARTING AT 20 3.
  IF ok_code <> fc_rw.
    affwb_tab-budat = affwb-budat.
    affwb_tab-bldat = affwb-bldat.
    affwb_tab-mhdat = affwb-mhdat.
    IF affwb_tab-vbkz <> vbkz_ins AND
       affwb_tab-vbkz <> vbkz_upd.
      affwb_tab-vbkz = vbkz_upd.
    ENDIF.
  ELSE.
    affwb_tab-budat = tmp_budat.
    affwb_tab-bldat = tmp_bldat.
    affwb_tab-mhdat = tmp_mhdat.
  ENDIF.
  IF affwb_tab-refbln IS INITIAL.
    MODIFY affwb_tab INDEX tabix_tab-ind
                     TRANSPORTING budat bldat mhdat vbkz.
  ELSE.
    MODIFY affwb_tab TRANSPORTING budat bldat mhdat vbkz
                     WHERE weblnr =  refblnr_tab-blnr
                     AND   vbkz   <> vbkz_del
                     AND   vbkz   <> vbkz_dll.
  ENDIF.
  SET PF-STATUS 'LIST' EXCLUDING cuafc_tab.
  SET TITLEBAR '001'.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SELECT_ENTRIES                                            *
*----------------------------------------------------------------------*
*       aus eingelesenen Fehlersätzen selektieren                      *
*----------------------------------------------------------------------*
FORM select_entries USING sel_subrc LIKE sy-subrc.

  sel_subrc = 4.
  CHECK NOT affw_tab-msgno IS INITIAL OR
        affw_tab-autyp = auftyp-corp.
  CHECK affw_tab-werks IN s_werks.
  CHECK affw_tab-matnr IN s_matnr.
  CHECK affw_tab-lgort IN s_lgort.
  CHECK affw_tab-dispo IN s_dispo.
  CHECK affw_tab-fevor IN s_fevor.
  CHECK affw_tab-aufnr IN s_aufnr.
  CHECK affw_tab-weblnr IN s_belnr.
  CHECK affw_tab-autyp IN s_autyp.
  CHECK affw_tab-bwart IN s_bwart.
  IF NOT p_kdauf IS INITIAL.
    CHECK affw_tab-kdauf = p_kdauf.
    IF NOT p_kdpos IS INITIAL.
      CHECK affw_tab-kdpos = p_kdpos.
    ENDIF.
  ENDIF.
  IF NOT p_msgid IS INITIAL.
    CHECK affw_tab-msgid = p_msgid.
    IF NOT p_msgno IS INITIAL.
      CHECK affw_tab-msgno = p_msgno.
    ENDIF.
  ENDIF.
  IF NOT p_datuv IS INITIAL.
    CHECK affw_tab-fwdat >= p_datuv.
    IF NOT p_uzeiv IS INITIAL.
      CHECK affw_tab-fwdat > p_datuv OR
            affw_tab-fwzet >= p_uzeiv.
    ENDIF.
  ENDIF.
  IF NOT p_datub IS INITIAL.
    CHECK affw_tab-fwdat <= p_datub.
    IF NOT p_uzeib IS INITIAL.
      CHECK affw_tab-fwdat < p_datub OR
            affw_tab-fwzet <= p_uzeib.
    ENDIF.
  ENDIF.
  IF NOT p_datpv IS INITIAL.
    CHECK affw_tab-budat >= p_datpv.
  ENDIF.
  IF NOT p_datpb IS INITIAL.
    CHECK affw_tab-budat <= p_datpb.
  ENDIF.
  sel_subrc = 0.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM CALL_DETAIL_NEW                                           *
*----------------------------------------------------------------------*
*       Neues Detailbild für Bearbeitung aufrufen                      *
*----------------------------------------------------------------------*
FORM call_detail_new.

* Key-Tabelle für AFFW
  DATA: BEGIN OF tmp_key_affwb OCCURS 0,
          mandt   LIKE affw-mandt,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF tmp_key_affwb.
* Tabelle für lokale Chargensperren
  DATA: lt_bdbatch LIKE bdbatch OCCURS 0 WITH HEADER LINE,
        l_menge    LIKE bdbatch-menge,
        ls_cowb_comp LIKE cowb_comp.

*     Konstante
  DATA: BEGIN OF herkunft,
          n VALUE 'N',
        END   OF herkunft.
  DATA: trtyp_anzeigen LIKE tc10-trtyp    VALUE 'A',
        trtyp_aendern  LIKE tc10-trtyp    VALUE 'V',
        geloescht                         VALUE 'D',
        tmp_counter    LIKE sy-tabix,
        tmp_index_kum  LIKE sy-tabix.
  DATA: tmp_weblnr     LIKE affw-weblnr.
  DATA: l_refblnr      LIKE affw-refbln.                 "abhängige WaBe

  CHECK NOT tabix_tab[] IS INITIAL.
* Aufruf des Komponentenbildes vorbereiten
  REFRESH: cowb_comp_tab,
           tmp_key_affwb,
           lt_bdbatch.
  CLEAR lt_bdbatch.
  CLEAR tmp_weblnr.
  READ TABLE tabix_tab INDEX 1.
  READ TABLE affwb_tab INDEX tabix_tab-ind.
  IF sy-subrc = 0.
    tmp_weblnr = affwb_tab-weblnr.
  ENDIF.

  REFRESH refblnr_tab.
  CLEAR   refblnr_tab.
* selektierte fehlerhafte Warenbewegungen bereitstellen
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    IF affwb_tab-weblnr <> tmp_weblnr.
      CLEAR tmp_weblnr.
    ENDIF.
    CLEAR cowb_comp_tab.
    IF NOT affwb_tab-rsnum IS INITIAL AND
       NOT affwb_tab-rspos IS INITIAL.
      resb_key-mandt = sy-mandt.
      resb_key-rsnum = affwb_tab-rsnum.
      resb_key-rspos = affwb_tab-rspos.
      resb_key-rsart = affwb_tab-rsart.
      READ TABLE resb_tab_ges WITH KEY resb_key BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING resb_tab_ges TO cowb_comp_tab.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING affwb_tab TO cowb_comp_tab.
*   falls Stornozeile, dann nur anzeigen, da es
*   sich um einen Belegstorno handelt
    IF affwb_tab-sjahr IS INITIAL.
      CLEAR cowb_comp_tab-flg_so.
    ELSE.
      cowb_comp_tab-flg_so = yx.       "Zeile nur anzeigen
    ENDIF.
    IF NOT affwb_tab-refbln IS INITIAL.
*     abhängige Warenbewegungsposition
*     REFBLN muß in laufende Nummer in REFIX umgesetzt werden, da der
*     Baustein CO_WB_MAINTAIN_GOODS_MOVEMENT abhängige Warenbewegungen
*     anhand des Index im Feld REFIX erkennt
      READ TABLE refblnr_tab WITH KEY blnr = affwb_tab-refbln
                                      BINARY SEARCH.
      cowb_comp_tab-refix = sy-tabix.
      IF sy-subrc <> 0.
        refblnr_tab-blnr = affwb_tab-refbln.
        INSERT refblnr_tab INDEX sy-tabix.
      ENDIF.
    ENDIF.
    APPEND cowb_comp_tab.
* Mengensperren für AFFW-Sätze mit Charge setzen
* sonst kann es bei manuellen Chargenfindungen zu falschen
* Ergebnissen kommen ANL
    IF affwb_tab-vbkz       IS INITIAL AND     "noch nicht geändert
       affwb_tab-kzbew      IS INITIAL AND     "Warenausgang
       NOT affwb_tab-charg  IS INITIAL AND     "Charge vorhanden
       cowb_comp_tab-flg_so IS INITIAL.        "änderbar
*   Mengensperre für AFFW-Sätze mit Charge vormerken
      MOVE-CORRESPONDING cowb_comp_tab TO lt_bdbatch.
*   Feld 'menge' mit Erfassungsmenge in Basismengeneinheit füllen -
*   so verlangt es der BF_SET_LOC_ENQ_TAB!
      IF lt_bdbatch-erfme NE lt_bdbatch-meins.
        CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
             EXPORTING
                  input                = lt_bdbatch-erfmg
                  kzmeinh              = yx
                  matnr                = lt_bdbatch-matnr
                  meinh                = lt_bdbatch-erfme
                  meins                = lt_bdbatch-meins
             IMPORTING
                  output               = l_menge
             EXCEPTIONS
                  conversion_not_found = 1
                  input_invalid        = 2
                  material_not_found   = 3
                  meinh_not_found      = 4
                  meins_missing        = 5
                  no_meinh             = 6
                  output_invalid       = 7
                  overflow             = 8
                  OTHERS               = 9.
        IF sy-subrc <> 0.
*... ... TODO: Meldung Error bei der Konvertierung
        ENDIF.
      ELSE.
        l_menge = lt_bdbatch-erfmg.
      ENDIF.
      lt_bdbatch-menge = l_menge.
      APPEND lt_bdbatch.
    ENDIF.
  ENDLOOP.
* lokale Mengensperren für Chargen setzen
  CALL FUNCTION 'BF_SET_LOC_ENQ_TAB'
       EXPORTING
            vbeln     = cowb_comp_tab-kdauf
            posnr     = cowb_comp_tab-kdpos
            pspnr     = cowb_comp_tab-ps_psp_pnr
       TABLES
            l_bdbatch = lt_bdbatch.
  CHECK NOT cowb_comp_tab[] IS INITIAL.
* Kopfdaten bereitstellen
  CLEAR cowb_ctrl.
  IF p_disply IS INITIAL.
    cowb_ctrl-trtyp = trtyp_aendern.
  ELSE.
    cowb_ctrl-trtyp = trtyp_anzeigen.
  ENDIF.
  cowb_ctrl-herkt = herkunft-n.        "Herkunft Nachbearbeitung
  cowb_ctrl-flg_mult_order = yx.       "Mehrauftragsfähig
  cowb_ctrl-flg_no_new_gm = yx.        "Keine Erfassung neue WB
* CUA-Status festlegen
  cowb_ctrl-pf_status = 'PF_NACHBEARBEITUNG'.

* CUA-Titel bereitstellen:
* Fehlerhafte Warenbewegungen
  cowb_ctrl-title = text-015.
* generelles Buchungs- und Belegdatum initialisieren
  CLEAR: imkpf-budat,
         imkpf-bldat.
* Aufruf des Warenbewegungsbildes
  CALL FUNCTION 'CO_WB_MAINTAIN_GOODS_MOVEMENTS'
       TABLES
            t_comp_tab  = cowb_comp_tab
            t_order_tab = gt_order_tab
       CHANGING
            c_control   = cowb_ctrl
       EXCEPTIONS
            OTHERS      = 1.

* für ungeänderte Sätze lokale Chargensperren wieder
* wieder zurücknehmen, sofern nicht gesichert wird
  IF cowb_ctrl-retcode <> fc_weit.
    REFRESH lt_bdbatch. CLEAR lt_bdbatch.
    LOOP AT cowb_comp_tab WHERE wbeakz IS INITIAL AND  "keine Änderung
                                kzbew  IS INITIAL AND  "Warenausgang
                            NOT charg  IS INITIAL AND  "Charge vorhanden
                                flg_so IS INITIAL.     "änderbar
*     Mengensperre für AFFW-Sätze mit Charge vormerken
      MOVE-CORRESPONDING cowb_comp_tab TO lt_bdbatch.
*     Feld 'menge' mit Erfassungsmenge in Basismengeneinheit füllen -
*     so verlangt es der BF_SET_LOC_ENQ_TAB!
      IF lt_bdbatch-erfme NE lt_bdbatch-meins.
        CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
             EXPORTING
                  input                = lt_bdbatch-erfmg
                  kzmeinh              = yx
                  matnr                = lt_bdbatch-matnr
                  meinh                = lt_bdbatch-erfme
                  meins                = lt_bdbatch-meins
             IMPORTING
                  output               = l_menge
             EXCEPTIONS
                  conversion_not_found = 1
                  input_invalid        = 2
                  material_not_found   = 3
                  meinh_not_found      = 4
                  meins_missing        = 5
                  no_meinh             = 6
                  output_invalid       = 7
                  overflow             = 8
                  OTHERS               = 9.
        IF sy-subrc <> 0.
*... ... TODO: Meldung Error bei der Konvertierung
        ENDIF.
      ELSE.
        l_menge = lt_bdbatch-erfmg.
      ENDIF.
      lt_bdbatch-menge = l_menge.
      APPEND lt_bdbatch.
    ENDLOOP.
    CALL FUNCTION 'BF_SET_LOC_ENQ_TAB'
         EXPORTING
              delete_stock = yx
         TABLES
              l_bdbatch    = lt_bdbatch.
  ELSE.
* vor dem Sichern lokale Mengensperren zurücksetzen, da diese
* dann keine Bedeutung mehr haben
    CALL FUNCTION 'BF_RESET_LOC_ENQ_TAB'.
  ENDIF.

* Änderungen übernehmen
* 1. gelöschte Positionen
  LOOP AT cowb_comp_tab WHERE wbeakz EQ geloescht.
    READ TABLE affwb_tab WITH KEY weblnr  = cowb_comp_tab-weblnr
                                  weblpos = cowb_comp_tab-weblpos.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz <> vbkz_del AND
          affwb_tab-vbkz <> vbkz_dll.
    CLEAR affwb_tab-flg_sel.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz = vbkz_del.
    ELSE.
      affwb_tab-vbkz = vbkz_dll.
    ENDIF.
    MODIFY affwb_tab INDEX sy-tabix.
*   abhängige Warenbewegung auch löschen
    IF NOT affw_tab-refbln IS INITIAL.
      l_refblnr = affw_tab-refbln.
      MODIFY affwb_tab TRANSPORTING vbkz
                       WHERE weblnr = l_refblnr.
      MODIFY cowb_comp_tab TRANSPORTING wbeakz
                       WHERE wbeakz NE geloescht
                       AND   weblnr = l_refblnr.
    ENDIF.
  ENDLOOP.
* geänderte Positionen bearbeiten
  LOOP AT cowb_comp_tab WHERE wbeakz NE geloescht.
    IF cowb_comp_tab-wbeakz EQ vbkz_upd. "geänderte WB
      READ TABLE affwb_tab WITH KEY weblnr  = cowb_comp_tab-weblnr
                                    weblpos = cowb_comp_tab-weblpos.
      CHECK sy-subrc = 0.
      tabix = sy-tabix.
    ELSEIF cowb_comp_tab-wbeakz EQ vbkz_ins. "Neue WB
*     neue WEBLPOS für neue Warenbewegungen vergeben
      PERFORM find_new_weblpos USING cowb_comp_tab-weblnr
                                     cowb_comp_tab-weblpos.
      MODIFY cowb_comp_tab.
      CLEAR affwb_tab.
      affwb_tab-mandt = sy-mandt.
      affwb_tab-vbkz = vbkz_ins.
      DESCRIBE TABLE affwb_tab LINES tabix.
      tabix = tabix + 1.
    ENDIF.
    CHECK affwb_tab-vbkz <> vbkz_del.
    MOVE-CORRESPONDING cowb_comp_tab TO affwb_tab.
    affwb_tab-mandt = sy-mandt.
*{   REPLACE        PA8K037066                                        1
*\    IF affwb_tab-kzbew = kzbew_f.
*\    IF affwb_tab-kzbew = kzbew_f.
*   PPC note 754563: new entries (resulted from batch/stock split)
*   must have the same flg_orig as the source line, identified by
*   the same rsnum and rspos.
    IF cowb_comp_tab-wbeakz EQ vbkz_ins.
      CLEAR ls_affw.
      READ TABLE affwb_tab
          INTO ls_affw
          WITH KEY rsnum = affwb_tab-rsnum
                   rspos = affwb_tab-rspos.
      IF sy-subrc EQ 0.
        "it should get the entry with lowest index = the initial one
        MOVE ls_affw-flg_orig TO affwb_tab-flg_orig.
      ENDIF.
    ENDIF.
*   not for production backflush (PPC)
    IF affwb_tab-kzbew EQ kzbew_f AND
      NOT affwb_tab-flg_orig EQ con_new_conf.
*}   REPLACE
      CLEAR: affwb_tab-rsnum,
             affwb_tab-rspos,
             affwb_tab-rsart.
    ENDIF.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz = vbkz_upd.
    ENDIF.
    IF NOT cowb_comp_tab-refix IS INITIAL.
*     abh. Position: urspr. REFBLN wieder einsetzen
      READ TABLE refblnr_tab INDEX cowb_comp_tab-refix.
      refblnr_tab-indx = 1.
      MODIFY refblnr_tab INDEX cowb_comp_tab-refix.
      affwb_tab-refbln = refblnr_tab-blnr.
    ENDIF.
*   MHD-Abwicklung: VFDAT oder HSDAT in MHDAT übernehmen
    IF cowb_comp_tab-shkzg EQ shkzg_soll AND
       ( NOT cowb_comp_tab-vfdat IS INITIAL OR
         NOT cowb_comp_tab-hsdat IS INITIAL ).
      CALL FUNCTION 'CO_FW_CHECK_MHD_NECESSARY'
           EXPORTING
                matnr_imp     = cowb_comp_tab-matnr
                werks_imp     = cowb_comp_tab-werks
                bwart_imp     = cowb_comp_tab-bwart
           EXCEPTIONS
                mhd_necessary = 1
                hsd_necessary = 2.
      IF sy-subrc IS INITIAL.
*       sollte nicht möglich sein
        CLEAR affwb_tab-mhdat.
      ELSEIF sy-subrc = 1.
*       Verfallsdatum
        affwb_tab-mhdat = cowb_comp_tab-vfdat.
      ELSEIF sy-subrc = 2.
*       Herstelldatum
        affwb_tab-mhdat = cowb_comp_tab-hsdat.
      ENDIF.
    ENDIF.
    IF cowb_comp_tab-wbeakz EQ vbkz_upd.
      MODIFY affwb_tab INDEX tabix.
    ELSEIF cowb_comp_tab-wbeakz EQ vbkz_ins.
      MOVE-CORRESPONDING affwb_tab TO tmp_key_affwb.
      APPEND tmp_key_affwb.
      IF affwb_tab-autyp = auftyp-corp AND
         NOT affwb_tab-rsnum IS INITIAL AND
         affwb_tab-rspos IS INITIAL.
*       unvollständigen Satz der Serienfertigung vervollständigen
        REFRESH affw_tab.
        MOVE-CORRESPONDING affwb_tab TO affw_tab.
        APPEND affw_tab.
        CALL FUNCTION 'RM_FILL_RSPOS_IN_AFFW'
             TABLES
                  t_affw                  = affw_tab
             EXCEPTIONS
                  incorrect_rsnum_in_affw = 1
                  OTHERS                  = 2.
        IF sy-subrc = 0.
          READ TABLE affw_tab INDEX 1.
*         Position der Reservierung übernehmen
          affwb_tab-rspos = affw_tab-rspos.
        ENDIF.
      ENDIF.
      INSERT affwb_tab INDEX tabix.
    ENDIF.
  ENDLOOP.
* abhängige Position komplett mit Verbuchungskennzeichen versehen
  DELETE refblnr_tab WHERE indx = 0.
  affwb_tab-vbkz = vbkz_upd.
  LOOP AT refblnr_tab.
    MODIFY affwb_tab TRANSPORTING vbkz
                     WHERE refbln = refblnr_tab-blnr
                     AND   vbkz IS INITIAL.
  ENDLOOP.
*
  DESCRIBE TABLE tmp_key_affwb LINES tmp_counter.
  IF NOT tmp_counter IS INITIAL.
* Einfügen von neuen Einträgen (ungeplante Entnahmen)
* in TABIX_TAB - als erste Einträge
    READ TABLE tabix_tab INDEX 1.
    tmp_index_kum = tabix_tab-ind_kum.
    LOOP AT tmp_key_affwb.
      LOOP AT affwb_tab WHERE mandt   = tmp_key_affwb-mandt
                         AND  weblnr  = tmp_key_affwb-weblnr
                         AND  weblpos = tmp_key_affwb-weblpos.
        tabix_tab-ind_kum = tmp_index_kum.
        tabix_tab-ind     = sy-tabix.
        INSERT tabix_tab INDEX 1.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
* Neuaufbau der kumulierten Sicht
  flg_refresh = yx.
* Nach Sichern im FBS Verbuchungsroutine aufrufen
  IF cowb_ctrl-retcode = fc_weit.
    PERFORM post_changes.
    flg_start = yx.
  ENDIF.

  IF r_single IS INITIAL.
*   Liste der fehlerhaften Warenbewegungen ausgeben, kumuliert
    PERFORM modif_screen_blocked.
  ELSE.
*   Liste der fehlerhaften Warenbewegungen ausgeben, Einzelsätze
    PERFORM modif_screen_single.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM CHANGE_MODE                                               *
*----------------------------------------------------------------------*
*       Anzeigemodus wechseln (Einzelsätze <--> verdichtete Anzeige)   *
*----------------------------------------------------------------------*
FORM change_mode.

  IF r_single IS INITIAL.
*   Liste der fehlerhaften Warenbewegungen ausgeben, Einzelsätze
    r_single = yx.
    r_cumul  = space.
    flg_start = yx.                                         "P45K046421
    PERFORM modif_screen_single.
  ELSE.
*   Liste der fehlerhaften Warenbewegungen ausgeben, kumuliert
    CLEAR r_single.
    r_cumul = yx.
    flg_start = yx.
    PERFORM modif_screen_blocked.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM MODIF_SCREEN_BLOCKED                                      *
*----------------------------------------------------------------------*
*       Anzeige der Einzelsätze aktualisieren                          *
*----------------------------------------------------------------------*
FORM modif_screen_blocked.

  DATA: sav_lsind LIKE sy-lsind,       " Nummer der Verzeigungsliste
        sav_cpage LIKE sy-cpage,       " Aktuelle Seitennummer
        sav_staro LIKE sy-staro.       " Seite angezeigt ab Zeile
*     sav_staco like sy-staco,    " Liste angezeigt ab Spalte
*     sav_curow like sy-curow,    " Cursorposition (Zeile)
*     sav_cucol like sy-cucol.    " Cursorposition (Spalte)

* alte Listenposition merken
  sav_lsind = sy-lsind.
  sav_cpage = sy-cpage.
  sav_staro = sy-staro.

* Ausgabetabelle für verdichtete Anzeige aufbauen und ausgeben
  PERFORM list_affwb_blocked_entries.
* Listendindex zurücksetzen
  sy-lsind = sy-lsind - 1.
* Aufsetzen auf die Seite des Absprungs
* nicht nach Start bzw. Sichern oder bei externer Sortierung
  IF flg_start IS INITIAL AND
     s_pname IS INITIAL.
    SCROLL LIST INDEX sav_lsind TO PAGE sav_cpage LINE sav_staro.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM MODIF_SCREEN_SINGLE                                       *
*----------------------------------------------------------------------*
*       Anzeige der Einzelsätze aktualisieren                          *
*----------------------------------------------------------------------*
FORM modif_screen_single.

  DATA: sav_lsind LIKE sy-lsind,       " Nummer der Verzeigungsliste
        sav_cpage LIKE sy-cpage,       " Aktuelle Seitennummer
        sav_staro LIKE sy-staro.       " Seite angezeigt ab Zeile
*     sav_staco like sy-staco,    " Liste angezeigt ab Spalte
*     sav_curow like sy-curow,    " Cursorposition (Zeile)
*     sav_cucol like sy-cucol.    " Cursorposition (Spalte)

* alte Listenposition merken
  sav_lsind = sy-lsind.
  sav_cpage = sy-cpage.
  sav_staro = sy-staro.

* Liste der Einzelsätze ausgeben
  PERFORM list_affwb_entries.
* Listendindex zurücksetzen
  sy-lsind = sy-lsind - 1.
* Aufsetzen auf die Seite des Absprungs
  SCROLL LIST INDEX sav_lsind TO PAGE sav_cpage LINE sav_staro.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM CALL_UPDATE_TASK                                          *
*----------------------------------------------------------------------*
*       Aufruf der Verbuchungsroutinen                                 *
*----------------------------------------------------------------------*
FORM call_update_task.

  LOOP AT affwb_tab WHERE weblnr IS INITIAL
                    AND   vbkz = vbkz_ins.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
*   nächste Belegnummer besorgen
    CALL FUNCTION 'NUMBER_GET_NEXT'
         EXPORTING
              nr_range_nr = nk_range
              object      = nk_object
         IMPORTING
              number      = weblnr_new
              returncode  = nk_numbrc
         EXCEPTIONS
              OTHERS      = 01.
    IF sy-subrc <> 0.
      MESSAGE a166 WITH sy-mandt.
    ENDIF.
    CASE nk_numbrc.
      WHEN '1'.
        MESSAGE w158.
      WHEN '2'.
        MESSAGE w159.
    ENDCASE.
    tabix = 0.
    LOOP AT affwb_tab WHERE weblnr IS INITIAL
                      AND   vbkz = vbkz_ins.
      tabix = tabix + 1.
      affwb_tab-weblnr  = weblnr_new.
      affwb_tab-weblpos = tabix.
      MODIFY affwb_tab.
    ENDLOOP.
  ENDIF.
  CALL FUNCTION 'CO_FW_AFFW_POST' IN UPDATE TASK
       EXPORTING
            i_called = 'A'
       TABLES
            affw_bt  = affwb_tab.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM CHANGE_ENTRIES                                            *
*----------------------------------------------------------------------*
*       Änderungsmodus für markierte Einträge aufrufen                 *
*----------------------------------------------------------------------*
FORM change_entries.

  DATA: tmp_found LIKE rc27x-flg_sel.

  CLEAR tmp_found.
  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      tmp_found = yx.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
      tmp_found = yx.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK affwb_tab-vbkz <> vbkz_bu AND
            affwb_tab-vbkz <> vbkz_shw.
      CHECK sy-subrc = 0.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
  ENDDO.
  IF tmp_found IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      tmp_found = yx.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
          MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yx.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
          MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yx.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  CHECK NOT tabix_tab[] IS INITIAL.
*
  REFRESH enq_loc.
  REFRESH refblnr_tab.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    refblnr_tab-indx = sy-tabix.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nur gemeinsam verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      DELETE tabix_tab.
      refblnr_tab-blnr = affwb_tab-refbln.
      APPEND refblnr_tab.
    ENDIF.
  ENDLOOP.
* AFFW-Sätze sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  IF NOT refblnr_tab[] IS INITIAL.
*   Indizes der abh. Positionen nach Sperren in TABIX_TAB aufnehmen
    PERFORM add_refered_pos.
  ENDIF.
*
  IF NOT tabix_tab[] IS INITIAL.
    PERFORM call_detail_new.
    REFRESH tabix_tab.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM DEL_ENTRY                                                 *
*----------------------------------------------------------------------*
*       markierte Einträge löschen                                     *
*----------------------------------------------------------------------*
FORM del_entry.

  DATA: tmp_tline(40).

  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz <> vbkz_del AND
            affwb_tab-vbkz <> vbkz_dll.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
          MODIFY LINE cntr FIELD VALUE affwb-flg_sel FROM yx.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
          MODIFY LINE cntr FIELD VALUE affwb-flg_sel FROM yx.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*
  cntr = 0.
  REFRESH enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nicht verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      cntr = cntr + 1.
      DELETE tabix_tab.
    ENDIF.
  ENDLOOP.
  IF cntr > 0.
    MESSAGE i507 WITH cntr.
    CHECK NOT tabix_tab[] IS INITIAL.
  ENDIF.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  cntr = 0.
  REFRESH matnr_tab.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz <> vbkz_bu  AND
          affwb_tab-vbkz <> vbkz_shw AND
          affwb_tab-vbkz <> vbkz_del AND
          affwb_tab-vbkz <> vbkz_dll.
    READ TABLE matnr_tab WITH KEY affwb_tab-matnr BINARY SEARCH.
    IF sy-subrc <> 0.
*     Sicherheitsabfrage je Material
      IF text-005 CA ' '.
      ENDIF.
      feldpos = sy-fdpos.
      WRITE text-005 TO tmp_tline(feldpos).
      feldpos = feldpos + 1.
      WRITE affwb_tab-matnr TO tmp_tline+feldpos.
*     Popup mit Sicherheitsabfrage beim Stornieren
      CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
           EXPORTING
                diagnosetext1 = text-004
                diagnosetext2 = tmp_tline
                textline1     = text-006
                titel         = text-002
           IMPORTING
                answer        = antwort.
      READ TABLE matnr_tab WITH KEY affwb_tab-matnr BINARY SEARCH.
      matnr_tab-matnr = affwb_tab-matnr.
      matnr_tab-antwo = antwort.
      INSERT matnr_tab INDEX sy-tabix.
    ENDIF.
    IF matnr_tab-antwo = abbruch.
      EXIT.
    ENDIF.
    CHECK matnr_tab-antwo = yja.
    IF affwb_tab-vbkz <> vbkz_ins.
      affwb_tab-vbkz = vbkz_del.
    ELSE.
      affwb_tab-vbkz = vbkz_dll.
      CLEAR affwb_tab-flg_sel.
    ENDIF.
    MODIFY affwb_tab INDEX tabix_tab-ind.
  ENDLOOP.
* Neuaufbau der kumulierten Sicht
  flg_refresh = yx.
  IF r_single IS INITIAL.
*   verdichtete Anzeige
    PERFORM modif_screen_blocked.
  ELSE.
*   Einzelsätze
    PERFORM modif_screen_single.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM DEL_MARK_ALL                                              *
*----------------------------------------------------------------------*
*       Alle Einträge entmarkieren                                     *
*----------------------------------------------------------------------*
FORM del_mark_all.

  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        CLEAR affwb_out_tab-flg_sel.
        MODIFY affwb_out_tab.
      ENDLOOP.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
        CLEAR affwb_tab-flg_sel.
        MODIFY affwb_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CLEAR affwb_tab-flg_sel.
      MODIFY affwb_tab INDEX sy-tabix.
    ENDIF.
    MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yblank.
  ENDDO.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM MARK_ALL                                                  *
*----------------------------------------------------------------------*
*       Alle Einträge markieren                                        *
*----------------------------------------------------------------------*
FORM mark_all.

  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      line3 = sy-lisel.
      CHECK line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        CHECK affwb_out_tab-vbkz <> vbkz_bu AND
              affwb_out_tab-vbkz <> vbkz_shw.
        affwb_out_tab-flg_sel = yx.
        MODIFY affwb_out_tab.
        EXIT.
      ENDLOOP.
      CHECK affwb_out_tab-vbkz <> vbkz_bu AND
            affwb_out_tab-vbkz <> vbkz_shw.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
        affwb_tab-flg_sel = yx.
        MODIFY affwb_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK line1-flg_sel IS INITIAL.
*     Key der Belegposition merken
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz <> vbkz_bu  AND
            affwb_tab-vbkz <> vbkz_shw AND
            affwb_tab-vbkz <> vbkz_del AND
            affwb_tab-vbkz <> vbkz_dll.
      affwb_tab-flg_sel = yx.
      MODIFY affwb_tab INDEX sy-tabix.
    ENDIF.
    MODIFY CURRENT LINE FIELD VALUE affwb-flg_sel FROM yx.
  ENDDO.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*     FORM LOCATE_ENTRY                                                *
*----------------------------------------------------------------------*
*       Eintrag markieren                                              *
*----------------------------------------------------------------------*
FORM locate_entry USING flg_all LIKE rc27x-flg_sel.

  tabix = 0.
  GET CURSOR LINE cntr.
  READ LINE cntr.
  IF sellin = 2.
    cntr = cntr - 1.
    READ LINE cntr.
  ENDIF.
  CHECK sellin = 1.
  IF r_single IS INITIAL.
*   Blockanzeige
    line3 = sy-lisel.
    LOOP AT affwb_out_tab WHERE werks = affwb-werks
                          AND   lgort = affwb-lgort
                          AND   matnr = affwb-matnr
                          AND   charg = affwb-charg
                          AND   bwart = affwb-bwart
                          AND   erfme = affwb-erfme
                          AND   msgid = affwb-msgid
                          AND   msgno = affwb-msgno.
      tabix = sy-tabix.
      EXIT.
    ENDLOOP.
  ELSE.
*   Einzelsatzanzeige
    line1 = sy-lisel.
*   Key der Belegposition merken
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = affwb-weblnr.
    affw_key-weblpos = affwb-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    IF flg_all IS INITIAL.
      CHECK affwb_tab-vbkz <> vbkz_del AND
            affwb_tab-vbkz <> vbkz_dll.
    ENDIF.
    tabix = sy-tabix.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM POST_BACKGROUND                                           *
*----------------------------------------------------------------------*
*       markierte Einträge für Hintergrundjob sichern                  *
*----------------------------------------------------------------------*
FORM post_background.

  DATA: tmp_count LIKE sy-tabix.

  REFRESH tabix_tab.
  REFRESH refblnr_tab.
  CLEAR flg_corp.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        IF affwb_tab-autyp = auftyp-corp.
          flg_corp = yx.               "Funktion für Serienf. unmöglich
        ENDIF.
        CHECK affwb_tab-autyp <> auftyp-corp.
        IF affwb_tab-refbln IS INITIAL.
          tabix_tab-ind = sy-tabix.
          APPEND tabix_tab.
        ELSE.
*         Nummer des Referenzbelegs sammeln
          refblnr_tab-blnr = affwb_tab-refbln.
          APPEND refblnr_tab.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     Key der Belegposition merken
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      IF affwb_tab-autyp = auftyp-corp.
        flg_corp = yx.                 "Funktion für Serienf. unmöglich
      ENDIF.
      CHECK affwb_tab-autyp <> auftyp-corp. "Nicht Serienfertigung
      IF affwb_tab-refbln IS INITIAL.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ELSE.
*       Nummer des Referenzbelegs sammeln
        refblnr_tab-blnr = affwb_tab-refbln.
        APPEND refblnr_tab.
      ENDIF.
    ENDIF.
  ENDDO.
*
  IF NOT flg_corp IS INITIAL.
    MESSAGE i508.
  ENDIF.
  LOOP AT refblnr_tab.
*   abhängige Positionen sind nur gemeinsam bearbeitbar
    LOOP AT affwb_tab WHERE weblnr = refblnr_tab-blnr.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDLOOP.
  ENDLOOP.
  REFRESH refblnr_tab.
*
  REFRESH enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
    enq_loc-weblnr  = affwb_tab-weblnr.
    enq_loc-weblpos = affwb_tab-weblpos.
    APPEND enq_loc.
  ENDLOOP.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    IF affwb_tab-vbkz IS INITIAL.
      affwb_tab-vbkz = vbkz_upd.
    ENDIF.
    affwb_tab-aenam = sy-uname.
    affwb_tab-laeda = sy-datum.
    CLEAR: affwb_tab-fwdat,
           affwb_tab-msgid,
           affwb_tab-msgno,
           affwb_tab-msgty,
           affwb_tab-msgv1,
           affwb_tab-msgv2,
           affwb_tab-msgv3,
           affwb_tab-msgv4.
    MODIFY affwb_tab INDEX tabix_tab-ind.
  ENDLOOP.
*
  FREE imseg_tab.
  LOOP AT affwb_tab.
    IF affwb_tab-vbkz = vbkz_dll.
      DELETE affwb_tab.
    ENDIF.
  ENDLOOP.
  IF NOT affwb_tab[] IS INITIAL.
    PERFORM call_update_task ON COMMIT.
    COMMIT WORK.
    tmp_count = 0.
    LOOP AT affwb_tab.
      CHECK affwb_tab-vbkz = vbkz_del OR
            affwb_tab-vbkz = vbkz_ins OR
            affwb_tab-vbkz = vbkz_upd.
      IF affwb_tab-vbkz = vbkz_ins OR
         affwb_tab-vbkz = vbkz_upd.
        tmp_count = tmp_count + 1.
      ENDIF.
      DELETE affwb_tab.
    ENDLOOP.
*   Nachricht über Anzahl der Positionen für Hintergrundjob
    MESSAGE s118 WITH tmp_count.
    IF r_single IS INITIAL.
      flg_start = yx.
      PERFORM modif_screen_blocked.
    ELSE.
      PERFORM modif_screen_single.
    ENDIF.
  ELSE.
*   keine Zeile markiert, geändert oder gelöscht: Sichern nicht notw.
    MESSAGE i099.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM BUILD_MAT_DOCUMENT                                        *
*----------------------------------------------------------------------*
*       Materialbeleg zusammenstellen und sichern                      *
*----------------------------------------------------------------------*
FORM build_mat_document.

* SS-Struktur für WM-Chargenfindung
  DATA: BEGIN OF tmp_lresb.
          INCLUDE STRUCTURE lresb.
  DATA: END   OF tmp_lresb.
* SS-Tabelle für WM-Chargenfindung
  DATA: BEGIN OF bdbatch_tab OCCURS 0.
          INCLUDE STRUCTURE bdbatch.
  DATA: END   OF bdbatch_tab.
  DATA: ls_marc LIKE marc.
  DATA: tmp_insmk LIKE qals-insmk,     "Kennz. Prüfbestand aus Matstamm
        tmp_insnc LIKE qals-insmk,
        tmp_menge LIKE resb-bdmng.     "Mengenfeld
*{   INSERT         PA8K037531                                        3
* IS-A-PPC local data declaration
  DATA:
    lf_lerr TYPE xflag,
    ls_akey TYPE ppc_rskey,
    ls_rkey TYPE ppc_rskey,
    lt_mdrr TYPE TABLE OF mdrr.
  FIELD-SYMBOLS:
    <mdrrx> TYPE mdrr.
*}   INSERT

  subrc = 4.
* nur Reservierungen sperren, bei denen nicht storniert wird
* wegen Buchung über MB_CANCEL_GOODS_MOVEMENT
  IF NOT affwb_tab-rsnum IS INITIAL AND
     NOT affwb_tab-rspos IS INITIAL AND
         affwb_tab-smbln IS INITIAL.
*   Reservierungen sind zu sperren
    READ TABLE enq_rsnum WITH KEY affwb_tab-rsnum BINARY SEARCH.
    tabix_enq = sy-tabix.
    IF sy-subrc = 0.
      enq_rsnum-count = enq_rsnum-count + 1.
      MODIFY enq_rsnum INDEX tabix_enq.
    ELSE.
*     Reservierungskopf shared sperren
      CALL FUNCTION 'ENQUEUE_EMRKPF'
           EXPORTING
                mode_rkpf      = 'S'
                rsnum          = affwb_tab-rsnum
                _scope         = '1'
           EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2.
      IF sy-subrc <> 0.
*       Reservierung bereits durch anderen Benutzer gesperrt
        n_locked = n_locked + 1.
        affwb_tab-msgid = 'M7'.
        affwb_tab-msgty = 'E'.
        affwb_tab-msgno = '112'.
        affwb_tab-msgv1 = sy-msgv1.
        affwb_tab-msgv2 = affwb_tab-rsnum.
        CLEAR: affwb_tab-msgv3,
               affwb_tab-msgv4.
        MODIFY affwb_tab INDEX tabix_do.
*       Backupdaten erfrischen
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = affwb_tab-weblnr.
        affw_key-weblpos = affwb_tab-weblpos.
        READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING affwb_tab TO affw_old.
          MODIFY affw_old INDEX sy-tabix.
        ENDIF.
        EXIT.
      ENDIF.
      enq_rsnum-rsnum = affwb_tab-rsnum.
      enq_rsnum-count = 1.
      INSERT enq_rsnum INDEX tabix_enq.
    ENDIF.
*
    READ TABLE enq_resb WITH KEY rsnum = affwb_tab-rsnum
                                 rspos = affwb_tab-rspos
                                 rsart = affwb_tab-rsart
                                 BINARY SEARCH.
    tabix_enq = sy-tabix.
    IF sy-subrc = 0.
      enq_resb-count = enq_resb-count + 1.
      MODIFY enq_resb INDEX tabix_enq.
    ELSE.
*     Reservierung exklusiv sperren
      CALL FUNCTION 'ENQUEUE_EMRESB'
           EXPORTING
                rsnum          = affwb_tab-rsnum
                rspos          = affwb_tab-rspos
                rsart          = affwb_tab-rsart
           EXCEPTIONS
                foreign_lock   = 1
                system_failure = 2.
      IF sy-subrc <> 0.
*       Reservierung bereits durch anderen Benutzer gesperrt.
        n_locked = n_locked + 1.
        affwb_tab-msgid = 'M7'.
        affwb_tab-msgty = 'E'.
        affwb_tab-msgno = '112'.
        affwb_tab-msgv1 = sy-msgv1.
        affwb_tab-msgv2 = affwb_tab-rsnum.
        CLEAR: affwb_tab-msgv3,
               affwb_tab-msgv4.
        MODIFY affwb_tab INDEX tabix_do.
*         Backupdaten erfrischen
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = affwb_tab-weblnr.
        affw_key-weblpos = affwb_tab-weblpos.
        READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING affwb_tab TO affw_old.
          MODIFY affw_old INDEX sy-tabix.
        ENDIF.
        EXIT.
      ENDIF.
      enq_resb-rsnum = affwb_tab-rsnum.
      enq_resb-rspos = affwb_tab-rspos.
      enq_resb-rsart = affwb_tab-rsart.
      enq_resb-count = 1.
      INSERT enq_resb INDEX tabix_enq.
    ENDIF.
*{   INSERT         PA8K037531                                        2
*   IS-A-PPC note 791901:
*   One RESB position is linked to more AFFW entries. Thus, when
*   an AFFW record changes in "important" fields, like LGORT or SOBKZ,
*   it must point to another RESB position (existing or new).
*   The redirection will be done in update task. Thus, we may have
*   to lock more than one RESB record, if the logical key is changed
*   For performace, we keep track of the locked "logical keys" as well
    IF affwb_tab-flg_orig EQ con_new_conf.
      CLEAR:  lf_lerr, ls_akey, ls_rkey.
      REFRESH lt_mdrr.
      MOVE-CORRESPONDING affwb_tab TO ls_akey.
      IF ( affwb_tab-vbkz NE vbkz_ins AND
           affwb_tab-vbkz NE vbkz_upd ).
*       Item with consistent link between AFFW and RESB - add the
*       current logical key (locked above) to the global table
        COLLECT ls_akey INTO gt_locked.
      ELSE.
        MOVE sy-mandt TO resb_key-mandt.
        MOVE affwb_tab-rsnum TO resb_key-rsnum.
        MOVE affwb_tab-rspos TO resb_key-rspos.
        MOVE affwb_tab-rsart TO resb_key-rsart.
        READ TABLE resb_tab_ges WITH KEY resb_key BINARY SEARCH.
        MOVE-CORRESPONDING resb_tab_ges TO ls_rkey.
        IF ls_akey EQ ls_rkey.
*         Again, this entry is ok. Its lock is good enough
          COLLECT ls_akey INTO gt_locked.
        ELSE.
*         Position with critical changes - check if the
*         logical key is already locked
          READ TABLE gt_locked WITH TABLE KEY matnr = affwb_tab-matnr
                                              werks = affwb_tab-werks
                                              lgort = affwb_tab-lgort
                                              charg = affwb_tab-charg
                                              prvbe = affwb_tab-prvbe
                                              bwart = affwb_tab-bwart
                                              shkzg = affwb_tab-shkzg
                                              sobkz = affwb_tab-sobkz
                                              kdauf = affwb_tab-kdauf
                                              kdpos = affwb_tab-kdpos
                               TRANSPORTING NO FIELDS.
          IF sy-subrc GT 0.
*           This is it. We must lock all related RESB records
            SELECT rsnum rspos rsart FROM resb
                INTO CORRESPONDING FIELDS OF TABLE lt_mdrr
                WHERE rsnum = affwb_tab-rsnum AND
                      matnr = affwb_tab-matnr AND
                      werks = affwb_tab-werks AND
                      lgort = affwb_tab-lgort AND
                      charg = affwb_tab-charg AND
                      prvbe = affwb_tab-prvbe AND
                      bwart = affwb_tab-bwart AND
                      shkzg = affwb_tab-shkzg AND
                      sobkz = affwb_tab-sobkz AND
                      kdauf = affwb_tab-kdauf AND
                      kdpos = affwb_tab-kdpos.
            LOOP AT lt_mdrr ASSIGNING <mdrrx>.
              READ TABLE enq_resb WITH KEY rsnum = <mdrrx>-rsnum
                                           rspos = <mdrrx>-rspos
                                           rsart = <mdrrx>-rsart
                                           BINARY SEARCH.
              tabix_enq = sy-tabix.
              IF sy-subrc = 0.
                enq_resb-count = enq_resb-count + 1.
                MODIFY enq_resb INDEX tabix_enq.
              ELSE.
*               Reservierung exklusiv sperren
                CALL FUNCTION 'ENQUEUE_EMRESB'
                     EXPORTING
                          rsnum          = <mdrrx>-rsnum
                          rspos          = <mdrrx>-rspos
                          rsart          = <mdrrx>-rsart
                     EXCEPTIONS
                          foreign_lock   = 1
                          system_failure = 2.
                IF sy-subrc <> 0.
*                 Reservierung bereits durch anderen Benutzer gesperrt.
                  n_locked = n_locked + 1.
                  affwb_tab-msgid = 'M7'.
                  affwb_tab-msgty = 'E'.
                  affwb_tab-msgno = '112'.
                  affwb_tab-msgv1 = sy-msgv1.
                  affwb_tab-msgv2 = affwb_tab-rsnum.
                  CLEAR: affwb_tab-msgv3,
                         affwb_tab-msgv4.
                  MODIFY affwb_tab INDEX tabix_do.
*                 Backupdaten erfrischen
                  affw_key-mandt   = sy-mandt.
                  affw_key-weblnr  = affwb_tab-weblnr.
                  affw_key-weblpos = affwb_tab-weblpos.
                  READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
                  IF sy-subrc = 0.
                    MOVE-CORRESPONDING affwb_tab TO affw_old.
                    MODIFY affw_old INDEX sy-tabix.
                  ENDIF.
                  "set error flag, because EXIT leaves the LOOP only
                  MOVE yx TO lf_lerr.
                  EXIT.
                ENDIF.
                enq_resb-rsnum = affwb_tab-rsnum.
                enq_resb-rspos = affwb_tab-rspos.
                enq_resb-rsart = affwb_tab-rsart.
                enq_resb-count = 1.
                INSERT enq_resb INDEX tabix_enq.
              ENDIF.
            ENDLOOP.
            IF NOT lf_lerr IS INITIAL.
              "we had at least one record locked
              EXIT.
            ENDIF.
            "This logical key has been locked succesfully
            COLLECT ls_akey INTO gt_locked.
          ENDIF.  "If key must be locked
        ENDIF.  "If logical key changed
      ENDIF.  "If update indicator matches
    ENDIF.  "If it's a PPC entry
*}   INSERT
  ENDIF.
  IF affwb_tab-budat <> affwb-budat        OR "Wechsel Buchungsdatum
     affwb_tab-bldat <> affwb-bldat        OR "Wechsel Belegdatum
     affwb_tab-werks <> affwb-werks        OR "Wechsel Werk
     affwb_tab-sjahr <> affwb-sjahr        OR "Wechsel Belegjahr
     ( xallp IS INITIAL  AND           "unabhängige Positionen
       tabix_para > 100 )                  OR  "Beleggröße > 100
     ( NOT xallp IS INITIAL  AND       "abhängige Positionen
       affwb_tab-refbln  <> affwb-refbln ) OR
     ( affwb_tab-rueck IS INITIAL AND  "mit/ohne Rückmeldung
       NOT affwb-rueck IS INITIAL )        OR               "
     ( NOT affwb_tab-rueck IS INITIAL AND     "mit/ohne Rückmeldung
       affwb-rueck IS INITIAL )            OR
     ( affwb_tab-autyp =  auftyp-corp AND     "Serienfertigung ja/nein
       affwb-autyp <> auftyp-corp )        OR               "
     ( affwb_tab-autyp <> auftyp-corp AND     "Serienfertigung ja/nein
       affwb-autyp = auftyp-corp )         OR
*{   INSERT         PA8K037066                                        1
     ( AFFWB_TAB-FLG_ORIG = '1' AND         "APO-Produktionsrückmeldung
       AFFWB-FLG_ORIG <> '1' ) OR           "Ja /Nein
     ( AFFWB_TAB-FLG_ORIG <> '1' AND        "APO-Produktionsrückmeldung
       AFFWB-FLG_ORIG = '1' ) OR            "Ja /Nein
*}   INSERT
     ( affwb_tab-autyp =  auftyp-corp AND     "Serienfertigung
       ( affwb_tab-aufnr <> affwb-aufnr OR    "Wechsel Kostensammler
         affwb_tab-kdauf <> affwb-kdauf OR    "Wechsel Kundenauftrag
         affwb_tab-kdpos <> affwb-kdpos OR    "Wechsel Position KDAUF
         affwb_tab-ps_psp_pnr <> affwb-ps_psp_pnr ) ). "Wechsel Projekt
*   Warenbeleg zum Buchungs-/Belegdatum buchen
    PERFORM post_goods_movement.
    FREE: affwb_task,
          bfwrt_tab,
          imseg_tab,
          mseg_key,
          resb_tab.
    tabix_para = 0.
*   AFFWB-Tabellensatz erneut einlesen
    READ TABLE affwb_tab INDEX tabix_do.
    affwb = affwb_tab.
    imkpf-bldat = affwb_tab-bldat.
    imkpf-budat = affwb_tab-budat.
  ENDIF.
  CLEAR: imseg_tab.
* Daten des Fehlerbelegs übernehmen
  MOVE-CORRESPONDING affwb_tab TO imseg_tab.
  imseg_tab-mat_kdauf = affwb_tab-kdauf.
  imseg_tab-mat_kdpos = affwb_tab-kdpos.
  imseg_tab-mat_pspnr = affwb_tab-ps_psp_pnr.
* bei der Verbuchung der AFFW-Sätze wird MHDAT
* nach IMSEG-VFDAT oder IMSEG-HSDAT geschrieben
  IF affwb_tab-shkzg EQ shkzg_soll.
    CALL FUNCTION 'CO_FW_CHECK_MHD_NECESSARY'
         EXPORTING
              matnr_imp     = affwb_tab-matnr
              werks_imp     = affwb_tab-werks
              bwart_imp     = affwb_tab-bwart
         EXCEPTIONS
              mhd_necessary = 1
              hsd_necessary = 2.
    IF sy-subrc = mhd_necessary.
      imseg_tab-vfdat = affwb_tab-mhdat.
      CLEAR imseg_tab-mhdat.
    ELSEIF sy-subrc = hsd_necessary.
      imseg_tab-hsdat = affwb_tab-mhdat.
      CLEAR imseg_tab-mhdat.
    ENDIF.
  ENDIF.
* Bedarfstermin und Bedarfsmenge bereitstellen
  IF NOT affwb_tab-rsnum IS INITIAL AND
     NOT affwb_tab-rspos IS INITIAL.
    resb_key-mandt = sy-mandt.
    resb_key-rsnum = affwb_tab-rsnum.
    resb_key-rspos = affwb_tab-rspos.
    resb_key-rsart = affwb_tab-rsart.
    READ TABLE resb_tab_ges WITH KEY resb_key BINARY SEARCH.
    IF sy-subrc = 0.
      IF resb_tab_ges-splkz = '1'.
*       Entnahme zum Summensplittsatz nicht erlaubt
        MESSAGE i496 WITH affwb_tab-rueck
                          affwb_tab-rmzhl
                          affwb_tab-aufnr.
        affwb_tab-msgid = 'RU'.
        affwb_tab-msgty = 'E'.
        affwb_tab-msgno = '496'.
        affwb_tab-msgv1 = affwb_tab-rueck.
        affwb_tab-msgv2 = affwb_tab-rmzhl.
        affwb_tab-msgv3 = affwb_tab-aufnr.
        CLEAR: affwb_tab-msgv4.
        MODIFY affwb_tab INDEX tabix_do.
*       Backupdaten erfrischen
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = affwb_tab-weblnr.
        affw_key-weblpos = affwb_tab-weblpos.
        READ TABLE affw_old WITH KEY affw_key
                            BINARY SEARCH.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING affwb_tab TO affw_old.
          MODIFY affw_old INDEX sy-tabix.
        ENDIF.
        EXIT.
      ENDIF.

      imseg_tab-rshkz = resb_tab_ges-shkzg.
      imseg_tab-bdter = resb_tab_ges-bdter.
      imseg_tab-bdmng = resb_tab_ges-bdmng.
      imseg_tab-enmng = resb_tab_ges-enmng.
      imseg_tab-kzbws = resb_tab_ges-kzbws. " Note 626859
      CLEAR imseg_tab-konto.
      IF NOT resb_tab_ges-ktoma IS INITIAL.
        imseg_tab-konto = resb_tab_ges-saknr.
      ENDIF.
      imseg_tab-ps_psp_pnr = resb_tab_ges-pspel.
      IF imseg_tab-autyp <> auftyp-corp.
        imseg_tab-prvbe = resb_tab_ges-prvbe.
        imseg_tab-berkz = resb_tab_ges-berkz.
        imseg_tab-lgnum = resb_tab_ges-lgnum.
        imseg_tab-lgtyp = resb_tab_ges-lgtyp.
        imseg_tab-lgpla = resb_tab_ges-lgpla.
      ENDIF.
      CLEAR imseg_tab-bwlvs.
*     SS-Reservierungstabelle füllen
      MOVE-CORRESPONDING resb_tab_ges TO resb_tab.
      APPEND resb_tab.
    ENDIF.
  ENDIF.
  IF NOT affwb_tab-rueck IS INITIAL AND
     NOT imseg_tab-erfmg IS INITIAL.
*   Goods Movement to confirmation -> KZEAR and ELIKZ will be set
*   in case after posting goods movements in update task.
*   Allow KZEAR/ELIKZ in case of quantity = zero.
    CLEAR: imseg_tab-kzear,
           imseg_tab-elikz.
  ENDIF.
  IF NOT imseg_tab-kzbew IS INITIAL.
*   Buchen in Inspektionsbestand ?
    CALL FUNCTION 'QPWE_PROD_ORDER_CHECK_INSMK'
         EXPORTING
              i_matnr = affwb_tab-matnr
              i_werks = affwb_tab-werks
         IMPORTING
              e_insmk = tmp_insmk
              e_insnc = tmp_insnc
         EXCEPTIONS
              no_tq32 = 01.
    IF sy-subrc = 0 AND NOT tmp_insnc IS INITIAL.
      imseg_tab-insmk = tmp_insmk.
    ENDIF.
  ENDIF.
* Fehlerbeleg merken
  imseg_tab-lfbnr = affwb_tab-weblnr.
  imseg_tab-lfpos = affwb_tab-weblpos.
* Sonderlogik für Netzpläne
  IF affwb_tab-autyp = auftyp-netw.
    imseg_tab-nplnr = imseg_tab-aufnr.
    IF aufk-aufnr = imseg_tab-aufnr.
      imseg_tab-objnr = aufk-objnr.
    ELSE.
      SELECT SINGLE aufnr objnr FROM aufk
                                INTO CORRESPONDING FIELDS OF aufk
                                WHERE aufnr = imseg_tab-aufnr.
      IF sy-subrc = 0.
        imseg_tab-objnr = aufk-objnr.
      ENDIF.
    ENDIF.
    CLEAR imseg_tab-aufnr.
  ENDIF.

  IF NOT imseg_tab-kzbew IS INITIAL.
*   Kundennummer darf nicht übergeben werden bei Wareneingang
    CLEAR imseg_tab-kunnr.
  ENDIF.
* Bei Stornoposition Key des Original-Materialdokuments merken
  IF NOT imseg_tab-sjahr IS INITIAL.
    mseg_key-mblnr = imseg_tab-smbln.
    mseg_key-mjahr = imseg_tab-sjahr.
    mseg_key-zeile = imseg_tab-smblp.
    APPEND mseg_key.
  ENDIF.

* Wenn automatischer WE, Entnahmebuchung der Serienfertigung oder
* ungeplante Entnahme, dann Schlüssel der Reservierung löschen
  IF NOT imseg_tab-kzbew IS INITIAL OR "Wareneingang
     affwb_tab-autyp = auftyp-corp  OR "Serienfertigung
     imseg_tab-rsnum IS INITIAL.       "ungeplante Entnahme
    CLEAR: imseg_tab-rsnum,
           imseg_tab-rspos,
           imseg_tab-rsart.
  ENDIF.

* Fehlersätze pro paralleler Task sammeln
  CLEAR affwb_task.
  MOVE-CORRESPONDING affwb_tab TO affwb_task.
  IF affwb_tab-dnotw <> plus AND
    affwb_tab-vbkz <> vbkz_ins.
    affwb_task-vbkz  = vbkz_upd.
  ELSE.
    affwb_task-ersda = sy-datum.
    affwb_task-erzet = sy-uzeit.
    affwb_task-ernam = sy-uname.
    affwb_task-vbkz  = vbkz_ins.
  ENDIF.

* auf Chargenpflicht prüfen
  CALL FUNCTION 'MARC_SINGLE_READ'
       EXPORTING
            matnr             = imseg_tab-matnr
            werks             = imseg_tab-werks
       IMPORTING
            wmarc             = ls_marc
       EXCEPTIONS
            lock_on_marc      = 1
            lock_system_error = 2
            wrong_call        = 3
            not_found         = 4.
* WM-Chargenfindung nur aufrufen, wenn das Material
* chargenpflichtig mit initialer Charge oder nicht
* chargenpflichtig mit initialer Bewertungsart ist
  IF ( ( imseg_tab-charg IS INITIAL AND
     NOT ls_marc-xchpf IS INITIAL ) OR
       ( imseg_tab-bwtar IS INITIAL AND
         ls_marc-xchpf IS INITIAL ) ) AND
     NOT imseg_tab-erfmg IS INITIAL AND
     NOT imseg_tab-lgnum IS INITIAL AND
         imseg_tab-berkz <> '0'     AND
     NOT imseg_tab-kzech = '1'      AND
     NOT imseg_tab-kzech = '2'      AND
     NOT imseg_tab-kzech = '3'      AND
     NOT imseg_tab-rsnum IS INITIAL AND
     NOT imseg_tab-rspos IS INITIAL.
*   Chargenfindung erfolgt über WM
    MOVE-CORRESPONDING resb_tab_ges TO tmp_lresb.
    MOVE-CORRESPONDING imseg_tab    TO tmp_lresb.
    IF imseg_tab-erfme = resb_tab_ges-meins.
      tmp_lresb-bdmng = imseg_tab-erfmg.
    ELSE.
*     Entnahmemenge in Basiseinheit umrechnen
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
           EXPORTING
                input   = imseg_tab-erfmg
                kzmeinh = yx
                matnr   = imseg_tab-matnr
                meinh   = imseg_tab-erfme
                meins   = resb_tab_ges-meins
           IMPORTING
                output  = tmp_lresb-bdmng
           EXCEPTIONS
                OTHERS  = 1.
      IF sy-subrc <> 0.
        tmp_lresb-bdmng = 0.
      ENDIF.
    ENDIF.
*   WM-Chargenfindung
    CALL FUNCTION 'L_PPIM_BATCH_DETERMINATION_INT'
         EXPORTING
              i_lresb       = tmp_lresb
         TABLES
              t_bdbatch     = bdbatch_tab
         EXCEPTIONS
              error_message = 2
              OTHERS        = 1.
    IF sy-subrc <> 0.
      IF affwb_tab-fwdat IS INITIAL  OR
         affwb_tab-msgid <> sy-msgid OR
         affwb_tab-msgno <> sy-msgno.
        affwb_tab-fwdat = sy-datum.
        affwb_tab-fwzet = sy-uzeit.
      ENDIF.
      affwb_tab-msgid = sy-msgid.
      affwb_tab-msgty = sy-msgty.
      affwb_tab-msgno = sy-msgno.
      affwb_tab-msgv1 = sy-msgv1.
      affwb_tab-msgv2 = sy-msgv2.
      affwb_tab-msgv3 = sy-msgv3.
      affwb_tab-msgv4 = sy-msgv4.
      affwb_tab-aenam = sy-uname.
      affwb_tab-laeda = sy-datum.
      MODIFY affwb_tab INDEX tabix_do.
*     Backupdaten erfrischen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb_tab-weblnr.
      affw_key-weblpos = affwb_tab-weblpos.
      READ TABLE affw_old WITH KEY affw_key
                          BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING affwb_tab TO affw_old.
        MODIFY affw_old INDEX sy-tabix.
      ENDIF.
      EXIT.
    ELSE.
*     Setzen der lokalen Sperren
      CALL FUNCTION 'BF_SET_LOC_ENQ_TAB'
           EXPORTING
                vbeln     = imseg_tab-kdauf
                posnr     = imseg_tab-kdpos
                pspnr     = imseg_tab-ps_psp_pnr
           TABLES
                l_bdbatch = bdbatch_tab
           EXCEPTIONS
                OTHERS    = 1.
*     Übernahme der Chargen
      tmp_menge = imseg_tab-erfmg.
      imseg_tab-kzech = minus.
      IF bdbatch_tab[] IS INITIAL.
*       keine Info zur Reservierung im WM gefunden
        tabix_para = tabix_para + 1.
        APPEND affwb_task.
        APPEND imseg_tab.
      ELSE.
        LOOP AT bdbatch_tab.
          IF sy-tabix > 1.
*           neue Belegposition vergeben, falls die Warenbewegung
*           schiefgeht
            affw-weblnr = affwb_task-weblnr.
            PERFORM find_new_weblpos USING affw-weblnr
                                           affw-weblpos.
*           inoffizielle Verpointerung mseg --> affw
            imseg_tab-lfbnr    = affw-weblnr.
            imseg_tab-lfpos    = affw-weblpos.
            affwb_task-weblpos = affw-weblpos.
            affwb_task-vbkz    = vbkz_ins.
          ENDIF.
          IF NOT ls_marc-xchpf IS INITIAL.
            imseg_tab-charg = bdbatch_tab-charg.
            affwb_task-charg = bdbatch_tab-charg.
          ELSE.
            imseg_tab-bwtar = bdbatch_tab-charg.
            affwb_task-bwtar = bdbatch_tab-charg.
          ENDIF.
          imseg_tab-erfmg = bdbatch_tab-erfmg.
          imseg_tab-erfme = bdbatch_tab-erfme.
          affwb_task-erfmg = bdbatch_tab-erfmg.
          affwb_task-erfme = bdbatch_tab-erfme.
          APPEND imseg_tab.
          APPEND affwb_task.
          tabix_para = tabix_para + 1.
          tmp_menge = tmp_menge - bdbatch_tab-erfmg.
        ENDLOOP.
        IF tmp_menge > 0.
*         neue Belegposition vergeben, falls die Warenbewegung
*         schiefgeht
          affw-weblnr = affwb_task-weblnr.
          PERFORM find_new_weblpos USING affw-weblnr
                                         affw-weblpos.
*         inoffizielle Verpointerung mseg --> affw
          imseg_tab-lfbnr = affw-weblnr.
          imseg_tab-lfpos = affw-weblpos.
          CLEAR imseg_tab-charg.
          imseg_tab-erfmg = tmp_menge.
          imseg_tab-erfme = bdbatch_tab-erfme.
          APPEND imseg_tab.
          affwb_task-weblpos = affw-weblpos.
          CLEAR affwb_task-charg.
          affwb_task-erfmg = tmp_menge.
          affwb_task-erfme = bdbatch_tab-erfme.
          affwb_task-vbkz  = vbkz_ins.
          tabix_para = tabix_para + 1.
          APPEND affwb_task.
        ENDIF.
      ENDIF.                           "Splitt erfolgreich ?
    ENDIF.
  ELSE.                                "kein Chargensplitt über WM
    tabix_para = tabix_para + 1.
    APPEND affwb_task.
    APPEND imseg_tab.
  ENDIF.

  subrc = 0.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM POST_CHANGES                                              *
*----------------------------------------------------------------------*
*       markierte Einträge sichern                                     *
*----------------------------------------------------------------------*
FORM post_changes.

  DATA: BEGIN OF tmp_affwb OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF tmp_affwb.
* table of AFFW entries for cost collectors of repetitve manufacturing
  DATA: BEGIN OF lt_rem OCCURS 0,
          budat LIKE affw-budat,
          bldat LIKE affw-bldat,
          aufnr LIKE affw-aufnr,
          kdauf LIKE affw-kdauf,
          kdpos LIKE affw-kdpos,
          indx  LIKE sy-tabix,
        END   OF lt_rem.
  DATA: tmp_count LIKE sy-tabix.

  REFRESH: enq_loc,
           tabix_tab,
           refblnr_tab.
  CLEAR    refblnr_tab.
  cntr = 0.
  if sy-batch is initial.
  DO.
     cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz IS INITIAL.
        IF affwb_tab-refbln IS INITIAL.
          affwb_tab-vbkz = vbkz_upd.
          MODIFY affwb_tab TRANSPORTING vbkz.
          enq_loc-weblnr  = affwb_tab-weblnr.
          enq_loc-weblpos = affwb_tab-weblpos.
          APPEND enq_loc.
        ELSE.
*         Nummer des Referenzbelegs sammeln
          refblnr_tab-blnr = affwb_tab-refbln.
          APPEND refblnr_tab.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     Key der Belegposition merken
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz IS INITIAL.
      IF affwb_tab-refbln IS INITIAL.
        affwb_tab-vbkz = vbkz_upd.
        MODIFY affwb_tab INDEX sy-tabix TRANSPORTING vbkz.
        enq_loc-weblnr  = affwb_tab-weblnr.
        enq_loc-weblpos = affwb_tab-weblpos.
        APPEND enq_loc.
      ELSE.
*       Nummer des Referenzbelegs sammeln
        refblnr_tab-blnr = affwb_tab-refbln.
        APPEND refblnr_tab.
      ENDIF.
    ENDIF.
  ENDDO.

  else.

   IF r_single IS INITIAL.
      LOOP AT affwb_tab WHERE flg_sel = 'X'.
        CHECK affwb_tab-vbkz IS INITIAL.
        IF affwb_tab-refbln IS INITIAL.
          affwb_tab-vbkz = vbkz_upd.
          MODIFY affwb_tab TRANSPORTING vbkz.
          enq_loc-weblnr  = affwb_tab-weblnr.
          enq_loc-weblpos = affwb_tab-weblpos.
          APPEND enq_loc.
        ELSE.
*         Nummer des Referenzbelegs sammeln
          refblnr_tab-blnr = affwb_tab-refbln.
          APPEND refblnr_tab.
        ENDIF.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     Key der Belegposition merken
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz IS INITIAL.
      IF affwb_tab-refbln IS INITIAL.
        affwb_tab-vbkz = vbkz_upd.
        MODIFY affwb_tab INDEX sy-tabix TRANSPORTING vbkz.
        enq_loc-weblnr  = affwb_tab-weblnr.
        enq_loc-weblpos = affwb_tab-weblpos.
        APPEND enq_loc.
      ELSE.
*       Nummer des Referenzbelegs sammeln
        refblnr_tab-blnr = affwb_tab-refbln.
        APPEND refblnr_tab.
      ENDIF.
    ENDIF.
  endif.

*
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* für nicht sperrbare AFFW-Sätze Verbuchungskennzeichen zurücknehmen
  LOOP AT enq_loc.
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = enq_loc-weblnr.
    affw_key-weblpos = enq_loc-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    CLEAR affwb_tab-vbkz.
    MODIFY affwb_tab INDEX sy-tabix.
  ENDLOOP.

  REFRESH tabix_tab.
  IF NOT refblnr_tab[] IS INITIAL.
*   Indizes der abh. Positionen nach Sperren in TABIX_TAB aufnehmen
    PERFORM add_refered_pos.
  ENDIF.
  affwb_tab-vbkz = vbkz_upd.
  LOOP AT tabix_tab.
    MODIFY affwb_tab INDEX tabix_tab-ind TRANSPORTING vbkz.
  ENDLOOP.
* Zunächt sind die abhängigen Positionen sowie die
* Einträge der Serienfertigung zu bearbeiten
  REFRESH: refblnr_tab,
           lt_rem,
           tabix_tab.
  LOOP AT affwb_tab WHERE ( autyp = auftyp-corp OR
                            NOT refbln IS INITIAL )
                    AND   ( vbkz = vbkz_upd OR
                            vbkz = vbkz_ins ).
    IF affwb_tab-autyp = auftyp-corp.
*     Assigned to cost collector of repetitve manufacturing
      lt_rem-budat = affwb_tab-budat.
      lt_rem-bldat = affwb_tab-bldat.
      lt_rem-aufnr = affwb_tab-aufnr.
      lt_rem-kdauf = affwb_tab-kdauf.
      lt_rem-kdpos = affwb_tab-kdpos.
      lt_rem-indx  = sy-tabix.
      APPEND lt_rem.
    ELSE.
*     Assigned to combined goods receipt/goods issue of collective order
      refblnr_tab-blnr  = affwb_tab-refbln.
      refblnr_tab-kzbew = affwb_tab-kzbew.
      refblnr_tab-indx  = sy-tabix.
      APPEND refblnr_tab.
    ENDIF.
  ENDLOOP.
  SORT refblnr_tab BY blnr
                      kzbew DESCENDING "WE's zuerst
                      indx.
* Sort REM entries by dates cost collector and sales order
  SORT lt_rem BY budat bldat aufnr kdauf kdpos.

* Process entries assigned to cost collectors of repetitive manufact.
  CLEAR xallp.
  REFRESH imseg_tab.
  CLEAR affwb.
  tabix_para = 0.
  tabix_do   = 0.
  n_locked   = 0.
  tmp_count  = 0.
  READ TABLE lt_rem INDEX 1.
  IF sy-subrc = 0.
    READ TABLE affwb_tab INDEX lt_rem-indx.
    affwb = affwb_tab.
    imkpf-bldat = affwb_tab-bldat.
    imkpf-budat = affwb_tab-budat.
  ENDIF.
  DO.
    tmp_count = tmp_count + 1.
    READ TABLE lt_rem INDEX tmp_count.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    READ TABLE affwb_tab INDEX lt_rem-indx.
    tabix_do = lt_rem-indx.
*   Collect items for material document and create it when key changes
    PERFORM build_mat_document.
    IF subrc <> 0.
      REFRESH: affwb_task,
               bfwrt_tab,
               imseg_tab,
               mseg_key,
               resb_tab.
      tabix_para = 0.
      CLEAR affwb.
*     read next AFFWB entry
      tmp_count = tmp_count + 1.
      READ TABLE lt_rem INDEX tmp_count.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      READ TABLE affwb_tab INDEX lt_rem-indx.
      affwb = affwb_tab.
      imkpf-bldat = affwb_tab-bldat.
      imkpf-budat = affwb_tab-budat.
      tmp_count = tmp_count - 1.
    ENDIF.
  ENDDO.
  IF NOT affwb IS INITIAL.
*   Post material document for collected items
    PERFORM post_goods_movement.
  ENDIF.
  REFRESH: lt_rem,
           affwb_task,
           bfwrt_tab,
           imseg_tab,
           mseg_key,
           resb_tab.
  tabix_para = 0.

* Process combined "goods receipts/goods issues" of collective order
  xallp = yx.
  REFRESH imseg_tab.
  CLEAR affwb.
  tabix_para = 0.
  tabix_do   = 0.
  n_locked   = 0.
  tmp_count  = 0.
  READ TABLE refblnr_tab INDEX 1.
  IF sy-subrc = 0.
    READ TABLE affwb_tab INDEX refblnr_tab-indx.
    affwb = affwb_tab.
    imkpf-bldat = affwb_tab-bldat.
    imkpf-budat = affwb_tab-budat.
  ENDIF.
  DO.
    tmp_count = tmp_count + 1.
    READ TABLE refblnr_tab INDEX tmp_count.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    READ TABLE affwb_tab INDEX refblnr_tab-indx.
    tabix_do = refblnr_tab-indx.
*   Materialbeleg zusammenstellen und portionsweise buchen
    PERFORM build_mat_document.
    IF subrc <> 0.
      REFRESH: affwb_task,
               bfwrt_tab,
               imseg_tab,
               mseg_key,
               resb_tab.
      tabix_para = 0.
      CLEAR affwb.
*     nächten AFFWB-Tabellensatz mit nächster REFBLN einlesen
      tmp_count = tmp_count + 1.
      DO.
        READ TABLE refblnr_tab INDEX tmp_count.
        IF sy-subrc <> 0 OR
           refblnr_tab-blnr <> affwb_tab-refbln.
          EXIT.
        ENDIF.
        tmp_count = tmp_count + 1.
      ENDDO.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      READ TABLE affwb_tab INDEX refblnr_tab-indx.
      affwb = affwb_tab.
      imkpf-bldat = affwb_tab-bldat.
      imkpf-budat = affwb_tab-budat.
      tmp_count = tmp_count - 1.
    ENDIF.
  ENDDO.
  IF NOT affwb IS INITIAL.
*   Warenbeleg zum Buchungs-/Belegdatum buchen
    PERFORM post_goods_movement.
  ENDIF.
  REFRESH: refblnr_tab,
           affwb_task,
           bfwrt_tab,
           imseg_tab,
           mseg_key,
           resb_tab.
  tabix_para = 0.
*
* Anschliessend sind die unabhängigen Positionen zu bearbeiten
  CLEAR xallp.
  SORT affwb_tab BY budat bldat.
  CLEAR affwb.
* ersten zu sichernden Satz lesen
  LOOP AT affwb_tab WHERE autyp <> auftyp-corp
                    AND   refbln IS INITIAL
                    AND   ( vbkz = vbkz_ins OR
                            vbkz = vbkz_upd ).
    affwb = affwb_tab.
    imkpf-bldat = affwb_tab-bldat.
    imkpf-budat = affwb_tab-budat.
    EXIT.
  ENDLOOP.
  FREE imseg_tab.
  tabix_para = 0.
  tabix_do   = 0.
  n_locked   = 0.

***  D O - L O O P
  DO.
    tabix_do = tabix_do + 1.
    READ TABLE affwb_tab INDEX tabix_do.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK affwb_tab-autyp <> auftyp-corp AND
          affwb_tab-refbln IS INITIAL    AND
          ( affwb_tab-vbkz = vbkz_ins OR
            affwb_tab-vbkz = vbkz_upd ).
*   Materialbeleg zusammenstellen und portionsweise buchen
    PERFORM build_mat_document.
  ENDDO.
***  END OF  D O - L O O P

  IF NOT affwb IS INITIAL.
*   Warenbeleg zum Buchungs-/Belegdatum buchen
    PERFORM post_goods_movement.
    FREE: affwb_task,
          bfwrt_tab,
          imseg_tab,
          mseg_key,
          resb_tab.
  ENDIF.
  IF n_locked > 0.
    MESSAGE i454 WITH n_locked n_locked 0.
  ENDIF.

* Weitere Verarbeitung erst, wenn alle parallelen Prozesse beendet sind
  IF NOT parallel IS INITIAL.
    WAIT UNTIL acti_pr <= 0.
  ENDIF.

* Einträge löschen, die innerhalb der Transaktion zunächst neu
* angelegt, dann aber wieder gelöscht wurden (in gleicher Transaktion)
  DELETE affwb_tab WHERE vbkz = vbkz_dll.
* AFFWB retten
  tmp_affwb[] = affwb_tab[].
  DELETE affwb_tab WHERE vbkz IS initial.
  IF NOT affwb_tab[] IS INITIAL.
*   AFFW-sätze löschen
    PERFORM call_update_task ON COMMIT.
  ENDIF.
  COMMIT WORK AND WAIT.
* AFFWB zurückladen
  affwb_tab[] = tmp_affwb[].
  FREE tmp_affwb.
  IF NOT affwb_tab[] IS INITIAL.
    tmp_count = 0.
    LOOP AT affwb_tab WHERE NOT wablnr IS INITIAL.
      tmp_count = tmp_count + 1.
    ENDLOOP.
*   Nachricht über erfolgreich gebuchte Warenbewegungen
    MESSAGE s091 WITH tmp_count.
*   erfolgreich gebuchte Positionen entfernen
    DELETE affwb_tab WHERE NOT wablnr IS initial
                     OR    vbkz = vbkz_del.
*   Reset Verbuchungskennz. der geänderten bzw. eingefügten Positionen
    CLEAR affwb_tab-vbkz.
    MODIFY affwb_tab TRANSPORTING vbkz
                     WHERE NOT vbkz IS INITIAL.
*   AFFW_OLD neu aufbauen
    REFRESH affw_old.
    LOOP AT affwb_tab.
      MOVE-CORRESPONDING affwb_tab TO affw_old.
      APPEND affw_old.
    ENDLOOP.
    SORT affw_old BY weblnr weblpos.
  ELSE.
*   keine Zeile markiert, geändert oder gelöscht: Sichern nicht notw.
    MESSAGE i099.
  ENDIF.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM POST_GOODS_MOVEMENT                                       *
*----------------------------------------------------------------------*
*       Materialbelg buchen                                            *
*----------------------------------------------------------------------*
FORM post_goods_movement.

* Tabellen für serielle Schnittstelle zu CORUAFW3
  DATA: BEGIN OF fw_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF fw_tab.
  DATA: BEGIN OF im_tab OCCURS 0.
          INCLUDE STRUCTURE imseg.
  DATA: END   OF im_tab.
  DATA: BEGIN OF lt_lres OCCURS 0.
          INCLUDE STRUCTURE lresb.
  DATA: END   OF lt_lres.
  DATA: i_wmrel LIKE t340d-wmppi,
        i_obest LIKE t320-obest,
        e_werks LIKE imseg-werks,
        e_lgort LIKE imseg-lgort.
  DATA lt_affw LIKE affw OCCURS 0 WITH HEADER LINE.
  DATA: l_aufnr_old LIKE affw-aufnr,
        l_maufnr    LIKE afko-aufnr,
        tmp_subrc   LIKE sy-subrc,
        flg_wrong_status TYPE xflag.

  CHECK NOT imseg_tab[] IS INITIAL.
* Originalpreis und Konto übernehmen
  IF NOT mseg_key[] IS INITIAL.
    SELECT mblnr mjahr zeile sakto dmbtr exbwr bstmg
                             bwart menge meins FROM mseg
             APPENDING CORRESPONDING FIELDS
             OF TABLE bfwrt_tab
             FOR ALL ENTRIES IN mseg_key
             WHERE mblnr = mseg_key-mblnr
             AND   mjahr = mseg_key-mjahr
             AND   zeile = mseg_key-zeile.
    SORT bfwrt_tab BY mblnr mjahr zeile.
    DELETE ADJACENT DUPLICATES FROM bfwrt_tab.
    LOOP AT imseg_tab WHERE sjahr > 0.
      READ TABLE bfwrt_tab WITH KEY mblnr = imseg_tab-smbln
                                    mjahr = imseg_tab-sjahr
                                    zeile = imseg_tab-smblp
                                    BINARY SEARCH.
      IF sy-subrc = 0.
        IF bfwrt_tab-bwart NE t156n-bwart.
          SELECT SINGLE * FROM t156n
                          WHERE fcode EQ 'ST  '
                          AND bwart EQ bfwrt_tab-bwart.
          IF sy-subrc <> 0.
            CLEAR t156n.
          ENDIF.
        ENDIF.
        imseg_tab-xstor = t156n-xstor. "Kz für Belegstorno
        imseg_tab-menge = bfwrt_tab-menge.
        imseg_tab-meins = bfwrt_tab-meins.
        imseg_tab-konto = bfwrt_tab-sakto.
        CLEAR imseg_tab-exbwr.
        imseg_tab-dmbtr = bfwrt_tab-dmbtr.
        imseg_tab-bstmg = bfwrt_tab-bstmg.
        MODIFY imseg_tab.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Auftragsstatus für Entnahmepositionen prüfen
  CLEAR l_aufnr_old.
  LOOP AT imseg_tab WHERE autyp <> auftyp-corp.
    IF l_aufnr_old <> imseg_tab-aufnr.
      CLEAR flg_wrong_status.
      CLEAR tmp_subrc.
      CALL FUNCTION 'CO_SF_HEADER_GOODS_ISSUE'
           EXPORTING
                aufnr        = imseg_tab-aufnr
           EXCEPTIONS
                not_found    = 1
                not_activ    = 2
                wrong_status = 3
                wrong_type   = 4.
      l_aufnr_old = imseg_tab-aufnr.
      IF NOT sy-subrc IS INITIAL.
        flg_wrong_status = yx.
        tmp_subrc = sy-subrc.
      ELSE.
*       Bei Auftragsnetzen im Falle des WE auch den
*       übergeordneten Auftrag auf Status prüfen.
        CLEAR l_maufnr.
        IF NOT imseg_tab-kzbew IS INITIAL AND
           NOT imseg_tab-aufnr IS INITIAL AND
           NOT xallp IS INITIAL.
          SELECT SINGLE maufnr FROM afko INTO l_maufnr
                WHERE aufnr = imseg_tab-aufnr.
          IF NOT l_maufnr IS INITIAL.
            CALL FUNCTION 'CO_SF_HEADER_GOODS_ISSUE'
                 EXPORTING
                      aufnr        = l_maufnr
                 EXCEPTIONS
                      not_found    = 1
                      not_activ    = 2
                      wrong_status = 3
                      wrong_type   = 4.
            IF NOT sy-subrc IS INITIAL.
*             Der Status des übergeordneten Auftrags erlaubt keinen
*             WA, daher muss auch dieser WE einen Fehler erhalten.
              IF 1 = 2. MESSAGE e523(ru). ENDIF.
              affwb_tab-msgid = 'RU'.
              affwb_tab-msgty = 'E'.
              affwb_tab-msgno = '523'.
              affwb_tab-fwdat = sy-datum.
              affwb_tab-fwzet = sy-uzeit.
              CLEAR: affwb_tab-msgv1,
                     affwb_tab-msgv2,
                     affwb_tab-msgv3,
                     affwb_tab-msgv4.
              MODIFY affwb_tab TRANSPORTING msgid msgty msgno
                                            msgv1 msgv2 msgv3 msgv4
                                            fwdat fwzet
                                  WHERE matnr  = imseg_tab-matnr
                                  AND  weblnr  = imseg_tab-lfbnr
                                  AND  weblpos = imseg_tab-lfpos.
*             Imseg-Satz kann gelöscht werden.
              DELETE imseg_tab.
              DELETE affwb_task WHERE matnr = imseg_tab-matnr
                                AND weblnr  = imseg_tab-lfbnr
                                AND weblpos = imseg_tab-lfpos.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF NOT flg_wrong_status IS INITIAL.
      CHECK imseg_tab-kzbew IS INITIAL.
      CASE tmp_subrc.
*       unzulässiger Status -> Fehlerinformation in AFFWB schreiben
        WHEN '1'.                               "Auftrag existiert nicht
          IF 1 = 2. MESSAGE e001(c6) WITH space. ENDIF.
          affwb_tab-msgid = 'C6'.
          affwb_tab-msgty = 'E'.
          affwb_tab-msgno = '001'.
          affwb_tab-msgv1 = imseg_tab-aufnr.
          affwb_tab-fwdat = sy-datum.
          affwb_tab-fwzet = sy-uzeit.
        WHEN '2'.                               "Auftrag ist gelöscht
          IF 1 = 2. MESSAGE e002(c6) WITH space. ENDIF.
          affwb_tab-msgid = 'C6'.
          affwb_tab-msgty = 'E'.
          affwb_tab-msgno = '002'.
          affwb_tab-msgv1 = imseg_tab-aufnr.
          affwb_tab-fwdat = sy-datum.
          affwb_tab-fwzet = sy-uzeit.
        WHEN '3'.                               "Keine Entnahmen erlaubt
          IF affwb_tab-autyp = auftyp-netw. "Netzpläne
            IF 1 = 2. MESSAGE e014(c6) WITH space. ENDIF.
            affwb_tab-msgid = 'C6'.
            affwb_tab-msgty = 'E'.
            affwb_tab-msgno = '014'.
            affwb_tab-msgv1 = imseg_tab-aufnr.
            affwb_tab-fwdat = sy-datum.
            affwb_tab-fwzet = sy-uzeit.
          ELSE.                             "Aufträge
            IF 1 = 2. MESSAGE e013(c6) WITH space. ENDIF.
            affwb_tab-msgid = 'C6'.
            affwb_tab-msgty = 'E'.
            affwb_tab-msgno = '013'.
            affwb_tab-msgv1 = imseg_tab-aufnr.
            affwb_tab-fwdat = sy-datum.
            affwb_tab-fwzet = sy-uzeit.
          ENDIF.
        WHEN '4'.                               "Falscher Auftragstyp
          IF 1 = 2. MESSAGE e003(c6) WITH space. ENDIF.
          affwb_tab-msgid = 'C6'.
          affwb_tab-msgty = 'E'.
          affwb_tab-msgno = '003'.
          affwb_tab-msgv1 = imseg_tab-aufnr.
          affwb_tab-fwdat = sy-datum.
          affwb_tab-fwzet = sy-uzeit.
      ENDCASE.
      CLEAR: affwb_tab-msgv2,
             affwb_tab-msgv3,
             affwb_tab-msgv4.
      MODIFY affwb_tab TRANSPORTING msgid msgty msgno
                                    msgv1 msgv2 msgv3 msgv4
                                    fwdat fwzet
                          WHERE matnr  = imseg_tab-matnr
                          AND  weblnr  = imseg_tab-lfbnr
                          AND  weblpos = imseg_tab-lfpos.
*     Warenausgangsposition der IMSEG gleich löschen
      DELETE imseg_tab.
      DELETE affwb_task WHERE matnr = imseg_tab-matnr
                        AND weblnr  = imseg_tab-lfbnr
                        AND weblpos = imseg_tab-lfpos.
    ENDIF.
  ENDLOOP.

* Bei Einträgen aus der Serienfertigung müssen vor Aufruf der Bestands-
* führung evtl. noch WM-Daten nachgelesen werden.
  LOOP AT imseg_tab WHERE autyp = auftyp-corp.

* Note 547209: Check HU management
    DATA  l_hu_managed TYPE c.
    IF NOT imseg_tab-werks IS INITIAL AND
       NOT imseg_tab-lgort IS INITIAL.
      CALL FUNCTION 'V51S_HU_LGORT'
           EXPORTING
                if_lgort        = imseg_tab-lgort
                if_werks        = imseg_tab-werks
           IMPORTING
                ef_hu_managed   = l_hu_managed
           EXCEPTIONS
                lgort_not_exist = 1
                OTHERS          = 2.
      IF NOT l_hu_managed IS INITIAL OR sy-subrc <> 0.
* store error information
        affwb_tab-msgid = 'RM'.
        affwb_tab-msgty = 'E'.
        affwb_tab-msgno = '892'.
        affwb_tab-msgv1 = imseg_tab-lgort.
        affwb_tab-msgv2 = imseg_tab-werks.
        affwb_tab-msgv3 = imseg_tab-matnr.
        affwb_tab-fwdat = sy-datum.
        affwb_tab-fwzet = sy-uzeit.
        CLEAR affwb_tab-msgv4.
        MODIFY affwb_tab TRANSPORTING msgid msgty msgno msgv1
                                      msgv2 msgv3 msgv4 fwdat fwzet
                          WHERE matnr = imseg_tab-matnr
                          AND weblnr  = imseg_tab-lfbnr
                          AND weblpos = imseg_tab-lfpos.
        DELETE imseg_tab.
        DELETE affwb_task WHERE matnr = imseg_tab-matnr
                          AND weblnr  = imseg_tab-lfbnr
                          AND weblpos = imseg_tab-lfpos.
        CONTINUE.
      ENDIF.
    ENDIF. "Note 547209

    CHECK imseg_tab-shkzg = shkzg_haben.
    CHECK NOT imseg_tab-prvbe IS INITIAL.
    CHECK NOT imseg_tab-lgort IS INITIAL.
*   prüfen, ob Lagerort WM-relevant
    e_werks = imseg_tab-werks.
    e_lgort = imseg_tab-lgort.
    CALL FUNCTION 'L_WMPP_RELEVANCE_CHECK'
         EXPORTING
              werks       = e_werks
              lgort       = e_lgort
         IMPORTING
              wm_relevant = i_wmrel
              o_obest     = i_obest.
    IF NOT i_wmrel IS INITIAL AND
           i_obest IS INITIAL.
*     Übergabestruktur LT_LRES füllen, um Lagerkoordinaten zu ermitteln
      CLEAR   lt_lres.
      REFRESH lt_lres.
      MOVE-CORRESPONDING imseg_tab TO lt_lres.
      APPEND lt_lres.
*     Aufruf der WM-Schnittstelle
      CALL FUNCTION 'L_WMPP_GET_PROD_BIN'
           EXPORTING
                matnr          = lt_lres-matnr
                prvbe          = lt_lres-prvbe
                werks          = lt_lres-werks
           IMPORTING
                lgnum          = lt_lres-lgnum
                berkz          = lt_lres-berkz
                nlpla          = lt_lres-lgpla
                nltyp          = lt_lres-lgtyp
                ablad          = lt_lres-ablad
           EXCEPTIONS
                no_bin_located = 1.
      IF NOT sy-subrc IS INITIAL.
*       Fehlerinformation schreiben
        affwb_tab-msgid = 'RM'.
        affwb_tab-msgty = 'E'.
        affwb_tab-msgno = '007'.
        affwb_tab-msgv1 = lt_lres-matnr.
        affwb_tab-msgv2 = lt_lres-werks.
        affwb_tab-fwdat = sy-datum.
        affwb_tab-fwzet = sy-uzeit.
        CLEAR: affwb_tab-msgv3,
               affwb_tab-msgv4.
        MODIFY affwb_tab TRANSPORTING msgid msgty msgno msgv1
                                      msgv2 msgv3 msgv4 fwdat fwzet
                          WHERE matnr = imseg_tab-matnr
                          AND weblnr  = imseg_tab-lfbnr
                          AND weblpos = imseg_tab-lfpos.
        DELETE imseg_tab.
        DELETE affwb_task WHERE matnr = imseg_tab-matnr
                          AND weblnr  = imseg_tab-lfbnr
                          AND weblpos = imseg_tab-lfpos.
      ELSE.
*       Übergabe der Lagerkoordinaten
        imseg_tab-lgnum = lt_lres-lgnum.
        imseg_tab-lgtyp = lt_lres-lgtyp.
        imseg_tab-lgpla = lt_lres-lgpla.
        imseg_tab-berkz = lt_lres-berkz.
        MODIFY imseg_tab.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT imseg_tab[] IS INITIAL.
    IF imkpf-bldat IS INITIAL.
      imkpf-bldat = sy-datlo.
    ENDIF.
    IF imkpf-budat IS INITIAL.
      imkpf-budat = sy-datlo.
    ENDIF.
*   parallele Tasks für Bestandsfühfrungs-Aufrufe
    CLEAR msg_text.
    tasknam = tasknam + 1.

*   Tabelle der eingelesenen Reservierungsdaten sortieren
    SORT resb_tab BY rsnum rspos rsart.

*   LQUAY-Sperren zurücksetzen
    CALL FUNCTION 'L_PPIM_BATCH_DETERMIN_INIT_INT'.

*   Kundenerweiterung aufrufen
    PERFORM aufruf_badi_cogi_post CHANGING imseg_tab[]
                                           affwb_task[].

    IF parallel IS INITIAL.

      fw_tab[] = affwb_task[].
      im_tab[] = imseg_tab[].
*     Bestandsführung aufrufen
      EXPORT pakumul
             imkpf
             xallp
             fw_tab
             im_tab
             resb_tab TO MEMORY ID 'GOODSMOVE_COGI'.
      SUBMIT coruafw3 AND RETURN.
      PERFORM receive_result USING tasknam.

    ELSE.
      CALL FUNCTION 'CO_FW_GOODS_MOVEMENTS_BY_AFFW'
           STARTING NEW TASK tasknam
           DESTINATION IN GROUP DEFAULT
           PERFORMING receive_result ON END OF TASK
           EXPORTING
                imkpf_str = imkpf
                xallp_imp = xallp
           TABLES
                affwb_tab = affwb_task
                imseg_tab = imseg_tab
                resb_tab = resb_tab
         EXCEPTIONS
                communication_failure = 1 MESSAGE msg_text
                system_failure        = 1 MESSAGE msg_text
                RESOURCE_FAILURE      = 2
                OTHERS                = 3.

      IF sy-subrc <> 0.
        IF sy-subrc = 1.
          MESSAGE i888 WITH msg_text.
        ELSE.
          MESSAGE i455.
        ENDIF.
      ELSE.
*       Anzahl aktiver Prozesse aktualisieren
        acti_pr = acti_pr + 1.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM RECEIVE_RESULT
*----------------------------------------------------------------------*
*       Ergebnisse der parallelen Tasks empfangen                      *
*----------------------------------------------------------------------*
FORM receive_result USING rr_task.

  DATA: BEGIN OF fw_tab OCCURS 0.
          INCLUDE STRUCTURE affwb.
  DATA: END   OF fw_tab.
  DATA: BEGIN OF im_tab OCCURS 0.
          INCLUDE STRUCTURE imseg.
  DATA: END   OF im_tab.
  DATA: rr_tabix  LIKE sy-tabix,
        rr_tabx1  LIKE sy-tabix.

  IF parallel IS INITIAL.

    IMPORT fw_tab
           im_tab
           resb_tab FROM MEMORY ID 'GOODSMOVE_COGI'.
  ELSE.
    CLEAR msg_text.
    RECEIVE RESULTS FROM FUNCTION 'CO_FW_GOODS_MOVEMENTS_BY_AFFW'
            TABLES
                 affwb_tab = fw_tab
                 imseg_tab = im_tab
                 resb_tab  = resb_tab
            EXCEPTIONS
                 communication_failure = 1 MESSAGE msg_text
                 system_failure        = 1 MESSAGE msg_text.
    IF sy-subrc <> 0.
      MESSAGE a888 WITH msg_text.
    ENDIF.
  ENDIF.

* aktuelle Daten aus lokaler RESB_TAB in globale RESB_TAB_GES übernehmen
  LOOP AT resb_tab.
    READ TABLE resb_tab_ges WITH KEY rsnum = resb_tab-rsnum
                                     rspos = resb_tab-rspos
                                     rsart = resb_tab-rsart
                                     BINARY SEARCH.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING resb_tab TO resb_tab_ges.
    MODIFY resb_tab_ges INDEX sy-tabix.
  ENDLOOP.

* Ungeplante Entnahmen:
* --> WEBLNR und WEBLPOS übernehmen
* --> Insert auf Tabelle AFFW_OLD, wenn WB gescheitert
  LOOP AT fw_tab WHERE vbkz = vbkz_ins.
    READ TABLE affwb_tab WITH KEY weblnr = space
                                  vbkz = vbkz_ins.
    IF sy-subrc = 0.
      IF fw_tab-wablnr IS INITIAL.
*       Buchung gescheitert: vollständigen AFFW-Satz aufnehmen
        MOVE fw_tab TO affwb_tab.
        MODIFY affwb_tab INDEX sy-tabix.
*       Eintrag in Tabelle AFFW_OLD einfügen
        affw_key-mandt   = sy-mandt.
        affw_key-weblnr  = fw_tab-weblnr.
        affw_key-weblpos = fw_tab-weblpos.
        READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
        IF sy-subrc <> 0.
          MOVE-CORRESPONDING fw_tab TO affw_old.
          INSERT affw_old INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
* aktuelle Daten der bearbeiteten Positionen übernehmen
  LOOP AT fw_tab.
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = fw_tab-weblnr.
    affw_key-weblpos = fw_tab-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    rr_tabix = sy-tabix.
    CHECK sy-subrc = 0.
    IF NOT fw_tab-wablnr IS INITIAL.
*     Fehlerbeleg wurde erfolgreich nachgebucht
      affwb_tab-wablnr = fw_tab-wablnr.
      affwb_tab-mblpo  = fw_tab-mblpo.
      affwb_tab-mjahr  = fw_tab-mjahr.
      affwb_tab-kzear  = fw_tab-kzear.
      CLEAR affwb_tab-flg_sel.
      IF NOT affwb_tab-rsnum IS INITIAL AND
         NOT affwb_tab-rspos IS INITIAL.
*       Entnommene Menge der Reservierung aktualisieren
        resb_key-mandt = sy-mandt.
        resb_key-rsnum = affwb_tab-rsnum.
        resb_key-rspos = affwb_tab-rspos.
        resb_key-rsart = affwb_tab-rsart.
        READ TABLE resb_tab_ges WITH KEY resb_key BINARY SEARCH.
        rr_tabx1 = sy-tabix.
        IF sy-subrc = 0.
*         Entnahmemenge aus RESB_TAB übernehmen
          READ TABLE resb_tab WITH KEY resb_key BINARY SEARCH.
        ENDIF.
        IF sy-subrc = 0.
          resb_tab_ges-enmng = resb_tab-enmng.
          resb_tab_ges-kzear = resb_tab-kzear.
          MODIFY resb_tab_ges INDEX rr_tabx1.
        ENDIF.
      ENDIF.
    ELSE.
*     Warenbewegung konnte wegen Fehler nicht bearbeitet werden
      affwb_tab-dispo = fw_tab-dispo.
      affwb_tab-ernam = fw_tab-ernam.
      affwb_tab-ersda = fw_tab-ersda.
      affwb_tab-erzet = fw_tab-erzet.
      affwb_tab-fwdat = fw_tab-fwdat.
      affwb_tab-fwzet = fw_tab-fwzet.
      affwb_tab-msgid = fw_tab-msgid.
      affwb_tab-msgty = fw_tab-msgty.
      affwb_tab-msgno = fw_tab-msgno.
      affwb_tab-msgv1 = fw_tab-msgv1.
      affwb_tab-msgv2 = fw_tab-msgv2.
      affwb_tab-msgv3 = fw_tab-msgv3.
      affwb_tab-msgv4 = fw_tab-msgv4.
    ENDIF.
    affwb_tab-aenam = fw_tab-aenam.
    affwb_tab-laeda = fw_tab-laeda.
    CLEAR affwb_tab-vbkz.
    MODIFY affwb_tab INDEX rr_tabix.
    IF affwb_tab-wablnr IS INITIAL.
*     Backup-Tabelle im Fehlerfalle aktualisieren
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb_tab-weblnr.
      affw_key-weblpos = affwb_tab-weblpos.
      READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING affwb_tab TO affw_old.
        CLEAR affw_old-dnotw.
        MODIFY affw_old INDEX sy-tabix.
      ENDIF.
    ENDIF.
*   AFFW-Sperre freigeben
    CALL FUNCTION 'DEQUEUE_ESAFFW'
         EXPORTING
              weblnr  = affwb_tab-weblnr
              weblpos = affwb_tab-weblpos
         EXCEPTIONS
              OTHERS  = 1.
    READ TABLE enq_glob WITH KEY weblnr  = affwb_tab-weblnr
                                 weblpos = affwb_tab-weblpos
                                 BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      DELETE enq_glob INDEX sy-tabix.
    ENDIF.
    IF NOT affwb_tab-rsnum IS INITIAL AND
       NOT affwb_tab-rspos IS INITIAL.
*     RESB-Sperre freigeben, wenn möglich
      READ TABLE enq_resb WITH KEY rsnum = affwb_tab-rsnum
                                   rspos = affwb_tab-rspos
                                   rsart = affwb_tab-rsart
                                   BINARY SEARCH.
      rr_tabix = sy-tabix.
      enq_resb-count = enq_resb-count - 1.
      IF enq_resb-count = 0.
        DELETE enq_resb INDEX rr_tabix.
*       einzelne Reservierung entsperren
        CALL FUNCTION 'DEQUEUE_EMRESB'
             EXPORTING
                  rsnum  = affwb_tab-rsnum
                  rspos  = affwb_tab-rspos
                  rsart  = affwb_tab-rsart
             EXCEPTIONS
                  OTHERS = 1.
      ELSE.
        MODIFY enq_resb INDEX rr_tabix.
      ENDIF.
*     Shared-Sperre auf Reservierungskopf freigeben, wenn möglich
      READ TABLE enq_rsnum WITH KEY rsnum = affwb_tab-rsnum
                                    BINARY SEARCH.
      rr_tabix = sy-tabix.
      enq_rsnum-count = enq_rsnum-count - 1.
      IF enq_rsnum-count = 0.
        DELETE enq_rsnum INDEX rr_tabix.
*       Reservierungskopf entsperren
        CALL FUNCTION 'DEQUEUE_EMRKPF'
             EXPORTING
                  mode_rkpf = 'S'
                  rsnum     = affwb_tab-rsnum
                  _scope    = '1'
             EXCEPTIONS
                  OTHERS    = 1.
      ELSE.
        MODIFY enq_rsnum INDEX rr_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Anzahl aktive Prozesse aktualisieren
  acti_pr = acti_pr - 1.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SELECT_ENTRY                                              *
*----------------------------------------------------------------------*
*       Detailbild aufrufen                                            *
*----------------------------------------------------------------------*
FORM select_entry.

  PERFORM locate_entry USING space.
  CHECK tabix <> 0.
  FREE tabix_tab.
  IF r_single IS INITIAL.
*   verdichtte Anzeige
    READ TABLE affwb_out_tab INDEX tabix.
    IF affwb_out_tab-vbkz <> vbkz_shw.
      LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                        AND   lgort = affwb_out_tab-lgort
                        AND   matnr = affwb_out_tab-matnr
                        AND   charg = affwb_out_tab-charg
                        AND   bwart = affwb_out_tab-bwart
                        AND   erfme = affwb_out_tab-erfme
                        AND   msgid = affwb_out_tab-msgid
                        AND   msgno = affwb_out_tab-msgno.
        CHECK affwb_tab-vbkz <> vbkz_shw AND
              affwb_tab-vbkz <> vbkz_bu.
        tabix_tab-ind_kum = tabix.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ENDIF.
  ELSE.
*   Einzelsatzanzeige
    READ TABLE affwb_tab INDEX tabix.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz <> vbkz_shw AND
          affwb_tab-vbkz <> vbkz_bu.
    tabix_tab-ind = tabix.
    APPEND tabix_tab.
  ENDIF.

  REFRESH enq_loc.
  REFRESH refblnr_tab.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    refblnr_tab-indx = sy-tabix.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nur gemeinsam verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      DELETE tabix_tab.
      refblnr_tab-blnr = affwb_tab-refbln.
      APPEND refblnr_tab.
    ENDIF.
  ENDLOOP.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
* nicht sperrbare Objekte aus TABIX_TAB entfernen
  LOOP AT enq_loc.
    LOOP AT affwb_tab WHERE weblnr  = enq_loc-weblnr
                      AND   weblpos = enq_loc-weblpos.
      LOOP AT tabix_tab WHERE ind = sy-tabix.
        DELETE tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ENDLOOP.
  ENDLOOP.
  IF NOT refblnr_tab[] IS INITIAL.
*   Indizes der abh. Positionen nach Sperren in TABIX_TAB aufnehmen
    PERFORM add_refered_pos.
  ENDIF.
*
  IF NOT tabix_tab[] IS INITIAL.
    PERFORM call_detail_new.
  ENDIF.
  REFRESH tabix_tab.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SHOW_CONF                                                 *
*----------------------------------------------------------------------*
*       Rückmeldung anzeigen                                           *
*----------------------------------------------------------------------*
FORM show_conf.

  DATA: tmp_vornr LIKE afvgd-vornr.
  DATA: BEGIN OF l_prtnr_range OCCURS 1,                   "note 157525
           sign      TYPE c,
           option(2) TYPE c,
           low    LIKE  blpk-prtnr,
           high   LIKE  blpk-prtnr,
        END OF l_prtnr_range.

  FREE tabix_tab.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    line1 = sy-lisel.
    CHECK NOT line1-flg_sel IS INITIAL.
*   vollständiger Eintrag in Tabelle lesen
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = affwb-weblnr.
    affw_key-weblpos = affwb-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    tabix_tab-ind = sy-tabix.
    APPEND tabix_tab.
    EXIT.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
* Daten zur selektierten Zeile bereitstellen
  IF r_single IS INITIAL.
    READ TABLE affwb_out_tab INDEX tabix_tab-ind.
    affwb_tab = affwb_out_tab.
  ELSE.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK sy-subrc = 0.
* Parameter übergeben
  SET PARAMETER ID 'RCK' FIELD affwb_tab-rueck.
  SET PARAMETER ID 'RZL' FIELD affwb_tab-rmzhl.
  IF affwb_tab-autyp = auftyp-bord.
    SET PARAMETER ID 'BR1' FIELD affwb_tab-aufnr.
  ELSE.
    SET PARAMETER ID 'ANR' FIELD affwb_tab-aufnr.
  ENDIF.
  CLEAR tmp_vornr.
  SET PARAMETER ID 'VGN' FIELD tmp_vornr.
  CASE affwb_tab-autyp.
    WHEN auftyp-corp.
* Belegprotokoll Serienfertigung
      l_prtnr_range-sign   = 'I'.                          "note 157525
      l_prtnr_range-option = 'EQ'.
      l_prtnr_range-low    = affwb_tab-prtnr.
      CLEAR l_prtnr_range-high.
      APPEND l_prtnr_range.
      CALL FUNCTION 'RM_DOCUMENT_LOG_ALV'
           TABLES
                t_so_prtnr       = l_prtnr_range
           EXCEPTIONS
                no_entries       = 1
                too_many_entries = 2
                OTHERS           = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
*      SUBMIT RMSERI12 WITH SO_PRTNR EQ AFFWB_TAB-PRTNR.
    WHEN auftyp-fert.
      IF affwb_tab-aplzl IS INITIAL.
        SET PARAMETER ID 'RCK' FIELD rueck_space.
      ENDIF.
      CALL TRANSACTION tc_co14 AND SKIP FIRST SCREEN.
    WHEN auftyp-netw.
      CALL TRANSACTION tc_cn28 AND SKIP FIRST SCREEN.
    WHEN auftyp-inst.
      CALL TRANSACTION tc_iw43 AND SKIP FIRST SCREEN.
    WHEN auftyp-bord.
      IF affwb_tab-aplzl IS INITIAL.
        SET PARAMETER ID 'RCK' FIELD rueck_space.
      ENDIF.
      CALL TRANSACTION tc_cort AND SKIP FIRST SCREEN.       "3.1H FR
  ENDCASE.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SHOW_ADMIN                                                *
*----------------------------------------------------------------------*
*       Verwaltungsdaten anzeigen                                      *
*----------------------------------------------------------------------*
FORM show_admin.

  CLEAR tabix_tab.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    line1 = sy-lisel.
    CHECK NOT line1-flg_sel IS INITIAL.
*   vollständiger Eintrag in Tabelle lesen
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = affwb-weblnr.
    affw_key-weblpos = affwb-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    tabix_tab-ind = sy-tabix.
    EXIT.
  ENDDO.
  IF tabix_tab-ind IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  affwb = affwb_tab.
  IF affwb_tab-refbln IS INITIAL.
    CLEAR cowb_comp-flgref.
  ELSE.
    cowb_comp-flgref = yx.
  ENDIF.
  CALL SCREEN screen_0500 STARTING AT 10 3.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SHOW_FAILURE                                              *
*----------------------------------------------------------------------*
*       Fehlermeldung anzeigen                                         *
*----------------------------------------------------------------------*
FORM show_failure.

  CLEAR tabix_tab.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     verdichtete Anzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        tabix_tab-ind = sy-tabix.
        EXIT.
      ENDLOOP.
      EXIT.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      tabix_tab-ind = sy-tabix.
      EXIT.
    ENDIF.
  ENDDO.
  IF tabix_tab-ind IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  IF r_single IS INITIAL.
    READ TABLE affwb_out_tab INDEX tabix_tab-ind.
    affwb_tab = affwb_out_tab.
  ELSE.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK sy-subrc = 0.
  MESSAGE ID affwb_tab-msgid TYPE msgty_i NUMBER affwb_tab-msgno
          WITH affwb_tab-msgv1 affwb_tab-msgv2
               affwb_tab-msgv3 affwb_tab-msgv4.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SHOW_MATERIAL                                             *
*----------------------------------------------------------------------*
*       Material anzeigen                                              *
*----------------------------------------------------------------------*
FORM show_material.

  FREE tabix_tab.
  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     verdichtete Anzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
        EXIT.
      ENDLOOP.
      EXIT.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
      EXIT.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  IF r_single IS INITIAL.
    READ TABLE affwb_out_tab INDEX tabix_tab-ind.
    affwb_tab = affwb_out_tab.
  ELSE.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK sy-subrc = 0.
  SET PARAMETER ID 'MAT' FIELD affwb_tab-matnr.
  CALL TRANSACTION tc_mm03 AND SKIP FIRST SCREEN.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SHOW_ORDER                                                *
*----------------------------------------------------------------------*
*       Auftrag anzeigen                                               *
*----------------------------------------------------------------------*
FORM show_order.

  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    line1 = sy-lisel.
    CHECK NOT line1-flg_sel IS INITIAL.
*   vollständiger Eintrag in Tabelle lesen
    affw_key-mandt   = sy-mandt.
    affw_key-weblnr  = affwb-weblnr.
    affw_key-weblpos = affwb-weblpos.
    READ TABLE affwb_tab WITH KEY affw_key.
    CHECK sy-subrc = 0.
    tabix_tab-ind = sy-tabix.
    APPEND tabix_tab.
    EXIT.
  ENDDO.
  DESCRIBE TABLE tabix_tab LINES sy-tfill.
  IF sy-tfill = 0.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  READ TABLE affwb_tab INDEX tabix_tab-ind.
  CHECK sy-subrc = 0.
  IF affwb_tab-autyp = auftyp-bord.
    SET PARAMETER ID 'BR1' FIELD affwb_tab-aufnr.
  ELSE.
    SET PARAMETER ID 'ANR' FIELD affwb_tab-aufnr.
  ENDIF.
  CASE affwb_tab-autyp.
    WHEN auftyp-corp.
      IF affwb_tab-prtnr IS INITIAL.
        MESSAGE i508.
      ELSE.
*       Anzeigen des Serienauftrags zu einer Belegprotokollnummer
        CALL FUNCTION 'RM_SHOW_RSH'
             EXPORTING
                  i_prtnr = affwb_tab-prtnr
             EXCEPTIONS
                  OTHERS  = 1.
      ENDIF.
    WHEN auftyp-fert.
      CALL TRANSACTION tc_co03 AND SKIP FIRST SCREEN.
    WHEN auftyp-netw.
      CALL TRANSACTION tc_net_inf AND SKIP FIRST SCREEN.
    WHEN auftyp-inst.
      CALL TRANSACTION tc_iw33 AND SKIP FIRST SCREEN.
    WHEN auftyp-bord.
      CALL TRANSACTION tc_cor3 AND SKIP FIRST SCREEN.       "3.1H FR
  ENDCASE.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SHOW_STOCK_LIST                                           *
*----------------------------------------------------------------------*
*       Bestandsübersicht anzeigen                                     *
*----------------------------------------------------------------------*
FORM show_stock_list.

  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     verdichtete Anzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
        EXIT.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
    EXIT.
  ENDDO.
  DESCRIBE TABLE tabix_tab LINES sy-tfill.
  IF sy-tfill = 0.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  IF r_single IS INITIAL.
    READ TABLE affwb_out_tab INDEX tabix_tab-ind.
    affwb_tab = affwb_out_tab.
  ELSE.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK sy-subrc = 0.
  SET PARAMETER ID 'MAT' FIELD affwb_tab-matnr.
  CLEAR: affwb_tab-werks,
         affwb_tab-lgort.
  SET PARAMETER ID 'WRK' FIELD affwb_tab-werks.
  SET PARAMETER ID 'LAG' FIELD affwb_tab-lgort.
  CALL TRANSACTION tc_mmbe AND SKIP FIRST SCREEN.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SHOW_STOCK_REQU_LIST                                      *
*----------------------------------------------------------------------*
*       Bestands-/Bedarfsübersicht anzeigen                            *
*----------------------------------------------------------------------*
FORM show_stock_requ_list.

  cntr = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     verdichtete Anzeige
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_out_tab WHERE werks = affwb-werks
                            AND   lgort = affwb-lgort
                            AND   matnr = affwb-matnr
                            AND   charg = affwb-charg
                            AND   bwart = affwb-bwart
                            AND   erfme = affwb-erfme
                            AND   msgid = affwb-msgid
                            AND   msgno = affwb-msgno.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
        EXIT.
      ENDLOOP.
    ELSE.
*     Einzelsatzanzeige
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
    EXIT.
  ENDDO.
  DESCRIBE TABLE tabix_tab LINES sy-tfill.
  IF sy-tfill = 0.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING yx.
    tabix_tab-ind = tabix.
  ENDIF.
  CHECK tabix_tab-ind <> 0.
  IF r_single IS INITIAL.
    READ TABLE affwb_out_tab INDEX tabix_tab-ind.
    affwb_tab = affwb_out_tab.
  ELSE.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
  ENDIF.
  CHECK sy-subrc = 0.
  SET PARAMETER ID 'MAT' FIELD affwb_tab-matnr.
  SET PARAMETER ID 'WRK' FIELD affwb_tab-werks.
  CALL TRANSACTION tc_md04 AND SKIP FIRST SCREEN.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM INIT_SORTTAB                                              *
*----------------------------------------------------------------------*
*       Liste sortieren                                                *
*----------------------------------------------------------------------*
FORM init_sorttab.

*--> Sorttabelle füllen
  CLEAR sortx.
  REFRESH sortx.

  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'ERSDA'      TO sortx-fname.
  MOVE '1'          TO sortx-sopri.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'ERZET'      TO sortx-fname.
  MOVE '2'          TO sortx-sopri.
  APPEND sortx.
  CLEAR sortx-sopri.

  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'AENAM'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'AUFNR'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'BUDAT'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'BLDAT'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'BWART'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'CHARG'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'DISPO'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'ERFME'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'ERFMG'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'ERNAM'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'FEVOR'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'FWDAT'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'FWZET'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'KDAUF'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'KDPOS'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'LGORT'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'LIFNR'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'MATNR'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'MSGID'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'MSGNO'      TO sortx-fname.
  APPEND sortx.
  MOVE 'AFFWB_TAB'  TO sortx-tname.
  MOVE 'WERKS'      TO sortx-fname.
  APPEND sortx.
* Kennzeichen für Erstaufruf der Sortierroutinen
  flg_sort_b = 'F'.

ENDFORM.
*eject
*----------------------------------------------------------------------*
*       FORM SORT_ENTRIES                                              *
*----------------------------------------------------------------------*
*       Liste sortieren                                                *
*----------------------------------------------------------------------*
FORM sort_entries.

  DATA lt_all_field_for_sort_tab LIKE mcs01 OCCURS 5 WITH HEADER LINE.
  DATA lt_sort_krit_tab LIKE mcs01 OCCURS 5 WITH HEADER LINE.
  DATA lt_field_tab LIKE dfies OCCURS 10 WITH HEADER LINE.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
       EXPORTING
            tabname        = 'AFFW_SORT_FIELDS'
            langu          = sy-langu
       TABLES
            dfies_tab      = lt_field_tab
       EXCEPTIONS
            not_found      = 1
            internal_error = 2
            OTHERS         = 3.
  IF sy-subrc <> 0.
  ENDIF.
  REFRESH lt_all_field_for_sort_tab.
  APPEND LINES OF lt_field_tab TO lt_all_field_for_sort_tab.

  CALL FUNCTION 'CNSG_SET_SORT_FIELDS'
       EXPORTING
            i_flg_display           = yx
            i_flg_new_actual_fields = yx
            i_action                = ' '
            i_max_number            = 15  "Maximale Anzal an Felder
       TABLES
            t_actual_fields         = gt_field_for_sort_tab
            t_all_fields            = lt_all_field_for_sort_tab
       CHANGING
            c_handle                = g_sort_handle
       EXCEPTIONS
            cancel                  = 1
            no_allowed_fields       = 2
            OTHERS                  = 3.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'CNSG_SORT_TABLE'
         TABLES
              t_table  = affwb_tab
         CHANGING
              c_handle = g_sort_handle.
    s_pname = 'SORT'. "Damit die Sortierung für die Ausgabe funktioniert
    IF r_single IS INITIAL.
      PERFORM modif_screen_blocked.
    ELSE.
      PERFORM modif_screen_single.
    ENDIF.
  ENDIF.
*{   INSERT         PA8K037066                                        1
  EXIT.
* Ab hier alte Codingstrecke: zu löschen !!!
  DATA: tmp_fcode LIKE rm61m-fcode,
        tmp_pname LIKE rs38m-programm.

  CLEAR: tmp_pname,
         tmp_fcode.
*  > Aufruf Sortier-Popup / Generieren des Sortier-Codings
  CALL FUNCTION 'MD_CREATE_SORT'
       EXPORTING
            esname = 'AFFWB'
            ebuild = flg_sort_b
       IMPORTING
            ipname = tmp_pname
            ifcode = tmp_fcode
       TABLES
            sortx  = sortx.
* für Folgeaufrufe auf "X" setzen
  flg_sort_b = yx.
  IF  NOT tmp_pname IS INITIAL
  AND     tmp_fcode EQ 'DOSO'.
    s_pname = tmp_pname.
*  > Sortierung durchführen
    PERFORM do_sort IN PROGRAM (s_pname) TABLES affwb_tab.
  ENDIF.

* Einträge aus sortierten Tabelle anzeigen
  IF r_single IS INITIAL.
    PERFORM modif_screen_blocked.
  ELSE.
    PERFORM modif_screen_single.
  ENDIF.
*}   INSERT

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SPERREN_AFFW                                              *
*----------------------------------------------------------------------*
*       Fehlersatz sperren                                             *
*----------------------------------------------------------------------*
FORM sperren_affw.

  DATA: BEGIN OF affw_tmp OCCURS 0.
          INCLUDE STRUCTURE affw.
  DATA: END   OF affw_tmp.
  DATA: BEGIN OF pre_tmp OCCURS 0,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF pre_tmp.
  DATA: n_locked  LIKE sy-tabix,
        n_changed LIKE sy-tabix,
        n_total   LIKE sy-tabix.

  REFRESH: affw_tmp,
           pre_tmp.

  n_locked  = 0.
  n_changed = 0.

  SORT enq_loc BY weblnr weblpos.

  LOOP AT enq_loc.
*   prüfen, ob der Fehlersatz bereits durch die eigene Transaktion
*   gesperrt wurde
    READ TABLE enq_glob WITH KEY weblnr  = enq_loc-weblnr
                                 weblpos = enq_loc-weblpos
                                 BINARY SEARCH.
    IF sy-subrc = 0.
*     Fehlersatz wurde bereits zuvor gesperrt
      DELETE enq_loc.
    ELSE.
*     Fehlersatz ist noch zu sperren
      CALL FUNCTION 'ENQUEUE_ESAFFW'
           EXPORTING
                weblnr  = enq_loc-weblnr
                weblpos = enq_loc-weblpos
           EXCEPTIONS
                OTHERS  = 1.
      IF sy-subrc = 0.
*       Fehlersatz konnte erfolgreich gesperrt werden
        pre_tmp-weblnr  = enq_loc-weblnr.
        pre_tmp-weblpos = enq_loc-weblpos.
        APPEND pre_tmp.
      ELSE.
        n_locked = n_locked + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF NOT pre_tmp[] IS INITIAL.
    SELECT * FROM affw INTO TABLE affw_tmp
                       FOR ALL ENTRIES IN pre_tmp
                       WHERE weblnr  = pre_tmp-weblnr
                       AND   weblpos = pre_tmp-weblpos.
    SORT affw_tmp BY weblnr weblpos.

    LOOP AT pre_tmp.
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = pre_tmp-weblnr.
      affw_key-weblpos = pre_tmp-weblpos.
      READ TABLE affw_tmp WITH KEY affw_key BINARY SEARCH.
      IF sy-subrc = 0.
*       Backup-Tabelle im Fehlerfalle aktualisieren
        READ TABLE affw_old WITH KEY affw_key BINARY SEARCH.
      ENDIF.
      IF sy-subrc = 0.
        IF affw_tmp-autyp = auftyp-corp.
*         alte AFFW-Einträge erhielten nach dem Einlesen eine RSPOS
*         da früher keine auf die DB geschrieben wurde
          affw_tmp-rspos = affw_old-rspos.
        ENDIF.
        IF affw_tmp = affw_old.
*         Fehlersatz wurde zwischenzeitlich nicht geändert
          IF affw_old-autyp = auftyp-corp.
*           Zusätzliche Sperren der Serienfertigung absetzen
            CALL FUNCTION 'RM_ENQUEUE_COGI'
                 EXPORTING
                      i_weblnr                = pre_tmp-weblnr
                      i_weblpos               = pre_tmp-weblpos
                 EXCEPTIONS
                      no_affw_entry           = 1
                      no_blpk_entry           = 2
                      no_t001w_entry          = 3
                      material_version_locked = 4
                      mbew_locked             = 5
                      assy_order_locked       = 6
                      aufk_locked             = 7
                      lock_error              = 8
                      OTHERS                  = 9.
            IF sy-subrc <> 0.
*             zusätzliche Sperren konnten nicht gesetzt werden
              n_locked = n_locked + 1.
              CONTINUE.
            ENDIF.
          ENDIF.
          READ TABLE enq_loc WITH KEY pre_tmp BINARY SEARCH.
          DELETE enq_loc INDEX sy-tabix.
          READ TABLE enq_glob WITH KEY pre_tmp BINARY SEARCH.
          IF sy-subrc <> 0.
            enq_glob = pre_tmp.
            INSERT enq_glob INDEX sy-tabix.
          ENDIF.
        ELSE.
          n_changed = n_changed + 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
* Tabelle ENQ_LOC beinhaltet Keys der nicht sperrbaren Fehlersätze
  IF n_locked > 0 OR
     n_changed > 0.
    n_total = n_locked + n_changed.
    MESSAGE i454 WITH n_total n_locked n_changed.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM SPLIT_ENTRIES                                             *
*----------------------------------------------------------------------*
*       markierte Einträge splitten                                    *
*----------------------------------------------------------------------*
FORM split_entries.

* Temporäre Indextabelle zum Aufbau der TABIX_TAB für kum. Sicht
  DATA: BEGIN OF split_tabix_tab OCCURS 0,
          ind_kum LIKE sy-tabix,
          weblnr  LIKE affw-weblnr,
          weblpos LIKE affw-weblpos,
        END   OF split_tabix_tab.

  FREE tabix_tab.
  cntr = 0.
  tabix_kum = 0.
  DO.
    cntr = cntr + 1.
    READ LINE cntr.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    CHECK sellin = 1.
    IF r_single IS INITIAL.
*     Blockanzeige
      tabix_kum = tabix_kum + 1.
      line3 = sy-lisel.
      CHECK NOT line3-flg_sel IS INITIAL.
      LOOP AT affwb_tab WHERE werks = affwb-werks
                        AND   lgort = affwb-lgort
                        AND   matnr = affwb-matnr
                        AND   charg = affwb-charg
                        AND   bwart = affwb-bwart
                        AND   erfme = affwb-erfme
                        AND   msgid = affwb-msgid
                        AND   msgno = affwb-msgno.
        CHECK affwb_tab-vbkz <> vbkz_bu AND
              affwb_tab-vbkz <> vbkz_shw.
*       Nicht bei Storno-Zeile, da Beleg-Storno
        CHECK affwb_tab-sjahr IS INITIAL.
        tabix_tab-ind_kum = tabix_kum.
        tabix_tab-ind = sy-tabix.
        APPEND tabix_tab.
      ENDLOOP.
    ELSE.
*     Einzelsatzbearbeitung
      line1 = sy-lisel.
      CHECK NOT line1-flg_sel IS INITIAL.
*     vollständiger Eintrag in Tabelle lesen
      affw_key-mandt   = sy-mandt.
      affw_key-weblnr  = affwb-weblnr.
      affw_key-weblpos = affwb-weblpos.
      READ TABLE affwb_tab WITH KEY affw_key.
      CHECK sy-subrc = 0.
      CHECK affwb_tab-vbkz <> vbkz_del AND
            affwb_tab-vbkz <> vbkz_dll.
*     Nicht bei Storno-Zeile, da Beleg-Storno
      CHECK affwb_tab-sjahr IS INITIAL.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDIF.
  ENDDO.
  IF tabix_tab[] IS INITIAL.
*   wenn kein Eintrag markiert, Eintrag mit Cursor markieren
    PERFORM locate_entry USING space.
    IF tabix <> 0.
      IF r_single IS INITIAL.
        READ TABLE affwb_out_tab INDEX tabix.
        IF affwb_out_tab-vbkz <> vbkz_bu AND
           affwb_out_tab-vbkz <> vbkz_shw.
          LOOP AT affwb_tab WHERE werks = affwb_out_tab-werks
                            AND   lgort = affwb_out_tab-lgort
                            AND   matnr = affwb_out_tab-matnr
                            AND   charg = affwb_out_tab-charg
                            AND   bwart = affwb_out_tab-bwart
                            AND   erfme = affwb_out_tab-erfme
                            AND   msgid = affwb_out_tab-msgid
                            AND   msgno = affwb_out_tab-msgno.
            CHECK affwb_tab-vbkz <> vbkz_bu AND
                  affwb_tab-vbkz <> vbkz_shw.
*           Nicht bei Storno-Zeile, da Beleg-Storno
            CHECK affwb_tab-sjahr IS INITIAL.
            tabix_tab-ind_kum = tabix.
            tabix_tab-ind = sy-tabix.
            APPEND tabix_tab.
          ENDLOOP.
        ENDIF.
      ELSE.
        READ TABLE affwb_tab INDEX tabix.
        IF affwb_tab-vbkz <> vbkz_bu AND
           affwb_tab-vbkz <> vbkz_shw.
*         Nicht bei Storno-Zeile, da Beleg-Storno
          CHECK affwb_tab-sjahr IS INITIAL.
          tabix_tab-ind = tabix.
          APPEND tabix_tab.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*
  cntr = 0.
  REFRESH split_tab.
  CLEAR split_tab.
  REFRESH enq_loc.
* Fehlersätze sperren
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
    CHECK affwb_tab-vbkz IS INITIAL.
*   Positionen, die von anderen Positionen abhängig sind, dürfen
*   nicht verarbeitet werden
    IF affwb_tab-refbln IS INITIAL.
      enq_loc-weblnr  = affwb_tab-weblnr.
      enq_loc-weblpos = affwb_tab-weblpos.
      APPEND enq_loc.
    ELSE.
      cntr = cntr + 1.
      DELETE tabix_tab.
    ENDIF.
  ENDLOOP.
  IF cntr > 0.
    MESSAGE i507 WITH cntr.
    CHECK NOT tabix_tab[] IS INITIAL.
  ENDIF.
* AFFW-Satz sperren und nachlesen
* Wenn nicht sperrbar oder Daten sich geändert haben, dann bleibt
* der Eintrag in Tabelle ENQ_LOC erhalten
  PERFORM sperren_affw.
  LOOP AT tabix_tab.
    READ TABLE affwb_tab INDEX tabix_tab-ind.
    CHECK sy-subrc = 0.
*   gegen nicht sperrbare Objekte prüfen
    READ TABLE enq_loc WITH KEY weblnr  = affwb_tab-weblnr
                                weblpos = affwb_tab-weblpos
                                BINARY SEARCH.
    CHECK sy-subrc <> 0.
*   Fehlersatz konnte erfolgreich gesperrt werden
    MOVE-CORRESPONDING affwb_tab TO split_tab.
    split_tab-ind_kum = tabix_tab-ind_kum.
*   zu splittende Einzelsätze sammeln
    APPEND split_tab.
  ENDLOOP.
  REFRESH tabix_tab.
  DESCRIBE TABLE split_tab LINES rc27x-entries.
  CHECK rc27x-entries > 0.
  rc27x-index_auf = rc27x-index_act = 1.
* zu splittende Einträge nach der Menge sortieren
  SORT split_tab BY matnr werks lgort charg bwart erfmg.
  CALL SCREEN screen_0400 STARTING AT 10 3.
  CLEAR b_aend.
  LOOP AT split_tab.
    IF split_tab-splmg > 0.
      b_aend = yx.
      LOOP AT affwb_tab WHERE weblnr  = split_tab-weblnr
                        AND   weblpos = split_tab-weblpos.
        affwb_tab-erfmg = affwb_tab-erfmg - split_tab-splmg.
        IF affwb_tab-vbkz <> vbkz_ins AND
           affwb_tab-vbkz <> vbkz_upd.
          affwb_tab-vbkz  = vbkz_upd.
        ENDIF.
        MODIFY affwb_tab.
        tabix = sy-tabix + 1.
*       Aufnehmen des Splittsatzes in die temporäre TABIX-Tabelle
        MOVE-CORRESPONDING split_tab TO split_tabix_tab.
        APPEND split_tabix_tab.
        affwb_tab-erfmg = split_tab-splmg.
        affwb_tab-vbkz  = vbkz_ins.
        PERFORM find_new_weblpos USING affwb_tab-weblnr
                                       affwb_tab-weblpos.
*       Aufnehmen des zugefügten gesplitteten Satzes
*       in die globale Sperrtabelle
        enq_glob-weblnr  = affwb_tab-weblnr.
        enq_glob-weblpos = affwb_tab-weblpos.
        READ TABLE enq_glob WITH KEY weblnr  = affwb_tab-weblnr
                                     weblpos = affwb_tab-weblpos
                                     BINARY SEARCH.
        IF sy-subrc <> 0.
          INSERT enq_glob INDEX sy-tabix.
          INSERT affwb_tab INDEX tabix.
          split_tabix_tab-ind_kum = split_tab-ind_kum.
          split_tabix_tab-weblnr  = affwb_tab-weblnr.
          split_tabix_tab-weblpos = affwb_tab-weblpos.
          APPEND split_tabix_tab.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  FREE split_tab.
* Aufbau der TABIX_TAB zum Ausgeben der kumulierten Liste
  IF r_single IS INITIAL.
    LOOP AT split_tabix_tab.
      READ TABLE affwb_tab WITH KEY weblnr  = split_tabix_tab-weblnr
                                    weblpos = split_tabix_tab-weblpos.
      tabix_tab-ind_kum = split_tabix_tab-ind_kum.
      tabix_tab-ind = sy-tabix.
      APPEND tabix_tab.
    ENDLOOP.
    IF NOT b_aend IS INITIAL.
*     Liste der fehlerhaften Warenbewegungen ausgeben, kumuliert
      PERFORM modif_screen_blocked.
    ELSE.
*     Auf Liste stehen bleiben
      SET PF-STATUS 'BLOCK' EXCLUDING cuafc_tab.
      SET TITLEBAR '003'.
    ENDIF.
  ELSE.
    IF NOT b_aend IS INITIAL.
*     Liste der fehlerhaften Warenbewegungen ausgeben, Einzelsätze
      PERFORM modif_screen_single.
    ELSE.
*     auf Liste stehen bleiben
      SET PF-STATUS 'LIST' EXCLUDING cuafc_tab.
      SET TITLEBAR '001'.
    ENDIF.
  ENDIF.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       FORM FIND_NEW_WEBLPOS                                          *
*----------------------------------------------------------------------*
*       neue Positionsnummer für Fehlerbeleg suchen                    *
*----------------------------------------------------------------------*
FORM find_new_weblpos USING fw_blnr
                            fw_blpos.

  DATA: fw_subrc LIKE sy-subrc,
        fw_tabix LIKE sy-tabix.

* größte WEBLPOS je vergebener WEBLNR suchen
  READ TABLE weblpos_max WITH KEY weblnr = fw_blnr
                                           BINARY SEARCH.
  fw_subrc = sy-subrc.
  fw_tabix = sy-tabix.
  IF sy-subrc <> 0.
    CLEAR weblpos_max-weblpos.
    SELECT MAX( weblpos ) FROM affw INTO weblpos_max-weblpos
                          WHERE weblnr = fw_blnr.
    weblpos_max-weblnr = fw_blnr.
  ENDIF.
  DO.
    weblpos_max-weblpos = weblpos_max-weblpos + 1.
*   Fehlersatz sperren
    CALL FUNCTION 'ENQUEUE_ESAFFW'
         EXPORTING
              weblnr  = fw_blnr
              weblpos = weblpos_max-weblpos
         EXCEPTIONS
              OTHERS  = 01.
    IF sy-subrc = 0.
      EXIT.
    ENDIF.
  ENDDO.
  IF fw_subrc = 0.
    MODIFY weblpos_max INDEX fw_tabix.
  ELSE.
    INSERT weblpos_max INDEX fw_tabix.
  ENDIF.
  fw_blpos = weblpos_max-weblpos.

ENDFORM.
*eject.
*----------------------------------------------------------------------*
*       MODULE SET_CUA_0100                                            *
*----------------------------------------------------------------------*
*       CUA-Status und Titel für Screen 0100 setzen                    *
*----------------------------------------------------------------------*
MODULE set_cua_0100 OUTPUT.

  CLEAR ok_code.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR '002'.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE SET_CUA_0200                                            *
*----------------------------------------------------------------------*
*       CUA-Status und Titel für Screen 0200 setzen                    *
*----------------------------------------------------------------------*
MODULE set_cua_0200 OUTPUT.

  CLEAR ok_code.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR '004'.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE SET_CUA_0300                                            *
*----------------------------------------------------------------------*
*       CUA-Status und Titel für Screen 0300 setzen                    *
*----------------------------------------------------------------------*
MODULE set_cua_0300 OUTPUT.

  CLEAR ok_code.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR '005'.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE SET_CUA_0400                                            *
*----------------------------------------------------------------------*
*       CUA-Status und Titel für Screen 0400 setzen                    *
*----------------------------------------------------------------------*
MODULE set_cua_0400 OUTPUT.

  CLEAR ok_code.
  SET PF-STATUS 'SPLIT'.
  SET TITLEBAR '006'.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE SET_CUA_0500                                            *
*----------------------------------------------------------------------*
*       CUA-Status und Titel für Screen 0500 setzen                    *
*----------------------------------------------------------------------*
MODULE set_cua_0500 OUTPUT.

  CLEAR ok_code.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR '007'.

  LOOP AT SCREEN.
    IF affwb_tab-rsnum IS INITIAL AND
       screen-group2 = 'RES'.
      screen-output = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF affwb_tab-rueck IS INITIAL AND
       screen-group2 = 'CNF'.
      screen-output = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE  LST_SYS                                                *
*----------------------------------------------------------------------*
*       Listenverarbeitung initialisieren                              *
*----------------------------------------------------------------------*
MODULE lst_sys OUTPUT.

* Scrollbar-Tabelle aufbauen
  FREE scroll_tab.
  scroll_cursor = rc27x-index_auf.
* So lange Sätze anhängen, bis SCROLL_TAB Länge = RC27X-ENTRIES
  DO rc27x-entries TIMES.
    APPEND scroll_tab.
  ENDDO.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE SHOW_SPLT_ENTRY                                         *
*----------------------------------------------------------------------*
*       Splitsatz anzeigen                                             *
*----------------------------------------------------------------------*
MODULE show_splt_entry OUTPUT.

  rc27x-loops = sy-loopc.
* Ende der Liste erreicht?
  IF rc27x-index_act > rc27x-entries OR
     rc27x-entries   = 0.
*   Ende der Liste, restliche Seite leer lassen
    EXIT FROM STEP-LOOP.
  ELSE.
*   Splitt-Tabelle lesen
    READ TABLE split_tab INDEX rc27x-index_act.
    rc27x-index_act =  rc27x-index_act + 1.
    affw = split_tab.
    affwb-erfmg = split_tab-splmg.
    affwb-erfme = split_tab-erfme.
*   Schlüssel für Lesebaustein füllen
    mtcom-kenng = kenng_makt.
    mtcom-spras = sy-langu.
    mtcom-matnr = affw-matnr.
*   Materialstamm lesen
    CALL FUNCTION 'MATERIAL_READ'
         EXPORTING
              schluessel         = mtcom
         IMPORTING
              matdaten           = makt
         TABLES
              seqmat01           = dummy_tab
         EXCEPTIONS
              material_not_found = 04
              plant_not_found    = 08
              account_not_found  = 12.
    IF sy-subrc <> 0.
      CLEAR makt-maktx.
    ENDIF.
  ENDIF.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE  LST_SCROLL_PAI                                         *
*----------------------------------------------------------------------*
*       neuen Aufsetzpunkt nahc Scroll über Scroll-Bar bestimmen       *
*----------------------------------------------------------------------*
MODULE lst_scroll_pai.

* nur wenn keine OK-Code-Eingabe erfolgt ist
  CHECK ok_code IS INITIAL.
  CHECK scroll_cursor <> 0.
* ursprüngliche Cursorposition bestimmen
  cursor_old = rc27x-index_auf.
  IF scroll_cursor <> cursor_old.
*   neuer Aufsetzpunkt bestimmen
    rc27x-index_auf = scroll_cursor.
  ENDIF.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE  OK_CODE_SPLIT                                          *
*----------------------------------------------------------------------*
*       OK-Code-Verarbeitung auf Splittbild                            *
*----------------------------------------------------------------------*
MODULE ok_code_split.

  CASE ok_code.
    WHEN fc_rw.
      REFRESH split_tab.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN fc_minusminus.
      rc27x-index_auf = 1.
    WHEN fc_minus.
      rc27x-index_auf = rc27x-index_auf - rc27x-loops.
      IF rc27x-index_auf < 1.
        rc27x-index_auf = 1.
      ENDIF.
    WHEN fc_plus.
      IF rc27x-index_auf < rc27x-entries.
        rc27x-index_auf = rc27x-index_auf + rc27x-loops.
      ENDIF.
    WHEN fc_plusplus.
      rc27x-index_auf = rc27x-entries - rc27x-loops + 1.
    WHEN fc_bacs.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
  rc27x-index_act = rc27x-index_auf.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE LEAVE_SCREEN                                            *
*----------------------------------------------------------------------*
*       Bildschirm verlassen                                           *
*----------------------------------------------------------------------*
MODULE leave_screen.

  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE CHECK_LGORT                                             *
*----------------------------------------------------------------------*
*       neuer Lagerort prüfen                                          *
*----------------------------------------------------------------------*
MODULE check_lgort.

  CHECK NOT affw-lgort IS INITIAL.
  SELECT COUNT(*) FROM t001l UP TO 1 ROWS
                             WHERE lgort = affw-lgort.
  IF sy-subrc <> 0.
    MESSAGE e439 WITH affw-lgort.
  ENDIF.

ENDMODULE.
*eject
*----------------------------------------------------------------------*
*       MODULE CHECK_ERFMG                                             *
*----------------------------------------------------------------------*
*       Splitmenge prüfen                                              *
*----------------------------------------------------------------------*
MODULE check_erfmg.

  tabix = rc27x-index_auf + sy-stepl - 1.
  READ TABLE split_tab INDEX tabix.
  IF affwb-erfmg < 0 OR
     affwb-erfmg >= split_tab-erfmg.
    MESSAGE e444.
  ELSE.
    split_tab-splmg = affwb-erfmg.
  ENDIF.
  MODIFY split_tab INDEX tabix.

ENDMODULE.
*eject
*&---------------------------------------------------------------------*
*&      Form  CONVERT_PSP_TO_INTERN
*&---------------------------------------------------------------------*
*       Konvertiert die Selektionrange von PSP-Element
*       von externen Nummern in die internen Nummern
*----------------------------------------------------------------------*
*      -->P_S_PSP_AF   Externe PSP-Nr für Selektion der AFFW
*      -->P_S_PSP_OR   Externe PSP-Nr für Selektion der Aufträge
*      <--PT_PSP_AF    Interne PSP-Nr für Selektion der AFFW
*      <--PT_PSP_OR    Interne PSP-Nr für Selektion der Aufträge
*----------------------------------------------------------------------*
FORM convert_psp_to_intern TABLES p_s_psp_af STRUCTURE s_psp_af
                                  p_s_psp_or STRUCTURE s_psp_or
                                  pt_psp_af  STRUCTURE psp_affw_range
                                  pt_psp_or  STRUCTURE psp_order_range.
  REFRESH pt_psp_af.
  REFRESH pt_psp_or.
  IF NOT p_s_psp_af[] IS INITIAL.
*   Konvertieren der PSP-Nummern für die Selektion der AFFW
    SELECT pspnr AS low FROM prps
                        INTO CORRESPONDING FIELDS OF TABLE pt_psp_af
                        WHERE posid IN p_s_psp_af.
    pt_psp_af-sign = 'I'.
    pt_psp_af-option = 'EQ'.
    MODIFY pt_psp_af TRANSPORTING sign option
                     WHERE NOT low IS INITIAL.
  ENDIF.
  IF NOT p_s_psp_or[] IS INITIAL.
*   Konvertieren der PSP-Nummern für die Selektion der Aufträge
    SELECT pspnr AS low FROM prps
                        INTO CORRESPONDING FIELDS OF TABLE pt_psp_or
                        WHERE posid IN p_s_psp_or.
    pt_psp_or-sign = 'I'.
    pt_psp_or-option = 'EQ'.
    MODIFY pt_psp_or TRANSPORTING sign option
                     WHERE NOT low IS INITIAL.

  ENDIF.

ENDFORM.                               " CONVERT_PSP_TO_INTERN
*&---------------------------------------------------------------------*
*&      Form  AUFRUF_BADI_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*   Ruft das Business-Add-In zum Prüfen der Funktion auf
*----------------------------------------------------------------------*
FORM aufruf_badi_authority_check USING    lt_affwb TYPE ty_t_affwb
                                          l_ucomm  LIKE sy-ucomm
                                 CHANGING l_error  TYPE xflag.

* BADI 'WORKORDER_GOODSMVT'
  DATA: lp_badi_if TYPE REF TO if_ex_workorder_goodsmvt.

* Konstante für BADI Bezeichnung
  CONSTANTS:
    con_badi_name_order_goodsmvt TYPE rsexscrn-exit_name
                                 VALUE 'WORKORDER_GOODSMVT'.

* BADI nicht aufrufen bei 'Zurück', 'Abbrechen' oder 'Beenden'
  CHECK NOT l_ucomm EQ fc_back.
  CHECK NOT l_ucomm EQ fc_esc.
  CHECK NOT l_ucomm EQ fc_rw.

* Prüfen ob BADI 'WORKORDER_GOODSMVT' aktiv ist
  CALL FUNCTION 'CO_BADI_GET_BUSINESS_ADD_IN'
       EXPORTING
            i_badi_name     = con_badi_name_order_goodsmvt
       CHANGING
            c_badi_instance = lp_badi_if
       EXCEPTIONS
            not_active      = 1
            OTHERS          = 2.

  IF sy-subrc IS INITIAL.
*   further processing only if BADI is active
*   -> call BADI method 'COGI_AUTHORITY_CHECK'
    CALL METHOD lp_badi_if->cogi_authority_check
         EXPORTING
              it_affwb           = lt_affwb
              i_ucomm            = l_ucomm
         CHANGING
              c_error_flg        = l_error.
  ENDIF.

ENDFORM.                     "AUFRUF_BADI_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  AUFRUF_BADI_COGI_POST
*&---------------------------------------------------------------------*
*   Ruft das Business-Add-In zum Prüfen/Ändern der Warenbewegungen auf
*----------------------------------------------------------------------*
FORM aufruf_badi_cogi_post CHANGING lt_imseg TYPE ty_t_imseg
                                    lt_affwb TYPE ty_t_affwb.

  DATA: ls_imseg LIKE imseg.
* BADI 'WORKORDER_GOODSMVT'
  DATA: lp_badi_if TYPE REF TO if_ex_workorder_goodsmvt.

* Konstante für BADI Bezeichnung
  CONSTANTS:
    con_badi_name_order_goodsmvt TYPE rsexscrn-exit_name
                                 VALUE 'WORKORDER_GOODSMVT'.

* Prüfen ob BADI 'WORKORDER_GOODSMVT' aktiv ist
  CALL FUNCTION 'CO_BADI_GET_BUSINESS_ADD_IN'
       EXPORTING
            i_badi_name     = con_badi_name_order_goodsmvt
       CHANGING
            c_badi_instance = lp_badi_if
       EXCEPTIONS
            not_active      = 1
            OTHERS          = 2.

  IF sy-subrc IS INITIAL.
    READ TABLE lt_imseg INDEX 1 INTO ls_imseg.
*   Storno-Warenbewegungen dürfen NICHT bearbeitet werden
    IF NOT ls_imseg-smbln IS INITIAL.

*     further processing only if BADI is active
*     -> call BADI method 'COGI_POST'
      CALL METHOD lp_badi_if->cogi_post
           EXPORTING
              it_imseg             = lt_imseg
              it_affwb             = lt_affwb.
    ELSE.
*     further processing only if BADI is active
*     -> call BADI method 'COGI_POST'
      CALL METHOD lp_badi_if->cogi_post
           CHANGING
              ct_imseg             = lt_imseg
              ct_affwb             = lt_affwb.
    ENDIF.
  ENDIF.

*{   INSERT         UD1K921615                                        1
ENDFORM.                     "AUFRUF_BADI_COGI_POST
*}   INSERT
*{   INSERT         UD1K921615                                        3
* note 835810
FORM affwresb tables it_resb structure resb
                     it_affw structure affw
              changing e_lock.
  data: begin of lt_sel occurs 0,
          matnr type MATNR,
          werks type WERKS_D,
        end of lt_sel,
       ls_resb   TYPE resb,
       lt_affw   TYPE TABLE OF affw,
       lt_affw_u TYPE TABLE OF affw,
       lt_resb   TYPE TABLE OF resb,
       lt_resb_i TYPE TABLE OF resb,
       lt_resb_u TYPE TABLE OF resb,
       lt_resb_d TYPE TABLE OF resb,
       lt_output TYPE ty_tab_affwresb,
       gf_rsnum  TYPE rsnum.
  RANGES:
    so_rsnum  FOR gf_rsnum.


  field-symbols: <fs_resb> like resb,
                 <fs_affw> TYPE affw.

* check the resb negative entry
  loop at it_resb assigning <fs_resb>
                  where bdmng < 0.
    lt_sel-matnr = <fs_resb>-matnr.
    lt_sel-werks = <fs_resb>-werks.
    append lt_sel.
  endloop.
  check not lt_sel[] is initial.
* lock the AFFW
  clear e_lock.
  do 10 times.
    CALL FUNCTION 'ENQUEUE_ESAFFW'
         EXPORTING
              mode_affw      = 'E'
              mandt          = sy-mandt
              _scope         = '3'
              _wait          = 'X'
         EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
    IF sy-subrc eq 0.
*     lock was successful
      e_lock = 'X'.
      exit.
    ENDIF.
  enddo.
  if e_lock = 'X'.
    clear e_lock.
    do 10 times.
      CALL FUNCTION 'ENQUEUE_E_PPC_RES_HDR'
           EXPORTING
                mode_ppc_res_hdr = 'E'
                mandt            = sy-mandt
                _scope           = '3'
                _wait            = 'X'
           EXCEPTIONS
                foreign_lock     = 1
                system_failure   = 2
                OTHERS           = 3.
      IF sy-subrc eq 0.
*        lock was successful
        e_lock = 'X'.
        exit.
      ENDIF.
    enddo.
  endif.
  if e_lock is initial.
*   lock was not set -> skip the check.
*   remove the wrong reservation from the internal table
    loop at it_resb assigning <fs_resb>
                   where bdmng < 0.
      clear: <fs_resb>-bdmng, <fs_resb>-erfmg.
    endloop.
  else.
* after setting the lock we have to do the expensiv select again
*----------------------------------------------------------------------*
* PART 1:                                                              *
*         -Check the wrong reservations without reprocessing records   *
*           RESB entry without AFFW -> delete them                     *
*----------------------------------------------------------------------*
    SELECT * INTO TABLE lt_resb
             FROM resb
             for all entries in lt_sel
             WHERE matnr eq lt_sel-matnr
               AND werks eq lt_sel-werks
               AND bdart = 'SB'
               AND plnum = '          '.
    IF NOT lt_resb[] IS INITIAL.
*   check the entries corresponding entries in the table AFFW
      SELECT * FROM affw INTO TABLE lt_affw
               FOR ALL ENTRIES IN lt_resb
               WHERE rsnum EQ lt_resb-rsnum
                 AND rspos EQ lt_resb-rspos.
      SORT lt_affw BY rsnum rspos.
      LOOP AT lt_resb ASSIGNING <fs_resb>.
        READ TABLE lt_affw TRANSPORTING NO FIELDS
                           WITH KEY rsnum = <fs_resb>-rsnum
                                    rspos = <fs_resb>-rspos
                           BINARY SEARCH.
        IF NOT sy-subrc IS INITIAL.
*       for the reservation there is no AFFW entry -> wrong reservation
*          WRITE: /1 'RESB w/o AFFW entry:',
*                    <fs_resb>-rsnum,
*                    <fs_resb>-rspos,
*                    <fs_resb>-matnr,
*                    <fs_resb>-werks.
          APPEND <fs_resb> TO lt_resb_d.
        ENDIF.
      ENDLOOP.
      IF not lt_resb_d[] IS INITIAL.
        DELETE resb FROM TABLE lt_resb_d.
        PERFORM send_reservation tables lt_resb_d[]
                                 USING 'D'.
        COMMIT WORK.
      ENDIF.
    ENDIF.
*----------------------------------------------------------------------*
* PART 2:                                                              *
*         -Compare the key fields of the tables AFFW-RESB              *
*           AFFW entry should be the right one                         *
*           do not check the quantity                                  *
*----------------------------------------------------------------------*
    REFRESH: lt_resb_d, lt_resb, lt_affw.
    FREE:  lt_resb_d, lt_resb, lt_affw.
*   Get affw entries for selected materials
    SELECT * FROM affw
      INTO TABLE lt_affw
      for all entries in lt_sel
      WHERE matnr eq lt_sel-matnr
        AND werks eq lt_sel-werks
        AND flg_orig = '1'.
*   Get referrenced resb entries
    so_rsnum-sign = 'I'.
    so_rsnum-option = 'EQ'.
    LOOP AT lt_affw ASSIGNING <fs_affw>.
      so_rsnum-low = <fs_affw>-rsnum.
      COLLECT so_rsnum.
    ENDLOOP.
    IF NOT so_rsnum[] IS INITIAL.
*     select the reservations
      SELECT * FROM resb
        INTO TABLE lt_resb
        WHERE rsnum IN so_rsnum AND
              bdart EQ 'SB'.
      SORT lt_resb BY rsnum rspos.
    ENDIF.
*   Start comparing entries
    LOOP AT lt_affw ASSIGNING <fs_affw>.
*   ... get referenced resb line
      READ TABLE lt_resb WITH KEY rsnum = <fs_affw>-rsnum
                                  rspos = <fs_affw>-rspos
                                  ASSIGNING <fs_resb>
                                  BINARY SEARCH.
      IF NOT sy-subrc IS INITIAL.
*       no resb entry for this affw!!!
*       create the missing entry
        PERFORM create_new_resb USING    <fs_affw>
                                CHANGING lt_resb_i
                                         lt_affw_u
                                         lt_output.
        CONTINUE. "to next pass
      ENDIF.
*   .. do the comparison of the key fields
      IF NOT ( <fs_affw>-matnr = <fs_resb>-matnr AND
               <fs_affw>-werks = <fs_resb>-werks AND
               <fs_affw>-lgort = <fs_resb>-lgort AND
               <fs_affw>-charg = <fs_resb>-charg AND
               <fs_affw>-sobkz = <fs_resb>-sobkz AND
               <fs_affw>-bwart = <fs_resb>-bwart AND
               <fs_affw>-erfme = <fs_resb>-erfme ).
*       entries do not match! change the RESB entry!
        PERFORM modif_wrong_resb USING    lt_affw
                                 CHANGING <fs_resb>
                                          <fs_affw>
                                          lt_resb_i
                                          lt_resb_u
                                          lt_affw_u
                                          lt_output.
      ENDIF.
    ENDLOOP. "end of comparison
    IF not ( lt_resb_i[] IS INITIAL AND lt_resb_u[] IS INITIAL ).
*   Now we have to modify database
      IF NOT lt_resb_i[] IS INITIAL.
*     check whether RSPOS are used. shouldn't be because we locked
        SELECT * FROM resb INTO TABLE lt_resb
                 FOR ALL ENTRIES IN lt_resb_i
                 WHERE rsnum EQ lt_resb_i-rsnum
                   AND rspos EQ lt_resb_i-rspos.
        LOOP AT lt_resb ASSIGNING <fs_resb>.
          DELETE lt_resb_i WHERE rsnum EQ <fs_resb>-rsnum
                             AND rspos EQ <fs_resb>-rspos.
*          WRITE: /1 'Please check RESB entry manually',
*                    <fs_resb>-rsnum,
*                    <fs_resb>-rspos,
*                    <fs_resb>-matnr.
        ENDLOOP.
*     INSERT THE RESB ENTRIES
        INSERT resb FROM TABLE lt_resb_i.
        PERFORM send_reservation tables lt_resb_i[]
                                 USING 'I'.
      ENDIF.
      IF NOT lt_resb_u[] IS INITIAL.
*     UPDATE THE RESB ENTRIES
        UPDATE resb FROM TABLE lt_resb_u.
        PERFORM send_reservation tables lt_resb_u[]
                                 USING 'U'.
      ENDIF.
      IF NOT lt_affw_u[] IS INITIAL.
*     update AFFW entries with new RSPOS
        UPDATE affw FROM TABLE lt_affw_u.
      ENDIF.
      COMMIT WORK.
*     do protocol if it is requiered
    endif.
*----------------------------------------------------------------------*
* PART 3:                                                              *
*         -Check the quantity between the tables AFFW-RESB             *
*           AFFW entry should be the right one                         *
*----------------------------------------------------------------------*
    REFRESH: lt_resb_d, lt_resb_i, lt_resb_u, lt_resb, lt_affw.
    FREE: lt_resb_d, lt_resb_i, lt_resb_u, lt_resb, lt_affw.
    DATA: lf_quant   TYPE erfmg,
          lf_redflag TYPE c.
*   Get affw entries for selected materials
    SELECT * FROM affw
      INTO TABLE lt_affw
      for all entries in lt_sel
      WHERE matnr eq lt_sel-matnr
        AND werks eq lt_sel-werks
        AND flg_orig = '1'.
    IF NOT lt_affw[] IS INITIAL.
      SELECT * FROM resb
        INTO TABLE lt_resb
        FOR ALL ENTRIES IN lt_affw
        WHERE rsnum EQ lt_affw-rsnum
         AND  rspos EQ lt_affw-rspos
         AND  bdart EQ 'SB'.
    ENDIF.
    SORT lt_resb BY rsnum rspos.
    SORT lt_affw BY rsnum rspos.
*   for each resb entry (individual component...)
    LOOP AT lt_resb ASSIGNING <fs_resb>.
      CLEAR: lf_quant, lf_redflag.
*     1. get individual affw entries
      LOOP AT lt_affw ASSIGNING <fs_affw>
                WHERE rsnum = <fs_resb>-rsnum
                  AND rspos = <fs_resb>-rspos.
        ADD <fs_affw>-erfmg TO lf_quant.
*       double check consistency affw-resb
        IF NOT ( <fs_affw>-matnr = <fs_resb>-matnr AND
                 <fs_affw>-werks = <fs_resb>-werks AND
                 <fs_affw>-lgort = <fs_resb>-lgort AND
                 <fs_affw>-charg = <fs_resb>-charg AND
                 <fs_affw>-sobkz = <fs_resb>-sobkz AND
                 <fs_affw>-bwart = <fs_resb>-bwart AND
                 <fs_affw>-erfme = <fs_resb>-erfme ).
*         key fields should have been corrected!
          lf_redflag = 'X'.
        ENDIF.
      ENDLOOP. "loop at referenced affw entries
*     2. check errors
      IF NOT lf_redflag IS INITIAL.
**    consistency error (wrong key fields)
*        WRITE: /1 'CHECK ERROR MANUALLY',
*                   <fs_resb>-rsnum,
*                   <fs_resb>-rspos,
*                   <fs_resb>-matnr.
      ELSEIF NOT ( lf_quant = <fs_resb>-bdmng AND
                   lf_quant = <fs_resb>-erfmg ).
*       quantity error
        PERFORM modif_wrong_resb_with_q USING    lf_quant
                                        CHANGING <fs_resb>
                                                 lt_resb_u.
      ENDIF.
    ENDLOOP. "loop at resb entries

    IF not lt_resb_u IS INITIAL.
*     ... correct wrong stuff
      IF NOT lt_resb_u IS INITIAL.
*       UPDATE THE RESB ENTRIES with quantity
        UPDATE resb FROM TABLE lt_resb_u.
        PERFORM send_reservation tables lt_resb_u[]
                                 USING 'U'.
      ENDIF.
      COMMIT WORK.
    ENDIF.
* do protocol
  endif. " e_lock
*   unlock the locking objects
  CALL FUNCTION 'DEQUEUE_ESAFFW'
       EXPORTING
            mode_affw = 'E'
            mandt     = sy-mandt
            _scope    = '3'.
  CALL FUNCTION 'DEQUEUE_E_PPC_RES_HDR'
       EXPORTING
            mode_ppc_res_hdr = 'E'
            mandt            = sy-mandt
            _scope           = '3'.
endform.
*&---------------------------------------------------------------------*
*&      Form  send_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0581   text
*      -->P_LT_RESB_D  text
*----------------------------------------------------------------------*
FORM send_reservation tables   it_resb  structure resb
                      USING    i_action TYPE char01.
  DATA: lt_res_hdr LIKE ppc_res_hdr OCCURS 0,
        ls_res_hdr LIKE ppc_res_hdr,
        ls_resb    LIKE resb,
        l_tabix    TYPE sytabix,
        BEGIN OF lt_tab OCCURS 0,
         conflogsys  TYPE ppc_conflogsys,
         flg_info_dest TYPE  ppc_flg_info_dest,
         resb type ttyp_resb,
       END OF lt_tab.

  SELECT * INTO TABLE lt_res_hdr
           FROM ppc_res_hdr FOR ALL ENTRIES IN it_resb
           WHERE rsnum = it_resb-rsnum.
  LOOP AT lt_res_hdr INTO ls_res_hdr.
    READ TABLE lt_tab WITH KEY conflogsys = ls_res_hdr-conflogsys
                               flg_info_dest = ls_res_hdr-flg_info_dest.
    IF sy-subrc EQ 0.
      l_tabix = sy-tabix.
      LOOP AT it_resb INTO ls_resb.
        IF i_action = 'D'.
          CLEAR: ls_resb-bdmng, ls_resb-erfmg, ls_resb-enmng.
        ENDIF.
        APPEND ls_resb TO lt_tab-resb[].
      ENDLOOP.
      MODIFY lt_tab INDEX l_tabix.
    ELSE.
      lt_tab-conflogsys = ls_res_hdr-conflogsys.
      lt_tab-flg_info_dest = ls_res_hdr-flg_info_dest.
      REFRESH lt_tab-resb.
      LOOP AT it_resb INTO ls_resb.
        IF i_action = 'D'.
          CLEAR: ls_resb-bdmng, ls_resb-erfmg.
        ENDIF.
        APPEND ls_resb TO lt_tab-resb[].
      ENDLOOP.
      APPEND lt_tab.
    ENDIF.
  ENDLOOP.

  LOOP AT lt_tab.
    IF i_action = 'D'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_delete = lt_tab-resb[].
    ELSEIF i_action = 'U'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_change = lt_tab-resb[].
    ELSEIF i_action = 'I'.
      CALL FUNCTION 'PPC1IF_RESERV_DISTRIB'
           EXPORTING
                if_flt_val            = lt_tab-flg_info_dest
                if_logdestsys         = lt_tab-conflogsys
           TABLES
                it_dep_require_create = lt_tab-resb[].
    ENDIF.
  ENDLOOP.
ENDFORM.                    " send_reservation
*---------------------------------------------------------------------*
*       FORM create_new_resb                                          *
*---------------------------------------------------------------------*
FORM create_new_resb  USING    is_affw   TYPE affw
                      CHANGING et_resb_i type ttyp_resb
                               et_affw_u type ttyp_affw
                               et_output TYPE ty_tab_affwresb.
  DATA:
    ls_resb TYPE resb.

  CLEAR ls_resb.
* basically we have to create a new entry for is_affw-rsnum and -rspos
* but we have to check whether this entry has been already created
* by a previous affw entry -> to avoid duplicate_insert dumps
  READ TABLE et_resb_i
      INTO ls_resb
      WITH KEY rsnum = is_affw-rsnum
               rspos = is_affw-rspos.
  IF sy-subrc GT 0.
*   it is safe to create new resb entry
*   it does not exist in database, nor in resb_i
    CLEAR ls_resb.
    MOVE is_affw-rsnum TO ls_resb-rsnum.
    MOVE is_affw-rspos TO ls_resb-rspos.
    MOVE 'SB'          TO ls_resb-bdart.
    MOVE is_affw-matnr TO ls_resb-matnr.
    MOVE is_affw-werks TO ls_resb-werks.
    MOVE is_affw-lgort TO ls_resb-lgort.
    MOVE is_affw-prvbe TO ls_resb-prvbe.
    MOVE is_affw-charg TO ls_resb-charg.
    MOVE is_affw-ersda TO ls_resb-bdter.
    MOVE is_affw-ersda TO ls_resb-sbter.
    MOVE is_affw-erfmg TO ls_resb-bdmng.
    MOVE is_affw-erfme TO ls_resb-meins.
    MOVE is_affw-erfmg TO ls_resb-erfmg.
    MOVE is_affw-erfme TO ls_resb-erfme.
    MOVE is_affw-shkzg TO ls_resb-shkzg.
    MOVE is_affw-aufnr TO ls_resb-aufnr.
    MOVE is_affw-sobkz TO ls_resb-bwart.
    MOVE is_affw-bwart TO ls_resb-bwart.
*   and insert it into resb_i
    APPEND ls_resb TO et_resb_i.
  ELSE.
*   check if this new resb entry is consistent with the affw
    IF is_affw-matnr = ls_resb-matnr AND
           is_affw-werks = ls_resb-werks AND
           is_affw-lgort = ls_resb-lgort AND
           is_affw-charg = ls_resb-charg AND
           is_affw-sobkz = ls_resb-sobkz AND
           is_affw-bwart = ls_resb-bwart AND
           is_affw-erfme = ls_resb-erfme.
*      yes, consistent, then add the qty
      ADD is_affw-erfmg TO ls_resb-bdmng.
      ADD is_affw-erfmg TO ls_resb-erfmg.
      MODIFY et_resb_i INDEX sy-tabix FROM ls_resb.
    ELSE.
*     no, this refer to different material
*     CHANGE ALSO THE AFFW RSPOS because it used already by an other
*     AFFW item!!!
      MOVE 9999 TO is_affw-rspos.
*     check whether this entry is used (hope only by us)
      READ TABLE et_resb_i TRANSPORTING NO FIELDS
                         WITH KEY rsnum = is_affw-rsnum
                                  rspos = is_affw-rspos.
      WHILE sy-subrc IS INITIAL AND
          is_affw-rspos NE 1.
        is_affw-rspos = is_affw-rspos - 1.
        READ TABLE et_resb_i TRANSPORTING NO FIELDS
                             WITH KEY rsnum = is_affw-rsnum
                                      rspos = is_affw-rspos.
      ENDWHILE.
*     please process this entry asap with COGI
      IF is_affw-rspos NE 9999.
        IF is_affw-rspos EQ 1.
*         do not update RESB
*          WRITE: /1 'RESB should be checked manually',
*                    ls_resb-matnr,
*                    ls_resb-rsnum,
*                    ls_resb-rspos.
          EXIT.
        ENDIF.
*        WRITE: /1 'Please process this entry in COGI asap:',
*                  ls_resb-matnr,
*                  ls_resb-rsnum,
*                  ls_resb-rspos.
      ENDIF.
      PERFORM create_new_resb USING is_affw
                           CHANGING et_resb_i
                                    et_affw_u
                                    et_output.
*     update AFFW rspos
      APPEND is_affw TO et_affw_u.
    ENDIF.  "if the resb entry does not match
  ENDIF. "if the
ENDFORM.                    "create_new_resb
*---------------------------------------------------------------------*
*       FORM modif_wrong_resb                                         *
*---------------------------------------------------------------------*
FORM modif_wrong_resb USING    it_affw   type ttyp_affw
                      CHANGING cs_resb   TYPE resb
                               cs_affw   TYPE affw
                               et_resb_i type ttyp_resb
                               et_resb_u type ttyp_resb
                               et_affw_u type ttyp_affw
                               et_output TYPE ty_tab_affwresb.
  DATA:
    lf_weblnr TYPE affw-weblnr,
    lf_subrc  TYPE sysubrc.
* basically, we have resb which does not match affw
* we should modify it.
* before that, we must check whether or not this actual entry
* is used in any affw entry (in this case, we shouldn't modify it
* - instead, create a new matching entry.
  SELECT SINGLE weblnr FROM affw INTO lf_weblnr
      WHERE rsnum = cs_resb-rsnum AND
            rspos = cs_resb-rspos AND
            matnr = cs_resb-matnr AND
            werks = cs_resb-werks AND
            lgort = cs_resb-lgort AND
            charg = cs_resb-charg AND
            sobkz = cs_resb-sobkz AND
            bwart = cs_resb-bwart AND
            erfme = cs_resb-erfme AND
            flg_orig = '1'.
  MOVE sy-subrc TO lf_subrc.
  READ TABLE et_affw_u TRANSPORTING NO FIELDS
         WITH KEY rsnum = cs_resb-rsnum
                  rspos = cs_resb-rspos
                  matnr = cs_resb-matnr
                  werks = cs_resb-werks
                  lgort = cs_resb-lgort
                  charg = cs_resb-charg
                  sobkz = cs_resb-sobkz
                  bwart = cs_resb-bwart
                  erfme = cs_resb-erfme.
  IF sy-subrc LT lf_subrc.
    MOVE sy-subrc TO lf_subrc.
  ENDIF.
  IF lf_subrc EQ 0. "or in database, or in affw_u
*   CHANGE ALSO THE AFFW RSPOS because it used already by an other
*   AFFW item!!!
    MOVE 9999 TO cs_affw-rspos.
*   check whether this entry is used (hope only by us)
    READ TABLE et_resb_i TRANSPORTING NO FIELDS
                         WITH KEY rsnum = cs_affw-rsnum
                                  rspos = cs_affw-rspos.
    WHILE sy-subrc IS INITIAL AND
          cs_affw-rspos NE 1.
      cs_affw-rspos = cs_affw-rspos - 1.
      READ TABLE et_resb_i TRANSPORTING NO FIELDS
                           WITH KEY rsnum = cs_affw-rsnum
                                    rspos = cs_affw-rspos.
    ENDWHILE.
*   please process this entry asap with COGI
    IF cs_affw-rspos NE 9999.
      IF cs_affw-rspos EQ 1.
*       do not update RESB
        WRITE: /1 'RESB should be checked manually',
                  cs_resb-matnr,
                  cs_resb-rsnum,
                  cs_resb-rspos.
        EXIT.
      ENDIF.
      WRITE: /1 'Please process this entry in COGI asap:',
                cs_resb-matnr,
                cs_resb-rsnum,
                cs_resb-rspos.
    ENDIF.
    PERFORM create_new_resb USING    cs_affw
                            CHANGING et_resb_i
                                     et_affw_u
                                     et_output.
*   update AFFW rspos
    APPEND cs_affw TO et_affw_u.
  ELSE.
*   this resb entry is wrong, but not used anywhere else
*   then correct the entry!!
    MOVE cs_affw-matnr TO cs_resb-matnr.
    MOVE cs_affw-werks TO cs_resb-werks.
    MOVE cs_affw-lgort TO cs_resb-lgort.
    MOVE cs_affw-charg TO cs_resb-charg.
    MOVE cs_affw-sobkz TO cs_resb-sobkz.
    MOVE cs_affw-bwart TO cs_resb-bwart.
    MOVE cs_affw-erfme TO cs_resb-erfme.
    MOVE cs_affw-erfme TO cs_resb-meins.
    APPEND cs_resb TO et_resb_u.
  ENDIF.
ENDFORM.                    "modif_wrong_resb
*---------------------------------------------------------------------*
*       FORM modif_wrong_resb_with_q
*
*---------------------------------------------------------------------*
FORM modif_wrong_resb_with_q USING    if_quant  TYPE erfmg
                             CHANGING cs_resb   TYPE resb
                                      et_resb_u type ttyp_resb.
* just take the sum of quantity of the corresponding AFFW
  cs_resb-erfmg = if_quant.
  cs_resb-bdmng = if_quant.
  APPEND cs_resb TO et_resb_u.
ENDFORM.                    "modif_wrong_resb_with_q

*}   INSERT
*{   DELETE         UD1K921615                                        2
*\ENDFORM.                     "AUFRUF_BADI_COGI_POST
*}   DELETE
