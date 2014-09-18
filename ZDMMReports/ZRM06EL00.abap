REPORT rm06el00 NO STANDARD PAGE HEADING LINE-SIZE 81 MESSAGE-ID me.
*{SEL-OPT Begin} http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments KA5 20010927
INITIALIZATION.
  DATA: mgv_matnr_prog LIKE rsvar-report,
        mgv_matnr_selopt_tab LIKE rsldbdfs OCCURS 0 WITH HEADER LINE.
  FIELD-SYMBOLS <mgv_matnr_selopt_conv> TYPE STANDARD TABLE.
  mgv_matnr_prog = sy-repid.
  mgv_matnr_selopt_tab-name = 'S_MATNR' .
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
*{   INSERT         KA5K015999                                        1
  DATA:  conv_exit_check1 LIKE t130f-kzkma,
         conv_exit_check2 LIKE tka53-sign.
*}   INSERT
************************************************************************
*        Anzeigen Einkaufsbelege zum Lieferanten                       *
************************************************************************
* note 210280: LINE-SIZE 81
*----------------------------------------------------------------------*
*  Tabellen                                                            *
*----------------------------------------------------------------------*
  INCLUDE zfm06lto1.
*INCLUDE FM06LTO1.
*{   INSERT         KA5K015999                                        2
  TABLES: mara.
*}   INSERT

*----------------------------------------------------------------------*
*  Parameter und Select-Options                                        *
*----------------------------------------------------------------------*
  SELECT-OPTIONS:
    el_lifnr  FOR  ekko-lifnr MEMORY ID lif MATCHCODE OBJECT kred,
    el_ekorg  FOR  ekko-ekorg MEMORY ID eko.
  SELECTION-SCREEN FUNCTION KEY 1.
  PARAMETERS:
    el_selkb  LIKE ekko-bstyp NO-DISPLAY DEFAULT 'X',
    el_selkk  LIKE ekko-bstyp NO-DISPLAY DEFAULT 'X',
    el_selkl  LIKE ekko-bstyp NO-DISPLAY DEFAULT 'X',
    el_selka  LIKE ekko-bstyp NO-DISPLAY DEFAULT 'X'.
  INCLUDE zfm06lcs1.
*INCLUDE FM06LCS1.
  INCLUDE zfm06lcs3.
*INCLUDE FM06LCS3.
  INCLUDE zfm06lcs4.
*INCLUDE FM06LCS4.
  INCLUDE zfm06lcs2.
*INCLUDE FM06LCS2.
*{   INSERT         KA5K015999                                        3
  SELECT-OPTIONS:  s_mfrpn FOR mara-mfrpn MATCHCODE OBJECT htn.
*}   INSERT

*----------------------------------------------------------------------*
*  Hilfsfelder                                                         *
*----------------------------------------------------------------------*
  INCLUDE zfm06lcek.
*INCLUDE FM06LCEK.

*----------------------------------------------------------------------*
*  Intitialisierung                                                    *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR '001'.
*  perform anforderungsbild(sapfm06l) using el_selkb el_selkk
*                                           el_selkl el_selka.

  PERFORM anforderungsbild(sapfm06l) USING 'X'  ' '  'X'  ' '.


*----------------------------------------------------------------------*
*  Selektionsbild / Selection screen                                   *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON selpa.
  PERFORM selpa_analyse(sapfm06l).

AT SELECTION-SCREEN ON listu.
  PERFORM listumfang(sapfm06l) USING listu.

AT SELECTION-SCREEN.
  CALL FUNCTION 'ME_ITEM_CATEGORY_SELOPT_INPUT'
       TABLES
            ext_pstyp = s_pstyp
            int_pstyp = r_pstyp.
*  PERFORM bstyp_bestimmen(sapfm06l) USING el_selkb el_selkk
*                                       el_selkl el_selka.

  PERFORM bstyp_bestimmen(sapfm06l) USING 'X'  ' '  'X'  ' '.

AT SELECTION-SCREEN OUTPUT.
*  Andere Reports: modify_anfo ueber log. DB SAPDBxx dort wird
*  in aehnliche Form-Routine modify_anfo(sapfm06d) gesprungen!!!
  PERFORM modify_anfo(sapfm06l) USING 'L'.
*{   INSERT         KA5K001235                                        1
  CALL FUNCTION 'MPN01_CHECK_CONVEXT_ACTIVATED'
       IMPORTING
            convexit_activated = conv_exit_check1
            delimiter          = conv_exit_check2.

  IF conv_exit_check1 = ' '.
    LOOP AT SCREEN.
      IF screen-name EQ 'S_MFRPN-LOW' OR
         screen-name EQ 'S_MFRPN-HIGH' OR
         screen-name EQ '%_S_MFRPN_%_APP_%-TEXT' OR
         screen-name EQ '%_S_MFRPN_%_APP_%-OPTI_PUSH' OR
         screen-name EQ '%_S_MFRPN_%_APP_%-TO_TEXT' OR
         screen-name EQ '%_S_MFRPN_%_APP_%-VALU_PUSH'.
        screen-invisible = 1.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
*}   INSERT

*----------------------------------------------------------------------*
*  F4 auf dem Selektionsbild / F4 on the selection screen              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pstyp-low.

  CALL FUNCTION 'HELP_VALUES_EPSTP'
       EXPORTING
            program = sy-cprog
            dynnr   = sy-dynnr
            fieldname = 'S_PSTYP-LOW'
*            BSART   =
*            BSTYP   =
       IMPORTING
            epstp   = s_pstyp-low
       EXCEPTIONS
            OTHERS  = 1.

*----------------------------------------------------------------------*
*  Beginn der Selektion                                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*PERFORM PF_STATUS(SAPFM06L) USING EL_SELKB EL_SELKK
*                                  EL_SELKL EL_SELKA.

  PERFORM pf_status(sapfm06l) USING 'X'  ' '  'X'  ' '.


*  set pf-status 'LISM'.


*{   INSERT         KA5K001235                                        1
  LOOP AT s_mfrpn.
    MOVE-CORRESPONDING s_mfrpn TO r_mfrpn.
    APPEND r_mfrpn.
  ENDLOOP.
*}   INSERT
  SET TITLEBAR '001'.
  not_found = 'X'.

*- Lesen Belegköpfe --------------------------------------------------*
  SELECT * FROM ekko
     WHERE lifnr IN el_lifnr
       AND ekorg IN el_ekorg
       AND bstyp IN r_bstyp
       AND bsart IN s_bsart
       AND ekgrp IN s_ekgrp
       AND ebeln IN s_ebeln
       AND reswk IN s_reswk
       AND bedat IN s_bedat
     ORDER BY LIFNR bstyp EBELN.

*- Köpfe verarbeiten -------------------------------------------------*
    PERFORM ekko_merken(sapfm06l).

  ENDSELECT.

*- Köpfe verabeiten für letztes Belegnummernintervall ----------------*
  PERFORM zugriff_kopf(sapfm06l).
  PERFORM pos_nach_kopf(sapfm06l).

*----------------------------------------------------------------------*
*  Ende der Selektion                                                  *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM end_of_selection(sapfm06l).

*{   REPLACE        KA5K001235                                        1
*\WRITE /(81) SY-ULINE.
  ULINE AT /(output_line_width).
*}   REPLACE

*----------------------------------------------------------------------*
*  OK-Code-Eingabe                                                     *
*----------------------------------------------------------------------*
AT USER-COMMAND.
  PERFORM user_command(sapfm06l).

*----------------------------------------------------------------------*
*  Seitenueberschrift                                                  *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM top(sapfm06l).

TOP-OF-PAGE DURING LINE-SELECTION.
  IF sy-pfkey = 'EKAB'.
    PERFORM ekab_top(sapfm06l).
  ELSE.
    PERFORM top(sapfm06l).
  ENDIF.
