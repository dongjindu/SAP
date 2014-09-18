************************************************************************
* Program Name      : ZEMMPM42E_TOCR
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.02.04.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K906845
* Addl Documentation:
* Description       : Create Transfer Order from List
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.02.04.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT rls10034 NO STANDARD PAGE HEADING
                MESSAGE-ID l1.
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
*---------------------------------------------------------------------*
*       Umlagern per Mausklick                                        *
*       Listausgabe über FB mit ALV                                   *
*---------------------------------------------------------------------*
  INCLUDE <symbol>.

  TABLES: lagp ,                         " Lagerplätze
          lqua ,                         " Lagerquants
          ltak ,                         " Transportbedarfsköpfe
          ltap,                          " Transportbedarfspositionen
          lein ,                         " Lagereinheiten
          rlist,
          rl034,                         " Struktur für RLS10034
          rl03t,
          rldru,
          rl01s,                         " E/A-Felder für Stammdaten
          t333 ,
          t302 ,
          t300t,
          t301t,
          t329f,
          t300 ,
          t340d,                         " Sperrlogik in der Lagernummer
          t331 .                         " Lagertypsteuerung

*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - begin
*&<<<<-----------------------------------------------------------------*
  TABLES : zst_rl034,
           makt.
*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - end
*&<<<<-----------------------------------------------------------------*


*---------------------------------------------------------------------*
*        Konstanten                                                   *
*---------------------------------------------------------------------*

*......Farben..........................................................
  TYPES: farbe(3) TYPE c.

  CONSTANTS: white  TYPE farbe VALUE 'C20', " weiß, intensified off
             yellow TYPE farbe VALUE 'C30', " gelb, intensified off
             red    TYPE farbe VALUE 'C60', " rot,  intensified off
             green  TYPE farbe VALUE 'C50'. " grün, intensified off

*........Allgemeine LVS-Konstanten.....................................
*  INCLUDE  mllvskon.
  INCLUDE  zmllvskon.

*---------------------------------------------------------------------*
*        Hilfsfelder                                                  *
*---------------------------------------------------------------------*

  DATA:    sav_sy_repid          LIKE sy-repid,
           sav_lgpla             LIKE lqua-lgpla,
           sav_lgtyp             LIKE lqua-lgtyp,
           sav_lqnum             LIKE lqua-lqnum,
           sav_index             LIKE sy-index,
           sav_lenum             LIKE lqua-lenum,
           sav_tabix             LIKE sy-tabix,
           nach_lgtyp            LIKE lagp-lgtyp,
           nach_lgpla            LIKE lagp-lgpla,
           nach_plpos            LIKE lqua-plpos,
           flg_no_mark           TYPE c,
           flg_error             TYPE c,
           flg_diff              TYPE c,
           hlp_tanum             LIKE ltak-tanum,
           sonum                 LIKE lqua-sonum,
           con_maxalter(6)       TYPE c    VALUE '999999',
           dauer                 LIKE rl01s-dauer.

*---------------------------------------------------------------------*
*        Interne Tabellen                                             *
*---------------------------------------------------------------------*

*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/04 - begin
*&<<<<-----------------------------------------------------------------*
*  DATA: itab LIKE rl034 OCCURS 0 WITH HEADER LINE.
*&<<<<-----------------------------------------------------------------*
* block by stlim - 2004/02/04 - end
*&<<<<-----------------------------------------------------------------*

*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - begin
*&<<<<-----------------------------------------------------------------*
  DATA: itab LIKE zst_rl034 OCCURS 0 WITH HEADER LINE.
*  DATA : BEGIN OF itab OCCURS 0.
*          INCLUDE STRUCTURE rl034.
*  DATA :   cont_reg_numb1 LIKE leci_tra_dyn-cont_reg_numb1,
*         END OF itab.
*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - end
*&<<<<-----------------------------------------------------------------*

  DATA: BEGIN OF seltab OCCURS 0.
          INCLUDE STRUCTURE rsparams.
  DATA: END OF seltab.

  DATA: ltap_creat LIKE ltap_creat OCCURS 0 WITH HEADER LINE.

*---------------------------------------------------------------------*
*        Daten für den ALV                                            *
*---------------------------------------------------------------------*
  TYPE-POOLS: slis.

  DATA: xheader TYPE slis_t_listheader WITH HEADER LINE.

  DATA: variant LIKE disvariant.

*---------------------------------------------------------------------*
*        Sonstige Daten                                               *
*---------------------------------------------------------------------*
  DATA: returncode TYPE c.

  DATA: sicht TYPE c VALUE 'Q'.

  DATA: lnumt LIKE t300t-lnumt,
        ltypt LIKE t301t-ltypt.

*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*---------------------------------------------------------------------*
INITIALIZATION.

  SET TITLEBAR '002'.

*......Select-Einstellungen............................................

  SELECTION-SCREEN  BEGIN OF BLOCK xxx WITH FRAME TITLE text-100.

  SELECT-OPTIONS:  werks FOR lqua-werks NO INTERVALS NO-EXTENSION.

  SELECT-OPTIONS:  lgort FOR lqua-lgort NO INTERVALS NO-EXTENSION.

  SELECT-OPTIONS:  lgber FOR lagp-lgber NO INTERVALS NO-EXTENSION.

  SELECT-OPTIONS:  matnr FOR lqua-matnr.

  SELECT-OPTIONS:  bestq FOR lqua-bestq.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) text-081.
  PARAMETERS: sobkz LIKE rl01s-sobkz.
  PARAMETERS: lsonr LIKE rl01s-lsonr.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) text-080.
  PARAMETERS:   dauvo LIKE rlist-dauvo.
  SELECTION-SCREEN POSITION 58.
  PARAMETERS:   daubi LIKE rlist-daubi  DEFAULT '999999'.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK xxx.

  SELECTION-SCREEN BEGIN OF BLOCK zzz WITH FRAME TITLE text-102.

  SELECT-OPTIONS:  bwlvs FOR t333-bwlvs MEMORY ID bwa
                                        NO INTERVALS
                                        NO-EXTENSION
                                        OBLIGATORY
                                        DEFAULT '999'.
  PARAMETERS listv LIKE rl01s-listv.

  SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN BEGIN OF BLOCK yyy WITH FRAME TITLE text-101.

  PARAMETERS: p_quant RADIOBUTTON GROUP rad1,
              p_leinh RADIOBUTTON GROUP rad1,
              p_platz RADIOBUTTON GROUP rad1.

  SELECTION-SCREEN END OF BLOCK yyy.

  SELECTION-SCREEN END OF BLOCK zzz.

* selektieren des Parameter Lagertyp
  GET PARAMETER ID 'LGT' FIELD s1_lgtyp-low.
  APPEND s1_lgtyp.


*---------------------------------------------------------------------*
*        AT SELECTION SCREEN ON VALUE-REQUEST                         *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR listv.
  PERFORM f4_for_variant.

*---------------------------------------------------------------------*
*        AT-SELECTION-SCREEN                                          *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM check_variant_existence.

  IF sobkz IS INITIAL AND NOT lsonr IS INITIAL.
    MESSAGE e603.
*  Sonderbestandsnummer erfordert Eingabe des Sonderbestandskennzeichens
  ENDIF.

*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*
START-OF-SELECTION.

*Setzen des Parameter Lagertyp
  SET PARAMETER ID 'LGT' FIELD s1_lgtyp-low.


*........Prüfen, ob Lagereinheiten-Sicht sinnvoll......................
  IF p_leinh = con_x.
*    perform t331_lesen.           "Bei HU's muß das erlaubt sein
  ENDIF.

  CLEAR itab.
  REFRESH itab.

*---------------------------------------------------------------------*
*        GET LAGP                                                     *
*---------------------------------------------------------------------*
GET lagp.

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

*.........Sonderbestand gem. Selektion filtern.........................

  IF NOT sobkz IS INITIAL.
    CHECK lqua-sobkz = sobkz.
  ENDIF.
  IF NOT lsonr IS INITIAL.
    PERFORM sonum_conv_ext_int(sapfl000) USING sobkz
                                               lsonr
                                               sonum.
    CHECK lqua-sonum = sonum.
  ENDIF.

  CLEAR itab.
*........Platzposition in MISCH_PLATZ einmischen.......................

  IF lqua-plpos NE space.
    CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
         EXPORTING
              lgpla   = lqua-lgpla
              plpos   = lqua-plpos
         IMPORTING
              o_lgpla = itab-misch_platz.
  ELSE.
    itab-misch_platz = lqua-lgpla.
  ENDIF.
*......Daten in interne Tabelle übertragen.............................
  MOVE-CORRESPONDING lqua TO itab.
  MOVE-CORRESPONDING lagp TO itab.


*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - begin
*&<<<<-----------------------------------------------------------------*
  TABLES : ztmm_container.

  SELECT * FROM ztmm_container
          WHERE lgpla EQ lagp-lgpla
            AND kzler EQ space
       ORDER BY pass_date DESCENDING
*--- insert by stlim (2004/07/29)
                erdat     DESCENDING
                erzet     DESCENDING.
*--- end of insert
    MOVE : ztmm_container-cont_reg_numb1
                          TO itab-cont_reg_numb1.
    EXIT.
  ENDSELECT.
*&<<<<-----------------------------------------------------------------*
* insert by stlim - 2004/02/04 - end
*&<<<<-----------------------------------------------------------------*

**--- insert by stlim (2004/05/06)
  CLEAR : makt.
  SELECT SINGLE maktx INTO itab-maktx
                      FROM makt
                     WHERE matnr EQ lqua-matnr
                       AND spras EQ sy-langu.
**--- end of insert

*........doppelte Felder einzeln einfügen (aus LAGP und LQUA)..........
  MOVE: lqua-lgpla  TO itab-lqua_lgpla,
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
*........Farbe initialisieren..........................................
  MOVE white TO itab-farbe.

*........je nach Umlagerungs-Sicht: Ikonen hinzufügen..................

*........Lagerquantsicht aufbereiten...................................
*........(wird immer durchlaufen, da setzen der Ikonen auf Quantbasis..
*........erfolgt)......................................................
  PERFORM quant_pruefen.

  APPEND itab.


*---------------------------------------------------------------------*
*        END-OF-SELECTION                                             *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM read_blocking_logic CHANGING t340d-enque.

  IF p_leinh = con_x.
*........Lagereinheitensicht aufbereiten...............................
    sicht = 'L'.
    PERFORM leinh_pruefen.
  ENDIF.

  IF p_platz = con_x.
*........Lagerplatzsicht aufbereiten...................................
    sicht = 'P'.
    PERFORM platz_pruefen.
  ENDIF.

*........Listkopf aufbauen.............................................

  PERFORM listkopf_aufbauen.

*........Liste ausgeben................................................

  PERFORM ausgabe.

  LEAVE TO TRANSACTION sy-tcode.

***********************************************************************
*                                                                     *
*        FORM-Routinen                                                *
*                                                                     *
***********************************************************************

*---------------------------------------------------------------------*
*      Form  AUSGABE                                                  *
*---------------------------------------------------------------------*
*      Ausgabe der selektierten Daten über FB mit ALV                 *
*---------------------------------------------------------------------*
*  -->  ITAB      Selektierte Daten aus LAGP und LQUA
*----------------------------------------------------------------------*
FORM ausgabe.

*  sav_sy_repid = sy-repid.

  sav_sy_repid = 'RLS10034'.

  CALL FUNCTION 'ZL_QUANT_SELECT'
*  CALL FUNCTION 'L_QUANT_SELECT'
       EXPORTING
            i_repid   = sav_sy_repid
            i_title   = '001'
            i_status  = 'ALVSTAND'
            i_header  = xheader[]
            i_sicht   = sicht
            i_variant = variant
            i_str_name = 'ZST_Rl034'
            i_listv   = listv
       TABLES
            t_list    = itab
       EXCEPTIONS
            OTHERS    = 0.

ENDFORM.                               " AUSGABE
*---------------------------------------------------------------------*
*      Form  QUANT_PRUEFEN                                            *
*---------------------------------------------------------------------*
*      Quant prüfen und entsprechende Ikone setzen                    *
*---------------------------------------------------------------------*
*  -->  ITAB      Selektierte Quants                                  *
*  <--  ITAB      Selektierte Quants mit passender Ikone              *
*---------------------------------------------------------------------*
FORM quant_pruefen.

*........Quants sind zunächst alle markierbar..........................
  itab-ikone = sym_large_square.

*........Quants mit negativer verfügbarer Menge nicht markierbar!......
  IF itab-verme LE 0.
    itab-ikone = sym_locked.
    itab-msgid = 'L1'.
    itab-msgno = '604'.
    itab-msgty = 'E'.
  ENDIF.
*........Quants mit einzulagernden Mengen nicht markierbar!............
  IF itab-einme NE 0.
    itab-ikone = sym_locked.
    itab-msgid = 'L1'.
    itab-msgno = '605'.
    itab-msgty = 'E'.
  ENDIF.
*........Quants mit auszulagernden Mengen nicht markierbar!............
  IF itab-ausme NE 0.
    itab-ikone = sym_locked.
    itab-msgid = 'L1'.
    itab-msgno = '606'.
    itab-msgty = 'E'.
  ENDIF.
*........gesperrte Quants (Auslagerung, Inventur) nicht markierbar!....
  IF NOT itab-lqua_skzua IS INITIAL
    OR NOT itab-lqua_skzsa IS INITIAL
    OR NOT itab-lqua_skzsi IS INITIAL
    OR NOT itab-skzua IS INITIAL
    OR NOT itab-skzsa IS INITIAL
    OR NOT itab-skzsi IS INITIAL.
    itab-ikone = sym_locked.
    itab-msgid = 'L1'.
    itab-msgno = '607'.
    itab-msgty = 'E'.
  ENDIF.
*........Quants auf Lagereinheiten 'in Bewegung' nicht markierbar!.....
  IF NOT itab-lenum IS INITIAL.
    SELECT SINGLE * FROM lein WHERE lenum = itab-lenum.
    IF sy-subrc = 0 AND lein-statu NE ' '.
      itab-ikone = sym_locked.
      itab-msgid = 'L1'.
      itab-msgno = '608'.
      itab-msgty = 'E'.
    ENDIF.
  ENDIF.
*.......Bei Einstieg über LE-Sicht nur Quants mit Lenum(HU's)..........
  IF p_leinh = con_x AND
     itab-lenum IS INITIAL.
    itab-ikone = sym_locked.
    itab-msgid = 'L1'.
    itab-msgno = '205'.
    itab-msgty = 'E'.
  ENDIF.

ENDFORM.                               " QUANT_PRUEFEN
*---------------------------------------------------------------------*
*      Form  USER_COMMAND                                             *
*---------------------------------------------------------------------*
*      User-Commands abarbeiten                                       *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm.

  IF r_ucomm = 'UMLD' OR r_ucomm = 'UMLH'.
*........Prüfen, ob etwas markiert wurde...............................
    READ TABLE itab WITH KEY ikone = sym_checkbox
                    TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      MESSAGE e085.
*    Markieren Sie zuerst die zu bearbeitenden Positionen
    ENDIF.

*........Popup-Felder initialisieren...................................

    CLEAR: nach_lgtyp,
           nach_lgpla,
           lagp,
           lein,
           rldru,
           ltak,
           returncode.

    IF p_quant = 'X'.
*........Lagerquantsicht...............................................
      PERFORM quant_umlagern USING r_ucomm.
    ENDIF.

    IF p_leinh = 'X'.
*........Lagereinheitensicht...........................................
      PERFORM leinh_umlagern USING r_ucomm.
    ENDIF.

    IF p_platz = 'X'.
*........Lagerplatzsicht...............................................
      PERFORM platz_umlagern USING r_ucomm.
    ENDIF.

  ENDIF.

  IF r_ucomm = 'REFR'.
*........Liste auffrischen.............................................

    sav_sy_repid = sy-repid.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
         EXPORTING
              curr_report     = sav_sy_repid
         TABLES
              selection_table = seltab.
*           exceptions
*               not_found       = 1
*               no_report       = 2
*               others          = 3.

    IF sy-subrc = 0.
      SUBMIT (sav_sy_repid) WITH SELECTION-TABLE seltab.
    ENDIF.

  ENDIF.

ENDFORM.                               " USER_COMMAND
*---------------------------------------------------------------------*
*      Form  PLATZ_SPERREN                                            *
*---------------------------------------------------------------------*
*      Platz zur weiteren Verarbeitung sperren                        *
*---------------------------------------------------------------------*
*      -->P_ITAB_LGPLA  text                                          *
*---------------------------------------------------------------------*
FORM platz_sperren USING p_itab_lgpla.

  CALL FUNCTION 'L_BIN_LOCATION_ENQUEUE'
       EXPORTING
            i_lgnum       = s1_lgnum
            i_lgtyp       = s1_lgtyp-low
            i_lgpla       = p_itab_lgpla
            i_lenum       = ' '
            i_enque       = t340d-enque
       EXCEPTIONS
            error_message = 99.

ENDFORM.                               " PLATZ_SPERREN
*---------------------------------------------------------------------*
*      Form  ERROR_MESSAGE                                            *
*---------------------------------------------------------------------*
*      Fehlermeldung aufbereiten                                      *
*---------------------------------------------------------------------*
FORM error_message.

  itab-msgid = sy-msgid.
  itab-msgty = sy-msgty.
  itab-msgno = sy-msgno.
  itab-msgv1 = sy-msgv1.
  itab-msgv2 = sy-msgv2.
  itab-msgv3 = sy-msgv3.
  itab-msgv4 = sy-msgv4.

ENDFORM.                               " ERROR_MESSAGE
*---------------------------------------------------------------------*
*      Form  L_TO_CREATE_SINGLE                                       *
*---------------------------------------------------------------------*
*      Funktionsbaustein L_TO_CREATE_SINGLE aufrufen                  *
*---------------------------------------------------------------------*
FORM l_to_create_single.

  CALL FUNCTION 'L_TO_CREATE_SINGLE'
       EXPORTING
            i_lgnum               = itab-lgnum
            i_bwlvs               = bwlvs-low
*           I_BETYP               = ' '
*           I_BENUM               = ' '
            i_matnr               = itab-matnr
            i_werks               = itab-werks
            i_lgort               = itab-lgort
            i_charg               = itab-charg
            i_bestq               = itab-bestq
            i_sobkz               = itab-sobkz
            i_sonum               = itab-sonum
            i_letyp               = lein-letyp
            i_anfme               = itab-verme
            i_altme               = itab-meins
*           I_WDATU               = SY-DATLO
*           I_VFDAT               = INIT_DATUM
*           I_ZEUGN               = ' '
*           I_LZNUM               = ' '
            i_squit               = rl03t-squit
            i_nidru               = rldru-proto
            i_drukz               = rldru-drukz
            i_ldest               = rldru-ldest
*           I_WEMPF               = ' '
*           I_ABLAD               = ' '
            i_vltyp               = itab-lgtyp
*           I_VLBER               = ' '
            i_vlpla               = itab-lgpla
*           I_VPPOS               = ' '
*           I_VLENR               = ' '
            i_vlqnr               = itab-lqnum
            i_nltyp               = lagp-lgtyp
            i_nlber               = lagp-lgber
*           i_nlpla               = lagp-lgpla
            i_nlpla               = nach_lgpla
*           I_NPPOS               = ' '
            i_nppos               = nach_plpos
            i_nlenr               = lein-lenum
*           I_NLQNR               = ' '
*           I_RLTYP               = ' '
*           I_RLBER               = ' '
*           I_RLPLA               = ' '
*           I_RLQNR               = ' '
*           I_UPDATE_TASK         = ' '
            i_commit_work         = 'X'
*           I_BNAME               = SY-UNAME
*           I_KOMPL               = 'X'
       IMPORTING
            e_tanum               = itab-tanum
*           E_LTAP                =
       EXCEPTIONS
            error_message         = 99.

  IF sy-subrc EQ 99.
*........Fehler bei der TA-Erstellung aufgetreten......................
    itab-farbe = red.
    itab-ikone = sym_flash.
    PERFORM error_message.
  ELSE.
*........TA erzeugt....................................................
    itab-farbe = green.
    itab-ikone = sym_check_mark.
  ENDIF.

*........Enqueuesperren der LUW freigeben..............................
*    --> vgl. 0120024545 0001221580 1998 CSS...........................
  CALL FUNCTION 'DEQUEUE_ALL'.

ENDFORM.                               " L_TO_CREATE_SINGLE
*---------------------------------------------------------------------*
*      Form  QUANT_UMLAGERN                                           *
*---------------------------------------------------------------------*
*      Umlagern aus Lagerquantsicht (hell/dunkel)                     *
*---------------------------------------------------------------------*
FORM quant_umlagern USING r_ucomm.

*........Bei Hellablauf POPUP senden...................................
  PERFORM hell_dunkel USING r_ucomm.

  IF returncode = 'A'.
    EXIT.
  ENDIF.

*........Loop über alle markierten Einträge............................
  LOOP AT itab WHERE ikone = sym_checkbox.
*........Platz sperren.................................................
    PERFORM platz_sperren USING itab-lgpla.
    IF sy-subrc = 99.
*........Platz konnte nicht gesperrt werden............................
      PERFORM error_message.
      itab-ikone = sym_flash.
      itab-farbe = red.
    ELSE.
*........Platz gesperrt -> Daten nachlesen.............................
      SELECT SINGLE * FROM lqua WHERE lgnum = itab-lgnum
                                  AND lgtyp = itab-lgtyp
                                  AND lgpla = itab-lgpla
                                  AND lqnum = itab-lqnum.
      IF sy-subrc = 0.
*........Quant ist (noch) vorhanden -> Stimmen die Daten überein?......
        IF itab-gesme NE lqua-gesme OR
           itab-verme NE lqua-verme OR
           itab-einme NE lqua-einme OR
           itab-ausme NE lqua-ausme.
*........Daten stimmen nicht überein -> Mengen aktualisieren...........
          itab-gesme = lqua-gesme.
          itab-verme = lqua-verme.
          itab-einme = lqua-einme.
          itab-ausme = lqua-ausme.

          itab-farbe = red.
          itab-ikone = sym_flash.
          itab-msgid = 'L1'.
          itab-msgno = '609'.
          itab-msgty = 'E'.
        ELSE.
*........Daten stimmen überein -> TA erstellen.........................
          PERFORM l_to_create_single.
        ENDIF.
      ELSE.
*........Quant ist nicht mehr vorhanden -> Listzeile 'sperren'.........
        itab-farbe = red.
        itab-ikone = sym_locked.
        itab-msgid = 'L1'.
        itab-msgno = '610'.
        itab-msgty = 'E'.
      ENDIF.
    ENDIF.

    MODIFY itab.

  ENDLOOP.

ENDFORM.                               " QUANT_UMBUCHEN
*---------------------------------------------------------------------*
*      Module  STATUS_0100  OUTPUT                                    *
*---------------------------------------------------------------------*
*      Status für Popup setzen                                        *
*---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'POPUP'.
  SET TITLEBAR '003'.

ENDMODULE.                             " STATUS_0100  OUTPUT
*---------------------------------------------------------------------*
*      Module  USER_COMMAND_0100  INPUT                               *
*---------------------------------------------------------------------*
*      Funktionstasten im Popup bei 'Umlagern hell'                   *
*---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'GOON'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_0100  INPUT
*---------------------------------------------------------------------*
*      Module  EXIT_COMMAND  INPUT                                    *
*---------------------------------------------------------------------*
*      Exit im Popup bei 'Umlagern hell'                              *
*---------------------------------------------------------------------*
MODULE exit_command INPUT.

  CASE sy-ucomm.
    WHEN 'EESC'.
      returncode = 'A'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                             " EXIT_COMMAND  INPUT
*---------------------------------------------------------------------*
*      Form  LEINH_PRUEFEN                                            *
*---------------------------------------------------------------------*
*      Ikonen für Lagereinheiten-Sicht setzen                         *
*---------------------------------------------------------------------*
FORM leinh_pruefen.

*........Tabelle nach Lagereinheitennummer sortieren...................
  SORT itab BY lgnum lgtyp lenum.

  LOOP AT itab.
*........Bei jeder neuen LENUM, prüfen welche Ikone gesetzt werden muß.
    IF itab-lenum NE sav_lenum.
      sav_lenum = itab-lenum.
      CLEAR flg_no_mark.
      LOOP AT itab WHERE lenum = sav_lenum.
*........Für jedes Quant der LENUM prüfen, ob es 'gesperrt' ist........
        IF itab-ikone NE sym_large_square.
          flg_no_mark = con_x.
        ENDIF.
*........Ikonen der Quants löschen.....................................
        CLEAR itab-ikone.
        MODIFY itab TRANSPORTING ikone.
      ENDLOOP.
*........Ikone der Lagereinheit setzen.................................
      IF flg_no_mark = con_x.
        itab-ikone = sym_locked.
      ELSE.
        itab-ikone = sym_large_square.
      ENDIF.
      MODIFY itab TRANSPORTING ikone.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " LEINH_PRUEFEN
*---------------------------------------------------------------------*
*      Form  PLATZ_PRUEFEN                                            *
*---------------------------------------------------------------------*
*      Ikonen für Platz-Sicht setzen                                  *
*---------------------------------------------------------------------*
FORM platz_pruefen.

*........Tabelle nach Lagereinheitennummer sortieren...................
  SORT itab BY lgnum lgtyp misch_platz.

  LOOP AT itab.
*........Bei jedem neuen Platz prüfen, welche Ikone gesetzt werden muß.
    IF itab-lgpla NE sav_lgpla.
      sav_lgpla = itab-lgpla.
      CLEAR flg_no_mark.
      LOOP AT itab WHERE lgpla = sav_lgpla.
*........Für jedes Quant des Platzes prüfen, ob es 'gesperrt' ist......
        IF itab-ikone NE sym_large_square.
          flg_no_mark = con_x.
        ENDIF.
*........Ikonen der Quants löschen.....................................
        CLEAR itab-ikone.
        MODIFY itab TRANSPORTING ikone.
      ENDLOOP.
*........Ikone des Lagerplatzes setzen.................................
      IF flg_no_mark = con_x.
        itab-ikone = sym_locked.
      ELSE.
        itab-ikone = sym_large_square.
      ENDIF.
      MODIFY itab TRANSPORTING ikone.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " PLATZ_PRUEFEN
*---------------------------------------------------------------------*
*      Form  LEINH_UMLAGERN                                           *
*---------------------------------------------------------------------*
*      Lagereinheit umlagern                                          *
*---------------------------------------------------------------------*
FORM leinh_umlagern USING r_ucomm.

*........Bei Hellablauf POPUP senden...................................
  PERFORM hell_dunkel USING r_ucomm.

  IF returncode = 'A'.
    EXIT.
  ENDIF.

*........Loop über alle markierten Einträge............................
  LOOP AT itab WHERE ikone = sym_checkbox.
    CLEAR flg_diff.
    CLEAR flg_error.
    sav_lenum = itab-lenum.
    sav_index = sy-tabix.
*........Platz sperren.................................................
    PERFORM platz_sperren USING itab-lgpla.
    IF sy-subrc = 99.
*........Platz konnte nicht gesperrt werden............................
      itab-ikone = sym_flash.
      PERFORM error_message.
      CLEAR hlp_tanum.
      MODIFY itab.
      PERFORM le_faerben USING red hlp_tanum.
    ELSE.
*........Platz gesperrt -> Daten nachlesen.............................
*........1) Abgleich: ITAB -> LQUA.....................................
      LOOP AT itab WHERE lenum = sav_lenum.
        SELECT SINGLE * FROM lqua WHERE lgnum = itab-lgnum
                                    AND lgtyp = itab-lgtyp
                                    AND lgpla = itab-lgpla
                                    AND lenum = itab-lenum
                                    AND lqnum = itab-lqnum.
        IF sy-subrc NE 0.
*........Quant nicht mehr in LQUA vorhanden -> Fehler..................
          flg_error = con_x.
        ELSE.
*........Quant in LQUA vorhanden.......................................
          IF itab-gesme NE lqua-gesme OR
             itab-verme NE lqua-verme OR
             itab-einme NE lqua-einme OR
             itab-ausme NE lqua-ausme.
*........Daten stimmen nicht überein -> Mengen aktualisieren...........
            itab-gesme = lqua-gesme.
            itab-verme = lqua-verme.
            itab-einme = lqua-einme.
            itab-ausme = lqua-ausme.
            flg_diff = con_x.
          ENDIF.
        ENDIF.
      ENDLOOP.                         " innere Schleife über ITAB
*........2) Abgleich: LQUA -> ITAB.....................................
      SELECT * FROM lqua WHERE lgnum = s1_lgnum
                           AND lgtyp = s1_lgtyp-low
                           AND lenum = sav_lenum.
        READ TABLE itab WITH KEY lgnum = lqua-lgnum
                                 lgtyp = lqua-lgtyp
                                 lgpla = lqua-lgpla
                                 lenum = lqua-lenum
                                 lqnum = lqua-lqnum
                                 TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
*........Quant nicht in ITAB vorhanden.................................
          flg_error = con_x.
        ENDIF.
      ENDSELECT.

      IF flg_diff = con_x.
        CLEAR hlp_tanum.
        PERFORM le_faerben USING red hlp_tanum.
        itab-msgid = 'L1'.
        itab-msgno = '609'.
        itab-msgty = 'E'.
        itab-farbe = red.
        itab-ikone = sym_flash.
      ENDIF.

      IF flg_error = con_x.
        CLEAR hlp_tanum.
        PERFORM le_faerben USING red hlp_tanum.
        itab-msgid = 'L1'.
        itab-msgno = '610'.
        itab-msgty = 'E'.
        itab-farbe = red.
        itab-ikone = sym_locked.
      ENDIF.

      IF flg_error NE con_x AND flg_diff NE con_x.
*........keine Fehler und Unstimmigkeiten aufgetreten -> TA erstellen..
        PERFORM l_to_create_move_su.
        IF sy-subrc = 99.
*........Fehler bei der TA-Erstellung aufgetreten......................
*         perform error_message.
          CLEAR hlp_tanum.
          PERFORM le_faerben USING red hlp_tanum.
          itab-ikone = sym_flash.
          itab-farbe = red.
          PERFORM error_message.
        ELSE.
*........TA erfolgreich erstellt.......................................
          PERFORM le_faerben USING green hlp_tanum.
          itab-ikone = sym_check_mark.
          itab-farbe = green.
          itab-tanum = hlp_tanum.
        ENDIF.
*........Enqueuesperren der LUW freigeben..............................
*     -->vgl. 0120024545 0001221580 1998 CSS)..........................
        CALL FUNCTION 'DEQUEUE_ALL'.

      ENDIF.

      MODIFY itab TRANSPORTING ikone farbe tanum msgid msgty
                               msgno msgv1 msgv2 msgv3 msgv4.

    ENDIF.                             " Sperren

  ENDLOOP.                             " selektierte LEs

ENDFORM.                               " LEINH_UMLAGERN
*---------------------------------------------------------------------*
*      Form  LE_FAERBEN                                               *
*---------------------------------------------------------------------*
*      Alle Zeilen zu einer Lagereinheit färben                       *
*---------------------------------------------------------------------*
FORM le_faerben USING p_farbe p_tanum.

  sav_lenum = itab-lenum.

  WHILE sav_lenum = itab-lenum.
    itab-farbe = p_farbe.
    itab-tanum = p_tanum.
    MODIFY itab INDEX sav_index TRANSPORTING farbe tanum.
    sav_index = sav_index + 1.
    READ TABLE itab INDEX sav_index.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDWHILE.

ENDFORM.                               " LE_FAERBEN
*---------------------------------------------------------------------*
*      Form  PLATZ_UMLAGERN                                           *
*---------------------------------------------------------------------*
*      Gesamten Platz umlagern                                        *
*---------------------------------------------------------------------*
FORM platz_umlagern USING r_ucomm.

*........Bei Hellablauf POPUP senden...................................
  PERFORM hell_dunkel USING r_ucomm.

  IF returncode = 'A'.
    EXIT.
  ENDIF.

*........Loop über alle markierten Einträge............................
  LOOP AT itab WHERE ikone = sym_checkbox.
    CLEAR flg_diff.
    CLEAR flg_error.
    sav_lgpla = itab-lgpla.
    sav_index = sy-tabix.
*........Platz sperren.................................................
    PERFORM platz_sperren USING itab-lgpla.
    IF sy-subrc = 99.
*........Platz konnte nicht gesperrt werden............................
      itab-ikone = sym_flash.
      PERFORM error_message.
      CLEAR hlp_tanum.
      MODIFY itab.
      PERFORM platz_faerben USING red hlp_tanum.
    ELSE.
*........Platz gesperrt -> Daten nachlesen.............................
*........1) Abgleich: ITAB -> LQUA.....................................
      LOOP AT itab WHERE lgpla = sav_lgpla.
        SELECT SINGLE * FROM lqua WHERE lgnum = itab-lgnum
                                    AND lgtyp = itab-lgtyp
                                    AND lgpla = itab-lgpla
                                    AND lqnum = itab-lqnum.
        IF sy-subrc NE 0.
*........Quant nicht mehr in LQUA vorhanden -> Fehler..................
          flg_error = con_x.
        ELSE.
*........Quant in LQUA vorhanden.......................................
          IF itab-gesme NE lqua-gesme OR
             itab-verme NE lqua-verme OR
             itab-einme NE lqua-einme OR
             itab-ausme NE lqua-ausme.
*........Daten stimmen nicht überein -> Mengen aktualisieren...........
            itab-gesme = lqua-gesme.
            itab-verme = lqua-verme.
            itab-einme = lqua-einme.
            itab-ausme = lqua-ausme.
            flg_diff = con_x.
          ENDIF.
        ENDIF.
      ENDLOOP.                         " innere Schleife über ITAB
*........2) Abgleich: LQUA -> ITAB.....................................
      SELECT * FROM lqua WHERE lgnum = s1_lgnum
                           AND lgtyp = s1_lgtyp-low
                           AND lgpla = sav_lgpla.
        READ TABLE itab WITH KEY lgnum = lqua-lgnum
                                 lgtyp = lqua-lgtyp
                                 lgpla = lqua-lgpla
                                 lqnum = lqua-lqnum
                                 TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
*........Quant nicht in ITAB vorhanden.................................
          flg_error = con_x.
        ENDIF.
      ENDSELECT.

      IF flg_diff = con_x.
        CLEAR hlp_tanum.
        PERFORM platz_faerben USING red hlp_tanum.
        itab-msgid = 'L1'.
        itab-msgno = '609'.
        itab-msgty = 'E'.
        itab-farbe = red.
        itab-ikone = sym_flash.
      ENDIF.

      IF flg_error = con_x.
        CLEAR hlp_tanum.
        PERFORM platz_faerben USING red hlp_tanum.
        itab-msgid = 'L1'.
        itab-msgno = '610'.
        itab-msgty = 'E'.
        itab-farbe = red.
        itab-ikone = sym_locked.
      ENDIF.

      IF flg_error NE con_x AND flg_diff NE con_x.
*........keine Fehler oder Unstimmigkeiten aufgetreten -> TA erstellen.
        PERFORM l_to_create_multiple.
        IF sy-subrc = 99.
*........Fehler bei der TA-Erstellung aufgetreten......................
*         perform error_message.
          CLEAR hlp_tanum.
          PERFORM platz_faerben USING red hlp_tanum.
          itab-ikone = sym_flash.
          itab-farbe = red.
          PERFORM error_message.
        ELSE.
*........TA erfolgreich erstellt.......................................
          PERFORM platz_faerben USING green hlp_tanum.
          itab-ikone = sym_check_mark.
          itab-farbe = green.
          itab-tanum = hlp_tanum.
        ENDIF.
*........Enqueuesperren der LUW freigeben..............................
*     -->vgl. 0120024545 0001221580 1998 CSS)..........................
        CALL FUNCTION 'DEQUEUE_ALL'.
      ENDIF.

      MODIFY itab TRANSPORTING ikone farbe tanum msgid msgty
                               msgno msgv1 msgv2 msgv3 msgv4.

    ENDIF.                             " Sperren

  ENDLOOP.                             " selektierte LEs

ENDFORM.                               " PLATZ_UMLAGERN
*---------------------------------------------------------------------*
*      Form  PLATZ_FAERBEN                                            *
*---------------------------------------------------------------------*
*      Alle Zeilen zu einem Platz färben                              *
*---------------------------------------------------------------------*
FORM platz_faerben USING p_farbe p_tanum.

  sav_lgpla = itab-lgpla.

  WHILE sav_lgpla = itab-lgpla.
    itab-farbe = p_farbe.
    itab-tanum = p_tanum.
    MODIFY itab INDEX sav_index TRANSPORTING farbe tanum.
    sav_index = sav_index + 1.
    READ TABLE itab INDEX sav_index.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDWHILE.

ENDFORM.                               " PLATZ_FAERBEN
*---------------------------------------------------------------------*
*      Form  T331_LESEN                                               *
*---------------------------------------------------------------------*
*      Prüfen, ob Lagertyp LE-verwaltet ist                           *
*---------------------------------------------------------------------*
FORM t331_lesen.

  SELECT SINGLE * FROM t331 WHERE lgnum = s1_lgnum
                              AND lgtyp = s1_lgtyp-low.
  IF t331-lenvw NE con_x.
    MESSAGE s602 WITH s1_lgtyp-low.
*    Lagereinheitenverwaltung ist in Lagertyp & nicht aktiv
    SET SCREEN 0.
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

ENDFORM.                               " T331_LESEN
*---------------------------------------------------------------------*
*      Form  L_TO_CREATE_MOVE_SU                                      *
*---------------------------------------------------------------------*
*      FB L_TO_CREATE_MOVE_SU aufrufen                                *
*---------------------------------------------------------------------*
FORM l_to_create_move_su.

  CALL FUNCTION 'L_TO_CREATE_MOVE_SU'
       EXPORTING
            i_lenum               = itab-lenum
            i_bwlvs               = bwlvs-low
*           I_LZNUM               = ' '
            i_nltyp               = nach_lgtyp
            i_nlber               = lagp-lgber
            i_nlpla               = nach_lgpla
*           I_NPPOS               = ' '
            i_nppos               = nach_plpos
            i_squit               = rl03t-squit
            i_letyp               = lein-letyp
            i_nidru               = rldru-proto
            i_drukz               = rldru-drukz
            i_ldest               = rldru-ldest
*           I_UPDATE_TASK         = ' '
            i_commit_work         = con_x
*           I_BNAME               = SY-UNAME
       IMPORTING
            e_tanum               = hlp_tanum
*           E_NLTYP               =
*           E_NLBER               =
*           E_NLPLA               =
*           E_NPPOS               =
       EXCEPTIONS
            error_message         = 99.

ENDFORM.                               " L_TO_CREATE_MOVE_SU
*---------------------------------------------------------------------*
*      Form  HELL_DUNKEL                                              *
*---------------------------------------------------------------------*
*      Hellablauf -> Popup senden                                     *
*---------------------------------------------------------------------*
FORM hell_dunkel USING r_ucomm.

  IF r_ucomm = 'UMLH'.
*........Bei 'Umlagern hell' zuerst Eingabe-Popup senden...............

    lagp-lgnum = s1_lgnum.
    ltak-lgnum = s1_lgnum.
    lein-lgnum = s1_lgnum.
    MOVE bwlvs-low TO ltak-bwlvs.      " BWA vorbelegen

    CALL SCREEN 0100 STARTING AT 5 5 ENDING AT 41 16.

    IF returncode = 'A'.
*......Abbruch; keine Aktion notwendig.................................
      EXIT.
    ELSE.
*......Bewegungsart übernehmen..........................................
      MOVE ltak-bwlvs TO bwlvs-low.
    ENDIF.
  ENDIF.

ENDFORM.                               " HELL_DUNKEL
*---------------------------------------------------------------------*
*      Form  L_TO_CREATE_MULTIPLE                                     *
*---------------------------------------------------------------------*
*      FB L_TO_CREATE_MULTIPLE aufrufen                               *
*---------------------------------------------------------------------*
FORM l_to_create_multiple.

  CLEAR ltap_creat.
  REFRESH ltap_creat.

*........Daten der umzulagernden Quants in LTAP_CREAT schreiben........
  LOOP AT itab WHERE lgpla = sav_lgpla.
    MOVE: itab-werks  TO ltap_creat-werks, " VON-Daten
          itab-lgort  TO ltap_creat-lgort,
          itab-matnr  TO ltap_creat-matnr,
          itab-charg  TO ltap_creat-charg,
          itab-bestq  TO ltap_creat-bestq,
          itab-sobkz  TO ltap_creat-sobkz,
          itab-sonum  TO ltap_creat-sonum,
          itab-verme  TO ltap_creat-anfme,
          itab-meins  TO ltap_creat-altme,
          itab-lqnum  TO ltap_creat-vlqnr,
          itab-lenum  TO ltap_creat-vlenr,
          nach_lgpla  TO ltap_creat-nlpla, " NACH-Daten
          nach_plpos  TO ltap_creat-nppos,
          nach_lgtyp  TO ltap_creat-nltyp,
          lagp-lgber  TO ltap_creat-nlber,
          rldru-ldest TO ltap_creat-ldest,
          lein-lenum  TO ltap_creat-nlenr,
          lein-letyp  TO ltap_creat-letyp,
          rl03t-squit TO ltap_creat-squit.
    APPEND ltap_creat.
  ENDLOOP.

*........Funktionsbaustein aufrufen....................................
  CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
       EXPORTING
            i_lgnum                = s1_lgnum
            i_bwlvs                = bwlvs-low
*           I_BETYP                = ' '
*           I_BENUM                = ' '
*           I_LZNUM                = ' '
            i_nidru                = rldru-proto
            i_drukz                = rldru-drukz
*           I_UPDATE_TASK          = ' '
            i_commit_work          = 'X'
*           I_BNAME                = SY-UNAME
*           I_KOMPL                = 'X'
*           I_SOLEX                = 0
*           I_PERNR                = 0
*           I_MINWM                = ' '
       IMPORTING
            e_tanum                = hlp_tanum
       TABLES
            t_ltap_creat           = ltap_creat[]
       EXCEPTIONS
            error_message          = 99.


ENDFORM.                               " L_TO_CREATE_MULTIPLE
*---------------------------------------------------------------------*
*      Form  LISTKOPF_AUFBAUEN                                        *
*---------------------------------------------------------------------*
*      Listkopf für ALV aufbauen                                      *
*---------------------------------------------------------------------*
FORM listkopf_aufbauen.

  CLEAR xheader.
  REFRESH xheader.

*........Selektionsinfo................................................

  xheader-typ  = 'S'.
  xheader-key  = text-004.
  xheader-info = s1_lgnum.
  APPEND xheader.

  xheader-typ  = 'S'.
  xheader-key  = text-005.
  xheader-info = s1_lgtyp-low.
  APPEND xheader.

ENDFORM.                               " LISTKOPF_AUFBAUEN
*---------------------------------------------------------------------*
*      Form  F4_FOR_VARIANT                                           *
*---------------------------------------------------------------------*
*      F4-Hilfe für Anzeigevarianten                                  *
*---------------------------------------------------------------------*
FORM f4_for_variant.

  variant-report = 'SAPLL01L'.
  variant-handle = 'LT10'.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant = variant
            i_save     = 'A'
       IMPORTING
            es_variant = variant
       EXCEPTIONS
            not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE s205(0k).
*    Keine Auswahl vorhanden!
  ELSE.
    listv = variant-variant.
  ENDIF.
ENDFORM.                               " F4_FOR_VARIANT
*---------------------------------------------------------------------*
*      Form  CHECK_VARIANT_EXISTENCE                                  *
*---------------------------------------------------------------------*
*      Prüfen, ob eingegebene Anzeigevariante vorhanden ist           *
*---------------------------------------------------------------------*
FORM check_variant_existence.

  IF NOT listv IS INITIAL.
    MOVE: listv      TO variant-variant,
          'LT10'     TO variant-handle,
          'SAPLL01L' TO variant-report.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              i_save     = 'A'
         CHANGING
              cs_variant = variant.
  ENDIF.

ENDFORM.                               " CHECK_VARIANT_EXISTENCE
*---------------------------------------------------------------------*
*      Module  CHECK_IMMEDIATE_CONFIRMATION  INPUT                    *
*---------------------------------------------------------------------*
*      Prüfen, ob bei ausgewähltere BWA Sofortquittierung erlaubt     *
*---------------------------------------------------------------------*
MODULE check_immediate_confirmation INPUT.

  IF t333-squit NE con_x AND rl03t-squit EQ con_x.
    MESSAGE e181(l3) WITH ltak-bwlvs.
*    Bewegungsart & erlaubt keine Sofortquittierung
  ENDIF.

ENDMODULE.                 " CHECK_IMMEDIATE_CONFIRMATION  INPUT
*---------------------------------------------------------------------*
*      Module  CHECK_BIN_EXISTENCE  INPUT                             *
*---------------------------------------------------------------------*
*      Prüfe, ob ausgewählter Lagerplatz im Lagertyp existiert        *
*---------------------------------------------------------------------*
MODULE check_bin_existence INPUT.
  IF NOT lagp-lgpla IS INITIAL.
    IF lagp-lgtyp IS INITIAL.
*........bei Lagerplatzeingabe ist Lagertypeingabe zwingend............
      MESSAGE e038(l2).
*   Lagerplatz ohne Lagertyp nicht sinnvoll
    ENDIF.

    CALL FUNCTION 'L_PLATZ_POSITION_TRENNEN'
         EXPORTING
              lgnum     = lagp-lgnum
              lgtyp     = lagp-lgtyp
              lgpla     = lagp-lgpla
         IMPORTING
              o_lgpla   = nach_lgpla
              o_plpos   = nach_plpos
         EXCEPTIONS
              not_found = 1
              overflow  = 2
              OTHERS    = 3.
    IF sy-subrc = 0.
*........nichts tun....................................................
    ELSEIF sy-subrc = 1.
*........Platz ohne Veränderung übernehmen.............................
      nach_lgpla = lagp-lgpla.
      CLEAR nach_plpos.
    ELSE.
*........Fehlermeldung aus FB ausgeben.................................
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SELECT SINGLE * FROM lagp WHERE lgnum = lagp-lgnum
                                AND lgtyp = lagp-lgtyp
                                AND lgpla = nach_lgpla.
    IF sy-subrc NE 0.
      MESSAGE e007 WITH nach_lgpla.
*    Lagerplatz & ist nicht vorhanden
    ENDIF.
  ENDIF.
  nach_lgtyp = lagp-lgtyp.
* nach_lgpla = lagp-lgpla.




ENDMODULE.                             " CHECK_BIN_EXISTENCE  INPUT
*---------------------------------------------------------------------*
*      Module  CHECK_TYPE_SPECIFIED  INPUT                            *
*---------------------------------------------------------------------*
*      Prüfe, ob bei Bereichseingabe Lagertyp vorgegeben wurde        *
*---------------------------------------------------------------------*
MODULE check_type_specified INPUT.

  IF NOT lagp-lgber IS INITIAL.
    IF lagp-lgtyp IS INITIAL.
      MESSAGE e032.
*    Lagerbereich ohne Lagertyp nicht sinnvoll
    ELSE.
      SELECT SINGLE * FROM t302 WHERE lgnum = s1_lgnum
                                  AND lgtyp = lagp-lgtyp
                                  AND lgber = lagp-lgber.
      IF sy-subrc NE 0.
        MESSAGE e250 WITH s1_lgnum lagp-lgtyp lagp-lgber.
*    Lagerbereich & & & ist nicht vorhanden
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                             " CHECK_TYPE_SPECIFIED  INPUT
*---------------------------------------------------------------------*
*      Module  CHECK_MOVEMENT_TYPE  INPUT                             *
*---------------------------------------------------------------------*
*      Prüfen, ob Bewegungsart vorhanden ist                          *
*---------------------------------------------------------------------*
MODULE check_movement_type INPUT.

  SELECT SINGLE * FROM t333 WHERE lgnum = s1_lgnum
                              AND bwlvs = ltak-bwlvs.
  IF sy-subrc NE 0.
    MESSAGE e001(l3) WITH ltak-bwlvs.
*   Bewegungsart & ist nicht vorgesehen (Eingabe prüfen)
  ENDIF.

ENDMODULE.                             " CHECK_MOVEMENT_TYPE  INPUT
*&---------------------------------------------------------------------*
*&      Form  READ_BLOCKING_LOGIC
*&---------------------------------------------------------------------*
*       Read blocking logic of storage number
*----------------------------------------------------------------------*
*      <--P_T340D_ENQUEUE  blocking logic indicator
*----------------------------------------------------------------------*
FORM read_blocking_logic CHANGING t340d_enque.

  SELECT SINGLE * FROM t340d WHERE lgnum = s1_lgnum.

ENDFORM.                    " READ_BLOCKING_LOGIC
