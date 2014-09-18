
*---------------------------------------------------------------------*
*       Modulpool ML01SO00: Process-Before-Output zu SAPML01S         *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         D0103_DEFAULTWERTE      Einstellen von Defaultwerten        *
*         D0105_DEFAULTWERTE      Einstellen von Defaultwerten        *
*         D0203_SONUM_LSONR_CONV  Konvertierung Sonderbestandsnummer  *
*         D0209_DEFAULTWERTE      Einstellen von Defaultwerten        *
*         INVENTUR_ZEIGEN         Anzeigen der Inventurdaten          *
*         PFSTATUS_SETZEN         Setzen des PF-Status                *
*         PFSTATUS_WIND           Setzen des PF-Status 'WIND'         *
*         PLATZ_POSITION_MISCH    Mischen von Platzposition und Platz *
*         BASISREGELN             Bildschirmmodif. nach Basisregeln   *
*         T330T_LESEN_LEIN        Sperrgrundtext einlesen             *
*         T331_LESEN              Lagertypsteuerung lesen             *
*         PLATZ_POSITION_MI_LE      Mischen von Platzposition und Platz
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       MODULE D0103_DEFAULTWERTE                                     *
*---------------------------------------------------------------------*
*       Einstellen der Defaultwerte in Dynpro 103                     *
*---------------------------------------------------------------------*
MODULE d0103_defaultwerte OUTPUT.
  rl01s-anzlp = con_anzlp_default.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE D0106_DEFAULTWERTE                                     *
*---------------------------------------------------------------------*
*       Einstellen der Defaultwerte in Dynpro 106                     *
*---------------------------------------------------------------------*
MODULE d0105_defaultwerte OUTPUT.
  rl01s-mappe = con_mappe_default.
ENDMODULE.

*----------------------------------------------------------------------*
*       MODULE  D0203_SONUM_LSONR_CONV                                 *
*----------------------------------------------------------------------*
*       Konvertierung der Sonderbestandsnummer vom Datenbankformat     *
*       in Dynproformat.                                               *
*       Im Parameter SOBKZ wird das Sonderbestandskennzeichen          *
*                 in SONUM die Sonderbestandskennzeichen in Datenbank- *
*                          format übergeben.                           *
*       In LSONR wird die Sonderbestandsnummer in Dynproformat zurück- *
*                gegeben                                               *
*----------------------------------------------------------------------*
MODULE d0203_sonum_lsonr_conv OUTPUT.

  PERFORM  sonum_conv_int_ext(sapfl000)
                              USING lqua-sobkz lqua-sonum rl01s-lsonr.

ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE D0209_DEFAULTWERTE                                     *
*---------------------------------------------------------------------*
*       Einstellen der Defaultwerte in Dynpro 209                     *
*---------------------------------------------------------------------*
MODULE d0209_defaultwerte OUTPUT.
  rl01s-bestq = con_stern.
  rl01s-sobkz = con_stern.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE INVENTUR_ZEIGEN                                        *
*---------------------------------------------------------------------*
*       Ausgabe des Inventurtextes je nacu KZINV-Kennzeichen          *
*---------------------------------------------------------------------*
MODULE inventur_zeigen OUTPUT.
  CASE lagp-kzinv.
    WHEN kzinv_pe.
      MOVE text-002 TO inventurtext.   "Permanente Inventur
    WHEN kzinv_st.
      MOVE text-003 TO inventurtext.   "Stichtagsinventur
    WHEN kzinv_pn.
      MOVE text-004 TO inventurtext.   "Inventur durch Nullkontrolle
    WHEN init_kzinv.
      MOVE text-005 TO inventurtext.   "Nicht inventiert
  ENDCASE.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PFSTATUS_SETZEN                                        *
*---------------------------------------------------------------------*
*       Setzen des PF-Status, der in T340 definiert wurde             *
*---------------------------------------------------------------------*
MODULE pfstatus_setzen OUTPUT.
* UNPACK SY-DYNNR TO PFSTATUS.
* SHIFT PFSTATUS.
* PFSTATUS+3(1) = T340-TRTYP.
* SET PF-STATUS PFSTATUS.
* SET PF-STATUS T340-TRTYP.  "RODE TEST TEMPORAER
  MOVE sy-repid TO sav_repid.

  PERFORM pfstatus_setzen(sapfl000)
          USING sy-cprog sy-dynnr t340-trtyp t342-vorga t342-aktyp.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PFSTATUS_WIND                                          *
*---------------------------------------------------------------------*
*       Setzen des PF-Status 'WIND' für Windows                       *
*---------------------------------------------------------------------*
*ODULE PFSTATUS_WIND OUTPUT.
* SET PF-STATUS PFSTATUS_WIND.
*NDMODULE.


*---------------------------------------------------------------------*
*       MODULE PLATZ_POSITION_MISCH                                   *
*---------------------------------------------------------------------*
*       Mischen von Position und Platz in die Anzeige Lagerplatz      *
*---------------------------------------------------------------------*
MODULE platz_position_misch OUTPUT.
  IF lqua-plpos NE space.
    CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
         EXPORTING
              lgpla   = lqua-lgpla
              plpos   = lqua-plpos
         IMPORTING
              o_lgpla = lqua-lgpla.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE BASISREGELN                                            *
*---------------------------------------------------------------------*
*       Spezielle Feldauswahl anhand SCREEN-GROUP2.                   *
*       Die im Dynpro eingetragenen Werte spezifizieren die           *
*       Unterprogramme, die die Bildschirmmodifikation des Feldes     *
*       vornehmen.                                                    *
*---------------------------------------------------------------------*
MODULE basisregeln OUTPUT.
  LOOP AT SCREEN.
    IF screen-group2 NE space.
      PERFORM screen-group2 OF br001
                               br002
                               br003
                               br004
                               br005
                               br006
                               br007
                               br008
                               br009
                               br010
                               br011
                               br012.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T330T_LESEN_LEIN OUTPUT                                *
*---------------------------------------------------------------------*
*       Lesen der Sperrgrundbezeichnung aus Tabelle T330T             *
*---------------------------------------------------------------------*
MODULE t330t_lesen_lein OUTPUT.
  IF lein-spgru NE init_spgru.
    PERFORM t330t_lesen USING lein-lgnum lein-spgru.
    IF sy-subrc NE 0.
      MESSAGE w010 WITH lein-spgru sy-langu.   "Bezeichnung zu Sperrgr.
    ENDIF.
  ELSE.
    CLEAR t330t.
  ENDIF.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE PLATZ_POSITION_MI_LE OUTPUT                            *
*---------------------------------------------------------------------*
*       Mischen von Position und Platz in die Anzeige Lagerplatz      *
*---------------------------------------------------------------------*
MODULE platz_position_mi_le OUTPUT.
  IF lein-plpos NE space.
    CALL FUNCTION 'L_PLATZ_POSITION_MISCHEN'
         EXPORTING
              lgpla   = lein-lgpla
              plpos   = lein-plpos
         IMPORTING
              o_lgpla = lein-lgpla.
  ENDIF.
ENDMODULE.
*---------------------------------------------------------------------*
*      Module  MODIFY_SCREEN  OUTPUT                                  *
*---------------------------------------------------------------------*
*      Popup für Anzeige der Platzaufteilung aufbereiten              *
*---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  LOOP AT SCREEN.
    IF NOT screen-group1 IS INITIAL.
      ASSIGN TABLE FIELD (screen-name) TO <fieldname>.
      IF <fieldname> IS INITIAL.
*........Platzposition nicht vorhanden -> nicht ausgeben...............
        screen-invisible = con_on.
        MODIFY SCREEN.
      ELSE.
        SELECT SINGLE * FROM lqua WHERE lgnum = lagp-lgnum
                                    AND lgtyp = lagp-lgtyp
                                    AND lgpla = lagp-lgpla
                                    AND plpos = <fieldname>.
        IF sy-subrc NE 0.
*........Kein Quant auf der Platzposition -> inaktiv...................
          screen-input = con_off.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                             " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0400_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       Set SUBSCREEN depending on which page you have clicked
*----------------------------------------------------------------------*
MODULE d0400_subscreen OUTPUT.

  IF func_tabstrip-activetab CS fcode_allg.
    subscreen = dynpro_4_platz.
  ENDIF.
  IF func_tabstrip-activetab CS fcode_best.
    subscreen = dynpro_4_bestand.
  ENDIF.
  IF func_tabstrip-activetab CS fcode_inve.
    subscreen = dynpro_4_inventur.
  ENDIF.
  IF func_tabstrip-activetab CS fcode_auft.
    subscreen = dynpro_4_aufteilung.
  ENDIF.
  IF func_tabstrip-activetab CS fcode_stat.
    subscreen = dynpro_4_statistik.
  ENDIF.

  IF subscreen IS INITIAL.
    subscreen = dynpro_4_platz.
  ENDIF.

ENDMODULE.                 " D0400_SUBSCREEN  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  D0400_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*  Only processed on first PBO after transaction is called
*    to display data if the whole key is filled via SET/GET
*----------------------------------------------------------------------*
MODULE d0400_daten OUTPUT.

  MOVE t340-trtyp TO t342-trtyp.

  CHECK flg_d0400_firstcall NE con_true.
  MOVE con_true TO flg_d0400_firstcall.

  reset-seite = 1.
  reset2-seite = 1.

  PERFORM d0400_daten.

ENDMODULE.                 " D0400_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SONDERREGELN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sonderregeln OUTPUT.

  LOOP AT SCREEN.
    IF screen-group3 NE space.
      PERFORM screen-group3 OF sr001
                               sr002
                               sr003
                               sr004                        "140499
                               sr005.                       "140499

      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " SONDERREGELN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PFSTATUS_SETZEN_D0400  OUTPUT
*&---------------------------------------------------------------------*
*       Set PFSTATUS depending on T340-TRTYP
*----------------------------------------------------------------------*
MODULE pfstatus_setzen_d0400 OUTPUT.

  PERFORM pfstatus_setzen_d0400.

ENDMODULE.                 " PFSTATUS_SETZEN_D0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4002_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*       Read quant data only once per bin.
*       If you are on the first page (bin data) quant data will only
*         be read if number of quants in the bin is less 15.
*       The field QUANT_ON_FIRST is set in Form D0400_DATEN.
*----------------------------------------------------------------------*
MODULE d4002_daten OUTPUT.

  CHECK flg_d4002_firstcall NE con_true.

  IF ( ( func_tabstrip-activetab CS fcode_allg OR
         func_tabstrip-activetab IS INITIAL ) AND
     quant_on_first IS INITIAL ).
    EXIT.
  ENDIF.

  MOVE con_true TO flg_d4002_firstcall.

  PERFORM d4002_daten.

ENDMODULE.                 " D4002_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4003_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*       Read inventory data only once per bin
*----------------------------------------------------------------------*
MODULE d4003_daten OUTPUT.

  CHECK flg_d4003_firstcall NE con_true.
  MOVE con_true TO flg_d4003_firstcall.

  PERFORM d4003_daten.

ENDMODULE.                 " D4003_DATEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_D4001  OUTPUT
*&---------------------------------------------------------------------*
*       Position cursor on specific field
*----------------------------------------------------------------------*
MODULE set_cursor_d4001 OUTPUT.

  IF t340-trtyp NE con_anzeigen.
    IF NOT lagp-lgpla IS INITIAL.
      IF NOT bin_is_checked IS INITIAL.
        SET CURSOR FIELD 'LAGP-LGBER'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " SET_CURSOR_D4001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_D0400  OUTPUT
*&---------------------------------------------------------------------*
*       Position cursor on specific field
*----------------------------------------------------------------------*
MODULE set_cursor_d0400 OUTPUT.

  IF lagp-lgnum IS INITIAL.
    SET CURSOR FIELD 'LAGP-LGNUM'.
    EXIT.
  ENDIF.
  IF lagp-lgtyp IS INITIAL.
    SET CURSOR FIELD 'LAGP-LGTYP'.
    EXIT.
  ENDIF.

  SET CURSOR FIELD 'LAGP-LGPLA'.

ENDMODULE.                 " SET_CURSOR_D0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SONUM_LSONR_CONV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE sonum_lsonr_conv OUTPUT.

  PERFORM sonum_conv_int_ext(sapfl000)
                      USING linv-sobkz linv-sonum rl01s-lsonr.

ENDMODULE.                 " SONUM_LSONR_CONV  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  T341_LESEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE t341_lesen OUTPUT.

*  SELECT SINGLE * FROM T341
*  WHERE TCODE = SY-TCODE.
  SELECT SINGLE * FROM t341
  WHERE tcode = 'LS03N'.
  IF sy-subrc NE 0.
    MESSAGE e041 WITH sy-tcode.   "Transaktion nicht in T341 beschrieben
  ENDIF.

ENDMODULE.                 " T341_LESEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D4001_INIT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE d4001_init OUTPUT.

  CLEAR sav_datar.

ENDMODULE.                 " D4001_INIT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CONTAINER_NO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_container_no OUTPUT.
  IF lagp-lgpla IS INITIAL.
    GET PARAMETER ID 'LGP' FIELD lagp-lgpla.
  ENDIF.

*  SELECT SINGLE cont_reg_numb1
*    INTO leci_tra_dyn-cont_reg_numb1  "Container No.
*    FROM ztmm_container
*    WHERE lgpla = lagp-lgpla AND   "Storage bin
*          kzler = space.    "Indicator whether storage bin is empty

  TABLES : ztmm_container.

  SELECT * FROM ztmm_container
          WHERE lgpla EQ lagp-lgpla   " Storage Bin
            AND kzler EQ space   " Indicator whether St.Bin is not empty
       ORDER BY pass_date DESCENDING
*--- insert by stlim (2004/07/29)
                erdat     DESCENDING
                erzet     DESCENDING.
*--- end of insert
    MOVE : ztmm_container-cont_reg_numb1
                          TO leci_tra_dyn-cont_reg_numb1. " Con. No.
    EXIT.
  ENDSELECT.

  IF sy-subrc = 0.
  ELSE.
    CLEAR leci_tra_dyn-cont_reg_numb1.
  ENDIF.
ENDMODULE.                 " GET_CONTAINER_NO  OUTPUT
