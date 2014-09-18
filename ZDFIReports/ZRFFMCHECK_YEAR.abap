*&---------------------------------------------------------------------*
*& Report  Z_CHECK_GJAHR                                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT z_check_gjahr .

TABLES: fmifiit.

INCLUDE:
ififmcon_value_types.

PARAMETERS:
p_bukrs    LIKE    fmifiit-bukrs   OBLIGATORY,
p_gjahr    LIKE    fmifiit-kngjahr OBLIGATORY.

SELECT-OPTIONS:
so_belnr   FOR     fmifiit-knbelnr OBLIGATORY.

DATA:
g_cursor        TYPE    cursor,
g_t_orig        LIKE    fmifiit OCCURS 0 WITH HEADER LINE,
g_t_paym        LIKE    fmifiit OCCURS 0 WITH HEADER LINE,
g_t_wrong       LIKE    fmifiit OCCURS 0 WITH HEADER LINE.

*---- originalzeilen im neuen Geschäftsjahr lesen
OPEN CURSOR WITH HOLD g_cursor FOR
  SELECT * FROM fmifiit WHERE knbelnr IN so_belnr
                          AND kngjahr = p_gjahr
                          AND bukrs   = p_bukrs
                          AND btart   = '0100'
                          AND wrttp   = wrttp6
                          AND vrgng   <> 'AZUM'
                          AND vrgng   <> 'AZZA'.
DO.
*----- Belegzeilen einlesen
  FETCH NEXT CURSOR g_cursor
    INTO TABLE g_t_orig
    PACKAGE SIZE 10000.

*----- Letzter Satz erreicht cursor schließen
  IF sy-subrc <> 0.
    CLOSE CURSOR g_cursor.
    EXIT.
  ENDIF.

  REFRESH g_t_paym.
  SORT g_t_orig BY knbelnr kngjahr bukrs.

*---- gibt es zu diesen Rechnungen Zahlungen im alten Jahr ?
  SELECT * FROM fmifiit INTO TABLE g_t_paym
    FOR ALL ENTRIES IN g_t_orig
    WHERE knbelnr = g_t_orig-knbelnr
      AND kngjahr = g_t_orig-kngjahr
      AND bukrs   = g_t_orig-bukrs
      AND ( btart = '0200'
        OR  btart = '0250' )
      AND rldnr   = g_t_orig-rldnr
      AND gjahr   < g_t_orig-gjahr.
  CHECK sy-subrc = 0.

*---- Originalsätze zu fehlerhaften Belegen sammeln
  LOOP AT g_t_paym.
    READ TABLE g_t_orig WITH KEY knbelnr = g_t_paym-knbelnr
                                 kngjahr = g_t_paym-kngjahr
                                 bukrs   = g_t_paym-bukrs
                                 BINARY SEARCH.
    CHECK sy-subrc = 0.
    MOVE g_t_orig TO g_t_wrong.
    APPEND g_t_wrong.
  ENDLOOP.

  SORT g_t_wrong BY knbelnr kngjahr bukrs.
  DELETE ADJACENT DUPLICATES FROM g_t_wrong.
  PERFORM write_list TABLES g_t_wrong
                            g_t_paym.

  REFRESH: g_t_orig, g_t_wrong.
ENDDO.
*&---------------------------------------------------------------------*
*&      Form  WRITE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_ORIG  text                                             *
*      -->P_G_T_PAYM  text                                             *
*      -->P_P_TEST  text                                               *
*----------------------------------------------------------------------*
FORM write_list TABLES   u_t_fm_items STRUCTURE g_t_orig
                         u_t_delete   STRUCTURE g_t_paym.


*----Spaltenüberschriften
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.
  WRITE AT:  001 sy-vline, 002(5)  text-020,              "Bukrs
             007 sy-vline, 009(11) text-021,              "Belnr
             020 sy-vline, 021(5)  text-022,              "Gjahr
             026 sy-vline, 027(5)  text-023,              "Ledg.
             032 sy-vline, 035(5)  text-024,              "Wrttp
             039 sy-vline, 040(5)  text-025,              "Btart
             045 sy-vline, 046(5)  text-026,              "Vorgang
             051 sy-vline, 052(5)  text-027,              "FM-Jahr
             057 sy-vline, 058(19) text-028,              "Betrag
             073 sy-vline, 074(11) text-029,              "Zahlg.
             084 sy-vline, 085(2)  text-030,                "PF
             087 sy-vline, 088(10) text-031.

  LOOP AT u_t_fm_items.

*-----Schlüsselsatz ausgeben
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE AT: /001 sy-vline, 002(5) u_t_fm_items-bukrs    "Bukrs
                                    COLOR COL_KEY,
               007 sy-vline, 009(11) u_t_fm_items-knbelnr "Belnr
                                     COLOR COL_KEY,
               020 sy-vline, 021(5)  u_t_fm_items-kngjahr "Gjahr
                                     COLOR COL_KEY,
               026 sy-vline, 027(5)  u_t_fm_items-rldnr,  "Ledg.
               032 sy-vline, 035(5)  u_t_fm_items-wrttp,  "Wrttp
               039 sy-vline, 040(5)  u_t_fm_items-btart,  "Btart
               045 sy-vline, 046(5)  u_t_fm_items-vrgng,  "Vorgang
               051 sy-vline, 052(5)  u_t_fm_items-gjahr,  "FM-Jahr
               057 sy-vline, 058(15) u_t_fm_items-fkbtr,   "Betrag
*                              CURRENCY U_T_FM_ITEMS-WAERS,  "Währg
               073 sy-vline, 074(11) u_t_fm_items-vobelnr,"Zahlg.
               084 sy-vline, 085(2)  u_t_fm_items-payflg,   "PF
               087 sy-vline, 088(10) u_t_fm_items-psobt.

    AT END OF fmbelnr.
      LOOP AT u_t_delete WHERE fmbelnr = u_t_fm_items-fmbelnr.

        WRITE AT: /001 sy-vline,
                  007 sy-vline, 008(18) text-006 COLOR COL_NEGATIVE.

*------zugehörige gelöschte Sätze ausgeben
        WRITE AT:
                   026 sy-vline, 027(5)  u_t_delete-rldnr,"Ledg.
                   032 sy-vline, 035(5)  u_t_delete-wrttp,"Wrttp
                   039 sy-vline, 040(5)  u_t_delete-btart,"Btart
                   045 sy-vline, 046(5)  u_t_delete-vrgng,"Vorgang
                   051 sy-vline, 052(5)  u_t_delete-gjahr,  "FM-Jahr
                   057 sy-vline, 058(15) u_t_delete-fkbtr "Betrag
                                CURRENCY u_t_delete-twaer,  "Währg
                   073 sy-vline, 074(11) u_t_delete-vobelnr,"Zahlg.
                   084 sy-vline, 085(2)  u_t_delete-payflg, "PF
                   087 sy-vline, 088(10) u_t_delete-psobt.
      ENDLOOP.
    ENDAT.
    AT END OF fmbelnr.
      ULINE.
    ENDAT.
  ENDLOOP.

ENDFORM.                               " WRITE_LIST
