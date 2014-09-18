*&---------------------------------------------------------------------*
*& Report  Z_CHECK_PAYMENT                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  Z_CHECK_PAYMENT                                             .


INCLUDE:
ififmcon_value_types.

TABLES: fmifiit.

PARAMETERS:
p_bukrs           LIKE  fmifiit-bukrs,
p_gjahr           LIKE  fmifiit-gjahr.

SELECT-OPTIONS:
so_belnr           FOR   fmifiit-knbelnr.

DATA:
g_cursor          TYPE cursor,
g_t_payments      LIKE fmifiit    OCCURS 0 WITH HEADER LINE,
*g_t_payments_err  LIKE fmifiit    OCCURS 0 WITH HEADER LINE,
g_t_lines         LIKE fmifiit    OCCURS 0 WITH HEADER LINE.


*---- Zahlungen im neuen Jahr lesen
OPEN CURSOR WITH HOLD g_cursor FOR
  SELECT * FROM fmifiit WHERE vobukrs = p_bukrs
                          AND vogjahr = p_gjahr
                          AND vobelnr IN so_belnr
                          AND btart = '0250'
                          AND wrttp = wrttp9
                          AND vrgng = 'RFBU'.


DO.
  REFRESH g_t_lines.
  REFRESH g_t_payments.

*----- Belegzeilen einlesen
  FETCH NEXT CURSOR g_cursor
    INTO TABLE g_t_payments
    PACKAGE SIZE 5000.

*----- Letzter Satz erreicht cursor schlieﬂen
  IF sy-subrc <> 0.
    CLOSE CURSOR g_cursor.
    EXIT.
  ENDIF.


*--- ist diese Zahlung in alten Rechnungen fortgeschrieben ?
  SELECT * FROM fmifiit INTO TABLE g_t_lines
      FOR ALL ENTRIES IN g_t_payments
      WHERE vobukrs = g_t_payments-vobukrs
        AND vobelnr = g_t_payments-vobelnr
        AND vogjahr = g_t_payments-vogjahr
        AND btart   = '0250'
        AND wrttp   = wrttp9
        AND gjahr  < g_t_payments-gjahr.
  CHECK sy-subrc = 0.

  PERFORM write_list TABLES g_t_lines.

  PERFORM check_cf TABLES g_t_lines.
ENDDO.

************************************************************************
************************************************************************
FORM write_list TABLES   u_t_payments STRUCTURE fmifiit.
*                         u_t_items    STRUCTURE fmifiit

  SORT u_t_payments.

*----Spalten¸berschriften
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.
  WRITE AT:  001 sy-vline, 002(5)  text-020,              "Bukrs
             007 sy-vline, 009(11) text-021,              "Belnr
             020 sy-vline, 021(5)  text-022,              "Gjahr
             032 sy-vline, 035(5)  text-024,              "Wrttp
             039 sy-vline, 040(5)  text-025,              "Btart
             045 sy-vline, 046(5)  text-026,              "F-Dat
             051 sy-vline, 052(5)  text-027,              "FM-Jahr
             057 sy-vline, 058(19) text-028,              "Betrag
             073 sy-vline, 074(11) text-029,              "Zahlg.
             084 sy-vline, 085(2)  text-030,                "PF
             087 sy-vline, 088(10) text-031.

  LOOP AT u_t_payments.

*-----fehlerhaften Satz ausgeben
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE AT: /001 sy-vline, 002(5) u_t_payments-vobukrs    "Bukrs
                                    COLOR COL_KEY,
               007 sy-vline, 007(11) u_t_payments-vobelnr "Belnr
                                     COLOR COL_KEY,
               020 sy-vline, 021(5)  u_t_payments-vogjahr "Gjahr
                                     COLOR COL_KEY,
               022 sy-vline, 023(10)  u_t_payments-zhldt,  "F_Dat
               033 sy-vline, 034(5)  u_t_payments-gjahr,  "FM-Jahr
               039 sy-vline, 040(15) u_t_payments-fkbtr,   "Betrag
*                              CURRENCY U_T_FM_ITEMS-WAERS,  "W‰hrg
               055 sy-vline, 056(11) u_t_payments-knbelnr,"Rechnung
               067 sy-vline, 068(4)  u_t_payments-kngjahr,"Rech-Jahr.
               072 sy-vline, 073(5)  u_t_payments-wrttp,  "Wrttp
               078 sy-vline, 079(5)  u_t_payments-btart,  "Btart
               084 sy-vline, 085(2)  u_t_payments-payflg, "PF
               087 sy-vline, 088(10) u_t_payments-psobt.
    AT END OF fmbelnr.
      ULINE.
    ENDAT.
  ENDLOOP.

ENDFORM.                               " WRITE_LIST
*&---------------------------------------------------------------------*
*&      Form  CHECK_CF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_PAYMENTS_ERR  text
*----------------------------------------------------------------------*
FORM check_cf TABLES   l_t_err STRUCTURE g_t_payments.

  DATA:
   l_t_docs    LIKE fmifiit OCCURS 0 WITH HEADER LINE,
   BEGIN OF l_t_54 OCCURS 0,
     bukrs     LIKE fmifiit-bukrs,
     knbelnr   LIKE fmifiit-knbelnr,
     kngjahr   LIKE fmifiit-kngjahr,
     fkbtr     LIKE fmifiit-fkbtr,
   END OF l_t_54,
   l_t_orig    LIKE l_t_54 OCCURS 0 WITH HEADER LINE,
   l_t_cf      LIKE l_t_54 OCCURS 0 WITH HEADER LINE,
   l_t_57      LIKE l_t_54 OCCURS 0 WITH HEADER LINE,
   l_vgl1      LIKE fmifiit-fkbtr,
   l_vgl2      LIKE fmifiit-fkbtr.

  CHECK NOT l_t_err[] IS INITIAL.

  WRITE: '**********************************************'.

  LOOP AT l_t_err.
    REFRESH l_t_54.
    REFRESH l_t_57.
    REFRESH l_t_cf.
    REFRESH l_t_orig.

    SELECT * FROM fmifiit
             INTO TABLE l_t_docs
             WHERE bukrs   = l_t_err-bukrs
               AND knbelnr = l_t_err-knbelnr
               AND kngjahr = l_t_err-kngjahr
               AND gjahr   = l_t_err-gjahr.

    LOOP AT l_t_docs.
      CASE l_t_docs-wrttp.
        WHEN '54'.
          IF l_t_docs-btart = '0100' or l_t_docs-btart = '0350'.
            MOVE-CORRESPONDING l_t_docs TO l_t_orig.
            COLLECT l_t_orig.
          ELSEIF l_t_docs-btart = '0200'.
            MOVE-CORRESPONDING l_t_docs TO l_t_54.
            COLLECT l_t_54.
          ELSEIF l_t_docs-btart = '0300'.
            MOVE-CORRESPONDING l_t_docs TO l_t_cf.
            COLLECT l_t_cf.
          ENDIF.
        WHEN '57'.
          MOVE-CORRESPONDING l_t_docs TO l_t_57.
          COLLECT l_t_57.
      ENDCASE.
    ENDLOOP.

    READ TABLE l_t_54 INDEX 1.
    READ TABLE l_t_57 INDEX 1.
    READ TABLE l_t_cf INDEX 1.
    READ TABLE l_t_orig INDEX 1.

*----- Abbau- und Zahlung
    l_vgl1 = abs( l_t_54-fkbtr ).
    l_vgl2 = abs( l_t_57-fkbtr ).
    IF l_vgl1 <> l_vgl2.
      WRITE:/ 'Abweichung Abbau- und Zahlung:',
            / l_t_err-bukrs,
              l_t_err-knbelnr,
              l_t_err-kngjahr.
    ENDIF.

*----- GJW-Differenz 1
    l_vgl1 = abs( l_t_54-fkbtr + l_t_orig-fkbtr ).
    l_vgl2 = abs( l_t_cf-fkbtr ).
    IF l_vgl1 <> l_vgl2.
      WRITE:/ 'Abweichung 54 und Obligovortrag:',
            / l_t_err-bukrs,
              l_t_err-knbelnr,
              l_t_err-kngjahr.
    ENDIF.

*----- GJW-Differenz 2
    l_vgl1 = abs( l_t_54-fkbtr + l_t_orig-fkbtr + l_t_cf-fkbtr ).
    IF l_vgl1 <> 0.
      WRITE:/ 'Differenz Obligovortrag:',
            / l_t_err-bukrs,
              l_t_err-knbelnr,
              l_t_err-kngjahr.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " CHECK_CF
