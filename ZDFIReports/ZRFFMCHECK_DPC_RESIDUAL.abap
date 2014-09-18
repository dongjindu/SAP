*&---------------------------------------------------------------------*
*& Report  ZRFFMCHECK_DPC_RESIDUAL                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  z_rffm_dpc_open_items LINE-SIZE 126.

INCLUDE: ififmcon_value_types,
         ififmcon_bool.

TYPE-POOLS:
  fmfi.

TABLES:
  fmifiit.

DATA:
  g_t_fmifiit_sel     LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_t_fmifiit_key     LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_t_dpc_orp_sl      LIKE bseg            OCCURS 0 WITH HEADER LINE,
  g_t_dpc_op          LIKE bseg            OCCURS 0 WITH HEADER LINE,
  g_t_dpc_orp_pa      LIKE bseg            OCCURS 0 WITH HEADER LINE,
  g_t_bseg            LIKE bseg            OCCURS 0 WITH HEADER LINE,
  g_t_bseg_tmp        LIKE bseg            OCCURS 0 WITH HEADER LINE,
  g_t_rebzg           LIKE bkpf            OCCURS 0 WITH HEADER LINE,
  l_f_bseg            LIKE bseg,
  l_umskz             LIKE bseg-umskz,
  g_c_fmifiit TYPE cursor,

  g_cnt_lines TYPE i.

PARAMETERS:
  p_bukrs LIKE fmifiit-bukrs   OBLIGATORY,
  p_knghr LIKE fmifiit-kngjahr OBLIGATORY.
SELECT-OPTIONS:
  s_knbln FOR fmifiit-knbelnr.
SELECTION-SCREEN SKIP 1.

OPEN CURSOR WITH HOLD g_c_fmifiit FOR
  SELECT *
    FROM fmifiit
   WHERE knbelnr  IN s_knbln
     AND kngjahr  = p_knghr
     AND bukrs    = p_bukrs
     AND vrgng    = fmfi_con_orgvg_dpclearing.

DO.

*--- Selektion
  FETCH NEXT CURSOR g_c_fmifiit
    INTO TABLE g_t_fmifiit_sel
    PACKAGE SIZE 10000.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

*----- Belegkey extrahieren
  LOOP AT g_t_fmifiit_sel WHERE wrttp = wrttp9a.
    g_t_fmifiit_key-bukrs   = g_t_fmifiit_sel-bukrs.
    g_t_fmifiit_key-knbelnr = g_t_fmifiit_sel-knbelnr.
    g_t_fmifiit_key-kngjahr = g_t_fmifiit_sel-kngjahr.
    COLLECT g_t_fmifiit_key.
  ENDLOOP.

*----- FI Belege zu den AV nachlesen
  DESCRIBE TABLE g_t_fmifiit_key LINES sy-tfill.
  IF sy-tfill > 0.
    SELECT * FROM bseg
    INTO TABLE g_t_bseg
     FOR ALL ENTRIES IN g_t_fmifiit_key
   WHERE bukrs = g_t_fmifiit_key-bukrs
     AND belnr = g_t_fmifiit_key-knbelnr
     AND gjahr = g_t_fmifiit_key-kngjahr.

*----- Offene Restposten identifizieren
    APPEND LINES OF g_t_bseg TO g_t_bseg_tmp.
    LOOP AT g_t_bseg WHERE augbl IS initial.

*----- Offene Resposten auf dem SHB mit Rechnungsbezug
      IF     NOT g_t_bseg-umskz IS INITIAL
         AND   ( g_t_bseg-vorgn = fmfi_con_orgvg_dpclearing OR
                 g_t_bseg-vorgn = fmfi_con_orgvg_downpayment_fi )
         AND NOT g_t_bseg-rebzg IS INITIAL
         AND     g_t_bseg-rebzt = 'V'.

*----- nur Überzahlungen sind interessant
        LOOP AT g_t_bseg_tmp
                        WHERE   bukrs =  g_t_bseg-bukrs
                          AND   belnr =  g_t_bseg-belnr
                          AND   gjahr =  g_t_bseg-gjahr
                          AND   umskz <> l_umskz
                          AND   buzei <> g_t_bseg-buzei
                          AND ( vorgn =  fmfi_con_orgvg_dpclearing
                           OR   vorgn =  fmfi_con_orgvg_downpayment_fi )
                          AND   shkzg <> g_t_bseg-shkzg.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          APPEND g_t_bseg TO g_t_dpc_orp_sl.
          g_t_rebzg-bukrs = g_t_bseg-bukrs.
          g_t_rebzg-belnr = g_t_bseg-rebzg.
          g_t_rebzg-gjahr = g_t_bseg-rebzj.
          COLLECT g_t_rebzg.
        ENDIF.
      ENDIF.

*----- Offene Resposten auf dem Personenkonto mit Rechnungsbezug
      IF         g_t_bseg-umskz IS INITIAL
         AND     g_t_bseg-vorgn = fmfi_con_orgvg_fidocument
         AND NOT g_t_bseg-rebzg IS INITIAL
         AND     g_t_bseg-rebzt = 'V'.

*----- nur Überzahlungen sind interessant
        LOOP AT g_t_bseg_tmp
                        WHERE   bukrs =  g_t_bseg-bukrs
                          AND   belnr =  g_t_bseg-belnr
                          AND   gjahr =  g_t_bseg-gjahr
                          AND   umskz <> l_umskz
                          AND   buzei <> g_t_bseg-buzei
                          AND ( vorgn =  fmfi_con_orgvg_dpclearing
                           OR   vorgn =  fmfi_con_orgvg_downpayment_fi )
                          AND   shkzg <> g_t_bseg-shkzg.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          APPEND g_t_bseg TO g_t_dpc_orp_pa.
          g_t_rebzg-bukrs = g_t_bseg-bukrs.
          g_t_rebzg-belnr = g_t_bseg-rebzg.
          g_t_rebzg-gjahr = g_t_bseg-rebzj.
          COLLECT g_t_rebzg.
        ENDIF.
      ENDIF.

*----- Offene Resposten auf dem Personenkonto bzw. SHB ohne
*----- Rechnungsbezug
      IF   g_t_bseg-rebzg IS INITIAL
      OR ( NOT g_t_bseg-rebzg IS INITIAL
      AND  (   g_t_bseg-rebzt = 'Z'
       OR      g_t_bseg-rebzt = 'A' ) ).
        APPEND g_t_bseg TO g_t_dpc_op.
        IF NOT g_t_bseg-rebzg IS INITIAL.
          g_t_rebzg-bukrs = g_t_bseg-bukrs.
          g_t_rebzg-belnr = g_t_bseg-rebzg.
          g_t_rebzg-gjahr = g_t_bseg-rebzj.
          COLLECT g_t_rebzg.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  REFRESH: g_t_bseg,
           g_t_bseg_tmp,
           g_t_fmifiit_key,
           g_t_fmifiit_sel.
ENDDO.

*----- Ausgabe
PERFORM write_delete_items TABLES
                             g_t_dpc_orp_sl
                             g_t_dpc_orp_pa
                             g_t_dpc_op.

PERFORM write_rebzg TABLES
                       g_t_rebzg.
*&---------------------------------------------------------------------*
*&      Form  write_delete_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_DEL  text
*----------------------------------------------------------------------*
FORM write_delete_items TABLES
                          u_t_dpc_orp_sl  STRUCTURE bseg
                          u_t_dpc_orp_pa  STRUCTURE bseg
                          u_t_dpc_op      STRUCTURE bseg.


*----- offene Posten mit REBZG auf dem SHB
  DESCRIBE TABLE u_t_dpc_orp_sl LINES sy-tfill.
  IF sy-tfill = 0.
    WRITE / text-001 COLOR COL_TOTAL.  "keine offenen Posten auf dem SHB
  ELSE.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    WRITE: / text-002, sy-tfill.       "offenen Posten auf dem SHB...


*----Spaltenüberschriften
    SORT u_t_dpc_orp_sl.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    ULINE (126).
    WRITE AT: /001 sy-vline, 002(5)  text-020,              "Bukrs
               007 sy-vline, 008(10) text-021,              "Belnr
               018 sy-vline, 019(5)  text-022,              "Gjahr
               024 sy-vline, 025(5)  text-023,              "bschl
               030 sy-vline, 031(5)  text-024,              "umskz
               036 sy-vline, 037(16) text-025,              "dmbtr
               053 sy-vline, 054(10) text-026,              "hkont
               064 sy-vline, 065(10) text-027,              "kunnr
               075 sy-vline, 076(10) text-028,              "lifnr
               086 sy-vline, 087(10) text-029,              "rebzg
               097 sy-vline, 098(5)  text-030,              "rebzj
               103 sy-vline, 104(5)  text-031,              "rebzt
               109 sy-vline, 110(16) text-032,              "fipos
               126 sy-vline.
    ULINE (126).
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    LOOP AT u_t_dpc_orp_sl.
      WRITE AT: /001 sy-vline, 002(5)  u_t_dpc_orp_sl-bukrs
                                       COLOR COL_KEY,        "Bukrs
                 007 sy-vline, 008(10) u_t_dpc_orp_sl-belnr
                                       COLOR COL_KEY,        "Belnr
                 018 sy-vline, 019(5)  u_t_dpc_orp_sl-gjahr
                                       COLOR COL_KEY,        "Gjahr
                 024 sy-vline, 025(5)  u_t_dpc_orp_sl-bschl, "bschl
                 030 sy-vline, 031(5)  u_t_dpc_orp_sl-umskz, "umskz
                 036 sy-vline, 037(16) u_t_dpc_orp_sl-dmbtr, "dmbtr
                 053 sy-vline, 054(10) u_t_dpc_orp_sl-hkont, "hkont
                 064 sy-vline, 065(10) u_t_dpc_orp_sl-kunnr, "kunnr
                 075 sy-vline, 076(10) u_t_dpc_orp_sl-lifnr, "lifnr
                 086 sy-vline, 087(10) u_t_dpc_orp_sl-rebzg, "rebzg
                 097 sy-vline, 098(5)  u_t_dpc_orp_sl-rebzj, "rebzj
                 103 sy-vline, 104(5)  u_t_dpc_orp_sl-rebzt, "rebzt
                 109 sy-vline, 110(16) u_t_dpc_orp_sl-fipos, "fipos
                 126 sy-vline.
    ENDLOOP.
    ULINE (125).
  ENDIF.

*----- offene Posten mit REBZG auf dem Kreditor/Debitor
  SKIP 2.
  DESCRIBE TABLE u_t_dpc_orp_pa LINES sy-tfill.
  IF sy-tfill = 0.
    WRITE / text-003 COLOR COL_TOTAL. "keine offenen Posten auf dem SHB
  ELSE.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    WRITE: / text-004, sy-tfill.      "offenen Posten auf dem SHB...

*----Spaltenüberschriften
    SORT u_t_dpc_orp_pa.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    ULINE (126).
    WRITE AT: /001 sy-vline, 002(5)  text-020,              "Bukrs
               007 sy-vline, 008(10) text-021,              "Belnr
               018 sy-vline, 019(5)  text-022,              "Gjahr
               024 sy-vline, 025(5)  text-023,              "bschl
               030 sy-vline, 031(5)  text-024,              "umskz
               036 sy-vline, 037(16) text-025,              "dmbtr
               053 sy-vline, 054(10) text-026,              "hkont
               064 sy-vline, 065(10) text-027,              "kunnr
               075 sy-vline, 076(10) text-028,              "lifnr
               086 sy-vline, 087(10) text-029,              "rebzg
               097 sy-vline, 098(5)  text-030,              "rebzj
               103 sy-vline, 104(5)  text-031,              "rebzt
               109 sy-vline, 110(16) text-032,              "fipos
               126 sy-vline.
    ULINE (126).
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    LOOP AT u_t_dpc_orp_pa.
      WRITE AT: /001 sy-vline, 002(5)  u_t_dpc_orp_pa-bukrs
                                       COLOR COL_KEY,        "Bukrs
                 007 sy-vline, 008(10) u_t_dpc_orp_pa-belnr
                                       COLOR COL_KEY,        "Belnr
                 018 sy-vline, 019(5)  u_t_dpc_orp_pa-gjahr
                                       COLOR COL_KEY,        "Gjahr
                 024 sy-vline, 025(5)  u_t_dpc_orp_pa-bschl, "bschl
                 030 sy-vline, 031(5)  u_t_dpc_orp_pa-umskz, "umskz
                 036 sy-vline, 037(16) u_t_dpc_orp_pa-dmbtr, "dmbtr
                 053 sy-vline, 054(10) u_t_dpc_orp_pa-hkont, "hkont
                 064 sy-vline, 065(10) u_t_dpc_orp_pa-kunnr, "kunnr
                 075 sy-vline, 076(10) u_t_dpc_orp_pa-lifnr, "lifnr
                 086 sy-vline, 087(10) u_t_dpc_orp_pa-rebzg, "rebzg
                 097 sy-vline, 098(5)  u_t_dpc_orp_pa-rebzj, "rebzj
                 103 sy-vline, 104(5)  u_t_dpc_orp_pa-rebzt, "rebzt
                 109 sy-vline, 110(16) u_t_dpc_orp_pa-fipos, "fipos
                 126 sy-vline.
    ENDLOOP.
    ULINE (126).
  ENDIF.

*----- offene Posten ohne REBZG auf dem SHB/Kreditor/Debitor
  SKIP 2.
  DESCRIBE TABLE u_t_dpc_op LINES sy-tfill.
  IF sy-tfill = 0.
    WRITE / text-005 COLOR COL_TOTAL. "keine offenen Posten auf dem SHB
  ELSE.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    WRITE: / text-006, sy-tfill.      "offenen Posten auf dem SHB...

*----Spaltenüberschriften
    SORT u_t_dpc_op.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    ULINE (126).
    WRITE AT: /001 sy-vline, 002(5)  text-020,              "Bukrs
               007 sy-vline, 008(10) text-021,              "Belnr
               018 sy-vline, 019(5)  text-022,              "Gjahr
               024 sy-vline, 025(5)  text-023,              "bschl
               030 sy-vline, 031(5)  text-024,              "umskz
               036 sy-vline, 037(16) text-025,              "dmbtr
               053 sy-vline, 054(10) text-026,              "hkont
               064 sy-vline, 065(10) text-027,              "kunnr
               075 sy-vline, 076(10) text-028,              "lifnr
               086 sy-vline, 087(10) text-029,              "rebzg
               097 sy-vline, 098(5)  text-030,              "rebzj
               103 sy-vline, 104(5)  text-031,              "rebzt
               109 sy-vline, 110(16) text-032,              "fipos
               126 sy-vline.
    ULINE (126).
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    LOOP AT u_t_dpc_op.
      WRITE AT: /001 sy-vline, 002(5)  u_t_dpc_op-bukrs
                                       COLOR COL_KEY,        "Bukrs
                 007 sy-vline, 008(10) u_t_dpc_op-belnr
                                       COLOR COL_KEY,        "Belnr
                 018 sy-vline, 019(5)  u_t_dpc_op-gjahr
                                       COLOR COL_KEY,        "Gjahr
                 024 sy-vline, 025(5)  u_t_dpc_op-bschl, "bschl
                 030 sy-vline, 031(5)  u_t_dpc_op-umskz, "umskz
                 036 sy-vline, 037(16) u_t_dpc_op-dmbtr, "dmbtr
                 053 sy-vline, 054(10) u_t_dpc_op-hkont, "hkont
                 064 sy-vline, 065(10) u_t_dpc_op-kunnr, "kunnr
                 075 sy-vline, 076(10) u_t_dpc_op-lifnr, "lifnr
                 086 sy-vline, 087(10) u_t_dpc_op-rebzg, "rebzg
                 097 sy-vline, 098(5)  u_t_dpc_op-rebzj, "rebzj
                 103 sy-vline, 104(5)  u_t_dpc_op-rebzt, "rebzt
                 109 sy-vline, 110(16) u_t_dpc_op-fipos, "fipos
                 126 sy-vline.
    ENDLOOP.
    ULINE (126).
  ENDIF.
ENDFORM.                    " write_delete_items
*&---------------------------------------------------------------------*
*&      Form  WRITE_REBZG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_REBZG  text
*----------------------------------------------------------------------*
FORM write_rebzg TABLES
                  u_t_invoices_print STRUCTURE bkpf.

  DATA:
        l_color  TYPE  i,
        l_width_lst TYPE  i  VALUE '24'.

*----- offene Posten mit REBZG auf dem SHB
  DESCRIBE TABLE u_t_invoices_print LINES sy-tfill.
  IF sy-tfill > 0.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    WRITE: / text-011, sy-tfill.       "offenen Posten auf dem SHB...

*---- Spaltenüberschriften
    ULINE AT /(l_width_lst).
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE AT: /001 sy-vline, 002(5)  text-020,
               007 sy-vline, 009(9)  text-021,
               018 sy-vline, 019(5)  text-022,
               l_width_lst sy-vline.

    ULINE AT /(l_width_lst).

*----- Ausgabe der Belege
    l_color = 1.
    LOOP AT u_t_invoices_print.
      IF sy-index EQ l_color.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      ELSE.
        l_color = sy-index + 1.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
      WRITE AT: /001 sy-vline, 002(5)  u_t_invoices_print-bukrs,
                007 sy-vline, 008(10) u_t_invoices_print-belnr  HOTSPOT,
                 018 sy-vline, 019(5)  u_t_invoices_print-gjahr,
                 l_width_lst sy-vline.
      HIDE: u_t_invoices_print-bukrs,
            u_t_invoices_print-belnr,
            u_t_invoices_print-gjahr.
    ENDLOOP.
    ULINE AT /(l_width_lst).
  ENDIF.
ENDFORM.                    " WRITE_REBZG
