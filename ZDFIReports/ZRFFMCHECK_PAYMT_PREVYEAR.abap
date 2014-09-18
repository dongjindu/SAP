*&---------------------------------------------------------------------*
*& Report  Z_RFFM_PAYMENT_PREVIOUS_YEAR                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  z_rffm_payment_previous_year  .

INCLUDE: ififmcon_value_types,
         ififmcon_bool.

TYPE-POOLS:
  fmfi.

TABLES:
  fmifiit,
  bkpf.

DATA:
  g_t_fmifiit     LIKE fmifiit         OCCURS 0 WITH HEADER LINE,
  g_c_fmifiit     TYPE cursor,
  l_lifnr         LIKE bseg-lifnr,
  l_kunnr         LIKE bseg-kunnr,
  l_flg_buzei     TYPE c VALUE con_on,

  BEGIN OF g_t_fmifiit_output OCCURS 0,
    bukrs   LIKE fmifiit-bukrs,
    kngjahr LIKE fmifiit-kngjahr,
    knbelnr LIKE fmifiit-knbelnr,
    knbuzei LIKE fmifiit-knbuzei,
    vobukrs LIKE fmifiit-vobukrs,
    vogjahr LIKE fmifiit-vogjahr,
    vobelnr LIKE fmifiit-vobelnr,
    fkbtr   LIKE fmifiit-fkbtr,
*    augbl   LIKE bseg-augbl,
*    augdt   LIKE bseg-augdt,
*    lifnr   LIKE bseg-lifnr,
*    kunnr   LIKE bseg-kunnr,
  END OF g_t_fmifiit_output.

PARAMETERS:
  p_bukrs LIKE fmifiit-bukrs   OBLIGATORY,
  p_knghr LIKE fmifiit-kngjahr OBLIGATORY.


OPEN CURSOR WITH HOLD g_c_fmifiit FOR
  SELECT *
    FROM fmifiit
   WHERE kngjahr  = p_knghr
     AND bukrs    = p_bukrs
     AND vrgng   <> 'AZUM'
     AND vrgng   <> 'AZZA'
     AND wrttp    = wrttp9.

DO.

*--- Selektion
  FETCH NEXT CURSOR g_c_fmifiit
    INTO TABLE g_t_fmifiit
    PACKAGE SIZE 5000.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

*----- Sätze mit Zahlungen aus dem Vorjahr ermitteln
  LOOP AT g_t_fmifiit WHERE btart   = fmfi_con_btart_payed
                        AND wrttp   = wrttp9.
    IF g_t_fmifiit-vogjahr < g_t_fmifiit-gjahr.
      MOVE-CORRESPONDING g_t_fmifiit TO g_t_fmifiit_output.
      COLLECT g_t_fmifiit_output.
      CHECK g_t_fmifiit-knbuzei IS INITIAL.
      l_flg_buzei = con_off.
    ENDIF.
  ENDLOOP.

  REFRESH: g_t_fmifiit.
ENDDO.

**----- Ausgleichsinformation und den Kreditor/Debitor holen
*if l_flg_buzei = con_on and not g_t_fmifiit_output is initial.
*  SELECT SINGLE augbl augdt lifnr kunnr
*    FROM bseg
*    INTO CORRESPONDING FIELDS OF g_t_fmifiit_output
*    WHERE   bukrs = g_t_fmifiit_output-vobukrs
*      AND   belnr = g_t_fmifiit_output-vobelnr
*      AND   gjahr = g_t_fmifiit_output-vogjahr
*      AND ( lifnr <> l_lifnr
*       OR   kunnr <> l_kunnr ).
*
*  MODIFY g_t_fmifiit_output TRANSPORTING augbl augdt lifnr kunnr.
*ENDLOOP.


PERFORM output TABLES g_t_fmifiit_output.
*&---------------------------------------------------------------------*
*&      Form  output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_T_FMIFIIT_output  text
*----------------------------------------------------------------------*
FORM output TABLES  u_t_fmifiit_output STRUCTURE g_t_fmifiit_output.

  WRITE / text-026.
  WRITE:  025 text-027, p_bukrs.
  WRITE: /025 text-028, p_knghr.
  SKIP 1.

  DESCRIBE TABLE u_t_fmifiit_output LINES sy-tfill.
  IF sy-tfill = 0.
    WRITE /  text-001 COLOR COL_TOTAL.
  ELSE.
    FORMAT COLOR COL_TOTAL INTENSIFIED ON.
    WRITE: / text-002, sy-tfill.

    SKIP 1.
*----Spaltenüberschriften
    FORMAT COLOR COL_GROUP INTENSIFIED ON.
    ULINE (46).
    WRITE AT: /001 sy-vline, 010(12) text-024,
               024 sy-vline, 034(12) text-025,
               046 sy-vline.


    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    ULINE (103).
    WRITE AT: /001 sy-vline, 002(5)  text-020 COLOR COL_KEY,
               007 sy-vline, 008(10) text-021 COLOR COL_KEY,
               018 sy-vline, 019(5)  text-022 COLOR COL_KEY,
               024 sy-vline, 025(5)  text-020 COLOR COL_KEY,
               029 sy-vline, 030(10) text-021 COLOR COL_KEY,
               040 sy-vline, 041(5)  text-022 COLOR COL_KEY,
               046 sy-vline, 047(10) text-029,
               057 sy-vline, 058(10) text-030,
               068 sy-vline, 069(10) text-031,
               079 sy-vline, 080(10) text-032,
               090 sy-vline, 091(13) text-023,
               103 sy-vline.
    ULINE (103).
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    LOOP AT u_t_fmifiit_output.
      WRITE AT: /001 sy-vline, 002(5)  u_t_fmifiit_output-vobukrs
                                       COLOR COL_KEY,
                 007 sy-vline, 008(10) u_t_fmifiit_output-vobelnr
                                       COLOR COL_KEY,
                 018 sy-vline, 019(5)  u_t_fmifiit_output-vogjahr
                                       COLOR COL_KEY,
                 024 sy-vline, 025(5)  u_t_fmifiit_output-bukrs
                                       COLOR COL_KEY,
                 029 sy-vline, 030(10) u_t_fmifiit_output-knbelnr
                                       COLOR COL_KEY,
                 040 sy-vline, 041(5)  u_t_fmifiit_output-kngjahr
                                       COLOR COL_KEY,
*                 046 sy-vline, 047(10) u_t_fmifiit_output-augbl,
*                 057 sy-vline, 058(10) u_t_fmifiit_output-augdt,
*                 068 sy-vline, 069(10) u_t_fmifiit_output-lifnr,
*                 079 sy-vline, 080(10) u_t_fmifiit_output-kunnr,
                 047 sy-vline, 048(13) u_t_fmifiit_output-fkbtr,
                 62 sy-vline.
    ENDLOOP.
    ULINE (103).
  ENDIF.
ENDFORM.                    " output
