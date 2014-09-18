REPORT zacot030 .

TABLES: t030.
DATA: i_t030 LIKE t030 OCCURS 0 WITH HEADER LINE,
      w_t030 LIKE t030.

PARAMETERS: p_ktopl LIKE t030-ktopl OBLIGATORY ,
            p_ktosl LIKE t030-ktosl OBLIGATORY ,
            p_bwmod LIKE t030-bwmod,
            p_komok LIKE t030-komok,
            p_bklas LIKE t030-bklas.

PARAMETERS: p_act(1) TYPE c,
            p_hkont LIKE t030-konts OBLIGATORY.

DATA: e_mode(3) TYPE c.
*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  DATA: l_cousertype(3) TYPE c.
  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
  e_mode = l_cousertype.


*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  CHECK e_mode = 'ADM'.

  IF p_act NA 'I'.
    SELECT SINGLE * INTO w_t030
      FROM t030
      WHERE ktopl = p_ktopl
        AND ktosl = p_ktosl
        AND bwmod = p_bwmod
        AND komok = p_komok
        AND bklas = p_bklas.
    IF sy-subrc = 0.
      WRITE:/ w_t030-bwmod, w_t030-bklas, w_t030-konts.

      IF p_act = 'D'.
        DELETE t030 FROM w_t030.
        WRITE:/ '*** deleted'.
      ELSEIF p_act = 'U'.
        w_t030-konts = p_hkont.
        w_t030-konth = p_hkont.
        UPDATE t030 FROM w_t030.
        WRITE:/ '*** updated'.
      ENDIF.
    ENDIF.

  ELSEIF p_act = 'I'.
    w_t030-ktopl = p_ktopl.
    w_t030-ktosl = p_ktosl.
    w_t030-bwmod = p_bwmod.
    w_t030-komok = p_komok.
    w_t030-bklas = p_bklas.

    w_t030-konts = p_hkont.
    w_t030-konth = p_hkont.
    INSERT t030 FROM w_t030.
    WRITE:/ '*** inserted'.

  ENDIF.
