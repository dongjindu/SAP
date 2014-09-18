REPORT ZCORREKESGRRTV .
**********************************************************************
* THIS REPORT REPLACES ZCORREKESBYGR FOR ALL CASES, WHERE THE CHANGED
* BUSINESS PROCESS FOR RETURN DELIVERIES OF OSS NOTE 491785 IS USED
**********************************************************************
* Search P.O. confirmation records with confirmation key
* and GR assignment
* Compute GR quantity and check against DABMG
* Compute Total of GR
* correct EKES-DABMG, if not correct

TABLES: ekes.
TABLES: ekbe, t163g.
TABLES: ekpo.
DATA: t_ekpo LIKE ekpo OCCURS 0 WITH HEADER LINE.
DATA: t_ekes LIKE ekes OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF t_ekbe OCCURS 20.
        INCLUDE STRUCTURE ekbe                        .
DATA: END OF t_ekbe.                          .
DATA: BEGIN OF t_ekes_upd OCCURS 20,
       ebeln LIKE ekes-ebeln,
       ebelp LIKE ekes-ebelp,
       etens LIKE ekes-etens,
       menge LIKE ekes-menge,
       dabmg_old LIKE ekes-dabmg,
       dabmg_new LIKE ekes-dabmg,
       gr_total LIKE ekes-dabmg,
           END OF t_ekes_upd.                          .
DATA: BEGIN OF xt163g OCCURS 5.
        INCLUDE STRUCTURE t163g.
DATA: END OF xt163g.
DATA: h_ebtyp LIKE t163g-ebtyp,
      h_wemng LIKE ekbe-menge,
      h_wemng_total LIKE ekbe-menge,
      ekbe_shkz(1) TYPE c.

DATA: int_eket LIKE beket OCCURS 0,
      wa_eket TYPE beket,
      eket_updkz(1) TYPE c.


SELECT-OPTIONS: po_nr FOR ekpo-ebeln.
SELECT-OPTIONS: po_item FOR ekpo-ebelp.

PARAMETERS:  update AS CHECKBOX DEFAULT ' '.

START-OF-SELECTION.
  REFRESH  t_ekes_upd.

  SELECT * FROM t163g INTO TABLE xt163g
           WHERE wezuo NE space.

  SELECT * FROM ekpo INTO TABLE t_ekpo
           WHERE ebeln IN po_nr
           AND   ebelp IN po_item
           AND   bstae NE space.
  LOOP AT t_ekpo.
    MOVE t_ekpo TO ekpo.
    CLEAR xt163g.
    CLEAR h_ebtyp.
    LOOP AT xt163g WHERE bstae EQ t_ekpo-bstae.
      h_ebtyp = xt163g-ebtyp.
      EXIT.
    ENDLOOP.
    CHECK sy-subrc EQ 0.

* confirmation key with GR assignment found,
* check dabmg from EKES against GRs/reversals from EKBE
* where ekes-etens = ekbe-etens
    REFRESH t_ekes.
    SELECT * FROM ekes INTO TABLE t_ekes
              WHERE ebeln EQ ekpo-ebeln
              AND   ebelp EQ ekpo-ebelp
              AND   ebtyp EQ h_ebtyp.

    IF sy-subrc EQ 0.
      LOOP AT t_ekes.
        REFRESH t_ekbe.
        CLEAR h_wemng.
        SELECT * FROM ekbe INTO TABLE t_ekbe
                  WHERE ebeln EQ t_ekes-ebeln
                  AND   ebelp EQ t_ekes-ebelp
                  AND   zekkn EQ '00'
                  AND   vgabe EQ '1'
                  AND   etens EQ t_ekes-etens.
        IF sy-subrc EQ 0.
          LOOP AT t_ekbe.
            IF ( t_ekbe-shkzg = 'S' AND t_ekbe-et_upd IS INITIAL ) OR
               ( t_ekbe-shkzg = 'H' AND NOT t_ekbe-et_upd IS INITIAL ).
              ekbe_shkz = 'S'.
          ELSEIF ( t_ekbe-shkzg = 'H' AND t_ekbe-et_upd IS INITIAL ) OR
            ( t_ekbe-shkzg = 'S' AND NOT t_ekbe-et_upd IS INITIAL ).
              ekbe_shkz = 'H'.
            ENDIF.
            CASE ekbe_shkz.
              WHEN 'S'.                "Goods receipt
                h_wemng = h_wemng + t_ekbe-menge.
              WHEN 'H'.                "Reversal
                h_wemng = h_wemng - t_ekbe-menge.
              WHEN OTHERS.
************error.
            ENDCASE.
          ENDLOOP.

          CLEAR h_wemng_total.
          REFRESH t_ekbe.
          SELECT * FROM ekbe INTO TABLE t_ekbe
                    WHERE ebeln EQ t_ekes-ebeln
                    AND   ebelp EQ t_ekes-ebelp
                    AND   zekkn EQ '00'
                    AND   vgabe EQ '1'.
          LOOP AT t_ekbe.
            CASE t_ekbe-shkzg.
              WHEN 'S'.                "Goods receipt
                h_wemng_total = h_wemng_total + t_ekbe-menge.
              WHEN 'H'.                "Reversal
                h_wemng_total = h_wemng_total - t_ekbe-menge.
              WHEN OTHERS.
************error.
            ENDCASE.
          ENDLOOP.
          IF h_wemng NE t_ekes-dabmg.
            t_ekes_upd-ebeln = t_ekes-ebeln.
            t_ekes_upd-ebelp = t_ekes-ebelp.
            t_ekes_upd-etens = t_ekes-etens.
            t_ekes_upd-menge     = t_ekes-menge.
            t_ekes_upd-dabmg_old = t_ekes-dabmg.
            t_ekes_upd-dabmg_new = h_wemng.
            t_ekes_upd-gr_total  = h_wemng_total.
            APPEND  t_ekes_upd.
          ENDIF.
        ELSE.
          IF NOT t_ekes-dabmg IS INITIAL.
            CLEAR h_wemng_total.
            REFRESH t_ekbe.
            SELECT * FROM ekbe INTO TABLE t_ekbe
                      WHERE ebeln EQ t_ekes-ebeln
                      AND   ebelp EQ t_ekes-ebelp
                      AND   zekkn EQ '00'
                      AND   vgabe EQ '1'.
            LOOP AT t_ekbe.
              CASE t_ekbe-shkzg.
                WHEN 'S'.              "Goods receipt
                  h_wemng_total = h_wemng_total + t_ekbe-menge.
                WHEN 'H'.              "Reversal
                  h_wemng_total = h_wemng_total - t_ekbe-menge.
                WHEN OTHERS.
************error.
              ENDCASE.
            ENDLOOP.
            t_ekes_upd-ebeln = t_ekes-ebeln.
            t_ekes_upd-ebelp = t_ekes-ebelp.
            t_ekes_upd-etens = t_ekes-etens.
            t_ekes_upd-menge     = t_ekes-menge.
            t_ekes_upd-dabmg_old = t_ekes-dabmg.
            t_ekes_upd-dabmg_new = 0.
            APPEND  t_ekes_upd.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  WRITE: 'Confirmation records for changing'.
  WRITE: / 'P.O.No Item Sch.line              ',
      'Qty         DABMG_old         DABMG_new          Total_GR'.
  LOOP AT t_ekes_upd.
    WRITE: / t_ekes_upd-ebeln, t_ekes_upd-ebelp, t_ekes_upd-etens,
              t_ekes_upd-menge,
              t_ekes_upd-dabmg_old, t_ekes_upd-dabmg_new,
              t_ekes_upd-gr_total.
    IF NOT update IS INITIAL.
      CLEAR ekes.
      MOVE-CORRESPONDING t_ekes_upd TO ekes.
      UPDATE ekes
        SET dabmg = t_ekes_upd-dabmg_new
         WHERE ebeln = ekes-ebeln
         AND   ebelp = ekes-ebelp
         AND   etens = ekes-etens.
      IF sy-subrc NE 0.
*     raise update_mistake.
      ENDIF.
    ENDIF.
  ENDLOOP.

* redistribution

  LOOP AT t_ekes_upd.

    REFRESH int_eket.

    READ TABLE t_ekpo WITH KEY ebeln = t_ekes_upd-ebeln
                               ebelp = t_ekes_upd-ebelp
                          INTO ekpo.

    SELECT * FROM eket INTO TABLE int_eket
                  WHERE ebeln EQ ekpo-ebeln
                    AND ebelp EQ ekpo-ebelp.

    CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
         EXPORTING
              i_bstae    = ekpo-bstae
              i_ebeln    = ekpo-ebeln
              i_ebelp    = ekpo-ebelp
              i_funkt    = 'DV'
              i_werks    = ekpo-werks
         IMPORTING
              e_vb_updkz = eket_updkz
         TABLES
              xeket      = int_eket.
    IF NOT eket_updkz IS INITIAL AND NOT update IS INITIAL.
      LOOP AT int_eket INTO wa_eket WHERE updkz EQ 'U'.
        UPDATE eket
           SET dabmg = wa_eket-dabmg
           WHERE ebeln EQ wa_eket-ebeln
             AND ebelp EQ wa_eket-ebelp
             AND etenr EQ wa_eket-etenr.
      ENDLOOP.
    ENDIF.

  ENDLOOP. "t_ekes_upd
