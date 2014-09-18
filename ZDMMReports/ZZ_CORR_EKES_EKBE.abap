REPORT ZZ_CORR_EKES_EKBE .

TABLES: ekes, ekbe, ekbeh, ekpo, lips.

DATA: it_ekes   TYPE STANDARD TABLE OF ekes,
      wa_ekes   TYPE ekes,
      it_eket   TYPE STANDARD TABLE OF beket,
      wa_eket   TYPE beket,
      it_ekbe   TYPE STANDARD TABLE OF ekbe,
      wa_t163g  TYPE t163g,
      wa_t163d  TYPE t163d,
      lf_dabmg  LIKE ekes-dabmg,
      lf_locked TYPE xfeld.

SELECT-OPTIONS: p_ebeln FOR  ekbe-ebeln.
PARAMETERS:     p_test  TYPE xfeld DEFAULT 'X'.

START-OF-SELECTION.
  SELECT SINGLE * FROM t163d INTO wa_t163d
                  WHERE ibtyp = '2'.
  IF sy-subrc > 0.
    WRITE: / 'Customizing in table T163D missing'.
    STOP.
  ENDIF.
  SELECT * FROM ekes INTO TABLE it_ekes
           WHERE ebeln IN p_ebeln
           ORDER BY PRIMARY KEY.
  IF sy-subrc = 0.
    LOOP AT it_ekes INTO wa_ekes.
      AT NEW ebelp.
        CALL FUNCTION 'ENQUEUE_EMEKPOE'
          EXPORTING
            ebeln          = wa_ekes-ebeln
            ebelp          = wa_ekes-ebelp
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
        IF sy-subrc = 0.
          CLEAR lf_locked.
          CALL FUNCTION 'ME_EKPO_SINGLE_READ'
               EXPORTING
                    pi_ebeln         = wa_ekes-ebeln
                    pi_ebelp         = wa_ekes-ebelp
               IMPORTING
                    po_ekpo          = ekpo
               EXCEPTIONS
                    no_records_found = 1
                    OTHERS           = 2.
        ELSE.
          lf_locked = 'X'.
          WRITE: / wa_ekes-ebeln, wa_ekes-ebelp, 'is locked'.
        ENDIF.
      ENDAT.
      IF lf_locked = 'X'.
        CONTINUE.
      ENDIF.
      IF wa_t163g-bstae <> ekpo-bstae.
        CLEAR wa_t163g.
        SELECT SINGLE * FROM t163g INTO wa_t163g
                        WHERE bstae = ekpo-bstae
                        AND   ebtyp = wa_t163d-ebtyp.
      ENDIF.
      IF wa_t163g-wezuo IS INITIAL.
        WRITE: / wa_ekes-ebeln, wa_ekes-ebelp, 'no GR assignment'.
        CONTINUE.
      ENDIF.
      CLEAR: lf_dabmg.
      REFRESH it_ekbe.
      SELECT * FROM ekbe INTO TABLE it_ekbe
               WHERE ebeln = wa_ekes-ebeln
               AND   ebelp = wa_ekes-ebelp
               AND   vgabe = '1'
               AND   etens = wa_ekes-etens
               AND   belnr <> space.
      SELECT * FROM ekbeh APPENDING TABLE it_ekbe
               WHERE ebeln = wa_ekes-ebeln
               AND   ebelp = wa_ekes-ebelp
               AND   vgabe = '1'
               AND   etens = wa_ekes-etens.
      LOOP AT it_ekbe INTO ekbe.
        IF ekbe-shkzg = 'S'.
          lf_dabmg = lf_dabmg + ekbe-menge.
        ELSE.
          lf_dabmg = lf_dabmg - ekbe-menge.
        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.
        IF wa_ekes-estkz = '4' AND lf_dabmg < 0.
          lf_dabmg = ABS( lf_dabmg ).
        ENDIF.
        IF wa_ekes-dabmg <> lf_dabmg
        OR ( wa_ekes-dabmg = lf_dabmg  AND
             wa_ekes-menge < wa_ekes-dabmg ).
          WRITE: / wa_ekes-vbeln, wa_ekes-vbelp, 'DABMG old:',
                   wa_ekes-dabmg, 'new:', lf_dabmg.
          IF p_test IS INITIAL.
            UPDATE ekes SET menge = lf_dabmg
                            dabmg = lf_dabmg
                        WHERE ebeln = wa_ekes-ebeln
                        AND   ebelp = wa_ekes-ebelp
                        AND   etens = wa_ekes-etens.
          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE * FROM lips WHERE vbeln = wa_ekes-vbeln
                                  AND   posnr = wa_ekes-vbelp.
        IF sy-subrc = 0.
          IF wa_ekes-dabmg > 0.
            WRITE: / wa_ekes-vbeln, wa_ekes-vbelp, 'DABMG old:',
                     wa_ekes-dabmg, 'new:', 0.
            IF p_test IS INITIAL.
              UPDATE ekes SET dabmg = 0
                          WHERE ebeln = wa_ekes-ebeln
                          AND   ebelp = wa_ekes-ebelp
                          AND   etens = wa_ekes-etens.
            ENDIF.
          ENDIF.
        ELSE.
          WRITE: / wa_ekes-vbeln, wa_ekes-vbelp, 'DABMG old:',
                   wa_ekes-dabmg, 'new:', lf_dabmg.
          IF p_test IS INITIAL.
            UPDATE ekes SET menge = 0
                            dabmg = 0
                        WHERE ebeln = wa_ekes-ebeln
                        AND   ebelp = wa_ekes-ebelp
                        AND   etens = wa_ekes-etens.
          ENDIF.
        ENDIF.
      ENDIF.
      AT END OF ebelp.
        REFRESH it_eket.
        SELECT * FROM eket INTO TABLE it_eket
                 WHERE ebeln = wa_ekes-ebeln
                 AND   ebelp = wa_ekes-ebelp.
        CALL FUNCTION 'ME_EKPO_SINGLE_READ'
             EXPORTING
                  pi_ebeln         = wa_ekes-ebeln
                  pi_ebelp         = wa_ekes-ebelp
             IMPORTING
                  po_ekpo          = ekpo
             EXCEPTIONS
                  no_records_found = 1
                  OTHERS           = 2.
        CALL FUNCTION 'ME_CONFIRMATIONS_REFRESH'.
        CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
             EXPORTING
                  i_bstae = ekpo-bstae
                  i_ebeln = wa_ekes-ebeln
                  i_ebelp = wa_ekes-ebelp
                  i_funkt = 'DV'
                  i_werks = ekpo-werks
             TABLES
                  xeket   = it_eket.
        IF p_test IS INITIAL.
          LOOP AT it_eket INTO wa_eket WHERE updkz <> space.
            UPDATE eket SET dabmg = wa_eket-dabmg
                        WHERE ebeln = wa_eket-ebeln
                        AND   ebelp = wa_eket-ebelp
                        AND   etenr = wa_eket-etenr.
          ENDLOOP.
        ENDIF.
        CALL FUNCTION 'DEQUEUE_EMEKPOE'
          EXPORTING
            ebeln = wa_ekes-ebeln
            ebelp = wa_ekes-ebelp.
      ENDAT.
    ENDLOOP.
  ELSE.
    WRITE: / 'No EKES entries found for', p_ebeln.
  ENDIF.
