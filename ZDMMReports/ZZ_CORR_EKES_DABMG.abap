REPORT ZZ_CORR_EKES_DABMG .

TABLES: ekes.
DATA: it_ekes LIKE ekes OCCURS 0 WITH HEADER LINE,
      it_eket LIKE beket OCCURS 0 WITH HEADER LINE,
      ls_ekpo TYPE ekpo,
      ls_ekes TYPE ekes,
      lf_error TYPE xfeld.

SELECT-OPTIONS: s_ebeln FOR ekes-ebeln.
PARAMETERS: p_test TYPE xfeld DEFAULT 'X'.

START-OF-SELECTION.
  SELECT * FROM ekes INTO TABLE it_ekes
           WHERE ebeln IN s_ebeln
           AND   estkz = '4'
           AND   dabmg > 0.
  LOOP AT it_ekes.
    SELECT SINGLE * FROM ekes INTO ls_ekes
           WHERE ebeln = it_ekes-ebeln
           AND   ebelp = it_ekes-ebelp
           AND   etens = it_ekes-ref_etens
           AND   estkz = '2'.
    IF sy-subrc = 0.
      IF ls_ekes-dabmg <= 0.
        IF p_test IS INITIAL.
          CALL FUNCTION 'ENQUEUE_EMEKPOE'
               EXPORTING
                    ebeln          = ls_ekes-ebeln
                    ebelp          = ls_ekes-ebelp
               EXCEPTIONS
                    foreign_lock   = 1
                    system_failure = 2
                    OTHERS         = 3.
          IF sy-subrc <> 0.
            WRITE: /  ls_ekes-ebeln, ls_ekes-ebelp,
                     'is already locked'.
          ELSE.
            CLEAR lf_error.
            CALL FUNCTION 'ME_EKPO_SINGLE_READ'
                 EXPORTING
                      pi_ebeln         = ls_ekes-ebeln
                      pi_ebelp         = ls_ekes-ebelp
                 IMPORTING
                      po_ekpo          = ls_ekpo
                 EXCEPTIONS
                      no_records_found = 1
                      OTHERS           = 2.
            IF sy-subrc <> 0.
              lf_error = 'X'.
            ENDIF.
            REFRESH it_eket.
            SELECT * FROM eket INTO TABLE it_eket
                     WHERE ebeln = ls_ekes-ebeln
                     AND   ebelp = ls_ekes-ebelp.
            IF sy-subrc <> 0.
              lf_error = 'X'.
            ENDIF.
            UPDATE ekes SET dabmg = ls_ekes-menge
                        WHERE ebeln = ls_ekes-ebeln
                        AND   ebelp = ls_ekes-ebelp
                        AND   etens = ls_ekes-etens.
            IF sy-subrc <> 0.
              lf_error = 'X'.
            ENDIF.
            CALL FUNCTION 'ME_CONFIRMATIONS_REFRESH'.
            CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
                 EXPORTING
                      i_bstae = ls_ekpo-bstae
                      i_ebeln = ls_ekpo-ebeln
                      i_ebelp = ls_ekpo-ebelp
                      i_funkt = 'DV'
                      i_werks = ls_ekpo-werks
                 TABLES
                      xeket   = it_eket.
            LOOP AT it_eket WHERE updkz <> space.
              UPDATE eket SET dabmg = it_eket-dabmg
                          WHERE ebeln = it_eket-ebeln
                          AND   ebelp = it_eket-ebelp
                          AND   etenr = it_eket-etenr.
              IF sy-subrc <> 0.
                lf_error = 'X'.
              ENDIF.
            ENDLOOP.
            IF lf_error IS INITIAL.
              COMMIT WORK.
              WRITE: /  ls_ekes-ebeln, ls_ekes-ebelp, ls_ekes-etens,
                       'is updated'.
            ELSE.
              ROLLBACK WORK.
              WRITE: /  ls_ekes-ebeln, ls_ekes-ebelp, ls_ekes-etens,
                       'Error: update is not possible!'.
            ENDIF.
            CALL FUNCTION 'DEQUEUE_EMEKPOE'
                 EXPORTING
                      ebeln = ls_ekes-ebeln
                      ebelp = ls_ekes-ebelp.
          ENDIF.
        ELSE.
          WRITE: / ls_ekes-ebeln, ls_ekes-ebelp, ls_ekes-etens,
                   'needs a correction'.
        ENDIF.
      ENDIF.
    ELSE.
      WRITE: / it_ekes-ebeln, it_ekes-ebelp, it_ekes-etens,
               'Error: referenced entry', it_ekes-ref_etens,
               'is missing!'.
    ENDIF.
  ENDLOOP.
