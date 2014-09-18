** ------------------------------------------------------------------ **
** This report corrects the field EKES-MENGE and sets it to the
** quantity of the field EKES-DABMG if the confirmation is a shipping
** notification and the quantity of the shipping notification can not
** be changed via transaction VL32/VL32N.
** For further information see note 521164
** ------------------------------------------------------------------ **

REPORT ZCORR_EKESMENGE .

TABLES: ekpo, ekes.
SELECT-OPTIONS: mat_nr FOR ekpo-matnr,
                po_nr FOR ekpo-ebeln OBLIGATORY,
                avis_nr FOR ekes-vbeln.
PARAMETERS: datum LIKE ekes-eindt OBLIGATORY DEFAULT sy-datum,
            p_test AS CHECKBOX DEFAULT 'x'.
DATA:  int_wezuo LIKE t163g OCCURS 0,
       int_ekpo LIKE ekpo OCCURS 0,
       wa_ekpo TYPE ekpo,
       int_eket LIKE beket OCCURS 0,
       wa_eket TYPE beket,
       n LIKE sy-tabix,
       int_ekes LIKE ekes OCCURS 0,
       wa_ekes TYPE ekes,
       int_ekbe LIKE ekbe OCCURS 0,
       wa_ekbe TYPE ekbe,
       int_cor_ekes LIKE ekes OCCURS 0,
       wa_cor_ekes TYPE ekes,
       ekbe_storno LIKE ekbe-menge,
       wa_t163d TYPE t163d,
       wa_vbup TYPE vbup,
       no_retdlv(1) TYPE c,
       eket_updkz(1) TYPE c.

SELECT * FROM t163g INTO TABLE int_wezuo WHERE wezuo EQ 'X'.
SELECT * FROM ekpo  INTO TABLE int_ekpo FOR ALL ENTRIES IN int_wezuo
WHERE bstae EQ int_wezuo-bstae AND matnr IN mat_nr
                               AND ebeln IN po_nr
                               AND loekz EQ space.

SELECT SINGLE * FROM t163d INTO wa_t163d WHERE ibtyp = '2'.

* loop über die selektierten Belege


LOOP AT int_ekpo INTO wa_ekpo.
  CLEAR: int_ekes, int_cor_ekes, wa_ekes, wa_cor_ekes,
         int_eket, wa_eket.

* Selektion der ekes-Sätze zu dem ekpo-Satz

  SELECT * FROM ekes INTO TABLE int_ekes
           WHERE ebeln EQ wa_ekpo-ebeln AND
           ebelp EQ wa_ekpo-ebelp AND
           ebtyp EQ wa_t163d-ebtyp AND
           vbeln IN avis_nr AND
           eindt LE datum.

  LOOP AT int_ekes INTO wa_ekes.
    IF wa_ekes-menge LE wa_ekes-dabmg.
      CONTINUE.
    ELSE.
      CLEAR wa_vbup.
      SELECT SINGLE * FROM vbup INTO wa_vbup
                          WHERE vbeln EQ wa_ekes-vbeln
                            AND posnr EQ wa_ekes-vbelp.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      IF wa_vbup-wbsta EQ 'C'.


        SELECT * FROM ekbe into table int_ekbe
                 WHERE ebeln EQ wa_ekes-ebeln AND
                       ebelp EQ wa_ekes-ebelp AND
                       etens EQ wa_ekes-etens AND
                       bewtp EQ 'E' AND
                       shkzg EQ 'H'.

        CHECK sy-subrc EQ '0'.
        WRITE:/ wa_ekes-ebeln, wa_ekes-ebelp, wa_ekes-etens,
                wa_ekes-vbeln, wa_ekes-menge,'->',wa_ekes-dabmg.
        MOVE-CORRESPONDING wa_ekes TO wa_cor_ekes.
        wa_cor_ekes-menge = wa_ekes-dabmg.
        APPEND wa_cor_ekes TO int_cor_ekes.
        n = n + 1.
      ENDIF.
    ENDIF.
  ENDLOOP.         "int_ekes

* sperren des EKPO-satzes

  IF p_test EQ space.
    CALL FUNCTION 'ENQUEUE_EMEKPOE'
         EXPORTING
              ebeln          = wa_ekpo-ebeln
              ebelp          = wa_ekpo-ebelp
         EXCEPTIONS
              foreign_lock   = 2
              system_failure = 3.
    IF sy-subrc EQ 0.

* EKES-satz updaten

      LOOP AT int_cor_ekes INTO wa_cor_ekes.
        WRITE:/'sa-/poitem:', wa_cor_ekes-ebeln, wa_cor_ekes-ebelp,
                wa_cor_ekes-etens, 'updated'.
        UPDATE ekes SET menge = wa_cor_ekes-menge
                    WHERE ebeln EQ wa_cor_ekes-ebeln AND
                    ebelp EQ wa_cor_ekes-ebelp AND
                    etens EQ wa_cor_ekes-etens.
* Neuverteilung
        SELECT * FROM eket INTO TABLE int_eket
                      WHERE ebeln EQ wa_ekpo-ebeln
                        AND ebelp EQ wa_ekpo-ebelp.
        CALL FUNCTION 'ME_CONFIRMATION_MAINTAIN'
             EXPORTING
                  i_bstae    = wa_ekpo-bstae
                  i_ebeln    = wa_ekpo-ebeln
                  i_ebelp    = wa_ekpo-ebelp
                  i_funkt    = 'DV'
                  i_werks    = wa_ekpo-werks
             IMPORTING
                  e_vb_updkz = eket_updkz
             TABLES
                  xeket      = int_eket.
        IF NOT eket_updkz IS INITIAL.
          LOOP AT int_eket INTO wa_eket WHERE updkz EQ 'U'.
            UPDATE eket
               SET dabmg = wa_eket-dabmg
               WHERE ebeln EQ wa_eket-ebeln
                 AND ebelp EQ wa_eket-ebelp
                 AND etenr EQ wa_eket-etenr.
          ENDLOOP.
        ENDIF.


      ENDLOOP.     "int_cor_ekes

* entsperren des EKPO-satzes

      CALL FUNCTION 'DEQUEUE_EMEKPOE'
           EXPORTING
                ebeln  = wa_ekpo-ebeln
                ebelp  = wa_ekpo-ebelp
           EXCEPTIONS
                OTHERS = 1.
    ENDIF.
  ENDIF.
  CLEAR wa_ekpo.
ENDLOOP.              "int_ekpo
WRITE:/ n,'items selected/updated'.
