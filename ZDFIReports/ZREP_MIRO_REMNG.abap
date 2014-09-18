*&---------------------------------------------------------------------*
*& Report ZREP_MIRO_REMNG                                              *
*&---------------------------------------------------------------------*
*& Report to find POs with negative REMNG                              *
*&                                                                     *
*& Select options:                                                     *
*& EBELN = PO                                                          *
*& EBELP = PO item                                                     *
*& BUKRS = Company code                                                *
*& WERKS = Plant                                                       *
*& DATE  = Creation date of PO item                                    *
*& LEBRE = Flag: Only check PO items with Service-based IV active      *
*&---------------------------------------------------------------------*

REPORT zrep_miro_remng .

TABLES: ekko,
        ekpo.

*--- internal tables --------------------------------------------------*
DATA: s_ekko LIKE ekko.
DATA: s_ekpo LIKE ekpo.

DATA: BEGIN OF k_ekpo OCCURS 0.
        INCLUDE STRUCTURE ekpo.
DATA: END OF k_ekpo.

DATA: tab_ek08rn LIKE ek08rn OCCURS 0.
DATA: s_ek08rn   LIKE ek08rn.

DATA: ebeln_old LIKE ekpo-ebeln,
      zekkn99   TYPE c VALUE space.

*--- set up start screen ----------------------------------------------*
SELECT-OPTIONS:  ebeln  FOR ekpo-ebeln,
                 ebelp  FOR ekpo-ebelp,
                 bukrs  FOR ekpo-bukrs,
                 werks  FOR ekpo-werks,
                 date   FOR ekko-bedat.
PARAMETERS:      lebre  AS CHECKBOX DEFAULT ' '.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  FORMAT COLOR 4 ON.
  WRITE AT: /0(10) 'EBELN', 12(5)  'EBELP',
            18(5)  'BUKRS', 24(5)  'WERKS',
            30(5)  'PSTYP', 36(11) 'REMNG',
            48(11) 'BPREM', 60(11) 'WEMNG'.
  FORMAT COLOR OFF.

*--- select purchase orders -------------------------------------------*
  IF lebre EQ space.
*--- select only normal POs -------------------------------------------*
    SELECT * FROM ekpo INTO TABLE k_ekpo
             WHERE ebeln IN ebeln
             AND   ebelp IN ebelp
             AND   loekz EQ space
             AND   bukrs IN bukrs
             AND   werks IN werks
             AND   ( pstyp NE '1' AND pstyp NE '9' )
             ORDER BY ebeln ebelp.
    IF sy-subrc NE 0.
      WRITE: 'No PO items found.'.
      EXIT.
    ENDIF.
  ELSE.
*--- select only service POs with service-based IV --------------------*
    SELECT * FROM ekpo INTO TABLE k_ekpo
             WHERE ebeln IN ebeln
             AND   ebelp IN ebelp
             AND   loekz EQ space
             AND   bukrs IN bukrs
             AND   werks IN werks
             AND   pstyp NE '1'
             AND   lebre EQ 'X'
             ORDER BY ebeln ebelp.
    IF sy-subrc NE 0.
      WRITE: 'No PO items found.'.
      EXIT.
    ENDIF.
  ENDIF.

  SORT k_ekpo BY ebeln ebelp.
  ebeln_old = 0.

*--- loop over all selected POs ---------------------------------------*
  LOOP AT k_ekpo INTO s_ekpo.

    IF ebeln_old NE s_ekpo-ebeln.
      ebeln_old = s_ekpo-ebeln.

      CALL FUNCTION 'ME_EKKO_SINGLE_READ'
           EXPORTING
                pi_ebeln         = s_ekpo-ebeln
           IMPORTING
                po_ekko          = s_ekko
           EXCEPTIONS
                no_records_found = 1
                OTHERS           = 2.
      IF sy-subrc <> 0.
        WRITE: / 'PO ', k_ekpo-ebeln, ' does not exist.'.
      ENDIF.
    ENDIF.

    IF s_ekko-bedat IN date.

      CALL FUNCTION 'ME_READ_ITEM_INVOICE'
           EXPORTING
                display        = 'X'
                ebelp          = s_ekpo-ebelp
                iekko          = s_ekko
                re_kursf       = s_ekko-wkurs
                re_waers       = s_ekko-waers
                re_wwert       = sy-datum
           TABLES
                xek08rn        = tab_ek08rn
           EXCEPTIONS
                not_found_any  = 1
                not_found_one  = 2
                not_valid_any  = 3
                not_valid_one  = 4
                enqueue_failed = 5
                OTHERS         = 6.

      IF sy-subrc EQ 0.

        CLEAR zekkn99.
        LOOP AT tab_ek08rn INTO s_ek08rn.
          IF s_ek08rn-zekkn = '99'.
            zekkn99 = 'X'.
          ENDIF.
        ENDLOOP.

        IF zekkn99 = space.
          LOOP AT tab_ek08rn INTO s_ek08rn.
            IF s_ek08rn-remng < 0.
              FORMAT COLOR 3 ON.
              WRITE AT: /0(10) s_ek08rn-ebeln, 12(5)  s_ek08rn-ebelp,
                        18(5)  s_ek08rn-bukrs, 24(5)  s_ek08rn-werks,
                        30(5)  s_ek08rn-pstyp, 36(11) s_ek08rn-remng,
                        48(11) s_ek08rn-bprem, 60(11) s_ek08rn-wemng.
              FORMAT COLOR OFF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
