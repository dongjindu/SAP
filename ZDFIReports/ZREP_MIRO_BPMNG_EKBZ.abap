*&---------------------------------------------------------------------*
*& Report ZREP_MIRO_BPMNG_EKBZ                                         *
*&---------------------------------------------------------------------*
*& Report to correct wrong updates of field BPMNG in table EKBZ        *
*&                                                                     *
*& Select options:                                                     *
*& EBELN = PO                                                          *
*& EBELP = PO item                                                     *
*& GJAHR = document year                                               *
*& BELNR = MM invoice document                                         *
*& BUZEI = item in material document                                   *
*& BUKRS = company code                                                *
*& Parameters:                                                         *
*& UPDATE = Flag 'Update' ('X' = Update, ' ' = Test Run)               *
*                                                                      *
*&---------------------------------------------------------------------*

REPORT zrep_miro_bpmng_ekbz.

*--- defining tables and structures -----------------------------------*
TYPE-POOLS: mrm, mmcr.

TABLES: ekbz,
        rseg,
        ekpo.

DATA: s_ekbz LIKE ekbz,
      s_ekpo LIKE ekpo.

DATA: s_ekbz_ebeln_old LIKE s_ekbz-ebeln,
      s_ekbz_ebelp_old LIKE s_ekbz-ebelp.

DATA: BEGIN OF tab_ekbz OCCURS 0.
        INCLUDE STRUCTURE ekbz.
DATA: END OF tab_ekbz.

DATA: BEGIN OF tab_ekpo OCCURS 0.
        INCLUDE STRUCTURE ekpo.
DATA: END OF tab_ekpo.

DATA: BEGIN OF tab_ekpo_key OCCURS 100,
       ebeln LIKE s_ekbz-ebeln,
       ebelp LIKE s_ekbz-ebelp.
DATA: END OF tab_ekpo_key.

* set up start screen -------------------------------------------------*
PARAMETERS:      suser(10) TYPE c.
SELECT-OPTIONS:  ebeln FOR ekbz-ebeln MATCHCODE OBJECT mekkh,
                 ebelp FOR ekbz-ebelp MATCHCODE OBJECT mekkh,
                 gjahr FOR ekbz-gjahr MATCHCODE OBJECT mekkh,
                 belnr FOR ekbz-belnr MATCHCODE OBJECT mekkh,
                 buzei FOR ekbz-buzei MATCHCODE OBJECT mekkh,
                 bukrs FOR ekpo-bukrs MATCHCODE OBJECT mekkh.
PARAMETERS:      update AS CHECKBOX DEFAULT ' '.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*

  IF suser NE 'SAP' AND update EQ 'X'.
    WRITE: / 'Please enter correct user before update!'.
    EXIT.
  ENDIF.

*--- select affected entries from table EKBZ --------------------------*
  SELECT * FROM  ekbz INTO TABLE tab_ekbz
         WHERE  ebeln IN ebeln
         AND    ebelp IN ebelp
         AND    ( vgabe EQ '2' OR vgabe EQ '3' )
         AND    gjahr IN gjahr
         AND    belnr IN belnr
         AND    buzei IN buzei
         ORDER BY ebeln ebelp.


  IF sy-subrc NE 0.
    WRITE: 'No relevant entries found in table EKBZ.'.
    EXIT.
  ENDIF.

  WRITE AT: /0(10) 'EBELN', 12(5)  'EBELP',
            18(5)  'STUNR', 24(5)  'ZAEHK',
            30(5)  'KSCHL', 36(10) 'BELNR',
            47(5)  'BUZEI', 53(5)  'GJAHR',
            59(10) 'BUDAT', 70(12) 'MENGE',
            83(5)  'MEINS', 89(12) 'BPMNG',
            102(5) 'BPRME'.
  ULINE.

*--- extract entries from table tab_ekbz ------------------------------*
  LOOP AT tab_ekbz INTO s_ekbz.
    IF s_ekbz_ebeln_old NE s_ekbz-ebeln OR
       s_ekbz_ebelp_old NE s_ekbz-ebelp.

      s_ekbz_ebeln_old = s_ekbz-ebeln.
      s_ekbz_ebelp_old = s_ekbz-ebelp.

      tab_ekpo_key-ebeln = s_ekbz-ebeln.
      tab_ekpo_key-ebelp = s_ekbz-ebelp.

      APPEND tab_ekpo_key.
    ENDIF.
  ENDLOOP.

*--- only select relevant entries from table EKPO ---------------------*
  SELECT * FROM ekpo INTO TABLE tab_ekpo
           FOR ALL ENTRIES IN tab_ekpo_key
           WHERE ebeln = tab_ekpo_key-ebeln
           AND   ebelp = tab_ekpo_key-ebelp
           AND   pstyp NE '7'
           AND   bukrs IN bukrs.

  SORT tab_ekpo BY ebeln ebelp.

  IF sy-subrc NE 0.
    WRITE: 'No relevant entries found in table EKPO.'.
    EXIT.
  ENDIF.

*--- detect cases where BPMNG was updated with a wrong value ----------*
  LOOP AT tab_ekbz INTO s_ekbz.
    READ TABLE tab_ekpo INTO s_ekpo WITH KEY
                     ebeln = s_ekbz-ebeln
                     ebelp = s_ekbz-ebelp
                     BINARY SEARCH.

    IF sy-subrc EQ 0.

      IF s_ekpo-meins EQ s_ekpo-bprme AND
      s_ekbz-menge NE s_ekbz-bpmng.


        FORMAT COLOR 3 ON.
        WRITE AT: /0(10) s_ekbz-ebeln, 12(5)  s_ekbz-ebelp,
                  18(5)  s_ekbz-stunr, 24(5)  s_ekbz-zaehk,
                  30(5)  s_ekbz-kschl, 36(10) s_ekbz-belnr,
                  47(5)  s_ekbz-buzei, 53(5)  s_ekbz-gjahr,
                  59(10) s_ekbz-budat, 70(12) s_ekbz-menge,
                  83(5)  s_ekbz-bpmng, 89(12) s_ekpo-meins,
                  102(5) s_ekpo-bprme.
        FORMAT COLOR 3 OFF.

        FORMAT COLOR 2 ON.
        WRITE AT: /91 ' '.
        FORMAT COLOR 2 OFF.

        IF update EQ 'X'.
          UPDATE ekbz SET bpmng = s_ekbz-menge
                      WHERE ebeln EQ  s_ekbz-ebeln
                      AND   ebelp EQ  s_ekbz-ebelp
                      AND   stunr EQ  s_ekbz-stunr
                      AND   zaehk EQ  s_ekbz-zaehk
                      AND   vgabe EQ  s_ekbz-vgabe
                      AND   gjahr EQ  s_ekbz-gjahr
                      AND   belnr EQ  s_ekbz-belnr
                      AND   buzei EQ  s_ekbz-buzei.
          IF sy-subrc = 0.
            FORMAT COLOR 5 ON.
            WRITE AT: 92 'BPMNG corrected'.
            FORMAT COLOR 5 OFF.
          ENDIF.
        ELSE.
          FORMAT COLOR 6 ON.
          WRITE AT: 94 'to be updated'.
          FORMAT COLOR 6 OFF.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.
