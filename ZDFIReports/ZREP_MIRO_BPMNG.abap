*&---------------------------------------------------------------------*
*& Report ZREP_MIRO_BPMNG                                              *
*&---------------------------------------------------------------------*
*& Report to correct wrong updates of field BPMNG in table EKBE        *
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
*&---------------------------------------------------------------------*

REPORT zrep_miro_bpmng .

* defining tables and structures
TYPE-POOLS: mrm, mmcr.

TABLES: ekbe,
        rseg,
        ekpo.

DATA: s_ekbe LIKE ekbe,
      s_ekpo LIKE ekpo.

DATA: s_ekbe_ebeln_old LIKE s_ekbe-ebeln,
      s_ekbe_ebelp_old LIKE s_ekbe-ebelp.

DATA: BEGIN OF tab_ekbe OCCURS 0.
        INCLUDE STRUCTURE ekbe.
DATA: END OF tab_ekbe.

DATA: BEGIN OF tab_ekpo OCCURS 0.
        INCLUDE STRUCTURE ekpo.
DATA: END OF tab_ekpo.

DATA: BEGIN OF tab_ekpo_key OCCURS 100,
       ebeln LIKE s_ekbe-ebeln,
       ebelp LIKE s_ekbe-ebelp.
DATA: END OF tab_ekpo_key.

* set up start screen
PARAMETERS:      suser(10) TYPE c.
SELECT-OPTIONS:  ebeln FOR ekbe-ebeln MATCHCODE OBJECT mekkh,
                 ebelp FOR ekbe-ebelp MATCHCODE OBJECT mekkh,
                 gjahr FOR ekbe-gjahr MATCHCODE OBJECT mekkh,
                 belnr FOR ekbe-belnr MATCHCODE OBJECT mekkh,
                 buzei FOR ekbe-buzei MATCHCODE OBJECT mekkh,
                 zekkn FOR ekbe-zekkn MATCHCODE OBJECT mekkh,
                 bukrs FOR ekpo-bukrs MATCHCODE OBJECT mekkh.
PARAMETERS:      update AS CHECKBOX DEFAULT ' '.

*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*

  IF suser NE 'SAP' AND update EQ 'X'.
    WRITE: / 'Please enter correct user before update!'.
    EXIT.
  ENDIF.

* select affected entries from table EKBE
  SELECT * FROM ekbe INTO TABLE tab_ekbe
           WHERE ebeln IN ebeln
           AND   ebelp IN ebelp
           AND   zekkn IN zekkn
           AND   gjahr IN gjahr
           AND   belnr IN belnr
           AND   buzei IN buzei
           AND   ( vgabe EQ '2' OR vgabe EQ '3' )
           ORDER BY ebeln ebelp.

  IF sy-subrc NE 0.
    WRITE: 'No relevant entries found in table EKBE.'.
    EXIT.
  ENDIF.

  WRITE AT: /0(10) 'EBELN', 12(5)  'EBELP', 18(10) 'BELNR',
            29(5)  'BUZEI', 35(5)  'GJAHR', 41(10) 'BUDAT',
            52(12) 'MENGE', 65(10) 'BPMNG', 78(5)  'MEINS',
            84(5)  'BPRME', 90(5)  'PSTYP'.
  ULINE.

* extract entries from table tab_ekbe
  LOOP AT tab_ekbe INTO s_ekbe.
    IF s_ekbe_ebeln_old NE s_ekbe-ebeln OR
       s_ekbe_ebelp_old NE s_ekbe-ebelp.

      s_ekbe_ebeln_old = s_ekbe-ebeln.
      s_ekbe_ebelp_old = s_ekbe-ebelp.

      tab_ekpo_key-ebeln = s_ekbe-ebeln.
      tab_ekpo_key-ebelp = s_ekbe-ebelp.

      APPEND tab_ekpo_key.
    ENDIF.
  ENDLOOP.

* only select relevant entries from table EKPO
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

* detect cases where BPMNG was updated with a wrong value
  LOOP AT tab_ekbe INTO s_ekbe.
    READ TABLE tab_ekpo INTO s_ekpo WITH KEY
                     ebeln = s_ekbe-ebeln
                     ebelp = s_ekbe-ebelp
                     BINARY SEARCH.

    IF sy-subrc EQ 0.

      IF s_ekpo-meins EQ s_ekpo-bprme AND
      s_ekbe-menge NE s_ekbe-bpmng.

        WRITE AT: /0(10) s_ekbe-ebeln, 12(5)  s_ekbe-ebelp,
                  18(10) s_ekbe-belnr, 29(5)  s_ekbe-buzei,
                  35(5)  s_ekbe-gjahr, 41(10) s_ekbe-budat,
                  52(12) s_ekbe-menge, 65(12) s_ekbe-bpmng,
                  78(5)  s_ekpo-meins, 84(5)  s_ekpo-bprme,
                  90(2)  s_ekpo-pstyp.

        IF update EQ 'X'.
          UPDATE ekbe SET bpmng = s_ekbe-menge
                      WHERE ebeln EQ  s_ekbe-ebeln
                      AND   ebelp EQ  s_ekbe-ebelp
                      AND   zekkn EQ  s_ekbe-zekkn
                      AND   vgabe EQ  s_ekbe-vgabe
                      AND   gjahr EQ  s_ekbe-gjahr
                      AND   belnr EQ  s_ekbe-belnr
                      AND   buzei EQ  s_ekbe-buzei.
          IF sy-subrc = 0.
            WRITE AT: 96(15) 'BPMNG corrected'.
          ENDIF.
        ELSE.
          WRITE AT: 96(15) 'to be updated'.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.
