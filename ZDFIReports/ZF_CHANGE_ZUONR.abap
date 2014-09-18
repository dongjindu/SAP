*&---------------------------------------------------------------------*
*& Report  ZF_CHANGE_ZUONR                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Korrektur der Zuordnungsnummer im Feld ZUONR in den Tabellen BSEG
*& und BSIS/BSAS im Falle von reinen Sachkonten, die auch Bestands-
*& konten der Materialwirtschaft sein können.
*& Desweiteren Korrektur der Zuordnungsnummer im Feld ZUONR in den
*& Tabellen BSEG und BSID/BSAD bzw. BSIK/BSAK im Falle von Debitoren-
*& bzw. Kreditorenkonten. Für die zugehörigen Abstimmkonten werden die
*& entsprechenden Felder HZUON der Tabelle BSEG und ZUONR der Tabellen
*& BSIS/BSAS korrigiert.
*&
*& In this version the correction of HZUON for line items with KOART 'A'
*& has been added.
*&
*& Addition 29.05.2007
*& In the case of manually entered ZUONR or HZUON these existing entries
*& are no longer overwritten but kept.
*&
*& Changed on 11.11.2010:
*& Customer must decide if ALL ZUONR should be overwritten or if only
*& blank ZUONR should be filled with new values, therefore radio-
*& buttons were added on the selection screen and checks were adjusted.
*&
*& Addition on 15.11.2010:
*& Customer and vendor account on selection screen.
*&
*&
*&---------------------------------------------------------------------*

REPORT  zf_change_zuonr MESSAGE-ID fh LINE-SIZE 80.

TABLES: bsis,  bsas,
        bsid,  bsad,
        bsik,  bsak,
        skb1,  knb1, *knb1, lfb1, *lfb1,
        bkpf,  bseg, bsec.

DATA:   BEGIN OF t_bseg OCCURS 500.
        INCLUDE STRUCTURE bseg.
DATA:   END OF t_bseg.


DATA:   zuonr LIKE bseg-zuonr,
        hzuon LIKE bseg-hzuon,
        counter TYPE i.

DATA:   d_belnr    LIKE bseg-belnr,
        d_gjahr    LIKE bseg-gjahr,
        d_buzei    LIKE bseg-buzei.

*** begin of addition UD290507
DATA:   x_chg_z(1) TYPE c,
        x_chg_h(1) TYPE c.
*** end of addition UD290507

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE text_100.

selection-screen begin of line.
selection-screen comment 1(25) text_001 for field p_bukrs.
selection-screen position 25.
SELECT-OPTIONS p_bukrs  FOR  bkpf-bukrs OBLIGATORY.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_002 for field p_gjahr.
selection-screen position 25.
SELECT-OPTIONS p_gjahr  FOR  bkpf-gjahr.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_003 for field p_belnr.
selection-screen position 25.
SELECT-OPTIONS p_belnr  FOR  bseg-belnr.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_004 for field p_zuonr.
selection-screen position 25.
SELECT-OPTIONS p_zuonr  FOR  bseg-zuonr.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_005 for field p_hkont.
selection-screen position 25.
SELECT-OPTIONS p_hkont  FOR  bseg-hkont.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_006 for field p_kunnr.
selection-screen position 25.
SELECT-OPTIONS p_kunnr  FOR  bseg-kunnr.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(25) text_007 for field p_lifnr.
selection-screen position 25.
SELECT-OPTIONS p_lifnr  FOR  bseg-lifnr.
selection-screen end of line.

SELECTION-SCREEN END OF BLOCK 001.

*** begin of addition DB111110
SELECTION-SCREEN BEGIN OF BLOCK 002 WITH FRAME TITLE text_101.

selection-screen begin of line.
selection-screen position 5.
parameters: xempty radiobutton group 0001 default 'X'.
selection-screen comment 7(30) text_008 for field xempty.
selection-screen position 40.
parameters: xall radiobutton group 0001.
selection-screen comment 42(30) text_009 for field xall.
selection-screen end of line.

selection-screen skip.
selection-screen begin of line.
selection-screen comment 1(10) text_010 for field xupd.
selection-screen position 12.
PARAMETERS: xupd AS CHECKBOX.
selection-screen end of line.

SELECTION-SCREEN END OF BLOCK 002.
*** end of addition DB111110

initialization.
  text_001 = 'Company Code'.
  text_002 = 'Fiscal Year'.
  text_003 = 'Document number'.
  text_004 = 'Assignment'.
  text_005 = 'GL Account'.
  text_006 = 'Customer Account'.
  text_007 = 'Vendor Account'.
  text_008 = 'Process only initial fields'.
  text_010 = 'Update'.
  text_009 = 'Overwrite existing fields'.
  text_100 = 'General document selection'.
  text_101 = 'Processing parameters'.


*********************** Start of main programme: ***********************
START-OF-SELECTION.
  DO.
*-------------------Selection of documents with BSEG-------------------*
    PERFORM select_items_of_bseg.
    DESCRIBE TABLE t_bseg.
    IF sy-tfill = 0.
      EXIT.
    ENDIF.
    counter = counter + sy-tfill.
    LOOP AT t_bseg INTO bseg.
*** begin of addition UD290507
      CLEAR x_chg_z.
      CLEAR x_chg_h.
*** end of addition UD290507
      d_belnr = bseg-belnr.              " save of documentkey last read
      d_gjahr = bseg-gjahr.
      d_buzei = bseg-buzei.
*-------------------Selection of document headers with BKPF------------*
      AT NEW gjahr.
        PERFORM select_header_of_bkpf.
      ENDAT.
*-------------------Selection of BSEC,if ZUONR is created by-----------*
*------------------ CPD-information------------------------------------*
      IF bseg-xcpdd = 'X'.
        PERFORM select_bsec.
      ENDIF.
*-------------------Create ZUONR and HZUON-----------------------------*
      PERFORM zuonr_and_hzuon_set.
*-------------------Update BSIS/BSAS, BSID/BSAD, BSIK/BSAK-------------*
*-not for original documents of recurring entries and sample documents-*
      IF bkpf-bstat NE 'D' AND bkpf-bstat NE 'M'.
        PERFORM update_index.
      ENDIF.
*-------------------Update BSEG----------------------------------------*
      PERFORM update_bseg.
    ENDLOOP.
    COMMIT WORK.
  ENDDO.

END-OF-SELECTION.
  WRITE: / 'Es wurden', counter, 'Sätze gelesen.'.

*********************** End of main programme **************************

*&---------------------------------------------------------------------*
*&      Form  SELECT_ITEMS_OF_BSEG
*&---------------------------------------------------------------------*
*       Selection of documents with BSEG
*----------------------------------------------------------------------*
*      -->P_SY_TFILL
*----------------------------------------------------------------------*
FORM select_items_of_bseg.
  CLEAR t_bseg.  REFRESH t_bseg.
  SELECT * FROM bseg INTO TABLE t_bseg
                  WHERE bukrs IN p_bukrs
                    AND gjahr IN p_gjahr
                    AND belnr IN p_belnr
                    AND zuonr IN p_zuonr
                    AND hkont IN p_hkont
                    AND kunnr IN p_kunnr                    "DB151110
                    AND lifnr IN p_lifnr                    "DB151110
                    AND (   ( belnr GT d_belnr )
                         OR ( belnr EQ d_belnr AND gjahr GT d_gjahr )
                         OR ( belnr EQ d_belnr AND gjahr EQ d_gjahr
                                               AND buzei GT d_buzei ) )
                    ORDER BY PRIMARY KEY.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
ENDFORM.                    " SELECT_ITEMS_OF_BSEG

*&---------------------------------------------------------------------*
*&      Form  SELECT_HEADER_OF_BKPF
*&---------------------------------------------------------------------*
*       Selection of document header with BKPF
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM select_header_of_bkpf.
  SELECT SINGLE * FROM bkpf WHERE bukrs = bseg-bukrs
                              AND belnr = bseg-belnr
                              AND gjahr = bseg-gjahr.
  IF sy-subrc NE 0.
    MESSAGE a090 WITH bseg-bukrs bseg-belnr bseg-gjahr.
*   Belegkopf für Beleg & & & nicht vorhanden
  ENDIF.
ENDFORM.                    " SELECT_HEADER_OF_BKPF

*&---------------------------------------------------------------------*
*&      Form  ZUONR_AND_HZUON_SET
*&---------------------------------------------------------------------*
*       Create ZUONR and HZUON depnding on the type of account
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM zuonr_and_hzuon_set.
*** begin of addition DB151110
  CLEAR: zuonr, hzuon.
*** end of addition DB151110
  CASE bseg-koart.
    WHEN 'S' OR 'M'.                               "G/L account
*** begin of addition DB111110
      IF xempty EQ 'X'.
        CHECK bseg-zuonr EQ space.
      ENDIF.
*** end of addition db111110
      SELECT SINGLE * FROM skb1
                 WHERE bukrs = bseg-bukrs
                   AND saknr = bseg-hkont.
      IF sy-subrc NE 0.
        WRITE:/ skb1-bukrs,
                skb1-saknr,
                'NO_ENTRY_SKB1'.
      ELSE.
        IF skb1-zuawa = space.
          zuonr = space.
        ENDIF.
        PERFORM zuordnung_aufbauen(rszun000)          "skb1-mitkz=' '
           USING skb1-zuawa zuonr.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE a000(zzf).
      ENDIF.
*** begin of addition UD290507
      IF zuonr NE space.
        x_chg_z = 'X'.
      ENDIF.
*** end of addition UD290507

    WHEN 'D'.                                          "customer account
*** begin of addition DB111110
      IF xempty EQ 'X' AND bseg-zuonr EQ space OR
           xall EQ 'X'.
*** end of addition DB111110
        SELECT SINGLE * FROM knb1 INTO *knb1
                   WHERE kunnr = bseg-kunnr
                     AND bukrs = bseg-bukrs.
        IF sy-subrc NE 0.
          WRITE:/ knb1-kunnr,
                  knb1-bukrs,
                  'NO_ENTRY_KNB1'.
        ELSE.
          IF *knb1-zuawa = space.
            zuonr = space.
          ENDIF.
          PERFORM zuordnung_aufbauen(rszun000)
             USING *knb1-zuawa zuonr.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE a000(zzf).
        ENDIF.
*** begin of addition UD290507
        IF zuonr NE space.
          x_chg_z = 'X'.
        ENDIF.
      ENDIF.

*   Check if HZUON needs to be changed
*** begin of addition DB111110
      IF xempty EQ 'X' AND bseg-hzuon EQ space OR
           xall EQ 'X'.
*      ENDIF.
*** end of addition db111110
        SELECT SINGLE * FROM skb1
                   WHERE bukrs = bseg-bukrs
                     AND saknr = bseg-hkont.
        IF sy-subrc NE 0.
          WRITE:/ skb1-bukrs,
                  skb1-saknr,
                  'NO_ENTRY_SKB1'.
        ELSE.
          IF skb1-zuawa = space.
            hzuon = space.
          ENDIF.
          PERFORM zuordnung_aufbauen(rszun000)          "skb1-mitkz='D'
             USING skb1-zuawa hzuon.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE a000(zzf).
        ENDIF.
      ENDIF.
*** begin of addition UD290507
      IF hzuon NE space.
        x_chg_h = 'X'.
      ENDIF.
*** end of addition UD290507

    WHEN 'K'.                                      "vendor account
*** begin of addition DB111110
      IF xempty EQ 'X' AND bseg-zuonr EQ space OR
           xall EQ 'X'.
*** end of addition DB111110
        SELECT SINGLE * FROM lfb1 INTO *lfb1
                   WHERE lifnr = bseg-lifnr
                     AND bukrs = bseg-bukrs.
        IF sy-subrc NE 0.
          WRITE:/ lfb1-lifnr,
                  lfb1-bukrs,
                  'NO_ENTRY_LFB1'.
        ELSE.
          IF *lfb1-zuawa = space.
            zuonr = space.
          ENDIF.
          PERFORM zuordnung_aufbauen(rszun000)
             USING *lfb1-zuawa zuonr.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE a000(zzf).
        ENDIF.
*** begin of addition UD290507
        IF zuonr NE space.
          x_chg_z = 'X'.
        ENDIF.
      ENDIF.

*   Check if HZUON needs to be changed
*** begin of addition DB111110
      IF xempty EQ 'X' AND bseg-hzuon EQ space OR
           xall EQ 'X'.
*      ENDIF.
*** end of addition db111110

        SELECT SINGLE * FROM skb1
                   WHERE bukrs = bseg-bukrs
                     AND saknr = bseg-hkont.
        IF sy-subrc NE 0.
          WRITE:/ skb1-bukrs,
                  skb1-saknr,
                  'NO_ENTRY_SKB1'.
        ELSE.
          IF skb1-zuawa = space.
            hzuon = space.
          ENDIF.
          PERFORM zuordnung_aufbauen(rszun000)          "skb1-mitkz='K'
              USING skb1-zuawa hzuon.
        ENDIF.
        IF sy-subrc <> 0.
          MESSAGE a000(zzf).
        ENDIF.
*** begin of addition UD290507
        IF hzuon NE space.
          x_chg_h = 'X'.
        ENDIF.
      ENDIF.

*** end of addition UD290507

*** start of insertion ***
*************************** KOART 'A' ****************************
    WHEN 'A'.                                      "asset recon account
*** begin of addition DB111110
      IF xempty EQ 'X'.
        CHECK bseg-hzuon EQ space.
      ENDIF.
*** end of addition db111110
      SELECT SINGLE * FROM skb1
                 WHERE bukrs = bseg-bukrs
                   AND saknr = bseg-hkont.
      IF sy-subrc NE 0.
        WRITE:/ skb1-bukrs,
                skb1-saknr,
                'NO_ENTRY_SKB1'.
      ELSE.
        IF skb1-zuawa = space.
          hzuon = space.
        ENDIF.
        PERFORM zuordnung_aufbauen(rszun000)          "skb1-mitkz='A'
           USING skb1-zuawa hzuon.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE a000(zzf).
      ENDIF.
*** begin of addition UD290507
      IF hzuon NE space.
        x_chg_h = 'X'.
      ENDIF.
*** end of addition UD290507
    WHEN OTHERS.
*** end of insertion ***
  ENDCASE.
ENDFORM.                    " ZUONR_AND_HZUON_SET

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSEG
*&---------------------------------------------------------------------*
*       Update BSEG-ZUONR and BSEG-HZUON
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bseg.
  IF xupd = 'X'.
    CASE bseg-koart.
      WHEN 'S' OR 'M'.                              "G/L account
        IF sy-subrc = 0.
*** begin of addition UD290507
          CHECK x_chg_z = 'X'.
*** end of addition UD290507
          UPDATE bseg SET zuonr = zuonr
                    WHERE bukrs = bseg-bukrs
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
          bseg-zuonr = zuonr.
          IF sy-subrc = 0.
            WRITE:/   bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      bseg-zuonr,
                     'BSEG_UPDATE_OK'.
          ELSE.
            WRITE:/   bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      'WRONG_UPDATE_BSEG'.
            MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
          ENDIF.
        ELSE.
          MESSAGE e204 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
        ENDIF.

      WHEN 'D' OR 'K'.                              "customer account
        IF sy-subrc = 0.                           "or vendor account
*** begin of addition UD290507
*          UPDATE bseg SET zuonr = zuonr
*                          hzuon = hzuon
*                    WHERE bukrs = bseg-bukrs
*                      AND gjahr = bseg-gjahr
*                      AND belnr = bseg-belnr
*                      AND buzei = bseg-buzei.
*          bseg-zuonr = zuonr.
*          bseg-hzuon = hzuon.
*          IF sy-subrc = 0.
*            WRITE:/   bseg-bukrs,
*                      bseg-belnr,
*                      bseg-gjahr,
*                      bseg-buzei,
*                      bseg-zuonr,
*                      bseg-hzuon,
*                     'BSEG_UPDATE_OK'.
*          ELSE.
*            WRITE:/   bseg-bukrs,
*                      bseg-belnr,
*                      bseg-gjahr,
*                      bseg-buzei,
*                      'WRONG_UPDATE_BSEG'.
*            MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
*          ENDIF.

*** update ZUONR
          IF x_chg_z = 'X'.
            UPDATE bseg SET zuonr = zuonr
                      WHERE bukrs = bseg-bukrs
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
            bseg-zuonr = zuonr.
            IF sy-subrc = 0.
              WRITE:/   bseg-bukrs,
                        bseg-belnr,
                        bseg-gjahr,
                        bseg-buzei,
                        bseg-zuonr,
                       'BSEG_UPDATE_OK'.
            ELSE.
              WRITE:/   bseg-bukrs,
                        bseg-belnr,
                        bseg-gjahr,
                        bseg-buzei,
                        'WRONG_UPDATE_BSEG'.
              MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr.
            ENDIF.
          ENDIF.
*** update HZUON
          IF x_chg_h = 'X'.
            UPDATE bseg SET hzuon = hzuon
                      WHERE bukrs = bseg-bukrs
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
            bseg-hzuon = hzuon.
            IF sy-subrc = 0.
              WRITE:/   bseg-bukrs,
                        bseg-belnr,
                        bseg-gjahr,
                        bseg-buzei,
                        bseg-hzuon,
                       'BSEG_UPDATE_OK'.
            ELSE.
              WRITE:/   bseg-bukrs,
                        bseg-belnr,
                        bseg-gjahr,
                        bseg-buzei,
                        'WRONG_UPDATE_BSEG'.
              MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr.
            ENDIF.
          ENDIF.
*** end of addition UD290507
        ELSE.
          MESSAGE e204 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
        ENDIF.

*** start of insertion ***
*************************** KOART 'A' ****************************
      WHEN 'A'.
        IF sy-subrc = 0.                           "asset recon account
*** begin of addition UD290507
          CHECK x_chg_h = 'X'.
*** end of addition UD290507
          UPDATE bseg SET hzuon = hzuon
                    WHERE bukrs = bseg-bukrs
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
          bseg-hzuon = hzuon.
          IF sy-subrc = 0.
            WRITE:/   bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      bseg-zuonr,
                      bseg-hzuon,
                     'BSEG_UPDATE_OK'.
          ELSE.
            WRITE:/   bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      'WRONG_UPDATE_BSEG'.
            MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
          ENDIF.
        ELSE.
          MESSAGE e204 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
        ENDIF.
      WHEN OTHERS.
*** end of insertion ***
    ENDCASE.
  ENDIF.
ENDFORM.                    " UPDATE_BSEG

*&---------------------------------------------------------------------*
*&      Form  UPDATE_INDEX
*&---------------------------------------------------------------------*
*       Update ZUONR in BSIS/BSAS, BSID/BSAD, BSIK/BSAK
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_index.
  IF xupd = 'X'.
    CASE bseg-koart.
      WHEN 'S' OR 'M'.                                "G/L account
*** begin of addition UD290507
        CHECK x_chg_z = 'X'.
*** end of addition UD290507
        IF bseg-augbl = space.
          PERFORM update_bsis.
        ELSE.
          PERFORM update_bsas.
        ENDIF.
      WHEN 'D'.                                       "customer account
        IF bseg-augbl = space.
          PERFORM update_bsid_bsis.
        ELSE.
          PERFORM update_bsad_bsas.
        ENDIF.
      WHEN 'K'.                                       "vendor account
        IF bseg-augbl = space.
          PERFORM update_bsik_bsis.
        ELSE.
          PERFORM update_bsak_bsas.
        ENDIF.

*** start of insertion ***
*************************** KOART 'A' ****************************
      WHEN 'A'.                                       "asset account
        IF bseg-xhres = 'X'.
*** begin of addition UD290507
          CHECK x_chg_h = 'X'.
*** end of addition UD290507
          PERFORM update_bsis_aa.
        ENDIF.
      WHEN OTHERS.
*** end of insertion ***
    ENDCASE.
  ENDIF.
ENDFORM.                    " UPDATE_INDEX


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSIS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsis.
  DATA: wa_bsis LIKE bsis.

  CHECK bseg-xkres = 'X'.
  SELECT SINGLE * FROM bsis INTO wa_bsis
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-zuonr
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM bsis INTO wa_bsis
                   WHERE bukrs = bseg-bukrs
                     AND hkont = bseg-hkont
                     AND gjahr = bseg-gjahr
                     AND belnr = bseg-belnr
                     AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsis WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsis-zuonr = zuonr.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 .
    WRITE:/ wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsas.
  DATA: wa_bsas LIKE bsas.

  CHECK bseg-xkres = 'X'.
  SELECT SINGLE * FROM bsas INTO wa_bsas
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-zuonr
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM bsas INTO wa_bsas
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsas WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsas-zuonr = zuonr.
    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ wa_bsas-bukrs,
            wa_bsas-belnr,
            wa_bsas-gjahr,
            wa_bsas-buzei,
            wa_bsas-zuonr,
           'BSAS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSAS


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSID_BSIS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsid_bsis.
  DATA: wa_bsid LIKE bsid.
  DATA: wa_bsis LIKE bsis.

*** begin of addition UD290507
  IF x_chg_z = 'X'.
*** end of addition UD290507
    SELECT SINGLE * FROM bsid INTO wa_bsid                      "customer
                       WHERE bukrs = bseg-bukrs                  "account
                                                  AND kunnr = bseg-kunnr
                                                  AND umsks = bseg-umsks
                                                  AND umskz = bseg-umskz
                                                  AND augdt = bseg-augdt
                                                  AND zuonr = bseg-zuonr
                                                  AND gjahr = bseg-gjahr
                                                  AND belnr = bseg-belnr
                                                  AND buzei = bseg-buzei.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM bsid INTO wa_bsid
                      WHERE bukrs = bseg-bukrs
                        AND kunnr = bseg-kunnr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
      IF sy-subrc NE 0.
        WRITE:/ bseg-bukrs,
                bseg-gjahr,
                bseg-belnr,
               'NO_ENTRY_BSID'.
        MESSAGE e204 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      DELETE FROM bsid WHERE bukrs = bseg-bukrs
                         AND kunnr = bseg-kunnr
                         AND gjahr = bseg-gjahr
                         AND belnr = bseg-belnr
                         AND buzei = bseg-buzei.
    ELSE.
      MESSAGE e204 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0 AND
       sy-dbcnt = 1.
      wa_bsid-zuonr = zuonr.
      INSERT bsid FROM wa_bsid.
    ELSE.
      MESSAGE a201 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0.
      WRITE:/ wa_bsid-bukrs,
              wa_bsid-belnr,
              wa_bsid-gjahr,
              wa_bsid-buzei,
              wa_bsid-zuonr,
             'BSID_UPDATE_OK'.
    ELSE.
      MESSAGE a202 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
*** begin of addition UD290507
  ENDIF.
*** end of addition UD290507

  CHECK bseg-xhres = 'X'.
*** begin of addition UD290507
  CHECK x_chg_h = 'X'.
*** end of addition UD290507
  SELECT SINGLE * FROM bsis INTO wa_bsis                      "reconci-
                    WHERE bukrs = bseg-bukrs                  "lation
                      AND hkont = bseg-hkont                  "account
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-hzuon
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM bsis INTO wa_bsis
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsis WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsis-zuonr = hzuon.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSID_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAD_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsad_bsas.
  DATA: wa_bsad LIKE bsad.
  DATA: wa_bsas LIKE bsas.

*** begin of addition UD290507
  IF x_chg_z = 'X'.
*** end of addition UD290507
    SELECT SINGLE * FROM bsad INTO wa_bsad                      "customer
                       WHERE bukrs = bseg-bukrs                  "account
                                                  AND kunnr = bseg-kunnr
                                                  AND umsks = bseg-umsks
                                                  AND umskz = bseg-umskz
                                                  AND augdt = bseg-augdt
                                                  AND zuonr = bseg-zuonr
                                                  AND gjahr = bseg-gjahr
                                                  AND belnr = bseg-belnr
                                                  AND buzei = bseg-buzei.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM bsad INTO wa_bsad
                      WHERE bukrs = bseg-bukrs
                        AND kunnr = bseg-kunnr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
      IF sy-subrc NE 0.
        WRITE:/ bseg-bukrs,
                bseg-gjahr,
                bseg-belnr,
                'NO_ENTRY_BSAD'.
        MESSAGE e204 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      DELETE FROM bsad WHERE bukrs = bseg-bukrs
                         AND kunnr = bseg-kunnr
                         AND gjahr = bseg-gjahr
                         AND belnr = bseg-belnr
                         AND buzei = bseg-buzei.
    ELSE.
      MESSAGE e204 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0 AND
       sy-dbcnt = 1.
      wa_bsad-zuonr = zuonr.
      INSERT bsad FROM wa_bsad.
    ELSE.
      MESSAGE a201 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0.
      WRITE:/ wa_bsad-bukrs,
              wa_bsad-belnr,
              wa_bsad-gjahr,
              wa_bsad-buzei,
              wa_bsad-zuonr,
             'BSAD_UPDATE_OK'.
    ELSE.
      MESSAGE a202 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
*** begin of addition UD290507
  ENDIF.
*** end of addition UD290507

  CHECK bseg-xhres = 'X'.
*** begin of addition UD290507
  CHECK x_chg_h = 'X'.
*** end of addition UD290507
  SELECT SINGLE * FROM bsas INTO wa_bsas                      "reconci-
                    WHERE bukrs = bseg-bukrs                  "lation
                      AND hkont = bseg-hkont                  "account
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-hzuon
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM bsas INTO wa_bsas
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsas WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsas-zuonr = hzuon.
    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ wa_bsas-bukrs,
            wa_bsas-belnr,
            wa_bsas-gjahr,
            wa_bsas-buzei,
            wa_bsas-zuonr,
           'BSAS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSAD_BSAS


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSIK_BSIS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsik_bsis.
  DATA: wa_bsik LIKE bsik.
  DATA: wa_bsis LIKE bsis.

*** begin of addition UD290507
  IF x_chg_z = 'X'.
*** end of addition UD290507
    SELECT SINGLE * FROM bsik INTO wa_bsik                      "vendor
                      WHERE bukrs = bseg-bukrs                  "account
                        AND lifnr = bseg-lifnr
                        AND umsks = bseg-umsks
                        AND umskz = bseg-umskz
                        AND augdt = bseg-augdt
                        AND augbl = bseg-augbl
                        AND zuonr = bseg-zuonr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM bsik INTO wa_bsik
                      WHERE bukrs = bseg-bukrs
                        AND lifnr = bseg-lifnr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
      IF sy-subrc NE 0.
        WRITE:/ bseg-bukrs,
                bseg-gjahr,
                bseg-belnr,
                'NO_ENTRY_BSIK'.
        MESSAGE e204 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      DELETE FROM bsik WHERE bukrs = bseg-bukrs
                         AND lifnr = bseg-lifnr
                         AND gjahr = bseg-gjahr
                         AND belnr = bseg-belnr
                         AND buzei = bseg-buzei.
    ELSE.
      MESSAGE e204 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0 AND
       sy-dbcnt = 1.
      wa_bsik-zuonr = zuonr.
      INSERT bsik FROM wa_bsik.
    ELSE.
      MESSAGE a201 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0.
      WRITE:/ wa_bsik-bukrs,
              wa_bsik-belnr,
              wa_bsik-gjahr,
              wa_bsik-buzei,
              wa_bsik-zuonr,
             'BSIK_UPDATE_OK'.
    ELSE.
      MESSAGE a202 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
*** begin of addition UD290507
  ENDIF.
*** end of addition UD290507

  CHECK bseg-xhres = 'X'.
*** begin of addition UD290507
  CHECK x_chg_h = 'X'.
*** end of addition UD290507
  SELECT SINGLE * FROM bsis INTO wa_bsis                      "reconci-
                    WHERE bukrs = bseg-bukrs                  "lation
                      AND hkont = bseg-hkont                  "account
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-hzuon
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM bsis INTO wa_bsis
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsis WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsis-zuonr = hzuon.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSIK_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAK_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM update_bsak_bsas.
  DATA: wa_bsak LIKE bsak.
  DATA: wa_bsas LIKE bsas.

*** begin of addition UD290507
  IF x_chg_z = 'X'.
*** end of addition UD290507
    SELECT SINGLE * FROM bsak INTO wa_bsak                      "vendor
                      WHERE bukrs = bseg-bukrs                  "account
                        AND lifnr = bseg-lifnr
                        AND umsks = bseg-umsks
                        AND umskz = bseg-umskz
                        AND augdt = bseg-augdt
                        AND augbl = bseg-augbl
                        AND zuonr = bseg-zuonr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM bsak INTO wa_bsak
                      WHERE bukrs = bseg-bukrs
                        AND lifnr = bseg-lifnr
                        AND gjahr = bseg-gjahr
                        AND belnr = bseg-belnr
                        AND buzei = bseg-buzei.
      IF sy-subrc NE 0.
        WRITE:/ bseg-bukrs,
                bseg-gjahr,
                bseg-belnr,
                'NO_ENTRY_BSAK'.
        MESSAGE e204 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      DELETE FROM bsak WHERE bukrs = bseg-bukrs
                         AND lifnr = bseg-lifnr
                         AND gjahr = bseg-gjahr
                         AND belnr = bseg-belnr
                         AND buzei = bseg-buzei.
    ELSE.
      MESSAGE e204 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0 AND
       sy-dbcnt = 1.
      wa_bsak-zuonr = zuonr.
      INSERT bsak FROM wa_bsak.
    ELSE.
      MESSAGE a201 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
    IF sy-subrc = 0.
      WRITE:/ wa_bsak-bukrs,
              wa_bsak-belnr,
              wa_bsak-gjahr,
              wa_bsak-buzei,
              wa_bsak-zuonr,
             'BSAK_UPDATE_OK'.
    ELSE.
      MESSAGE a202 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
*** begin of addition UD290507
  ENDIF.
*** end of addition UD290507

  CHECK bseg-xhres = 'X'.
*** begin of addition UD290507
  CHECK x_chg_h = 'X'.
*** end of addition UD290507
  SELECT SINGLE * FROM bsas INTO wa_bsas                      "reconci-
                    WHERE bukrs = bseg-bukrs                  "lation
                      AND hkont = bseg-hkont                  "account
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-hzuon
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM bsas INTO wa_bsas
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsas WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsas-zuonr = hzuon.
    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/   wa_bsas-bukrs,
              wa_bsas-belnr,
              wa_bsas-gjahr,
              wa_bsas-buzei,
              wa_bsas-zuonr,
             'BSAS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
ENDFORM.                    " UPDATE_BSAK_BSAS
*&---------------------------------------------------------------------*
*&      Form  SELECT_BSEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_bsec .
  SELECT SINGLE * FROM bsec WHERE bukrs = bseg-bukrs
                              AND belnr = bseg-belnr
                              AND gjahr = bseg-gjahr.
  IF sy-subrc NE 0.
    MESSAGE a204 WITH 'BSEC' bseg-bukrs bseg-belnr bseg-gjahr.
  ENDIF.
ENDFORM.                    " SELECT_BSEC
*&---------------------------------------------------------------------*
*&      Form  update_bsis_aa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_bsis_aa.
  DATA: wa_bsis LIKE bsis.

*************************** KOART 'A' ****************************
  SELECT SINGLE * FROM bsis INTO wa_bsis                      "reconci-
                    WHERE bukrs = bseg-bukrs                  "lation
                      AND hkont = bseg-hkont                  "account
                      AND augdt = bseg-augdt
                      AND augbl = bseg-augbl
                      AND zuonr = bseg-hzuon
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM bsis INTO wa_bsis
                    WHERE bukrs = bseg-bukrs
                      AND hkont = bseg-hkont
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
    ENDIF.
  ENDIF.
  IF sy-subrc = 0.
    DELETE FROM bsis WHERE bukrs = bseg-bukrs
                       AND hkont = bseg-hkont
                       AND gjahr = bseg-gjahr
                       AND belnr = bseg-belnr
                       AND buzei = bseg-buzei.
  ELSE.
    MESSAGE e204 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 AND
     sy-dbcnt = 1.
    wa_bsis-zuonr = hzuon.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    MESSAGE a202 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.

ENDFORM.                    " update_bsis_aa
