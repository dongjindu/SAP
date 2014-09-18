*&---------------------------------------------------------------------*
*& Report  ZGGBS000_U
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT  zfchzuord_2 MESSAGE-ID fh LINE-SIZE 240.

TABLES: bsis,  bsas,
        bsid,  bsad,
        bsik,  bsak,
        skb1,  knb1, *knb1, lfb1, *lfb1,
        bkpf,  bseg, bsec.

DATA:   BEGIN OF t_bseg OCCURS 500.
        INCLUDE STRUCTURE bseg.
DATA:   END OF t_bseg.


DATA:   zuonr LIKE bseg-zuonr,
        xref1 LIKE bseg-xref1,
        xref2 LIKE bseg-xref2,
        xref3 LIKE bseg-xref3,
        hzuon LIKE bseg-hzuon,
        sgtxt LIKE bseg-sgtxt,
        counter TYPE i.

DATA:   d_belnr    LIKE bseg-belnr,
        d_gjahr    LIKE bseg-gjahr,
        d_buzei    LIKE bseg-buzei.

SELECT-OPTIONS p_bukrs  FOR  bkpf-bukrs OBLIGATORY.
SELECT-OPTIONS p_gjahr  FOR  bkpf-gjahr.
SELECT-OPTIONS p_budat  FOR  bkpf-budat.

SELECT-OPTIONS p_belnr  FOR  bseg-belnr.
SELECT-OPTIONS p_zuonr  FOR  bseg-zuonr.
SELECT-OPTIONS p_hkont  FOR  bseg-hkont.

PARAMETERS    xupd AS CHECKBOX.

DATA: g_chg(1) TYPE c.
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
      d_belnr = bseg-belnr.              " save of documentkey last read
      d_gjahr = bseg-gjahr.
      d_buzei = bseg-buzei.
*-------------------Selection of document headers with BKPF------------*
      AT NEW gjahr.
        PERFORM select_header_of_bkpf.
      ENDAT.

      CHECK bkpf-budat IN p_budat.
*-------------------Selection of BSEC,if ZUONR is created by-----------*
*------------------ CPD-information------------------------------------*
      IF bseg-xcpdd = 'X'.
        PERFORM select_bsec.
      ENDIF.
*-------------------Create ZUONR and HZUON-----------------------------*
*     PERFORM ZUONR_AND_HZUON_SET.

      PERFORM u903  USING g_chg.
      CHECK g_chg = 'X'.
      xref1 = bseg-xref1.
      xref2 = bseg-xref2.
      xref3 = bseg-xref3.
      zuonr = bseg-zuonr.
      hzuon = zuonr.
      sgtxt = bseg-sgtxt.

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
  WRITE: / 'Total Count:', counter.

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
  CASE bseg-koart.
    WHEN 'S' OR 'M'.                               "G/L account
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
    WHEN 'D'.                                      "customer account
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
    WHEN 'K'.                                      "vendor account
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
          UPDATE bseg SET zuonr = zuonr
                          xref1 = xref1
                          xref2 = xref2
                          xref3 = xref3
                          sgtxt = sgtxt
                    WHERE bukrs = bseg-bukrs
                      AND gjahr = bseg-gjahr
                      AND belnr = bseg-belnr
                      AND buzei = bseg-buzei.
*          BSEG-ZUONR = ZUONR.
          IF sy-subrc = 0.
            WRITE:/   'BSEG_UPDATE_OK',
                      bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      bseg-zuonr,
                      bseg-xref1,
                      bseg-xref2,
                      bseg-xref3,
                      bseg-sgtxt,
                      bseg-hzuon.
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
          UPDATE bseg SET zuonr = zuonr
                          xref1 = xref1
                          xref2 = xref2
                          xref3 = xref3
                          sgtxt = sgtxt
                   WHERE bukrs = bseg-bukrs
                     AND gjahr = bseg-gjahr
                     AND belnr = bseg-belnr
                     AND buzei = bseg-buzei.
*          bseg-zuonr = zuonr.
          bseg-hzuon = bseg-zuonr.
          IF sy-subrc = 0.
            WRITE:/   'BSEG_UPDATE_OK',
                      bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei,
                      bseg-zuonr,
                      bseg-xref1,
                      bseg-xref2,
                      bseg-xref3,
                      bseg-sgtxt,
                      bseg-hzuon.

          ELSE.
            WRITE:/   'WRONG_UPDATE_BSEG',
                      bseg-bukrs,
                      bseg-belnr,
                      bseg-gjahr,
                      bseg-buzei.

            MESSAGE e203 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
          ENDIF.
        ELSE.
          MESSAGE e204 WITH 'BSEG' bseg-bukrs bseg-belnr bseg-gjahr .
        ENDIF.
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
    PERFORM change_to_wa_gl USING wa_bsis.
*   WA_BSIS-ZUONR = ZUONR.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0 .
    WRITE:/ 'BSIS_UPDATE_OK',
            wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
            wa_bsis-xref3,
            wa_bsis-sgtxt.
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
    PERFORM change_to_wa_gl USING wa_bsas.
*    WA_BSAS-ZUONR = ZUONR.
    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSAS_UPDATE_OK',
            wa_bsas-bukrs,
            wa_bsas-belnr,
            wa_bsas-gjahr,
            wa_bsas-buzei,
            wa_bsas-zuonr,
            wa_bsas-xref3,
            wa_bsas-sgtxt.

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

    PERFORM change_to_wa_ar USING wa_bsid.
*   WA_BSID-ZUONR = ZUONR.

    INSERT bsid FROM wa_bsid.
  ELSE.
    MESSAGE a201 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSID_UPDATE_OK',
            wa_bsid-bukrs,
            wa_bsid-belnr,
            wa_bsid-gjahr,
            wa_bsid-buzei,
            wa_bsid-zuonr,
            wa_bsid-xref1,
            wa_bsid-xref2,
            wa_bsid-xref3,
            wa_bsid-sgtxt.
  ELSE.
    MESSAGE a202 WITH 'BSID' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.

  CHECK bseg-xhres = 'X'.
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
    PERFORM change_to_wa_gl USING wa_bsis.
*    WA_BSIS-ZUONR = HZUON.
    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSIS_UPDATE_OK',
            wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
            wa_bsis-xref3,
            wa_bsis-sgtxt.
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

    PERFORM change_to_wa_ar USING wa_bsad.
*    WA_BSAD-ZUONR = ZUONR.

    INSERT bsad FROM wa_bsad.
  ELSE.
    MESSAGE a201 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSAD_UPDATE_OK',
            wa_bsad-bukrs,
            wa_bsad-belnr,
            wa_bsad-gjahr,
            wa_bsad-buzei,
            wa_bsad-zuonr,
            wa_bsad-xref1,
            wa_bsad-xref2,
            wa_bsad-xref3,
            wa_bsad-sgtxt.

  ELSE.
    MESSAGE a202 WITH 'BSAD' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.

  CHECK bseg-xhres = 'X'.
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
    PERFORM change_to_wa_gl USING wa_bsas.
*    WA_BSAS-ZUONR = HZUON.
    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSAS_UPDATE_OK',
            wa_bsas-bukrs,
            wa_bsas-belnr,
            wa_bsas-gjahr,
            wa_bsas-buzei,
            wa_bsas-zuonr,
            wa_bsas-xref3,
            wa_bsas-sgtxt.
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
    PERFORM change_to_wa_ap USING wa_bsik.
*    WA_BSIK-ZUONR = ZUONR.

    INSERT bsik FROM wa_bsik.
  ELSE.
    MESSAGE a201 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSIK_UPDATE_OK',
            wa_bsik-bukrs,
            wa_bsik-belnr,
            wa_bsik-gjahr,
            wa_bsik-buzei,
            wa_bsik-zuonr,
            wa_bsik-xref1,
            wa_bsik-xref2,
            wa_bsik-xref3,
            wa_bsik-sgtxt.

  ELSE.
    MESSAGE a202 WITH 'BSIK' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.

  CHECK bseg-xhres = 'X'.
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
*    WA_BSIS-ZUONR = HZUON.
    PERFORM change_to_wa_gl USING wa_bsis.

    INSERT bsis FROM wa_bsis.
  ELSE.
    MESSAGE a201 WITH 'BSIS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSIS_UPDATE_OK',
            wa_bsis-bukrs,
            wa_bsis-belnr,
            wa_bsis-gjahr,
            wa_bsis-buzei,
            wa_bsis-zuonr,
            wa_bsis-xref3,
            wa_bsis-sgtxt.
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
    PERFORM change_to_wa_ap USING wa_bsak.
*    WA_BSAK-ZUONR = ZUONR.

    INSERT bsak FROM wa_bsak.
  ELSE.
    MESSAGE a201 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/ 'BSAK_UPDATE_OK',
            wa_bsak-bukrs,
            wa_bsak-belnr,
            wa_bsak-gjahr,
            wa_bsak-buzei,
            wa_bsak-zuonr,
            wa_bsak-xref1,
            wa_bsak-xref2,
            wa_bsak-xref3,
            wa_bsak-sgtxt.

  ELSE.
    MESSAGE a202 WITH 'BSAK' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.

  CHECK bseg-xhres = 'X'.
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
*    WA_BSAS-ZUONR = HZUON.
    PERFORM change_to_wa_gl USING wa_bsas.

    INSERT bsas FROM wa_bsas.
  ELSE.
    MESSAGE a201 WITH 'BSAS' bseg-bukrs bseg-belnr bseg-gjahr .
  ENDIF.
  IF sy-subrc = 0.
    WRITE:/  'BSAS_UPDATE_OK',
             wa_bsas-bukrs,
             wa_bsas-belnr,
             wa_bsas-gjahr,
             wa_bsas-buzei,
             wa_bsas-zuonr,
             wa_bsas-xref3,
             wa_bsas-sgtxt.
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


* Substitution
*&---------------------------------------------------------------------*
*&      Form  get_z11
*&---------------------------------------------------------------------*
FORM get_z11  USING p_chg.
  data: l_matnr like mara-matnr,
        l_mtart like mara-mtart.
  if bseg-matnr <> space.
    bseg-zuonr = bseg-matnr.
    select single matnr mtart into (l_matnr, l_mtart)
           from mara where matnr = l_matnr.
    bseg-xref3 = l_mtart.

    p_chg = 'X'.

  elseif bseg-zuonr <> space.
    l_matnr = bseg-zuonr.

    select single matnr mtart into (l_matnr, l_mtart)
           from mara where matnr = l_matnr.
    if sy-subrc <> 0.
      bseg-xref3 = l_mtart.
    else.
      clear bseg-zuonr.
    endif.
    p_chg = 'X'.
  endif.
ENDFORM.                                                    " get_z11
*&---------------------------------------------------------------------*
*&      Form  get_z12
*&---------------------------------------------------------------------*
FORM get_z12  USING p_chg.
  bseg-zuonr        = bseg-ebeln.
  bseg-zuonr+10(5)  = bseg-ebelp.
  IF bseg-sgtxt = space.
    bseg-sgtxt        = bseg-matnr.
  ENDIF.
  bseg-xref3        = bseg-menge.

  p_chg = 'X'.
ENDFORM.                                                    " get_z11
*&---------------------------------------------------------------------*
*&      Form  get_z13
*&---------------------------------------------------------------------*
FORM get_z13  USING p_chg.
  DATA: l_zuonr LIKE bseg-zuonr.
  DATA: l_lifnr LIKE bseg-lifnr,
        l_price LIKE bseg-dmbtr,
        l_punit LIKE konp-kpein.

* only for goods issue
  CHECK bkpf-glvor = 'RMWA'.

  PERFORM get_vendor_pb00
             USING     bkpf-bukrs bkpf-bldat bseg-matnr
             CHANGING  l_lifnr    l_price    l_punit.

*info-record
*  clear l_zuonr.
*  select a~lifnr into l_zuonr
*         from eina as a join eine as b
*         on a~infnr = b~infnr
*         where a~matnr =  bseg-matnr
*           and a~loekz = space           "DELETION
*           and b~loekz = space
*           and b~prdat >= bkpf-bldat.
*  endselect.
*  if sy-dbcnt = 1.
*    bseg-zuonr = l_zuonr.
*  elseif sy-dbcnt > 1.
*    concatenate l_zuonr '...'  into bseg-zuonr.
*  endif.
  CLEAR bseg-sgtxt.

  bseg-zuonr     = l_lifnr.
  bseg-sgtxt(10) = l_price.
  bseg-sgtxt+11(1) = '/'.
  bseg-sgtxt+12(3) = l_punit.
  bseg-xref3 = bseg-matnr.

  p_chg = 'X'.
ENDFORM.                                                    " get_z13
*&---------------------------------------------------------------------*
*&      Form  get_z14
*&---------------------------------------------------------------------*
FORM get_z14  USING p_chg.
  TABLES: ekko, ekkn, ckmlhd, likp.
  DATA: l_zterm LIKE ekko-zterm,
        l_aufnr LIKE ekkn-aufnr.


* only for invoice verification, goods receipt document
  CHECK bkpf-glvor = 'RMRP' OR bkpf-glvor = 'RMWE'.

* check gr/ir account posting (part, expense...)
  CHECK bseg-buzid = 'W'   "part
     OR bseg-buzid = 'F'   "freight/expense
     OR bseg-buzid = 'P'.  "variance


* check PO payment terms. (import)
  SELECT SINGLE * FROM  ekko
       WHERE ebeln = bseg-ebeln.
  CHECK sy-subrc = 0.

* check payment term for import
  l_zterm = ekko-zterm(2).
  CHECK l_zterm = 'DA' OR l_zterm = 'DP'
     OR l_zterm = 'TT' OR l_zterm = 'LC'.

* bill of lading from invoice document line text: XREF3
  IF bkpf-awtyp = 'RMRP'.
    bseg-xref3 = bseg-sgtxt.
    CLEAR bseg-sgtxt.
  ELSE.
** ... xblnr after posting is done... so following is unvalid.
*      select single BOLNR into T_BSEGSUB-xref3
*         from likp where vbeln = bkpf-XBLNR.
  ENDIF.

* Get HS Code : REF2
  IF bseg-ktosl = 'FR2' OR bseg-ktosl = 'WRX'.  "Duty, MIT
    CLEAR: bseg-xref2.
    SELECT SINGLE stawn FROM marc INTO bseg-xref2
       WHERE matnr = bseg-matnr
         AND werks = bseg-werks.
  ENDIF.

* Get duty rate : REF1
  CASE bseg-ktosl.
    WHEN 'FR2'.  "Duty
      TABLES: konh, konp.
      DATA: l_rate(5) TYPE c.
      SELECT SINGLE * FROM konh
         WHERE kschl = 'ZOA1'
           AND vakey = bseg-xref2.
*        and datab => sy-datum.
      SELECT SINGLE * FROM konp WHERE knumh = konh-knumh.
      l_rate = konp-kbetr / 10.
      CONCATENATE l_rate '%' INTO bseg-xref1.

    WHEN 'WRX'.
      bseg-xref1 = bseg-menge.  "Qty
  ENDCASE.

* material/asset/order into lineitem text
  SELECT SINGLE aufnr INTO l_aufnr
      FROM ekkn
      WHERE ebeln = bseg-ebeln
        AND ebelp = bseg-ebelp.
  IF l_aufnr = space.
    IF bseg-matnr <> space.
*...... get Cost est number
*      select single KALNR into BSEG-xref1 from ckmlhd
      bseg-sgtxt = bseg-matnr.
    ENDIF.
  ELSE.
    bseg-sgtxt = l_aufnr.
  ENDIF.

  p_chg = 'X'.
ENDFORM.                                                    " get_z14
*&---------------------------------------------------------------------*
*&      Form  get_skey
*&---------------------------------------------------------------------*
FORM get_skey USING    p_zuawa.
  SELECT SINGLE zuawa INTO p_zuawa
    FROM skb1
    WHERE bukrs = bkpf-bukrs
      AND saknr = bseg-hkont.
ENDFORM.                    " get_skey
*&---------------------------------------------------------------------*
*&      Form  get_vendor_pb00
*&---------------------------------------------------------------------*
FORM get_vendor_pb00 USING    p_bkpf_bukrs
                              p_bkpf_bldat
                              p_bseg_matnr
                     CHANGING p_l_lifnr
                              p_l_price
                              p_l_punit.
  DATA: BEGIN OF it_knumh OCCURS 0,
          knumh LIKE konh-knumh,
          datab LIKE konh-datab,
          datbi LIKE konh-datbi,
          lifnr LIKE lfa1-lifnr,
        END   OF it_knumh.
  RANGES: r_ekorg  FOR t024e-ekorg.
  TABLES: t024e.
  CLEAR: p_l_lifnr, p_l_price, p_l_punit.

*----- Read suitable Price
  r_ekorg-sign = 'I'.  r_ekorg-option = 'EQ'.
  SELECT * FROM t024e
     WHERE bukrs = p_bkpf_bukrs.
    r_ekorg-low = t024e-ekorg. APPEND r_ekorg.
  ENDSELECT.

  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab matnr lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  p_bseg_matnr
     AND ekorg IN r_ekorg
     AND esokz =  '0'
     AND datab <= p_bkpf_bldat
     AND datbi >= p_bkpf_bldat.

*----- Check Info Record Deletion Mark
  DATA: l_cnt TYPE i.
  LOOP AT it_knumh.
    SELECT COUNT( * )   INTO l_cnt
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = p_bseg_matnr
       AND a~lifnr = it_knumh-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg IN r_ekorg
       AND b~loekz = ' '.
    IF sy-subrc NE 0.   DELETE it_knumh.  ENDIF.
  ENDLOOP.

  READ TABLE it_knumh INDEX 1.
  CHECK sy-subrc = 0.
  p_l_lifnr = it_knumh-lifnr.

  SELECT SINGLE b~kbetr b~kpein
      INTO (p_l_price,p_l_punit)
      FROM konh AS a INNER JOIN konp AS b
        ON a~knumh = b~knumh
      WHERE a~knumh    = it_knumh-knumh
        AND b~loevm_ko = space.

ENDFORM.                    " get_vendor_pb00
*---------------------------------------------------------------------*
*       FORM U903                                                     *
*---------------------------------------------------------------------*
*       Lineitem - MM, FI
*---------------------------------------------------------------------*
FORM u903  USING p_chg.
  DATA: l_zuawa LIKE skb1-zuawa.
  CLEAR p_chg.

  PERFORM get_skey  USING l_zuawa.

  CASE l_zuawa.
* material -> assignment : sort key
    WHEN 'Z11'.   PERFORM get_z11  USING p_chg.
    WHEN 'Z12'.   PERFORM get_z12  USING p_chg.
* material, vendor, ...
    WHEN 'Z13'.   PERFORM get_z13  USING p_chg.
* goods issue to b/s
    WHEN 'Z14'.   PERFORM get_z14  USING p_chg.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  change_to_wa
*&---------------------------------------------------------------------*
FORM change_to_wa_gl USING wa_bsis  LIKE bsis.
  wa_bsis-sgtxt = sgtxt.
  wa_bsis-zuonr = zuonr.
  wa_bsis-xref3 = xref3.
ENDFORM.                    " change_to_wa
*&---------------------------------------------------------------------*
*&      Form  change_to_wa
*&---------------------------------------------------------------------*
FORM change_to_wa_ar USING wa_bsid  LIKE bsid.
  wa_bsid-sgtxt = sgtxt.
  wa_bsid-zuonr = zuonr.
  wa_bsid-xref1 = xref1.
  wa_bsid-xref2 = xref2.
  wa_bsid-xref3 = xref3.
ENDFORM.                    " change_to_wa
*&---------------------------------------------------------------------*
*&      Form  change_to_wa
*&---------------------------------------------------------------------*
FORM change_to_wa_ap USING wa_bsik  LIKE bsik.
  wa_bsik-sgtxt = sgtxt.
  wa_bsik-zuonr = zuonr.
  wa_bsik-xref1 = xref1.
  wa_bsik-xref2 = xref2.
  wa_bsik-xref3 = xref3.
ENDFORM.                    " change_to_wa
