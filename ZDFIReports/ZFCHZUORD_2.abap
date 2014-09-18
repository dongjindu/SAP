*&---------------------------------------------------------------------*
*& Report  ZFCHZUORD_2                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Korrektur der Zuordnungsnummer im Feld ZUONR in den Tabellen BSEG   *
*& und BSIS/BSAS im Falle von reinen Sachkonten, die auch Bestands-    *
*& konten der Materialwirtschaft sein können.                          *
*& Desweiteren Korrektur der Zuordnungsnummer im Feld ZUONR in den     *
*& Tabellen BSEG und BSID/BSAD bzw. BSIK/BSAK im Falle von Debitoren-  *
*& bzw. Kreditorenkonten. Für die zugehörigen Abstimmkonten werden die *
*& entsprechenden Felder HZUON der Tabelle BSEG und ZUONR der Tabellen *
*& BSIS/BSAS korrigiert.                                               *
*&---------------------------------------------------------------------*

REPORT  ZFCHZUORD_2 message-id FH LINE-SIZE 80.

TABLES: BSIS,  BSAS,
        BSID,  BSAD,
        BSIK,  BSAK,
        SKB1,  KNB1, *KNB1, LFB1, *LFB1,
        BKPF,  BSEG, BSEC.

DATA:   BEGIN OF T_BSEG OCCURS 500.
        INCLUDE STRUCTURE BSEG.
DATA:   END OF T_BSEG.


DATA:   ZUONR LIKE BSEG-ZUONR,
        HZUON LIKE BSEG-HZUON,
        counter type i.

DATA:   D_BELNR    LIKE BSEG-BELNR,
        D_GJAHR    LIKE BSEG-GJAHR,
        D_BUZEI    LIKE BSEG-BUZEI.

SELECT-OPTIONS P_BUKRS  FOR  BKPF-BUKRS OBLIGATORY.
SELECT-OPTIONS P_GJAHR  FOR  BKPF-GJAHR.

SELECT-OPTIONS P_BELNR  FOR  BSEG-BELNR.
SELECT-OPTIONS P_ZUONR  FOR  BSEG-ZUONR.
SELECT-OPTIONS P_HKONT  FOR  BSEG-HKONT.

PARAMETERS    xupd AS checkbox.

*********************** Start of main programme: ***********************
start-of-selection.
  DO.
*-------------------Selection of documents with BSEG-------------------*
    PERFORM SELECT_ITEMS_OF_BSEG.
    DESCRIBE TABLE T_BSEG.
    IF sy-tfill = 0.
      EXIT.
    ENDIF.
    counter = counter + sy-tfill.
    LOOP AT T_BSEG INTO BSEG.
      D_BELNR = BSEG-BELNR.              " save of documentkey last read
      D_GJAHR = BSEG-GJAHR.
      D_BUZEI = BSEG-BUZEI.
*-------------------Selection of document headers with BKPF------------*
      AT NEW GJAHR.
        PERFORM SELECT_HEADER_OF_BKPF.
      ENDAT.
*-------------------Selection of BSEC,if ZUONR is created by-----------*
*------------------ CPD-information------------------------------------*
      IF BSEG-XCPDD = 'X'.
        PERFORM SELECT_BSEC.
      ENDIF.
*-------------------Create ZUONR and HZUON-----------------------------*
      PERFORM ZUONR_AND_HZUON_SET.
*-------------------Update BSIS/BSAS, BSID/BSAD, BSIK/BSAK-------------*
*-not for original documents of recurring entries and sample documents-*
      IF BKPF-BSTAT NE 'D' and BKPF-BSTAT NE 'M'.
          PERFORM UPDATE_INDEX.
      ENDIF.
*-------------------Update BSEG----------------------------------------*
      PERFORM UPDATE_BSEG.
    ENDLOOP.
    COMMIT WORK.
  ENDDO.

end-of-selection.
  write: / 'Data:', counter, 'processed.'.

*********************** End of main programme **************************

*&---------------------------------------------------------------------*
*&      Form  SELECT_ITEMS_OF_BSEG
*&---------------------------------------------------------------------*
*       Selection of documents with BSEG
*----------------------------------------------------------------------*
*      -->P_SY_TFILL
*----------------------------------------------------------------------*
FORM SELECT_ITEMS_OF_BSEG.
  CLEAR T_BSEG.  REFRESH T_BSEG.
  SELECT * FROM BSEG INTO TABLE T_BSEG
                  WHERE BUKRS IN P_BUKRS
                    AND GJAHR IN P_GJAHR
                    AND BELNR IN P_BELNR
                    AND ZUONR IN P_ZUONR
                    AND HKONT IN P_HKONT
                    AND (   ( BELNR GT D_BELNR )
                         OR ( BELNR EQ D_BELNR AND GJAHR GT D_GJAHR )
                         OR ( BELNR EQ D_BELNR AND GJAHR EQ D_GJAHR
                                               AND BUZEI GT D_BUZEI ) )
                    ORDER BY PRIMARY KEY.
  if sy-subrc ne 0.
    exit.
  endif.
ENDFORM.                    " SELECT_ITEMS_OF_BSEG

*&---------------------------------------------------------------------*
*&      Form  SELECT_HEADER_OF_BKPF
*&---------------------------------------------------------------------*
*       Selection of document header with BKPF
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM SELECT_HEADER_OF_BKPF.
  SELECT SINGLE * FROM BKPF WHERE BUKRS = BSEG-BUKRS
                              AND BELNR = BSEG-BELNR
                              AND GJAHR = BSEG-GJAHR.
  IF SY-SUBRC NE 0.
    MESSAGE A090 WITH BSEG-BUKRS BSEG-BELNR BSEG-GJAHR.
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
FORM ZUONR_AND_HZUON_SET.
  CASE BSEG-KOART.
    WHEN 'S' OR 'M'.                               "G/L account
      SELECT SINGLE * FROM SKB1
                 WHERE BUKRS = BSEG-BUKRS
                   AND SAKNR = BSEG-HKONT.
      IF sy-subrc NE 0.
        WRITE:/ skb1-bukrs,
                skb1-saknr,
                'NO_ENTRY_SKB1'.
      ELSE.
        if SKB1-ZUAWA = SPACE.
           ZUONR = SPACE.
        Endif.
        PERFORM ZUORDNUNG_AUFBAUEN(RSZUN000)          "skb1-mitkz=' '
           USING SKB1-ZUAWA ZUONR.
      ENDIF.
      If sy-subrc <> 0.
        Message A000(ZZF).
      Endif.
    WHEN 'D'.                                      "customer account
      SELECT SINGLE * FROM KNB1 into *KNB1
                 WHERE KUNNR = BSEG-KUNNR
                   AND BUKRS = BSEG-BUKRS.
      IF sy-subrc NE 0.
        WRITE:/ knb1-kunnr,
                knb1-bukrs,
                'NO_ENTRY_KNB1'.
      ELSE.
        if *KNB1-ZUAWA = SPACE.
           ZUONR = SPACE.
        Endif.
        PERFORM ZUORDNUNG_AUFBAUEN(RSZUN000)
           USING *KNB1-ZUAWA ZUONR.
      ENDIF.
      If sy-subrc <> 0.
        Message A000(ZZF).
      Endif.
      SELECT SINGLE * FROM SKB1
                 WHERE BUKRS = BSEG-BUKRS
                   AND SAKNR = BSEG-HKONT.
      IF sy-subrc NE 0.
        WRITE:/ skb1-bukrs,
                skb1-saknr,
                'NO_ENTRY_SKB1'.
      ELSE.
        If SKB1-ZUAWA = SPACE.
           HZUON = SPACE.
        Endif.
        PERFORM ZUORDNUNG_AUFBAUEN(RSZUN000)          "skb1-mitkz='D'
           USING SKB1-ZUAWA HZUON.
      ENDIF.
      If sy-subrc <> 0.
        Message A000(ZZF).
      Endif.
    WHEN 'K'.                                      "vendor account
      SELECT SINGLE * FROM LFB1 into *LFB1
                 WHERE LIFNR = BSEG-LIFNR
                   AND BUKRS = BSEG-BUKRS.
      IF sy-subrc NE 0.
        WRITE:/ lfb1-lifnr,
                lfb1-bukrs,
                'NO_ENTRY_LFB1'.
      ELSE.
        If *LFB1-ZUAWA = SPACE.
           ZUONR = SPACE.
        Endif.
        PERFORM ZUORDNUNG_AUFBAUEN(RSZUN000)
           USING *LFB1-ZUAWA ZUONR.
      ENDIF.
      If sy-subrc <> 0.
        Message A000(ZZF).
      Endif.
      SELECT SINGLE * FROM SKB1
                 WHERE BUKRS = BSEG-BUKRS
                   AND SAKNR = BSEG-HKONT.
      IF sy-subrc NE 0.
        WRITE:/ skb1-bukrs,
                skb1-saknr,
                'NO_ENTRY_SKB1'.
      ELSE.
        If SKB1-ZUAWA = SPACE.
           HZUON = SPACE.
        Endif.
        PERFORM ZUORDNUNG_AUFBAUEN(RSZUN000)          "skb1-mitkz='K'
           USING SKB1-ZUAWA HZUON.
      ENDIF.
      If sy-subrc <> 0.
        Message A000(ZZF).
      Endif.
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
FORM UPDATE_BSEG.
  IF xupd = 'X'.
    CASE BSEG-KOART.
      WHEN 'S' OR 'M'.                              "G/L account
        IF sy-subrc = 0.
          UPDATE BSEG SET ZUONR = ZUONR
                    WHERE BUKRS = BSEG-BUKRS
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
          BSEG-ZUONR = ZUONR.
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
            Message E203 WITH 'BSEG' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
          ENDIF.
        ELSE.
          Message E204 WITH 'BSEG' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
        ENDIF.
      WHEN 'D' OR 'K'.                              "customer account
        IF sy-subrc = 0.                           "or vendor account
          UPDATE BSEG SET ZUONR = ZUONR HZUON = HZUON
                    WHERE BUKRS = BSEG-BUKRS
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
          BSEG-ZUONR = ZUONR.
          BSEG-HZUON = HZUON.
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
            Message E203 WITH 'BSEG' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
          ENDIF.
        ELSE.
          Message E204 WITH 'BSEG' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
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
FORM UPDATE_INDEX.
  IF xupd = 'X'.
    CASE BSEG-KOART.
      WHEN 'S' OR 'M'.                                "G/L account
        IF BSEG-AUGBL = SPACE.
          PERFORM UPDATE_BSIS.
        ELSE.
          PERFORM UPDATE_BSAS.
        ENDIF.
      WHEN 'D'.                                       "customer account
        IF BSEG-AUGBL = SPACE.
          PERFORM UPDATE_BSID_BSIS.
        ELSE.
          PERFORM UPDATE_BSAD_BSAS.
        ENDIF.
      WHEN 'K'.                                       "vendor account
        IF BSEG-AUGBL = SPACE.
          PERFORM UPDATE_BSIK_BSIS.
        ELSE.
          PERFORM UPDATE_BSAK_BSAS.
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
FORM UPDATE_BSIS.
  DATA: WA_BSIS LIKE BSIS.

  CHECK BSEG-XKRES = 'X'.
  SELECT SINGLE * FROM BSIS INTO WA_BSIS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM BSIS INTO WA_BSIS
                   WHERE BUKRS = BSEG-BUKRS
                     AND HKONT = BSEG-HKONT
                     AND GJAHR = BSEG-GJAHR
                     AND BELNR = BSEG-BELNR
                     AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSIS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSIS-ZUONR = ZUONR.
    INSERT BSIS FROM WA_BSIS.
  Else.
    Message A201 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  If sy-subrc = 0 .
    WRITE:/ bsis-bukrs,
            bsis-belnr,
            bsis-gjahr,
            bsis-buzei,
            bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  ENDIF.
ENDFORM.                    " UPDATE_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM UPDATE_BSAS.
  DATA: WA_BSAS LIKE BSAS.

  CHECK BSEG-XKRES = 'X'.
  SELECT SINGLE * FROM BSAS INTO WA_BSAS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM BSAS INTO WA_BSAS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSAS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSAS-ZUONR = ZUONR.
    INSERT BSAS FROM WA_BSAS.
  Else.
    Message A201 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsas-bukrs,
            bsas-belnr,
            bsas-gjahr,
            bsas-buzei,
            bsas-zuonr,
           'BSAS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
ENDFORM.                    " UPDATE_BSAS


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSID_BSIS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM UPDATE_BSID_BSIS.
  DATA: WA_BSID LIKE BSID.
  DATA: WA_BSIS LIKE BSIS.

  SELECT SINGLE * FROM BSID INTO WA_BSID                      "customer
                    WHERE BUKRS = BSEG-BUKRS                  "account
                      AND KUNNR = BSEG-KUNNR
                      AND UMSKS = BSEG-UMSKS
                      AND UMSKZ = BSEG-UMSKZ
                      AND AUGDT = BSEG-AUGDT
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSID INTO WA_BSID
                    WHERE BUKRS = BSEG-BUKRS
                      AND KUNNR = BSEG-KUNNR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
             'NO_ENTRY_BSID'.
      Message E204 WITH 'BSID' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSID WHERE BUKRS = BSEG-BUKRS
                       AND KUNNR = BSEG-KUNNR
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSID' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSID-ZUONR = ZUONR.
    INSERT BSID FROM WA_BSID.
  Else.
    Message A201 WITH 'BSID' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsid-bukrs,
            bsid-belnr,
            bsid-gjahr,
            bsid-buzei,
            bsid-zuonr,
           'BSID_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSID' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.

  CHECK BSEG-XHRES = 'X'.
  SELECT SINGLE * FROM BSIS INTO WA_BSIS                      "reconci-
                    WHERE BUKRS = BSEG-BUKRS                  "lation
                      AND HKONT = BSEG-HKONT                  "account
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-HZUON
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSIS INTO WA_BSIS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSIS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSIS-ZUONR = HZUON.
    INSERT BSIS FROM WA_BSIS.
  Else.
    Message A201 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsis-bukrs,
            bsis-belnr,
            bsis-gjahr,
            bsis-buzei,
            bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
ENDFORM.                    " UPDATE_BSID_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAD_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM UPDATE_BSAD_BSAS.
  DATA: WA_BSAD LIKE BSAD.
  DATA: WA_BSAS LIKE BSAS.

  SELECT SINGLE * FROM BSAD INTO WA_BSAD                      "customer
                    WHERE BUKRS = BSEG-BUKRS                  "account
                      AND KUNNR = BSEG-KUNNR
                      AND UMSKS = BSEG-UMSKS
                      AND UMSKZ = BSEG-UMSKZ
                      AND AUGDT = BSEG-AUGDT
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSAD INTO WA_BSAD
                    WHERE BUKRS = BSEG-BUKRS
                      AND KUNNR = BSEG-KUNNR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAD'.
      Message E204 WITH 'BSAD' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSAD WHERE BUKRS = BSEG-BUKRS
                       AND KUNNR = BSEG-KUNNR
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSAD' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSAD-ZUONR = ZUONR.
    INSERT BSAD FROM WA_BSAD.
  Else.
    Message A201 WITH 'BSAD' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsad-bukrs,
            bsad-belnr,
            bsad-gjahr,
            bsad-buzei,
            bsad-zuonr,
           'BSAD_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSAD' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.

  CHECK BSEG-XHRES = 'X'.
  SELECT SINGLE * FROM BSAS INTO WA_BSAS                      "reconci-
                    WHERE BUKRS = BSEG-BUKRS                  "lation
                      AND HKONT = BSEG-HKONT                  "account
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-HZUON
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSAS INTO WA_BSAS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSAS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSAS-ZUONR = HZUON.
    INSERT BSAS FROM WA_BSAS.
  Else.
    Message A201 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsas-bukrs,
            bsas-belnr,
            bsas-gjahr,
            bsas-buzei,
            bsas-zuonr,
           'BSAS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
ENDFORM.                    " UPDATE_BSAD_BSAS


*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSIK_BSIS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM UPDATE_BSIK_BSIS.
  DATA: WA_BSIK LIKE BSIK.
  DATA: WA_BSIS LIKE BSIS.

  SELECT SINGLE * FROM BSIK INTO WA_BSIK                      "vendor
                    WHERE BUKRS = BSEG-BUKRS                  "account
                      AND LIFNR = BSEG-LIFNR
                      AND UMSKS = BSEG-UMSKS
                      AND UMSKZ = BSEG-UMSKZ
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSIK INTO WA_BSIK
                    WHERE BUKRS = BSEG-BUKRS
                      AND LIFNR = BSEG-LIFNR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIK'.
      Message E204 WITH 'BSIK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSIK WHERE BUKRS = BSEG-BUKRS
                       AND LIFNR = BSEG-LIFNR
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSIK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSIK-ZUONR = ZUONR.
    INSERT BSIK FROM WA_BSIK.
  Else.
    Message A201 WITH 'BSIK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsik-bukrs,
            bsik-belnr,
            bsik-gjahr,
            bsik-buzei,
            bsik-zuonr,
           'BSIK_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSIK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.

  CHECK BSEG-XHRES = 'X'.
  SELECT SINGLE * FROM BSIS INTO WA_BSIS                      "reconci-
                    WHERE BUKRS = BSEG-BUKRS                  "lation
                      AND HKONT = BSEG-HKONT                  "account
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-HZUON
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSIS INTO WA_BSIS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSIS'.
      Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSIS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSIS-ZUONR = HZUON.
    INSERT BSIS FROM WA_BSIS.
  Else.
    Message A201 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsis-bukrs,
            bsis-belnr,
            bsis-gjahr,
            bsis-buzei,
            bsis-zuonr,
           'BSIS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSIS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
ENDFORM.                    " UPDATE_BSIK_BSIS

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BSAK_BSAS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  -->  p1
*  <--  p2
*----------------------------------------------------------------------*
FORM UPDATE_BSAK_BSAS.
  DATA: WA_BSAK LIKE BSAK.
  DATA: WA_BSAS LIKE BSAS.

  SELECT SINGLE * FROM BSAK INTO WA_BSAK                      "vendor
                    WHERE BUKRS = BSEG-BUKRS                  "account
                      AND LIFNR = BSEG-LIFNR
                      AND UMSKS = BSEG-UMSKS
                      AND UMSKZ = BSEG-UMSKZ
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-ZUONR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSAK INTO WA_BSAK
                    WHERE BUKRS = BSEG-BUKRS
                      AND LIFNR = BSEG-LIFNR
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAK'.
      Message E204 WITH 'BSAK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSAK WHERE BUKRS = BSEG-BUKRS
                       AND LIFNR = BSEG-LIFNR
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSAK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSAK-ZUONR = ZUONR.
    INSERT BSAK FROM WA_BSAK.
  Else.
    Message A201 WITH 'BSAK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/ bsak-bukrs,
            bsak-belnr,
            bsak-gjahr,
            bsak-buzei,
            bsak-zuonr,
           'BSAK_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSAK' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.

  CHECK BSEG-XHRES = 'X'.
  SELECT SINGLE * FROM BSAS INTO WA_BSAS                      "reconci-
                    WHERE BUKRS = BSEG-BUKRS                  "lation
                      AND HKONT = BSEG-HKONT                  "account
                      AND AUGDT = BSEG-AUGDT
                      AND AUGBL = BSEG-AUGBL
                      AND ZUONR = BSEG-HZUON
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
  IF sy-subrc <> 0.
    SELECT SINGLE * FROM BSAS INTO WA_BSAS
                    WHERE BUKRS = BSEG-BUKRS
                      AND HKONT = BSEG-HKONT
                      AND GJAHR = BSEG-GJAHR
                      AND BELNR = BSEG-BELNR
                      AND BUZEI = BSEG-BUZEI.
    IF sy-subrc NE 0.
      WRITE:/ bseg-bukrs,
              bseg-gjahr,
              bseg-belnr,
              'NO_ENTRY_BSAS'.
      Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
    ENDIF.
  ENDIF.
  If sy-subrc = 0.
    DELETE FROM BSAS WHERE BUKRS = BSEG-BUKRS
                       AND HKONT = BSEG-HKONT
                       AND GJAHR = BSEG-GJAHR
                       AND BELNR = BSEG-BELNR
                       AND BUZEI = BSEG-BUZEI.
  Else.
    Message E204 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0 and
     sy-dbcnt = 1.
    WA_BSAS-ZUONR = HZUON.
    INSERT BSAS FROM WA_BSAS.
  Else.
    Message A201 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
  Endif.
  IF sy-subrc = 0.
    WRITE:/   bsas-bukrs,
              bsas-belnr,
              bsas-gjahr,
              bsas-buzei,
              bsas-zuonr,
             'BSAS_UPDATE_OK'.
  ELSE.
    Message A202 WITH 'BSAS' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR .
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
FORM SELECT_BSEC .
   SELECT SINGLE * FROM BSEC WHERE BUKRS = BSEG-BUKRS
                               AND BELNR = BSEG-BELNR
                               AND GJAHR = BSEG-GJAHR.
  IF SY-SUBRC NE 0.
    MESSAGE A204 WITH 'BSEC' BSEG-BUKRS BSEG-BELNR BSEG-GJAHR.
  ENDIF.
ENDFORM.                    " SELECT_BSEC
