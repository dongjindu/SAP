*---------------------------------------------------------------*
*   Report zum Drucken von Kontoausz?en aus dem                *
*   Zwischenspeicher                                            *
*---------------------------------------------------------------*
REPORT RFEBKAP0 MESSAGE-ID FB
                LINE-SIZE 132
                NO STANDARD PAGE HEADING.

*---------------------------------------------------------------*
*  Include Common Data                                          *
*---------------------------------------------------------------*
*INCLUDE YFEBKA03.
INCLUDE RFEBKA03.
TABLES: RFSDO.

*---------------------------------------------------------------*
*  Parameters                                                   *
*---------------------------------------------------------------*
SELECTION-SCREEN  BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-165.
PARAMETERS:     P_ANWND LIKE FEBKO-ANWND DEFAULT '0001',   "46C
                P_BUKRS LIKE T012K-BUKRS,
                P_HBKID LIKE T012K-HBKID,
                P_HKTID LIKE PAYR-HKTID.
SELECT-OPTIONS: R_AZNUM FOR FEBKO-AZNUM,
                R_AZDAT FOR FEBKO-AZDAT.
SELECTION-SCREEN  END OF BLOCK 1.
SELECTION-SCREEN  BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-160.
SELECT-OPTIONS: R_KUKEY FOR FEBKO-KUKEY,
                R_ESNUM FOR FEBEP-ESNUM.
SELECTION-SCREEN  END OF BLOCK 2.
SELECTION-SCREEN  BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-161.
SELECT-OPTIONS: R_VGEXT FOR FEBEP-VGEXT,
                R_VGINT FOR FEBEP-VGINT,
                R_GRPNR FOR FEBEP-GRPNR,
                R_KWBTR FOR FEBEP-KWBTR.
SELECTION-SCREEN  END OF BLOCK 3.

AT SELECTION-SCREEN.
  IF R_KUKEY IS INITIAL
  AND ( P_BUKRS IS INITIAL
    OR  P_HBKID IS INITIAL
    OR  P_HKTID IS INITIAL ).
    MESSAGE E789.
  ENDIF.

*eject
AT LINE-SELECTION.
  PERFORM CALL_PROG.
*---------------------------------------------------------------*
*  START-OF-SELECTION                                           *
*---------------------------------------------------------------*
START-OF-SELECTION.

*  Felder initialisieren                                        *
*  MOVE '0001' TO ANWND.              "Anwendung Zwischenspeicher  46C
  MOVE P_ANWND TO ANWND.              "46C

  PERFORM KUKEY_RANGE_FUELLEN.
  PERFORM FILL_OTHER_RANGES.
  PERFORM DRUCK_KONTOAUSZUG.

*eject
*--------------------------------------------------------------*
*  Seitenanfangsverarbeitung                                   *
*--------------------------------------------------------------*
TOP-OF-PAGE.
*------------------------Batch-Heading-Routine aufrufen--------*
  PERFORM BATCH-HEADING(RSBTCHH0).
  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.

  PERFORM DRUCK_BANKUEBERSCHRIFT.

*eject
****************************************************************
** Form-Routinen                                               *
****************************************************************

FORM KUKEY_RANGE_FUELLEN.
  DATA: ABSND(50) TYPE C.
  DATA: TFILL_R_KUKEY TYPE I.

  DESCRIBE TABLE R_KUKEY LINES TFILL_R_KUKEY.

  IF TFILL_R_KUKEY = 0.
    SELECT SINGLE * FROM T012  WHERE BUKRS =  P_BUKRS
                                 AND HBKID =  P_HBKID.
    IF SY-SUBRC = 0.
      MOVE T012-BANKL TO ABSND+0(15).
    ELSE.
      MESSAGE E750 WITH 'T012' P_BUKRS P_HBKID ' '.
    ENDIF.
    SELECT SINGLE * FROM T012K WHERE BUKRS =  P_BUKRS
                                 AND HBKID =  P_HBKID
                                 AND HKTID =  P_HKTID.
    IF SY-SUBRC = 0.
*     MOVE T012K-BANKN TO ABSND+15.                           "30F
      MOVE '%'         TO ABSND+15.                           "30F
    ELSE.
      MESSAGE E750 WITH 'T012K' P_BUKRS P_HBKID P_HKTID.
    ENDIF.

    SELECT * FROM FEBKO WHERE ANWND = ANWND
                          AND BUKRS = P_BUKRS                 "30f
*                         AND absnd LIKE absnd                "30F"31H
                          AND HBKID = P_HBKID                 "31H
                          AND AZNUM IN R_AZNUM
                          AND AZDAT IN R_AZDAT
                          AND HKTID = P_HKTID.                "30F

*---------  AUTHORITY-CHECK -------------------------------------------
      AUTHORITY-CHECK OBJECT 'F_FEBB_BUK'
               ID 'BUKRS' FIELD FEBKO-BUKRS
               ID 'ACTVT' FIELD ACTVT_ANZ.
      IF SY-SUBRC = 0.
        CLEAR SKB1.
        SELECT SINGLE * FROM SKB1 WHERE BUKRS = FEBKO-BUKRS
                                  AND   SAKNR = FEBKO-HKONT.
        IF NOT SKB1-BEGRU IS INITIAL.
          AUTHORITY-CHECK OBJECT 'F_BKPF_BES'
                   ID 'BRGRU' FIELD SKB1-BEGRU
                   ID 'ACTVT' FIELD ACTVT_ANZ.
          IF SY-SUBRC NE 0.
            MESSAGE E210(FV) WITH SKB1-BEGRU FEBKO-HKONT.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE E204(FV) WITH 'F_FEBC_BUK' FEBKO-BUKRS.
      ENDIF.
*---------  in Range aufnehmen ----------------------------------------
      S_KUKEY-SIGN   = 'I'.
      S_KUKEY-OPTION = 'EQ'.
      S_KUKEY-LOW    = FEBKO-KUKEY.
      S_KUKEY-HIGH   = SPACE.
      APPEND S_KUKEY.
    ENDSELECT.
  ELSE.
    LOOP AT R_KUKEY.
      S_KUKEY = R_KUKEY.
      APPEND S_KUKEY.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE S_KUKEY LINES TFILL_S_KUKEY.
  IF TFILL_S_KUKEY = 0.
    MESSAGE E774.
  ENDIF.
ENDFORM.


*eject
*---------------------------------------------------------------*
*  Include der Form-Routinen  f? Ausdruck des Kontoauszuges    *
*---------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FILL_OTHER_RANGES
*&---------------------------------------------------------------------*
*       fill the rages for select certen entries                       *
*----------------------------------------------------------------------*
FORM FILL_OTHER_RANGES.
  LOOP AT R_ESNUM.
    S_ESNUM = R_ESNUM.
    APPEND S_ESNUM.
  ENDLOOP.
  LOOP AT R_VGINT.
    S_VGINT = R_VGINT.
    APPEND S_VGINT.
  ENDLOOP.
  LOOP AT R_VGEXT.
    S_VGEXT = R_VGEXT.
    APPEND S_VGEXT.
  ENDLOOP.
  LOOP AT R_GRPNR.
    S_GRPNR = R_GRPNR.
    APPEND S_GRPNR.
  ENDLOOP.
  LOOP AT R_KWBTR.
    S_KWBTR = R_KWBTR.
    APPEND S_KWBTR.
  ENDLOOP.

ENDFORM.                    " FILL_OTHER_RANGES


*----------------------------------------------------------------------*
*   INCLUDE YFEKAP00                                                   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Include mit den FORM-Routinen zum Drucken des elektronischen *
* Kontoauszugs aus dem Zwischenspeicher *
* *
* ak270599 applied note 151002 *
* ak190899 removed incorrectly applied note 151002 *
*----------------------------------------------------------------------*
DATA:
   SAVE_SY_SUBRC LIKE SY-SUBRC. "note 329127

*eject
*---------------------------------------------------------------*
* FORM DRUCK_KONTOAUSZUG.
*---------------------------------------------------------------*
FORM DRUCK_KONTOAUSZUG.
* GVC: Gesch?tsvorfallscode oder Textschl?sel f? Druck
* DATA: GVC(3) TYPE C.
  DATA: LINES_RES TYPE I.


* Selection der zu druckenden Einzelposten
  SELECT * FROM FEBKO WHERE KUKEY IN S_KUKEY
                               ORDER BY ABSND AZIDT AZNUM.
* CHECK FEBKO-EFART = 'E'.

*------- Berechtigungspr?ungen ---------------------------------------
    AUTHORITY-CHECK OBJECT 'F_FEBB_BUK'
             ID 'BUKRS' FIELD FEBKO-BUKRS
             ID 'ACTVT' FIELD ACTVT_ANZ.
    CHECK SY-SUBRC = 0.
    CLEAR SKB1.
    SELECT SINGLE * FROM SKB1 WHERE BUKRS = FEBKO-BUKRS
                              AND SAKNR = FEBKO-HKONT.
    IF NOT SKB1-BEGRU IS INITIAL.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BES'
               ID 'BRGRU' FIELD SKB1-BEGRU
               ID 'ACTVT' FIELD ACTVT_ANZ.
      CHECK SY-SUBRC = 0.
    ENDIF.

*------- Bankname besorgen aus FEBVW -----------------------------------
    SELECT SINGLE * FROM FEBVW WHERE ANWND = FEBKO-ANWND
                                 AND ABSND = FEBKO-ABSND
                                 AND AZIDT = FEBKO-AZIDT.

*------- Felder f? Batch-Heading f?len-------------------------------
    MOVE SPACE TO BHDGD-LINE1.
    MOVE SPACE TO BHDGD-LINE2.
    MOVE FEBKO-BUKRS TO BHDGD-BUKRS.
    MOVE SY-UNAME TO BHDGD-UNAME.
    MOVE SY-REPID TO BHDGD-REPID.
    MOVE TEXT-001 TO BHDGD-LINE1.
    BHDGD-INIFL = '0'.
    PRINTFLAG = 'A'. " Auszug drucken
    NEW-PAGE.

    PERFORM DRUCK_ANFANGSSALDO.
* FEBRE auf einmal in IFEBRE einlesen, wg Performance
    REFRESH IFEBRE.
    SELECT * FROM FEBRE INTO TABLE IFEBRE
                        WHERE KUKEY = FEBKO-KUKEY.
    SORT IFEBRE BY MANDT KUKEY ESNUM RSNUM.

    SELECT * FROM FEBEP WHERE KUKEY = FEBKO-KUKEY
                          AND ESNUM IN S_ESNUM
                          AND VGINT IN S_VGINT
                          AND VGEXT IN S_VGEXT
                          AND GRPNR IN S_GRPNR
                          AND KWBTR IN S_KWBTR.
      CLEAR XFEBRE.
      REFRESH XFEBRE.
      READ TABLE IFEBRE WITH KEY FEBEP(16) BINARY SEARCH.
      IDX_IFEBRE = SY-TABIX.
      DO.
        READ TABLE IFEBRE INDEX IDX_IFEBRE.
        IF IFEBRE(16) = FEBEP(16) AND SY-SUBRC = 0.
          XFEBRE = IFEBRE.
          APPEND XFEBRE.
          IDX_IFEBRE = IDX_IFEBRE + 1.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
      DESCRIBE TABLE XFEBRE LINES SY-DBCNT.
      IF SY-DBCNT > 0.
* LINES_RES = SY-DBCNT + 3. MODIFICATION
        LINES_RES = SY-DBCNT + 1.
        RESERVE LINES_RES LINES.
        LOOP AT XFEBRE.
*----------------------------------------------------------------------*
* MODIFICATION START
*----------------------------------------------------------------------*
* IF SY-TABIX = 1.
* PERFORM DRUCK_ERSTE_EINZELPOSTENZEILE.
* ELSEIF XFEBRE-VWEZW NE SPACE.
** Druck der n. Zeile
* WRITE: /1 SY-VLINE,
** 20(31) XFEBRE-VWEZW,
* 20 XFEBRE-VWEZW, "note 86952 / 80951
* 132 SY-VLINE.
* ENDIF.
          if sy-tabix = 1.
            PERFORM DRUCK_ERSTE_EINZELPOSTENZEILE.
          endif.

*----------------------------------------------------------------------*
* MODIFICATION END
*----------------------------------------------------------------------*
        ENDLOOP.
      ELSE.
* Keine Referenzs?ze da, nur FEBEP drucken
        RESERVE 4 LINES.
        PERFORM DRUCK_ERSTE_EINZELPOSTENZEILE.
      ENDIF.
*----------------------------------------------------------------------*
* MODIFICATION START
*----------------------------------------------------------------------*
** OCMT Betrag (Euro) - original currency / amount akEURO 101198
* IF NOT ( FEBEP-FWAER IS INITIAL OR FEBEP-FWBTR IS INITIAL ).
* IF FEBEP-EPVOZ = 'S'.
* FEBEP-FWBTR = FEBEP-FWBTR * -1.
* ENDIF.
* WRITE: /1 SY-VLINE,
* 20(16) TEXT-200, "????, ????
* 38 FEBEP-FWAER, FEBEP-FWBTR CURRENCY FEBEP-FWAER,
* 132 SY-VLINE.
* ENDIF.
** Referenznummer /Schecknummer
* IF NOT FEBEP-CHECT IS INITIAL.
* WRITE: /1 SY-VLINE,
* 20(16) TEXT-138,
* 38 FEBEP-CHECT,
* 132 SY-VLINE.
* ENDIF.
* Zusatzinformationen "note 82702
*      IF NOT FEBEP-INFO1 IS INITIAL OR
*         NOT FEBEP-INFO2 IS INITIAL.
*        WRITE: /1 SY-VLINE,
*                20(20) FEBEP-INFO1,
*                42(20) FEBEP-INFO2,
*                132 SY-VLINE.
*      ENDIF.
*----------------------------------------------------------------------*
* MODIFICATION END
*----------------------------------------------------------------------*

* AUFTRAGGEBERBANK UND AUFTRAGGEBERKONTO DRUCKEN
      IF NOT FEBEP-PARTN IS INITIAL.
        WRITE: /1 SY-VLINE,
                20(16) TEXT-137,
                38 FEBEP-PARTN, "??,???
                132 SY-VLINE.
      ENDIF.
      IF NOT FEBEP-PABLZ IS INITIAL.
        WRITE: /1 SY-VLINE,
                20(16) TEXT-135,
                38(15) FEBEP-PABLZ, "????? ????
                132 SY-VLINE.
      ENDIF.
      IF NOT FEBEP-PAKTO IS INITIAL.
        WRITE: /1 SY-VLINE,
                20(16) TEXT-136,
                38(18) FEBEP-PAKTO, "??????? ????
                132 SY-VLINE.
      ENDIF.

    ENDSELECT.

    SAVE_SY_SUBRC = SY-SUBRC. "note 329127

    IF SY-SUBRC NE 0.
      WRITE: /1 SY-VLINE,
              3 TEXT-118,
              132 SY-VLINE.
    ENDIF.
    PERFORM DRUCK_ENDSALDO.
  ENDSELECT.

ENDFORM.

*eject
*---------------------------------------------------------------*
* FORM DRUCK_BANKUEBERSCHRIFT.
*---------------------------------------------------------------*
FORM DRUCK_BANKUEBERSCHRIFT.
  NEW-PAGE.
  WRITE: /1 SY-VLINE,
          2(60) FEBVW-BANKA,
          63 FEBKO-KTOSB,
         132 SY-VLINE.

  IF NOT FEBKO-KTOIH IS INITIAL.
    WRITE: /1 SY-VLINE,
            2(14) TEXT-111, " Kontoinhaber
            17(35) FEBKO-KTOIH,
           132 SY-VLINE.
  ENDIF.

  WRITE: /1 SY-VLINE,
          2 TEXT-112,
          17(15) FEBVW-BANKL,
          33(15) TEXT-113,
          49(18) FEBKO-KTONR,
          68(15) TEXT-115,
          84(18) FEBKO-AZNUM,
         108(15) TEXT-119,
         124(08) FEBKO-KUKEY,
         132 SY-VLINE.


  WRITE: /1 SY-VLINE,
          2(15) TEXT-121,
          17(05) FEBKO-HBKID,
          33(15) TEXT-122,
          49(05) FEBKO-HKTID,
          68(15) TEXT-114,
          84(18) FEBKO-AZDAT,
         108(15) TEXT-117,
         124(03) FEBKO-WAERS,
         132 SY-VLINE.

  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
*----------------------------------------------------------------------*
* MODIFICATION START
*----------------------------------------------------------------------*
* WRITE: /1 SY-VLINE,
* 2 TEXT-120,
* 132 SY-VLINE.
  WRITE: /1 SY-VLINE NO-GAP,
         2(5) 'N.no' NO-GAP, "???????
         7 SY-VLINE NO-GAP,
         8(10)  'ValueDte' NO-GAP, "????
         18 SY-VLINE NO-GAP,
         19(10) 'PstDate'  NO-GAP, "????
         29 SY-VLINE NO-GAP,
         30(13) 'Bank Ref' NO-GAP, "????
         43 SY-VLINE NO-GAP,
         44(04) 'Code' NO-GAP,                              "???? "30F
         48 SY-VLINE NO-GAP,
         49(04) 'Rule' NO-GAP, "????
         53 SY-VLINE NO-GAP,
         54(10) 'Doc No.' NO-GAP, "????
         64 SY-VLINE NO-GAP,
         65(10) 'OnAccount' NO-GAP, "??????
         75 SY-VLINE NO-GAP,
         76(31) 'Info' NO-GAP, "?????
         107 SY-VLINE NO-GAP,
         108(18) 'Amount' NO-GAP, "30F
         132 SY-VLINE.
*----------------------------------------------------------------------*
* MODIFICATION END
*----------------------------------------------------------------------*
  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.

ENDFORM.



*eject
*---------------------------------------------------------------*
* FORM DRUCK_ANFANGSSALDO.
*---------------------------------------------------------------*
FORM DRUCK_ANFANGSSALDO.

  CLEAR TOT_DEBITS. "46a
  CLEAR TOT_CREDITS. "46a
  CLEAR TOT_TOTAL. "46a

  IF ( FEBKO-SSVOZ = 'D' OR FEBKO-SSVOZ = 'S' ) AND FEBKO-SSBTR > 0.
    FEBKO-SSBTR = FEBKO-SSBTR * -1.
  ENDIF.

  WRITE: /1 SY-VLINE,
*----------------------------------------------------------------------*
* MODIFICATION START
*----------------------------------------------------------------------*
* 83(12) TEXT-130,
* 95(18) FEBKO-SSBTR CURRENCY FEBKO-WAERS, "????
           96(12) TEXT-130,
          108(18) FEBKO-SSBTR CURRENCY FEBKO-WAERS, "????
*----------------------------------------------------------------------*
* MODIFICATION END
*----------------------------------------------------------------------*
          132 SY-VLINE.
  WRITE: /1 SY-VLINE,
          96(30) TEXT-134,
          132 SY-VLINE.
ENDFORM.

*---------------------------------------------------------------*
* FORM DRUCK_ENDSALDO.
*---------------------------------------------------------------*
FORM DRUCK_ENDSALDO.

  RESERVE 11 LINES.                                         "46A TT
* begin of changes for calculated debits / credits / total "46A TT
  TOT_TOTAL = FEBKO-SSBTR - TOT_DEBITS + TOT_CREDITS .
  IF ( FEBKO-SUMSO <> TOT_DEBITS OR FEBKO-SUMHA <> TOT_CREDITS )
    AND SY-DBCNT = FEBKO-ANZES AND SAVE_SY_SUBRC = 0. "note 329127
* when calculated totals differ from bank provided totals, print 2 parts
* section 1: bank provided totals
    FORMAT COLOR COL_TOTAL. "46b
*----------------------------------------------------------------------*
* MODIFICATION START
*----------------------------------------------------------------------*
* WRITE: /1 SY-VLINE,
* 38(75) TEXT-201 RIGHT-JUSTIFIED,
* 132 SY-VLINE.
* FORMAT COLOR OFF. "46b
* WRITE: /1 SY-VLINE,
* 40(42) TEXT-202 RIGHT-JUSTIFIED,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-132,
* 95(18) FEBKO-SUMSO CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-133,
* 95(18) FEBKO-SUMHA CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* IF ( FEBKO-ESVOZ = 'D' OR FEBKO-ESVOZ = 'S' ) AND FEBKO-ESBTR > 0.
* FEBKO-ESBTR = FEBKO-ESBTR * -1.
* ENDIF.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-131,
* 95(18) FEBKO-ESBTR CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
** section 2 : calcualated line item totals
* WRITE: /1 SY-VLINE,
* 40(42) TEXT-203 RIGHT-JUSTIFIED,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-132,
* 95(18) TOT_DEBITS CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-133,
* 95(18) TOT_CREDITS CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-131,
* 95(18) TOT_TOTAL CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* ELSE.
** when no difference between the bank totals and calculated totals
** print total dr, total cr, ending balance (as in the old way)
** End of changes for calculated debits / credits / total "46A TT
* WRITE: /1 SY-VLINE,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-132,
* 95(18) FEBKO-SUMSO CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-133,
* 95(18) FEBKO-SUMHA CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* WRITE: /1 SY-VLINE,
* 83(30) TEXT-134,
* 132 SY-VLINE.
* IF ( FEBKO-ESVOZ = 'D' OR FEBKO-ESVOZ = 'S' ) AND FEBKO-ESBTR > 0.
* FEBKO-ESBTR = FEBKO-ESBTR * -1.
* ENDIF.
* WRITE: /1 SY-VLINE,
* 83(12) TEXT-131,
* 95(18) FEBKO-ESBTR CURRENCY FEBKO-WAERS,
* 132 SY-VLINE.
* ENDIF. "46A TT
* WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            51(75) TEXT-201 RIGHT-JUSTIFIED,
            132 SY-VLINE.
    FORMAT COLOR OFF. "46b

* Andy Exit

*    WRITE: /1 SY-VLINE,
*            53(42) TEXT-202 RIGHT-JUSTIFIED,
*            96(30) TEXT-134,
*            132 SY-VLINE.
*    WRITE: /1 SY-VLINE,
*            96(12) TEXT-132,
*            108(18) FEBKO-SUMSO CURRENCY FEBKO-WAERS,
*           132 SY-VLINE.
*    WRITE: /1 SY-VLINE,
*            96(12) TEXT-133,
*            108(18) FEBKO-SUMHA CURRENCY FEBKO-WAERS,
*            132 SY-VLINE.
*    WRITE: /1 SY-VLINE,
*            96(30) TEXT-134,
*            132 SY-VLINE.
*    IF ( FEBKO-ESVOZ = 'D' OR FEBKO-ESVOZ = 'S' ) AND FEBKO-ESBTR > 0.
*      FEBKO-ESBTR = FEBKO-ESBTR * -1.
*    ENDIF.
*    WRITE: /1 SY-VLINE,
*            96(12) TEXT-131,
*            108(18) FEBKO-ESBTR CURRENCY FEBKO-WAERS,
*           132 SY-VLINE.

* section 2 : calcualated line item totals
    WRITE: /1 SY-VLINE,
            53(42) TEXT-203 RIGHT-JUSTIFIED,
            96(30) TEXT-134,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-132,
            108(18) TOT_DEBITS CURRENCY FEBKO-WAERS,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-133,
            108(18) TOT_CREDITS CURRENCY FEBKO-WAERS,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(30) TEXT-134,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-131,
            108(18) TOT_TOTAL CURRENCY FEBKO-WAERS,
            132 SY-VLINE.
  ELSE.
* when no difference between the bank totals and calculated totals
* print total dr, total cr, ending balance (as in the old way)
* End of changes for calculated debits / credits / total "46A TT
    WRITE: /1 SY-VLINE,
            96(30) TEXT-134,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-132,
            108(18) FEBKO-SUMSO CURRENCY FEBKO-WAERS,
           132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-133,
            108(18) FEBKO-SUMHA CURRENCY FEBKO-WAERS,
            132 SY-VLINE.
    WRITE: /1 SY-VLINE,
            96(30) TEXT-134,
            132 SY-VLINE.
    IF ( FEBKO-ESVOZ = 'D' OR FEBKO-ESVOZ = 'S' ) AND FEBKO-ESBTR > 0.
      FEBKO-ESBTR = FEBKO-ESBTR * -1.
    ENDIF.
    WRITE: /1 SY-VLINE,
            96(12) TEXT-131,
            108(18) FEBKO-ESBTR CURRENCY FEBKO-WAERS,
           132 SY-VLINE.
  ENDIF.                                                    "46A TT *
  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
*----------------------------------------------------------------------*
* MODIFICATION END
*----------------------------------------------------------------------*
ENDFORM.

*---------------------------------------------------------------*
* FORM DRUCK_ERSTE_EINZELPOSTENZEILE. *
*---------------------------------------------------------------*
* Druck der ersten Zeile, die zu einem Einzelposten geh?t *
*---------------------------------------------------------------*
FORM DRUCK_ERSTE_EINZELPOSTENZEILE.
  DATA: Z_VWEZW(31) TYPE C.
* DATA: GVC(3) TYPE C. "30F
  DATA: GVC(4) TYPE C. "30F
  IF FEBEP-VOZEI = 'D'
  OR FEBEP-VOZEI = 'RC'
  OR FEBEP-EPVOZ = 'S'.
    FEBEP-KWBTR = FEBEP-KWBTR * -1.
    TOT_DEBITS = TOT_DEBITS + ( FEBEP-KWBTR * -1 ).         "46A TT
  ELSE.                                                     "46A TT
    TOT_CREDITS = TOT_CREDITS + FEBEP-KWBTR.                "46A TT
  ENDIF.

* Gesch?tsvorfallscode oder Textschl?sel ausgeben
  IF NOT FEBEP-VORGC IS INITIAL.
    GVC = FEBEP-VORGC. "????
  ELSE.
    GVC = FEBEP-TEXTS. "????
  ENDIF.
*&---------------------------------------------------------------------*
*& MODIFICATION START
*&---------------------------------------------------------------------*
* WRITE: /1 SY-VLINE,
* 2(5) FEBEP-ESNUM,
* 8(5) FEBEP-VALUT DD/MM/YY,
* 14(5) FEBEP-BVDAT DD/MM/YY,
* 20(31) XFEBRE-VWEZW,
* 52(27) FEBEP-BUTXT,
** 80(03) GVC, "30F
* 80(04) GVC, "30F
* 84(10) FEBEP-PNOTA,
* 95(18) FEBEP-KWBTR CURRENCY FEBEP-KWAER, "30F
* 132 SY-VLINE.
* SHIFT XFEBRE-VWEZW BY 31 PLACES. "note 86952 / 80951
* IF XFEBRE-VWEZW NE ' '. "note 86952 / 80951
* WRITE: /1 SY-VLINE, "note 86952 / 80951
* 20 XFEBRE-VWEZW, "note 86952 / 80951
* 132 SY-VLINE. "note 86952 / 80951
* ENDIF. "note 86952 / 80951
  CLEAR Z_VWEZW.
  IF XFEBRE-VWEZW NE ' '.
    MOVE XFEBRE-VWEZW+31(31) TO Z_VWEZW.
  ENDIF.
  IF Z_VWEZW IS INITIAL.
    MOVE FEBEP-ZUONR TO Z_VWEZW.
  ENDIF.
  WRITE: /1 SY-VLINE NO-GAP,
          2(5) FEBEP-ESNUM NO-GAP, "???????
*         7 SY-VLINE NO-GAP,
          8(10) FEBEP-VALUT NO-GAP, "????
*         18 SY-VLINE NO-GAP,
          19(10) FEBEP-BVDAT NO-GAP, "????
*         29 SY-VLINE NO-GAP,
          30(13) FEBEP-CHECT NO-GAP, "????
*         43 SY-VLINE NO-GAP,
          44(04) febep-VGEXT NO-GAP,                        "???? "30F
*         48 SY-VLINE NO-GAP,
          49(04) FEBEP-VGINT NO-GAP, "????
*         53 SY-VLINE NO-GAP,
          54(10) FEBEP-BELNR NO-GAP, "????
*         64 SY-VLINE NO-GAP,
          65(10) FEBEP-AK1BL NO-GAP, "??????
*         75 SY-VLINE NO-GAP,
          76(31) Z_VWEZW NO-GAP, "?????
*         107 SY-VLINE NO-GAP,
          108(18) FEBEP-KWBTR CURRENCY FEBEP-KWAER NO-GAP, "30F
          132 SY-VLINE.
  hide:  febep-belnr, febep-bvdat, febep-AK1BL, febep-nbbln.
  clear: febep-belnr, febep-bvdat, febep-AK1BL, febep-nbbln.
*&---------------------------------------------------------------------*
*& MODIFICATION END
*&---------------------------------------------------------------------*
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_PROG
*&---------------------------------------------------------------------*
FORM CALL_PROG.
  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'GJR' FIELD febep-bvdat(4).

*  CASE BKPF-BSTAT.
*    WHEN 'D'.
*      SET PARAMETER ID 'BLD' FIELD BKPF-BELNR.
*    WHEN 'M'.
*      SET PARAMETER ID 'BLM' FIELD BKPF-BELNR.
*    WHEN 'V'.
*      SET PARAMETER ID 'BLV' FIELD BKPF-BELNR.
*    WHEN OTHERS.
*      SET PARAMETER ID 'BLN' FIELD BKPF-BELNR.

  if febep-belnr <> space.
    SET PARAMETER ID 'BLN' FIELD febep-belnr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

  elseif febep-AK1BL <> space.
    SET PARAMETER ID 'BLN' FIELD febep-ak1bl.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  endif.
ENDFORM.                    " CALL_PROG
