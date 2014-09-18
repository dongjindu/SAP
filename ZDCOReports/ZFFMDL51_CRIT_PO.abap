REPORT ZFFMDL51_CRIT_PO MESSAGE-ID FI.  "U9B  "Dezember_2002
***********************************************************************
*     Deletion of NONZERO  TR-CB line items for PO lines that either
*      - are not there (no EKPO entry for -EBELN -EBELP)
*      - have been deleted (EKPO-loekz <> space)
*      - have been fully invoiced (EKPO-erekz <> space)
*
*     Sum table is updated as well
***********************************************************************
*     Selection texts from DDIC, plus:
*     Other texts are hard-coded in language EN
***********************************************************************

*********************  Data ********************************************
TABLES: FMSU.

DATA:
  BEGIN OF L_T_SUM OCCURS 0,
    LEDNR  LIKE FMSU-LEDNR,
    GJAHR  LIKE FMSU-GJAHR,
    PERBL  LIKE FMSU-PERBL,
    GEBER  LIKE FMSU-GEBER,
    WRTTP  LIKE FMSU-WRTTP,
    VORGA  LIKE FMSU-VORGA,
    POSIT  LIKE FMSU-POSIT,
    TWAER  LIKE FMSU-TWAER,
    BUKRS  LIKE FMSU-BUKRS,
    GSBER  LIKE FMSU-GSBER,
    END OF L_T_SUM,
  BEGIN OF DELFLAGS,
    LOEKZ LIKE EKPO-LOEKZ,
    EREKZ LIKE EKPO-EREKZ,
    END OF DELFLAGS,
  L_PERIO LIKE FMEP-PERIO,
  L_ANSWER TYPE N,
  L_EBELN LIKE EKKO-EBELN,
  L_EBELP LIKE EKPO-EBELP,
  L_BUKRS LIKE BSEG-BUKRS,
  L_BUZEI LIKE BSEG-BUZEI,
  L_F_FMSU LIKE FMSU,
  L_LDBTR  LIKE FMSU-BTR001,
  L_TRBTR  LIKE FMSU-BTR001,
  BEGIN OF L_T_LINES OCCURS 0,
    OBJNRZ   LIKE FMEP-OBJNRZ,
    VGZEI    LIKE FMEP-VGZEI,
    END OF L_T_LINES,
  G_OBJNR   LIKE FMSU-OBJNR,
  G_CNT_ALL LIKE SY-TFILL,
  L_CNT_DEL LIKE SY-TFILL,
  G_CNT_DEL LIKE SY-TFILL.

***
CONSTANTS: C_CNT_COMMIT LIKE SY-TFILL VALUE 5000.

*********************  Parameters  *************************************

*--- Finanzkreis
SELECTION-SCREEN BEGIN OF BLOCK LA WITH FRAME TITLE TEXT-002.
PARAMETERS: FM_AREA LIKE FM01-FIKRS OBLIGATORY
                                      MATCHCODE OBJECT FIKR
                                      MEMORY ID FIK.

SELECTION-SCREEN END OF BLOCK LA.

SELECTION-SCREEN SKIP.

*--- Einzelposten
SELECTION-SCREEN BEGIN OF BLOCK EP WITH FRAME TITLE TEXT-001.

PARAMETERS:

*--- L?chen von-Periode/Gjahr
FROM_PER     LIKE FMRS-PEVON,
FROMYEAR     LIKE FMRS-GJVON.
SELECTION-SCREEN SKIP.

PARAMETERS:

*--- Zeitraum
TO_PERIO    LIKE FMRS-PEBIS OBLIGATORY,
TO_YEAR     LIKE FMRS-GJBIS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK EP.

SELECTION-SCREEN SKIP.

* optionale Einschraenkungen
SELECT-OPTIONS:
  S_BUKRS FOR FMSU-BUKRS NO-DISPLAY.

PARAMETERS:

*--- Testlauf
P_TEST LIKE FMDY-TESTMODE DEFAULT 'X',
*--- Detailliste
P_LISTE LIKE FMDY-FM_EXTPROT DEFAULT ' '.

************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*----- Finanzkreis und Berechtigung
  CALL FUNCTION 'FMFK_FIKRS_READ'
       EXPORTING
            IP_FIKRS = FM_AREA.

  CALL FUNCTION 'FMAU_AUTHORITY_FIFM'
       EXPORTING
            I_ACTVT       = '46'
            I_AUTH_OBJECT = 'F_FICB_FKR'
            I_FIKRS       = FM_AREA
            I_MSGTY       = 'E'.

*----- Zeitintervall
  IF ( FROMYEAR > TO_YEAR ) OR
     ( FROMYEAR = TO_YEAR AND FROM_PER > TO_PERIO ).
    MESSAGE E839.
*   Werte 'von Gjahr/Periode' sind gr?er als 'bis Gjahr/Periode'
  ENDIF.


************************************************************************
START-OF-SELECTION.
************************************************************************
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

*--- Bei Echtlauf
  IF ( P_TEST IS INITIAL AND SY-BATCH IS INITIAL ).
*--- Sicherheitsabfrage, ob wirklich geloescht werden soll

    CALL FUNCTION 'POPUP_TO_DECIDE'
         EXPORTING
              TEXTLINE1    = 'Actual Data being deleted'(101)
              TEXTLINE2    = 'Please choose whether you want to '(102)
              TEXTLINE3    = 'Delete or Exit'(103)
              TEXT_OPTION1 = 'Delete'(104)
              TEXT_OPTION2 = 'Exit'(105)
              TITEL        = 'Delete historical line items'(106)
         IMPORTING
              ANSWER       = L_ANSWER.
    "/ if ( Daten nicht l?chen )
    IF ( L_ANSWER <> 1 ).
      STOP.
    ENDIF.
  ENDIF.

  G_OBJNR(2)    = 'FK'.
  G_OBJNR+2(4)  = FM_AREA.

*----- Summenschluessel lesen (fuer effektiven Einzelposten-Zugriff)
  SELECT
              LEDNR
              GJAHR
              PERBL
              GEBER
              WRTTP
              VORGA
              POSIT
              TWAER
              BUKRS
              GSBER
                  FROM  FMSU
                  INTO CORRESPONDING FIELDS OF TABLE L_T_SUM
         WHERE  OBJNR  =  G_OBJNR
         AND    GJAHR  BETWEEN FROMYEAR AND TO_YEAR
         AND  ( WRTTP  = '51' OR WRTTP = '5A' )
         AND    BUKRS  IN S_BUKRS.

  LOOP AT  L_T_SUM.

*----- Einzelposten-Information pro Summenschluessel
*----- und Periode
    L_PERIO = L_T_SUM-PERBL - 16.

    DO 16 TIMES.
      ADD 1 TO L_PERIO.

      IF L_T_SUM-GJAHR = FROMYEAR.
        CHECK L_PERIO >= FROM_PER.
      ENDIF.
      IF L_T_SUM-GJAHR = TO_YEAR.
        CHECK L_PERIO <= TO_PERIO.
      ENDIF.

*----- Alle vorhandenen 'n-1' Schluessel FMEP lesen
*----- KOZEI wird nicht ausgewertet (technisches Feld)
      SELECT  OBJNRZ VGZEI  FROM  FMEP
        INTO CORRESPONDING FIELDS OF TABLE L_T_LINES
             WHERE  LEDNR    = L_T_SUM-LEDNR
             AND    OBJNR    = G_OBJNR
             AND    GJAHR    = L_T_SUM-GJAHR
             AND    PERIO    = L_PERIO
             AND    GEBER    = L_T_SUM-GEBER
             AND    WRTTP    = L_T_SUM-WRTTP
             AND    VORGA    = L_T_SUM-VORGA
             AND    POSIT    = L_T_SUM-POSIT
             AND    TWAER    = L_T_SUM-TWAER
             AND    BUKRS    = L_T_SUM-BUKRS
             AND    GSBER    = L_T_SUM-GSBER
             AND  ( LDBTR <> 0 OR TRBTR <> 0 ).

      CHECK SY-SUBRC = 0.
      ADD SY-DBCNT TO G_CNT_ALL.

      SORT L_T_LINES BY OBJNRZ VGZEI.
      DELETE ADJACENT DUPLICATES FROM  L_T_LINES.

*----- Untersuchung, ob dafuer Loeschen moeglich ist
      LOOP AT L_T_LINES.

*----- In FUNCTION 'FMFS_GET_ITEM_NUMBER_FI' /aus LFMMMF03
*----- Einkaufsbelegnummer wurde in VGZEI-BELNR eingebaut
*----- Bestell-Position zweigeteilt in Bukrs/Zeile
*----- Hier holen wir sie wieder heraus
        CALL FUNCTION 'FMFS_GET_DOCUMENT_NUMBER_FI'
             EXPORTING
                  I_EPONR = L_T_LINES-VGZEI
             IMPORTING
                  E_BELNR = L_EBELN                      "#EC DOM_EQUAL
                  E_BUKRS = L_BUKRS
                  E_BUZEI = L_BUZEI.

        L_EBELP(2)   = L_BUKRS(2).
        L_EBELP+2(3) = L_BUZEI.
        CLEAR DELFLAGS.

*----- Pruefung
        SELECT SINGLE LOEKZ EREKZ FROM  EKPO
         INTO CORRESPONDING FIELDS OF DELFLAGS
               WHERE  EBELN  = L_EBELN
               AND    EBELP  = L_EBELP.

*----- Falls nicht vorhanden oder Kennzeichen: Loeschen in TR-CB
        CHECK SY-DBCNT = 0 OR
              DELFLAGS-LOEKZ <> SPACE OR
              DELFLAGS-EREKZ <> SPACE.

        IF P_LISTE <> SPACE.
          WRITE: / L_EBELN, L_EBELP, DELFLAGS-LOEKZ, DELFLAGS-EREKZ.
          HIDE: L_EBELN, L_EBELP.
        ENDIF.
        IF P_TEST = SPACE.

*----- Summen ermitteln
          SELECT SUM( TRBTR ) INTO (L_TRBTR) FROM FMEP
                           WHERE  LEDNR    = L_T_SUM-LEDNR
                           AND    OBJNR    = G_OBJNR
                           AND    GJAHR    = L_T_SUM-GJAHR
                           AND    PERIO    = L_PERIO
                           AND    GEBER    = L_T_SUM-GEBER
                           AND    WRTTP    = L_T_SUM-WRTTP
                           AND    VORGA    = L_T_SUM-VORGA
                           AND    POSIT    = L_T_SUM-POSIT
                           AND    TWAER    = L_T_SUM-TWAER
                           AND    BUKRS    = L_T_SUM-BUKRS
                           AND    GSBER    = L_T_SUM-GSBER
                           AND    OBJNRZ   = L_T_LINES-OBJNRZ
                           AND    VGZEI    = L_T_LINES-VGZEI.
          SELECT SUM( LDBTR ) INTO (L_LDBTR) FROM FMEP
                           WHERE  LEDNR    = L_T_SUM-LEDNR
                           AND    OBJNR    = G_OBJNR
                           AND    GJAHR    = L_T_SUM-GJAHR
                           AND    PERIO    = L_PERIO
                           AND    GEBER    = L_T_SUM-GEBER
                           AND    WRTTP    = L_T_SUM-WRTTP
                           AND    VORGA    = L_T_SUM-VORGA
                           AND    POSIT    = L_T_SUM-POSIT
                           AND    TWAER    = L_T_SUM-TWAER
                           AND    BUKRS    = L_T_SUM-BUKRS
                           AND    GSBER    = L_T_SUM-GSBER
                           AND    OBJNRZ   = L_T_LINES-OBJNRZ
                           AND    VGZEI    = L_T_LINES-VGZEI.

*----- EP loeschen
          DELETE FROM  FMEP
                 WHERE  LEDNR    = L_T_SUM-LEDNR
                 AND    OBJNR    = G_OBJNR
                 AND    GJAHR    = L_T_SUM-GJAHR
                 AND    PERIO    = L_PERIO
                 AND    GEBER    = L_T_SUM-GEBER
                 AND    WRTTP    = L_T_SUM-WRTTP
                 AND    VORGA    = L_T_SUM-VORGA
                 AND    POSIT    = L_T_SUM-POSIT
                 AND    TWAER    = L_T_SUM-TWAER
                 AND    BUKRS    = L_T_SUM-BUKRS
                 AND    GSBER    = L_T_SUM-GSBER
                 AND    OBJNRZ   = L_T_LINES-OBJNRZ
                 AND    VGZEI    = L_T_LINES-VGZEI.

          L_CNT_DEL =  SY-DBCNT.
          ADD L_CNT_DEL TO G_CNT_DEL.

*----- Summen-Update
          IF L_LDBTR <> 0 OR L_TRBTR <> 0.
            MULTIPLY: L_LDBTR BY -1, L_TRBTR BY -1.
            MOVE-CORRESPONDING L_T_SUM TO L_F_FMSU.
            L_F_FMSU-OBJNR = G_OBJNR.

            CALL FUNCTION 'FMSU_UPDATE'
                 EXPORTING
                      I_AMOUNT_LD      = L_LDBTR
                      I_AMOUNT_TR      = L_TRBTR
                      I_FLG_BUFFER_ALL = ' '
                      I_FMSU_KEY       = L_F_FMSU
                      I_PERIO          = L_PERIO.
          ENDIF.

          PERFORM COMMIT_PACKAGE USING L_CNT_DEL.
        ELSE.

          SELECT COUNT(*) FROM  FMEP
                 WHERE  LEDNR    = L_T_SUM-LEDNR
                 AND    OBJNR    = G_OBJNR
                 AND    GJAHR    = L_T_SUM-GJAHR
                 AND    PERIO    = L_PERIO
                 AND    GEBER    = L_T_SUM-GEBER
                 AND    WRTTP    = L_T_SUM-WRTTP
                 AND    VORGA    = L_T_SUM-VORGA
                 AND    POSIT    = L_T_SUM-POSIT
                 AND    TWAER    = L_T_SUM-TWAER
                 AND    BUKRS    = L_T_SUM-BUKRS
                 AND    GSBER    = L_T_SUM-GSBER
                 AND    OBJNRZ   = L_T_LINES-OBJNRZ
                 AND    VGZEI    = L_T_LINES-VGZEI.

          ADD SY-DBCNT TO G_CNT_DEL.
        ENDIF.
      ENDLOOP.                         "lines
    ENDDO.                             "period
  ENDLOOP.                                                  "sum
  IF P_TEST = SPACE.
    COMMIT WORK.
  ENDIF.

*----- Protokoll ----------------------------------------------------*
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  WRITE: / 'FM Area'(201),   32 FM_AREA.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:   / 'From Period'(207),     32 FROM_PER,
           / 'From Fiscal Year'(206), 32 FROMYEAR,
           / 'To Period'(209),       32 TO_PERIO,
           / 'To Fiscal Year'(208),  32 TO_YEAR.
  ULINE.

  IF ( P_TEST = SPACE ) .              "/ Echtlauf
    WRITE: / 'Prod Run'(204) COLOR COL_POSITIVE. SKIP.
    FORMAT COLOR COL_NORMAL INTENSIFIED.
    WRITE: / 'Number of document lines found:'(010),
                            45 G_CNT_ALL,
          / 'Number of document lines deleted:'(011),
                           45 G_CNT_DEL.
  ELSE.                                "/ Testmode
    WRITE: / 'Test Run'(205) COLOR COL_NEGATIVE. SKIP.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: / 'Number of document lines found:'(010),
                          45 G_CNT_ALL,
           / 'Number of document lines to be deleted:'(012),
                          45 G_CNT_DEL.
  ENDIF.
  CLEAR L_EBELN.

*----------------------------------------------------------------------*
AT LINE-SELECTION.
*----------------------------------------------------------------------*

  IF L_EBELN IS INITIAL.
    MESSAGE I519(00).
*    Bitte Cursor auf eine g?tige Zeile stellen
  ELSE.
    SET PARAMETER ID 'BES' FIELD L_EBELN.
    SET PARAMETER ID 'BSP' FIELD L_EBELP.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    CLEAR L_EBELN.
  ENDIF.

*********************  SUBROUTINES *************************************
*&---------------------------------------------------------------------*
*&      Form  COMMIT_PACKAGE
*&---------------------------------------------------------------------*
FORM COMMIT_PACKAGE USING    U_ADD.

  STATICS: L_CNT_COMMIT LIKE SY-TFILL.

  ADD U_ADD TO L_CNT_COMMIT.

  CHECK L_CNT_COMMIT > C_CNT_COMMIT.
  CLEAR L_CNT_COMMIT.
  COMMIT WORK.

ENDFORM.                               " COMMIT_PACKAGE
