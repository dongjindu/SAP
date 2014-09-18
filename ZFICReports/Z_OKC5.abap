*---------------------------------------------------------------------*
*          Report-Directory : RKSCUS03                                *
*---------------------------------------------------------------------*
*  PERFORM FILL_DEL_SELTAB.                                           *
*  PERFORM KILL_ENTRIES.                                              *
*  PERFORM WRITE_LIST.                                                *
*---------------------------------------------------------------------*
*END_IHV  07.11.1997   09:58:38   40A   P40
REPORT RKSCUS03 MESSAGE-ID KZ.

* P30K028908: nicht alle A-Segmente werden gelöscht (CSKA/CSKU)

RANGES SELTAB FOR CSKB-KSTAR.
INCLUDE MKCICL01.

PARAMETERS KOKRS LIKE RKMAH-KOKRS OBLIGATORY MEMORY ID CAC.     "vxf/30F
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS SELKSTAR FOR CSKB-KSTAR.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT 4(10) TEXT-ODR.
PARAMETERS H LIKE TKA01-KHINR(10).
SELECTION-SCREEN SKIP 1.
PARAMETERS TESTLAUF LIKE KREO-TESTLAUF DEFAULT 'X'.
SELECTION-SCREEN FUNCTION KEY 1.       "vxf/30F
SELECTION-SCREEN FUNCTION KEY 2.       "vxf/30F

TABLES:  SSCRFIELDS.                   "vxf/30F

DATA: A_SUM       TYPE I,
      A_DEL       TYPE I,
      A_DEL_NOT   TYPE I,
      B_SUM       TYPE I,
      B_DEL       TYPE I,
      B_SUM_OTH   TYPE I,
      B_DEL_NOT   TYPE I,
      U_SUM       TYPE I,
      U_DEL       TYPE I,
      U_DEL_NOT   TYPE I,
      REC         TYPE I,
      CLFL(1)     TYPE C.

DATA: BEGIN OF CONT_XCSKB OCCURS 200.
        INCLUDE STRUCTURE CSKB.
DATA: END OF CONT_XCSKB.

RANGES: XTKA01 FOR TKA01-KOKRS.                             "P30K028908

* initialization                                                "vxf/30F
INITIALIZATION.                        "vxf/30F
  WRITE TEXT-FC1 TO SSCRFIELDS-FUNCTXT_01(20).                  "vxf/30F
  WRITE TEXT-FC2 TO SSCRFIELDS-FUNCTXT_02(20).                  "vxf/30F
                                       "vxf/30F
  IF KOKRS = SPACE.                    "vxf/30F
    SELECT * FROM TKA01 UP TO 1 ROWS.  "vxf/30F
      KOKRS = TKA01-KOKRS.             "vxf/30F
    ENDSELECT.                         "vxf/30F
  ENDIF.                               "vxf/30F

* At selection-Screen
AT SELECTION-SCREEN ON KOKRS.

  CASE SSCRFIELDS-UCOMM.               "vxf/30F
    WHEN 'FC01'.                       "vxf/30F
      SUBMIT RKCORR09 WITH TEST = 'X' AND RETURN.               "vxf/30F
    WHEN 'FC02'.                       "vxf/30F
      SUBMIT RKCORR09 WITH TEST = ' ' AND RETURN.               "vxf/30F
  ENDCASE.                             "vxf/30F

  PERFORM CHECK_KOKRS USING KOKRS 'X'.

* Kostenartenselektion prüfen
AT SELECTION-SCREEN.

  PERFORM POPUP_ASK USING TESTLAUF KOKRS.
  DESCRIBE TABLE SELKSTAR LINES SIZE_SELTAB.
  PERFORM CHECK_SELECTIONS USING H TKA01-KTOPL 'KSTAR'.

* start-of-selection
START-OF-SELECTION.

* prüfen, ob Bewegungsdaten vorhanden sind
* PERFORM CHECK_TRANSACTION_DATAS USING KOKRS.

* Eintrag in Applikation-Log schreiben, falls Echtlauf
* PERFORM WRITE_APPL_LOG USING TESTLAUF.
* This report makes really changes in the system =>
* Check that the current user has debug authorization.
  authority-check object 'S_DEVELOP'
    id 'DEVCLASS' dummy
    id 'OBJTYPE'  field 'DEBUG'
    id 'OBJNAME'  dummy
    id 'P_GROUP'  dummy
    id 'ACTVT'    field '03'.
  if  sy-subrc <> 0.
    message e895(m7) with 'Sorry, no authorization'.
  endif.


  PERFORM FILL_SPRAS.

  REFRESH: XTKA01.                                          "P30K028908
  XTKA01-SIGN = 'I'.                                        "P30K028908
  XTKA01-OPTION = 'EQ'.                                     "P30K028908
  SELECT * FROM TKA01 WHERE KTOPL = TKA01-KTOPL             "P30K028908
                      AND   KOKRS <> KOKRS.                 "P30K028908
    XTKA01-LOW = TKA01-KOKRS.                               "P30K028908
    APPEND XTKA01.                                          "P30K028908
  ENDSELECT.                                                "P30K028908

* B-Segmente holen
  IF SELHIE = 'X'.
    PERFORM FILL_SELTAB USING KHINR KOKRS
                        KOKRS GSETC_COSTELEMENT_SETCLASS.       "vxf/40A
  ELSE.
    SELECT * FROM CSKB INTO TABLE XCSKB
                       WHERE KOKRS = KOKRS
                       AND   KSTAR IN SELKSTAR.
  ENDIF.
  COMMIT WORK.
  DESCRIBE TABLE XCSKB LINES B_SUM.

  IF B_SUM = 0.
    MESSAGE I526 WITH TEXT-004.
  ELSE.
    IF B_SUM > STEP.
      WHILE BEG <= B_SUM.
        PERFORM FILL_DEL_SELTAB USING BEG FIN TESTLAUF.
        BEG = FIN + 1.
        FIN = BEG + STEP - 1.
      ENDWHILE.
    ELSE.
      PERFORM FILL_DEL_SELTAB USING BEG FIN TESTLAUF.
    ENDIF.
  ENDIF.

* ggf. Feld Kontenplan im Kostenrechnungskreis clearen
  SELECT COUNT(*) FROM CSKB WHERE KOKRS = KOKRS.
  IF SY-DBCNT = 0.
    SELECT KTOPL FROM TKA01 INTO CSKA-KTOPL        "vxf/R99/ALRK179599
                                 UP TO 1 ROWS      "vxf/R99/ALRK179599
                            WHERE KOKRS = KOKRS.   "vxf/R99/ALRK179599
    ENDSELECT.                         "vxf/R99/ALRK179599
    IF SY-SUBRC <> 0.                  "vxf/R99/ALRK179599
      CLEAR CSKA-KTOPL.                "vxf/R99/ALRK179599
    ENDIF.                             "vxf/R99/ALRK179599
    UPDATE TKA01 SET KTOPL = SPACE WHERE KOKRS = KOKRS.
    CLFL = 'X'.
    COMMIT WORK.                       "vxf/R99/ALRK179599
    PERFORM DELETE_OTHER_TABLES USING CSKA-KTOPL.  "vxf/R99/ALRK179599
  ENDIF.

* end-of-selection
END-OF-SELECTION.

  PERFORM WRITE_LIST.

*---------------------------------------------------------------------*
*       FORM FILL_DEL_SELTAB                                          *
*---------------------------------------------------------------------*
*       füllt SELTAB  mit Kostenarten und löscht diese                *
*---------------------------------------------------------------------*
*  -->  FROM : VON SY-TABIX XCSKB                                     *
*  -->  TO   : BIS SY-TABIX XCSKB                                     *
*  -->  DELFL: Löschen oder Lesen                                     *
*---------------------------------------------------------------------*
FORM FILL_DEL_SELTAB USING FROM TO DELFL.

  CLEAR:   SELTAB, CONT_XCSKB, CD_HEADER, LONGTXT.
  REFRESH: SELTAB, CONT_XCSKB, CD_HEADER, LONGTXT.
  SORT XCSKB BY KSTAR.
* SELTAB mit B-Segment-Nummern füllen
  SORT XCSKB.
  CD_HEADER-SIGN = 'I'. CD_HEADER-OPTION = 'EQ'.
  SELTAB-SIGN = 'I'.    SELTAB-OPTION = 'EQ'.
  CD_HEADER-LOW+0(4) = LONGTXT+0(4) = KOKRS.
  LOOP AT XCSKB FROM BEG TO FIN.
    IF SELTAB-LOW = XCSKB-KSTAR.
      ADD 1 TO B_SUM_OTH.
    ENDIF.
    SELTAB-LOW    = XCSKB-KSTAR.
    APPEND SELTAB.
    LONGTXT+4(10) = XCSKB-KSTAR.
    APPEND LONGTXT.
    CONT_XCSKB = XCSKB.
    APPEND CONT_XCSKB.
    CD_HEADER-LOW+4(10) = XCSKB-KSTAR.
    APPEND CD_HEADER.
  ENDLOOP.
* weitere Zeitintervalle zahlenmäßig erfassen
  DESCRIBE TABLE SELTAB LINES COUNT.

* A-Segmente holen
  IF COUNT > 0.
    SELECT * FROM CSKA INTO TABLE XCSKA
                  WHERE KTOPL = TKA01-KTOPL
                  AND   KSTAR IN SELTAB.
    DESCRIBE TABLE XCSKA LINES COUNT.
    A_SUM = A_SUM + COUNT.
    COMMIT WORK.
* Texte zu A-Segmenten holen
    SELECT * FROM CSKU INTO TABLE XCSKU
                  WHERE KTOPL = TKA01-KTOPL
                  AND   SPRAS IN TAB_SPRAS
                  AND   KSTAR IN SELTAB.
    DESCRIBE TABLE XCSKU LINES COUNT.

    U_SUM = U_SUM + COUNT.
    COMMIT WORK.
  ENDIF.

* Änderungsbelege holen
  IF DELFL = 'X'.                                           "P30K114948
    PERFORM READ_CLUSTER USING 'KSTAR'.
  ENDIF.                                                    "P30K114948

* Langtexte lesen
  PERFORM READ_LONGTEXT USING OBJ_CSKA.
  IF DELFL = SPACE.
    STX_DEL = STX_SUM.
  ENDIF.

* nicht löschbare A-Segmente u. Texte aus XCSKA/U entfernen
  DESCRIBE TABLE XTKA01 LINES SY-TFILL.                     "P30K028908
  IF SY-TFILL > 0.                                          "P30K028908
    LOOP AT XCSKA.
      SELECT * FROM CSKB
*                   WHERE KOKRS <> KOKRS                     "P30K028908
                    WHERE KOKRS IN XTKA01                   "P30K028908
                    AND   KSTAR = XCSKA-KSTAR.
        EXIT.
      ENDSELECT.

      IF SY-SUBRC = 0.
        DELETE XCSKA.
        A_DEL_NOT = A_DEL_NOT + 1.
        LOOP AT XCSKU WHERE KSTAR = XCSKA-KSTAR.
          DELETE XCSKU.
          U_DEL_NOT = U_DEL_NOT + 1.
        ENDLOOP.
      ENDIF.
      COMMIT WORK.
    ENDLOOP.
  ENDIF.                                                    "P30K028908

* Sätze entfernen, die noch verwendet werden
* PERFORM KILL_ENTRIES.                                      "P30K114948

  IF DELFL = 'X'.
* Testlauf: nichts zu tun
  ELSE.
* Echtlauf: löschen und Statistik fortschreiben
    LOOP AT LONGTXT_D.
      PERFORM DELETE_LONGTEXT USING OBJ_CSKA ID_LTXT LONGTXT.
    ENDLOOP.
    PERFORM DELETE_CLUSTER USING 'KSTAR'.
    DELETE CSKB FROM TABLE CONT_XCSKB.
    B_DEL = B_DEL + SY-DBCNT.
    COMMIT WORK.
    DELETE CSKA FROM TABLE XCSKA.
    A_DEL = A_DEL + SY-DBCNT.
    COMMIT WORK.
    DELETE CSKU FROM TABLE XCSKU.
    U_DEL = U_DEL + SY-DBCNT.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM KILL_ENTRIES                                             *
*---------------------------------------------------------------------*
*       entfernt nicht löschbare Sätze aus den Löschtabellen          *
*---------------------------------------------------------------------*
FORM KILL_ENTRIES.

  LOOP AT SELTAB WHERE HIGH = SPACE.
    LOOP AT XCSKB WHERE KSTAR = SELTAB-LOW.
      DELETE XCSKB.
      B_DEL_NOT = B_DEL_NOT + 1 .
    ENDLOOP.
    LOOP AT XCSKA WHERE KSTAR = SELTAB-LOW.
      DELETE XCSKA.
      A_DEL_NOT = A_DEL_NOT + 1 .
    ENDLOOP.
    LOOP AT XCSKU WHERE KSTAR = SELTAB-LOW.
      DELETE XCSKU.
      U_DEL_NOT = U_DEL_NOT + 1 .
    ENDLOOP.
    LOOP AT CD_HEADER.
      IF CD_HEADER-LOW+4(10) = SELTAB-LOW.
        DELETE CD_HEADER.
        CDH_DEL_NOT = CDH_DEL_NOT + 1 .
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_LIST                                               *
*---------------------------------------------------------------------*
*       Listausgabe                                                   *
*---------------------------------------------------------------------*
FORM WRITE_LIST.

* Ausgabe Listkopf
  B_SUM = B_SUM - B_SUM_OTH.
  T40 = TEXT-012.
  WRITE: / T40                           COLOR 1 INTENSIFIED,
         43 KOKRS                        COLOR 2 INTENSIFIED.
  CASE SELHIE.
    WHEN ' '.
      T40 = TEXT-006.
      WRITE: / T40        COLOR 1 INTENSIFIED.
      SORT SELKSTAR.
      LOOP AT SELKSTAR.
        WRITE: SELKSTAR-LOW UNDER KOKRS  COLOR 2 INTENSIFIED ,
               SELKSTAR-HIGH             COLOR 2 INTENSIFIED ,
               SELKSTAR-OPTION           COLOR 2 INTENSIFIED ,
               SELKSTAR-SIGN             COLOR 2 INTENSIFIED .
        NEW-LINE.
      ENDLOOP.
      IF SY-SUBRC <> 0.
        SELKSTAR-LOW = TEXT-ALL.
        WRITE: SELKSTAR-LOW UNDER KOKRS      COLOR 2 INTENSIFIED .
      ENDIF.
    WHEN 'X'.
      T40 = TEXT-005.
      WRITE: / TEXT-005                  COLOR 1 INTENSIFIED,
               KHINR                     COLOR 2 INTENSIFIED .
  ENDCASE.

* Ausgabe Statistik
  WRITE: /.
  CASE TESTLAUF.
    WHEN ' '.
      WRITE: / TEXT-013.
    WHEN 'X'.
      WRITE: / TEXT-014.
  ENDCASE.

  WRITE: /1 SY-ULINE(58).
  PERFORM WRITE_LINE USING TEXT-007 B_SUM.
  PERFORM WRITE_LINE USING TEXT-010 B_DEL_NOT.
  PERFORM WRITE_LINE USING TEXT-008 B_DEL.
  PERFORM WRITE_LINE USING TEXT-018 B_SUM_OTH.

  WRITE: /1 SY-ULINE(58).
  PERFORM WRITE_LINE USING TEXT-009 A_SUM.
  PERFORM WRITE_LINE USING TEXT-010 A_DEL_NOT.
  PERFORM WRITE_LINE USING TEXT-008 A_DEL.

  WRITE: /1 SY-ULINE(58).
  PERFORM WRITE_LINE USING TEXT-011 U_SUM.
  PERFORM WRITE_LINE USING TEXT-010 U_DEL_NOT.
  PERFORM WRITE_LINE USING TEXT-008 U_DEL.

  WRITE: /1 SY-ULINE(58).
  PERFORM WRITE_LINE USING TEXT-015 CDH_SUM_H.
  PERFORM WRITE_LINE USING TEXT-008 CDH_DEL_H.
  PERFORM WRITE_LINE USING TEXT-016 CDH_SUM_P.
  PERFORM WRITE_LINE USING TEXT-008 CDH_DEL_P.

  WRITE: /1 SY-ULINE(58).
  PERFORM WRITE_LINE USING TEXT-017 STX_SUM.
  PERFORM WRITE_LINE USING TEXT-008 STX_DEL.
  WRITE: /1 SY-ULINE(58).

  CHECK TESTLAUF = SPACE AND CLFL = 'X'.
  WRITE: /.
  T80 = TEXT-CLR.
  REPLACE: '&1' WITH TKA01-KTOPL INTO T80,
*          '&2' WITH TKA01-KOKRS INTO T80.                  "P30K114948
           '&2' WITH KOKRS       INTO T80.                  "P30K114948
  CONDENSE T80.
  WRITE: / T80.

* Endezeit setzen
  GET TIME FIELD TIME_E.
  PERFORM WRITE_TIME.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DELETE_OTHER_TABLES                  "vxf/R99/ALRK179599
*&---------------------------------------------------------------------*
FORM DELETE_OTHER_TABLES USING P_KTOPL LIKE CSKA-KTOPL.

  SELECT KOKRS FROM TKA01 INTO TKA01-KOKRS UP TO 1 ROWS
               WHERE KTOPL = P_KTOPL.
  ENDSELECT.
  CHECK SY-SUBRC <> 0.
  DELETE FROM TKSKA WHERE KTOPL = P_KTOPL.

ENDFORM.                               " DELETE_OTHER_TABLES
