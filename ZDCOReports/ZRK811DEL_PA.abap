*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000573510                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZRK811DEL_PA
*& Object Header   PROG ZRK811DEL_PA
*&-------------------------------------------------------------------*
*& REPORT ZRK811DEL_PA
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZRK811DEL_PA MESSAGE-ID GA NO STANDARD PAGE HEADING LINE-SIZE
100.
* Report for deletion of CO-PA cycles


TABLES: T811C  ,"    Allocation-Cycles
        T811D  ,"    Allocation Belegnummern
        T811DS ,"    Allocation Belegnummern Segment-Reverse/Rebook
        T811F  ,"    Allocation Elementtabelle
        T811G  ,"    Feldgruppen zu Allocation Cycle
        T811H  ,"    Allocation: Datenfeldbeschreibung
        T811I  ,"    Allokation Informationen Schl?selfeld
        T811IA ,"    Customizing Allocation
        T811J  ,"    Allocation Informationen Feldgruppen
        T811K  ,"    Schl?selfelder f? Allokation
        T811L  ,"    Umlage/Verteilung-Langtexte
        T811M  ,"    Allocations: Texte zu Feldgruppen
        T811R  ,"    Allocation Tabelle Empf?gerkontierungen
        T811S  ,"    Allocation Segmente
        T811T  ,"    Tabelleninformation Allocation
        T811U  ,"    Tabelleninformation Allocation Sender/Empf?ger-Bez
        T811X  ,"    Feste Selektionswerte f? Allokation

        TKEB.  "    Ergebnisbereich


SELECTION-SCREEN BEGIN OF BLOCK CYC WITH FRAME TITLE TEXT-020.
  PARAMETERS: P_ERKRS LIKE TKEB-ERKRS OBLIGATORY MEMORY ID ERB.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) TEXT-003.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: P_FR_CYC LIKE RKAL1-KSCYC OBLIGATORY.
    SELECTION-SCREEN COMMENT 45(10) TEXT-021.
    PARAMETERS: P_TO_CYC LIKE RKAL1-KSCYC.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) TEXT-022.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: P_FR_DAT LIKE RKGA2-SDATE.
    SELECTION-SCREEN COMMENT 45(10) TEXT-021.
    PARAMETERS: P_TO_DAT LIKE RKGA2-SDATE.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK CYC.

SELECTION-SCREEN BEGIN OF BLOCK OPT WITH FRAME TITLE TEXT-023.
  PARAMETERS: P_TEST   AS CHECKBOX DEFAULT 'X',
              P_CHECK  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK OPT.

DATA: KEEP_CYCLE LIKE T811C-CYCLE,
      SEL_FR_CYC LIKE T811C-CYCLE,
      SEL_TO_CYC LIKE T811C-CYCLE,
      LD_TAB LIKE T811C-TAB ,
      LD_NUMBER LIKE SY-DBCNT,
      LD_REVERS,
      ANSWER.

AT SELECTION-SCREEN.
  SELECT SINGLE * FROM TKEB WHERE ERKRS = P_ERKRS.
  IF SY-SUBRC NE 0.
    SET CURSOR FIELD 'P_ERKRS'.
    MESSAGE E131(KE) WITH P_ERKRS.
  ENDIF.

  IF P_TO_CYC IS INITIAL.
    P_TO_CYC = P_FR_CYC.
  ELSE.
    IF P_TO_CYC < P_FR_CYC.
      SET CURSOR FIELD 'P_TO_CYC'.
      MESSAGE E760(GU).
    ENDIF.
  ENDIF.

  IF P_TO_DAT IS INITIAL.
    IF NOT P_FR_DAT IS INITIAL.
      P_TO_DAT = P_FR_DAT.
    ELSE.
      P_TO_DAT = '30001231'.
    ENDIF.
  ENDIF.
  IF P_FR_DAT IS INITIAL.
    P_FR_DAT = '19000101'.
  ENDIF.
  IF P_TO_DAT < P_FR_DAT.
    SET CURSOR FIELD 'P_TO_DAT'.
    MESSAGE E760(GU).
  ENDIF.
******************Begin of processing***********************************
START-OF-SELECTION.

  SEL_FR_CYC(4)   = SEL_TO_CYC(4) = P_ERKRS.
  SEL_FR_CYC+4(6) = P_FR_CYC.
  SEL_TO_CYC+4(6) = P_TO_CYC.

  ld_tab(3) = 'CE7'.
  ld_tab+3(4) = P_ERKRS.

*Statistics
  IF P_TEST IS INITIAL.
    WRITE: / TEXT-005 INTENSIFIED.   "Delete run
  ELSE.
    WRITE: / TEXT-006 INTENSIFIED.   "test run
  ENDIF.

  WRITE: / SY-ULINE.

  WRITE: / TEXT-001 INTENSIFIED.     "Delete statistics

  WRITE: / SY-ULINE.
  CLEAR LD_REVERS.
  SELECT * FROM T811C WHERE TAB = LD_TAB
                       AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                       AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.
    WRITE: T811C-CYCLE, T811C-SDATE.
    IF P_CHECK = 'X'.
*.....check if cycle is totally reversed
      SELECT * FROM T811D WHERE TAB      = LD_TAB
                            AND CYCLE    = T811C-CYCLE
                            AND SDATE    = T811C-SDATE
                            AND REVERSED <> 'X'.
      ENDSELECT.
*.....cycle is not reversed
      IF SY-SUBRC = 0.
          LD_REVERS = 'X'.
          WRITE: AT 30 TEXT-011.
      ELSE.
*........check segment-reversal
         SELECT * FROM T811DS WHERE TAB      = LD_TAB
                                AND CYCLE    = T811C-CYCLE
                                AND SDATE    = T811C-SDATE
                                AND REVERSED <> 'X'.
         ENDSELECT.
         IF SY-SUBRC = 0.
            LD_REVERS = 'X'.
            WRITE: AT 30 TEXT-011.
         ENDIF.
      ENDIF.
    ENDIF.
    NEW-LINE.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    WRITE: / TEXT-004.                "no cycles found
  ENDIF.

  CHECK P_TEST IS INITIAL.
  IF LD_REVERS = 'X'.
     NEW-LINE.
     WRITE: / SY-ULINE.
     WRITE: / TEXT-024.
     EXIT.
  ENDIF.

*Popup to confirm step
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            TEXTLINE1      = TEXT-008
            TITEL          = TEXT-007
       IMPORTING
            ANSWER         = ANSWER
       EXCEPTIONS
            OTHERS         = 1.
  IF ANSWER NE 'J'.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.
*Delete Allocation tables
  DELETE FROM T811C WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.
  LD_NUMBER = SY-DBCNT.

  DELETE FROM T811D WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811DS WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811F WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811G WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811K WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811L WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811R WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  DELETE FROM T811S WHERE TAB = LD_TAB
                      AND CYCLE BETWEEN SEL_FR_CYC AND SEL_TO_CYC
                      AND SDATE BETWEEN P_FR_DAT   AND P_TO_DAT.

  COMMIT WORK.

  IF SY-SUBRC = 0.
     NEW-LINE.
     WRITE: / SY-ULINE.
     WRITE: TEXT-012, LD_NUMBER, TEXT-013.
  ENDIF.
*>>>> END OF INSERTION <<<<<<
