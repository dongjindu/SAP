*&-------------------------------------------------------------------*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMEWMS
*& Object Header   PROG ZAIMEWMS
*&-------------------------------------------------------------------*
*& REPORT ZAIMEWMS
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZAIMEWMS MESSAGE-ID AP.

TYPE-POOLS: IM.

INCLUDE LKBPPEQU.

TABLES: IMPR, IMTP, BPGE, BPJA, BPTR,
        TAI05, TAI08, TKA01, TBP1C.

DATA: BEGIN OF T_PARENTS OCCURS 100,
        PARNR LIKE IMPR-PARNR,
      END   OF T_PARENTS.

DATA: BEGIN OF T_VERSN OCCURS 10,
        VERSN LIKE BPGE-VERSN,
      END   OF T_VERSN.

DATA: BEGIN OF T_TRGKZ OCCURS 10,
        TRGKZ LIKE BPGE-TRGKZ,
      END   OF T_TRGKZ.

DATA: BEGIN OF T_POSIT OCCURS 10,
        POSIT LIKE BPGE-POSIT,
      END   OF T_POSIT.

DATA: BEGIN OF T_VORGA OCCURS 10,
        VORGA LIKE BPGE-VORGA,
      END   OF T_VORGA.

DATA: BEGIN OF T_WRTTP OCCURS 10,
        WRTTP LIKE BPGE-WRTTP,
      END   OF T_WRTTP.

RANGES: RG_POSIT FOR BPGE-POSIT.

DATA: L_IMTP             LIKE IMTP,
      L_IMPR_ENTRY       LIKE IMPR,
      L_PARNR_INITIAL    LIKE IMPR-PARNR,
      L_WRTTP            LIKE BPGE-WRTTP,
      L_SUBRC            LIKE SY-SUBRC,
      L_TABIX            LIKE SY-TABIX.

DATA: T_IMPR             LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_IMPR_LEAFS       LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_IMPR_SOURCE      LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_IMPR_TOP         LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_IMPR_TARGET      LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_IMPR_TARGET_TMP  LIKE IMPR OCCURS 100 WITH HEADER LINE.
DATA: T_COVOB_SOURCE     TYPE IM_COVOB_TYPE
                                  OCCURS 100 WITH HEADER LINE.
DATA: T_COVOB_SOURCE_TMP TYPE IM_COVOB_TYPE
                                  OCCURS 100 WITH HEADER LINE.
DATA: T_COVOB_TARGET     TYPE IM_COVOB_TYPE
                                  OCCURS 100 WITH HEADER LINE.
DATA: T_BPGE             LIKE BPGE OCCURS 100 WITH HEADER LINE.
DATA: T_BPJA             LIKE BPJA OCCURS 100 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK IPG.
PARAMETERS:    PROGRAM  LIKE IMTP-PRNAM,
               POSITION LIKE IMPR-POSID,
               APP_YEAR LIKE IMTP-GJAHR.
SELECTION-SCREEN END   OF BLOCK IPG.
PARAMETERS:      TEXT LIKE BPBK-SGTEXT,
                 ORIGINAL RADIOBUTTON GROUP TASK,
                 ROLLUP   RADIOBUTTON GROUP TASK,
                 BUDGET RADIOBUTTON GROUP BP,
                 PLAN RADIOBUTTON GROUP BP,
*                F? Simulationszwecke/Debugging.
                 RELEASE LIKE SY-SAPRL NO-DISPLAY.
SELECT-OPTIONS:  SO_VERSN FOR BPGE-VERSN,
                 SO_CATEG FOR TAI08-IPPOS.
PARAMETERS:      FROMLEAF AS CHECKBOX.


AT SELECTION-SCREEN ON BLOCK IPG.

*  Programmdefinition O.K.?
  SELECT SINGLE * FROM IMTP
    WHERE PRNAM = PROGRAM
    AND   GJAHR = APP_YEAR.
  IF SY-SUBRC <> 0.
    MESSAGE E003 WITH PROGRAM APP_YEAR.
  ENDIF.

*  Position O.K.?
  IF NOT POSITION IS INITIAL.
    SELECT * FROM IMPR INTO L_IMPR_ENTRY
      WHERE PRNAM = PROGRAM
      AND   POSID = POSITION
      AND   GJAHR = APP_YEAR.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      MESSAGE E005 WITH PROGRAM POSITION APP_YEAR.
    ENDIF.
  ENDIF.


START-OF-SELECTION.

* Release bestimmen, falls es nicht
* (zu Simulationszwecken) mitgegeben wurde.
  IF RELEASE IS INITIAL.
    RELEASE = SY-SAPRL.
  ENDIF.

* Bestimme zu bearbeitenden Werttyp und
* die zu bearbeitenden Versionen.
  IF PLAN <> SPACE.
    L_WRTTP = L_WIPLAN.
  ELSE.
    L_WRTTP = L_WIBUDGET.
    REFRESH SO_VERSN.
    SO_VERSN-SIGN   = 'I'.
    SO_VERSN-OPTION = 'EQ'.
    SO_VERSN-LOW    = '000'.
  ENDIF.

* Programmdefinition einlesen.
  SELECT SINGLE * FROM IMTP INTO L_IMTP
    WHERE PRNAM = PROGRAM
    AND   GJAHR = APP_YEAR.
* Programmpositionen einlesen.
  SELECT * FROM IMPR INTO TABLE T_IMPR
    WHERE PRNAM = PROGRAM
    AND   GJAHR = APP_YEAR.
* Einstiegsposition angegeben?
  IF NOT POSITION IS INITIAL.
*  Teilbaum bereitstellen.
    CALL FUNCTION 'AIPA_GET_SET_TO_ENTRY_ELEMENT'
         EXPORTING
              I_POSNR = L_IMPR_ENTRY-POSNR
         TABLES
              T_IMPR  = T_IMPR.
    LOOP AT T_IMPR
      WHERE POSNR = L_IMPR_ENTRY-POSNR.
      CLEAR: T_IMPR-PARNR.
      MODIFY T_IMPR TRANSPORTING PARNR.
    ENDLOOP.
    CLEAR L_IMPR_ENTRY-PARNR.
  ENDIF.

* ?erhaupt was zu tun?
  IF T_IMPR[] IS INITIAL.
    LEAVE PROGRAM.
  ENDIF.

* Selektion von Budgetarten:
* Wandle Selektion nach externer Darstellung
* in Selektion nach interner Darstellung um.
  IF L_IMTP-CAPEX <> SPACE.
    SELECT * FROM TAI08
      WHERE PRART =  L_IMTP-PRART
      AND   IPPOS IN SO_CATEG.
      RG_POSIT-SIGN   = 'I'.
      RG_POSIT-OPTION = 'EQ'.
      RG_POSIT-LOW    = TAI08-BPPOSIT.
      APPEND RG_POSIT.
    ENDSELECT.
  ENDIF.

* Alle vorkommenden
* Werte einlesen.
  SELECT * FROM BPGE
    INTO TABLE T_BPGE
    FOR ALL ENTRIES IN T_IMPR
    WHERE OBJNR = T_IMPR-OBJNR
    AND   VERSN IN SO_VERSN
    AND   TRGKZ =  L_NODE
    AND   POSIT IN RG_POSIT
    AND   WRTTP =  L_WRTTP
    AND   LEDNR = '0001'.
  SORT T_BPGE  BY OBJNR.
  SELECT * FROM BPJA
    INTO TABLE T_BPJA
    FOR ALL ENTRIES IN T_IMPR
    WHERE OBJNR =  T_IMPR-OBJNR
    AND   VERSN IN SO_VERSN
    AND   TRGKZ =  L_NODE
    AND   POSIT IN RG_POSIT
    AND   WRTTP =  L_WRTTP
    AND   LEDNR = '0001'.
  SORT T_BPJA  BY OBJNR.

* ?erhaupt was zu tun?
  IF T_BPGE[] IS INITIAL AND
     T_BPJA[] IS INITIAL .
    LEAVE PROGRAM.
  ENDIF.

* Eine Position hei? "wertetragend", wenn sie
* f? einen der Vorg?ge 'Original', 'Nachtrag' oder
* 'R?kgabe' einen von 0 verschiedenen Gesamt- oder
* Jahreswert aufeist (bei Budgetarten f? mindestens
* eine Budgetart und bei Planwerten f? mindestens eine
* (selektierte) Version).

* Hochrollen, aber nicht von Bl?tern sondern von
* den "untersten, wertetragenden Positionen":
* Das bedeutet, das Investitionsprogramm
* wird um genau die Positionen ausged?nt,
* o  die selbst nicht wertetragend sind und
* o  deren untergeordnete Positionen ebenfalls
*    alle nicht wertetragend sind.
  IF ROLLUP   <> SPACE AND
     FROMLEAF =  SPACE.
    PERFORM DETERMINE_VAL_CARRYING_SUBTREE.
  ENDIF.

* Noch was zu tun?
  IF T_IMPR[] IS INITIAL.
    LEAVE PROGRAM.
  ENDIF.

* Alle Parent-Beziehungen bestimmen.
  LOOP AT T_IMPR
    WHERE NOT PARNR IS INITIAL.
    T_PARENTS-PARNR = T_IMPR-PARNR.
    APPEND T_PARENTS.
  ENDLOOP.
  SORT T_PARENTS BY PARNR.
  DELETE ADJACENT DUPLICATES FROM T_PARENTS COMPARING PARNR.

* Hochrollen.
  IF ROLLUP <> SPACE.
*  Alle Bl?ter bestimmen.
    LOOP AT T_IMPR.
*    Position unter den V?ern?
      READ TABLE T_PARENTS
           TRANSPORTING NO FIELDS
           WITH KEY PARNR = T_IMPR-POSNR
           BINARY SEARCH.
*    Nein, ...
      IF SY-SUBRC <> 0.
*       ... dann ist sie ein Blatt.
        APPEND T_IMPR TO T_IMPR_LEAFS.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Top-Positionen bestimmen.
  IF POSITION IS INITIAL.
    SELECT * FROM IMPR INTO TABLE T_IMPR_TOP
      WHERE PRNAM = PROGRAM
      AND   GJAHR = APP_YEAR
      AND   PARNR = L_PARNR_INITIAL.
  ELSE.
    APPEND L_IMPR_ENTRY TO T_IMPR_TOP.
  ENDIF.

* Hochrollen, aber nicht von Bl?tern sondern von
* den "untersten, wertetragenden Positionen":
* Dann kann es sein, dass T_IMPR in der Routine
* DETERMINE_VAL_CARRYING_SUBTREE soweit abgespeckt
* wurde, dass es gar nicht mehr alle Top-Positionen
* enth?t. Dann muss aber auch T_IMPR_TOP
* entsprechend abgespeckt werden.
  IF ROLLUP   <> SPACE AND
     FROMLEAF =  SPACE.
*  Top-Positionen abloopen.
    LOOP AT T_IMPR_TOP.
*    Top-Position noch in T_IMPR?
      LOOP AT T_IMPR
        WHERE POSNR = T_IMPR_TOP-POSNR.
        EXIT.
      ENDLOOP.
*    Nein, ...
      IF SY-SUBRC <> 0.
*       ... dann wegwerfen.
        DELETE T_IMPR_TOP.
      ENDIF.
    ENDLOOP.
*  Noch was zu tun?
    IF T_IMPR_TOP[] IS INITIAL.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

* Alle vorkommenden
* -  Versionen,
* -  Genehmigungszeitr?me,
* -  Budgetarten,
* -  Vorg?ge,
* -  Werttypen
* bestimmen.

*Wenn zur?k zum Original, dann muss der Vorgang KBUD auf jeden Fall
*vorkommen
  IF ORIGINAL <> SPACE.
    T_VORGA-VORGA = 'KBUD'.
    APPEND T_VORGA.
  ENDIF.

  LOOP AT T_BPGE.
    T_VERSN-VERSN = T_BPGE-VERSN.
    APPEND T_VERSN.
    T_TRGKZ-TRGKZ = T_BPGE-TRGKZ.
    APPEND T_TRGKZ.
    T_POSIT-POSIT = T_BPGE-POSIT.
    APPEND T_POSIT.
    T_VORGA-VORGA = T_BPGE-VORGA.
    APPEND T_VORGA.
    T_WRTTP-WRTTP = T_BPGE-WRTTP.
    APPEND T_WRTTP.
  ENDLOOP.
  LOOP AT T_BPJA.
    T_VERSN-VERSN = T_BPJA-VERSN.
    APPEND T_VERSN.
    T_TRGKZ-TRGKZ = T_BPJA-TRGKZ.
    APPEND T_TRGKZ.
    T_POSIT-POSIT = T_BPJA-POSIT.
    APPEND T_POSIT.
    T_VORGA-VORGA = T_BPJA-VORGA.
    APPEND T_VORGA.
    T_WRTTP-WRTTP = T_BPJA-WRTTP.
    APPEND T_WRTTP.
  ENDLOOP.
  SORT T_VERSN BY VERSN.
  DELETE ADJACENT DUPLICATES FROM T_VERSN.
  SORT T_TRGKZ BY TRGKZ.
  DELETE ADJACENT DUPLICATES FROM T_TRGKZ.
  SORT T_POSIT BY POSIT.
  DELETE ADJACENT DUPLICATES FROM T_POSIT.
  SORT T_VORGA BY VORGA.
  DELETE ADJACENT DUPLICATES FROM T_VORGA.
  SORT T_WRTTP BY WRTTP.
  DELETE ADJACENT DUPLICATES FROM T_WRTTP.

* Quelltabelle f? Werte bestimmen.
  REFRESH T_IMPR_SOURCE.
*      Auf Original Setzen.
  IF     ORIGINAL <> SPACE.
*      Quellwerte kommen von allen Positionen.
    T_IMPR_SOURCE[] = T_IMPR[].
*      Hochrollen.
  ELSEIF ROLLUP <> SPACE.
*      Quellwerte kommen von Bl?tern.
    T_IMPR_SOURCE[] = T_IMPR_LEAFS[].
  ENDIF.

* Quellwerte in T_COVOB_SOURCE sammeln.
  REFRESH T_COVOB_SOURCE.
  LOOP AT T_IMPR_SOURCE.
    CLEAR T_COVOB_SOURCE.
    T_COVOB_SOURCE-OBJNR  = T_IMPR_SOURCE-OBJNR.
    READ TABLE T_BPGE
         WITH KEY OBJNR = T_IMPR_SOURCE-OBJNR
         BINARY SEARCH.
    L_SUBRC = SY-SUBRC.
    L_TABIX = SY-TABIX.
    IF L_SUBRC = 0.
      LOOP AT T_BPGE
        FROM L_TABIX.
        IF T_BPGE-OBJNR <> T_IMPR_SOURCE-OBJNR.
          EXIT.
        ENDIF.
*      Auf Original setzen?
        IF T_BPGE-WRTTP =  L_WIBUDGET AND
           ORIGINAL     <> SPACE      .
          T_BPGE-VORGA = L_VBUD.
        ENDIF.
        APPEND T_BPGE  TO T_COVOB_SOURCE-BPGE.
      ENDLOOP.
    ENDIF.
*
    READ TABLE T_BPJA
         WITH KEY OBJNR = T_IMPR_SOURCE-OBJNR
         BINARY SEARCH.
    L_SUBRC = SY-SUBRC.
    L_TABIX = SY-TABIX.
    IF L_SUBRC = 0.
      LOOP AT T_BPJA
        FROM L_TABIX.
        IF T_BPJA-OBJNR <> T_IMPR_SOURCE-OBJNR.
          EXIT.
        ENDIF.
*      Auf Original setzen?
        IF T_BPJA-WRTTP = L_WIBUDGET AND
           ORIGINAL     <> SPACE     .
          T_BPJA-VORGA = L_VBUD.
        ENDIF.
        APPEND T_BPJA  TO T_COVOB_SOURCE-BPJA.
      ENDLOOP.
    ENDIF.
*
    APPEND T_COVOB_SOURCE.
*
  ENDLOOP.

* Jeden Teilbaum zu Top-PSP-Element separat abarbeiten.
  LOOP AT T_IMPR_TOP.
    REFRESH T_IMPR_TARGET.
    T_IMPR_TARGET[] = T_IMPR[].
* Teilbaum zu Top-Position bestimmen, wenn keine
* Einstiegsposition angegeben wurde.
    IF POSITION IS INITIAL.
      CALL FUNCTION 'AIPA_GET_SET_TO_ENTRY_ELEMENT'
           EXPORTING
                I_POSNR = T_IMPR_TOP-POSNR
           TABLES
                T_IMPR  = T_IMPR_TARGET.
    ENDIF.
* Werte anpassen.
    LOOP AT T_TRGKZ.
      LOOP AT T_VERSN.
        LOOP AT T_POSIT.
          LOOP AT T_VORGA.
            LOOP AT T_WRTTP.
*           Plan.
              IF T_WRTTP-WRTTP = L_WIPLAN.
*              Nur Planungsvorg?ge interessieren.
                CHECK T_VORGA-VORGA = L_VPLAN.
              ENDIF.
*           Budget.
              IF T_WRTTP-WRTTP = L_WIBUDGET.
*              Nur Version 000 interessiert.
                CHECK T_VERSN-VERSN = '000'.
*              Nur Budgetierungsvorg?ge interessieren.
                CHECK T_VORGA-VORGA = L_VBUD OR
                      T_VORGA-VORGA = L_VBR0 OR
                      T_VORGA-VORGA = L_VBN0 .
*              Vortrag/Vorschau.
                IF T_TRGKZ-TRGKZ = L_INV_CF OR
                   T_TRGKZ-TRGKZ = L_INV_FU .
*                 Nur Orginalbudget interessiert.
                  CHECK T_VORGA-VORGA = L_VBUD.
                ENDIF.
              ENDIF.
*           Teilbaum ?ergeben.
              REFRESH T_IMPR_TARGET_TMP.
              T_IMPR_TARGET_TMP[] = T_IMPR_TARGET[].
*           Wertetabelle ?ergeben.
              REFRESH T_COVOB_SOURCE_TMP.
              T_COVOB_SOURCE_TMP[] = T_COVOB_SOURCE[].
              LOOP AT T_COVOB_SOURCE_TMP.
                DELETE T_COVOB_SOURCE_TMP-BPGE
                  WHERE ( TRGKZ <> T_TRGKZ-TRGKZ OR
                          VERSN <> T_VERSN-VERSN OR
                          POSIT <> T_POSIT-POSIT OR
                          VORGA <> T_VORGA-VORGA OR
                          WRTTP <> T_WRTTP-WRTTP ).
                DELETE T_COVOB_SOURCE_TMP-BPJA
                  WHERE ( TRGKZ <> T_TRGKZ-TRGKZ OR
                          VERSN <> T_VERSN-VERSN OR
                          POSIT <> T_POSIT-POSIT OR
                          VORGA <> T_VORGA-VORGA OR
                          WRTTP <> T_WRTTP-WRTTP ).
                MODIFY T_COVOB_SOURCE_TMP.
              ENDLOOP.
*           Werte aufbereiten.
              PERFORM AIPA_BUDGPROC_INPUT_CREATE
                TABLES
                  T_IMPR_TARGET_TMP      " IT_ZIEL_IMPR
                USING
                  L_IMTP                 " I_IMTP
                  T_VORGA-VORGA          " I_VORGA
                  T_TRGKZ-TRGKZ          " I_TRGKZ_FROM_1
                  ' '                    " I_TRGKZ_FROM_2
                  T_TRGKZ-TRGKZ          " I_TRGKZ_TO
                  T_WRTTP-WRTTP          " I_WRTTP_FROM_1
                  ' '                    " I_WRTTP_FROM_2
                  T_WRTTP-WRTTP          " I_WRTTP_TO
                  T_POSIT-POSIT          " I_POSIT_FROM
                  T_POSIT-POSIT          " I_POSIT_TO
                  T_VERSN-VERSN          " I_VERSN_FROM
                  T_VERSN-VERSN          " I_VERSN_TO
                  T_COVOB_SOURCE_TMP[]   " IT_QUELLE_COVOB
                  ROLLUP                 " I_FLG_ROLLUP
                  ' '                    " I_FLG_ADD
                CHANGING
                  T_COVOB_TARGET[].      " ET_ZIEL_COVOB
*          Zielwerte nachbereiten.
              LOOP AT T_COVOB_TARGET.
*            Hochrollen.
                IF ROLLUP <> SPACE.
*               Blatt?
                  READ TABLE T_PARENTS
                       WITH KEY PARNR = T_COVOB_TARGET-OBJNR
                       BINARY SEARCH.
*               Ja, ...
                  IF SY-SUBRC <> 0.
*                  ... dann rauswerfen, denn Bl?ter
*                  bleiben ja beim Hochrollen unber?rt.
                    DELETE T_COVOB_TARGET.
                    CONTINUE.
                  ENDIF.
                ENDIF.
*            Ansonsten 0-Werte raus.
                DELETE T_COVOB_TARGET-BPGE
                  WHERE WLGES = 0
                  AND   WTGES = 0.
                DELETE T_COVOB_TARGET-BPJA
                  WHERE WLJHR = 0
                  AND   WTJHR = 0.
                MODIFY T_COVOB_TARGET.
              ENDLOOP.
*          Beim Setzen auf Original muss zu jeder Position
*          in T_IMPR_TARGET ein Eintrag in T_COVOB_TARGET
*          existieren, beim Hochrollen muss zu jeder
*          Nicht-Blatt-Position in T_IMPR_TARGET ein Eintrag
*          in T_COVOB_TARGET existieren, da ja jeweils
*          das Delta zum DB-Zustand berechnet werden soll,
*          auch wenn der in T_COVOB_TARGET beschriebene
*          Endzustand f? einen bestimmten Vorgang den
*          Wert 0 vorsieht.
              SORT T_COVOB_TARGET BY OBJNR.
              LOOP AT T_IMPR_TARGET.
*            Hochrollen.
                IF ROLLUP <> SPACE.
*               Blatt?
                  READ TABLE T_PARENTS
                       WITH KEY PARNR = T_IMPR_TARGET-POSNR
                       BINARY SEARCH.
*               Ja, ...
                  IF SY-SUBRC <> 0.
*                   ... dann darf auch kein Eintrag
*                   in T_COVOB_TARGET erzeugt werden,
*                   Bl?ter beim Hochrollen unber?rt
*                   bleiben sollen.
                    CONTINUE.
                  ENDIF.
                ENDIF.
*            Eintrag in T_COVOB_TARGET vorhanden?
                READ TABLE T_COVOB_TARGET
                     TRANSPORTING NO FIELDS
                     WITH KEY OBJNR = T_IMPR_TARGET-OBJNR
                     BINARY SEARCH.
                L_SUBRC = SY-SUBRC.
                L_TABIX = SY-TABIX.
*            Nein, dann hinzuf?en.
                CHECK L_SUBRC <> 0.
                CLEAR T_COVOB_TARGET.
                T_COVOB_TARGET-OBJNR = T_IMPR_TARGET-OBJNR.
                INSERT T_COVOB_TARGET INDEX L_TABIX.
              ENDLOOP.
*          Gibt's was zu verbuchen?
              CHECK NOT T_COVOB_TARGET[] IS INITIAL.
*          In T_COVOB_TARGET steht nun f? die
*          zu ?dernden Positionen der gew?schte
*          Endzustand. Durch Setzung von
*          I_FLG_COMPUTE_DELTA = 'X' wird erreicht,
*          das genau das zur Erreichung dieses
*          Endzustandes n?ige Delta berechnet und
*          verbucht wird.
              PERFORM AIPA_BUDGPROC_INPUT_POST
                USING
                  T_COVOB_TARGET[]         " IT_COVOB
                  T_VORGA-VORGA            " I_VORGA
                  T_WRTTP-WRTTP            " I_WRTTP
                  T_TRGKZ-TRGKZ            " I_TRGKZ
                  T_VERSN-VERSN            " I_VERSN
                  T_POSIT-POSIT            " I_POSIT
                  TEXT                     " I_SGTXT
                  'X'                      " I_FLG_COMPUTE_DELTA
                  ' '.                     " I_FLG_COMMIT
*
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
*
    COMMIT WORK.
* Verteiltwerte neurechnen.
    PERFORM AIPA_RECALC_DIST
      USING
        PROGRAM                          " I_PRNAM
        ' '                              " I_POSID
        APP_YEAR                         " I_GJAHR
        'X'                              " I_FLG_NO_LEAFS
        ' '.                             " I_BUT_CALC

  ENDLOOP.

END-OF-SELECTION.


*---------------------------------------------------------------------*
*       FORM AIPA_BUDGPROC_INPUT_CREATE                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_ZIEL_IMPR                                                  *
*  -->  I_IMTP                                                        *
*  -->  I_VORGA                                                       *
*  -->  I_TRGKZ_FROM_1                                                *
*  -->  I_TRGKZ_FROM_2                                                *
*  -->  I_TRGKZ_TO                                                    *
*  -->  I_WRTTP_FROM_1                                                *
*  -->  I_WRTTP_FROM_2                                                *
*  -->  I_WRTTP_TO                                                    *
*  -->  I_POSIT_FROM                                                  *
*  -->  I_POSIT_TO                                                    *
*  -->  I_VERSN_FROM                                                  *
*  -->  I_VERSN_TO                                                    *
*  -->  IT_QUELLE_COVOB                                               *
*  -->  I_FLG_ROLLUP                                                  *
*  -->  I_FLG_ADD                                                     *
*  -->  ET_ZIEL_COVOB                                                 *
*---------------------------------------------------------------------*
FORM AIPA_BUDGPROC_INPUT_CREATE
     TABLES
               IT_ZIEL_IMPR STRUCTURE  IMPR
     USING
               I_IMTP LIKE IMTP
               I_VORGA LIKE BPGE-VORGA
               I_TRGKZ_FROM_1 LIKE BPGE-TRGKZ
               I_TRGKZ_FROM_2 TYPE C
               I_TRGKZ_TO LIKE BPGE-TRGKZ
               I_WRTTP_FROM_1 LIKE BPGE-WRTTP
               I_WRTTP_FROM_2 TYPE C
               I_WRTTP_TO LIKE BPGE-WRTTP
               I_POSIT_FROM LIKE BPGE-POSIT
               I_POSIT_TO LIKE BPGE-POSIT
               I_VERSN_FROM LIKE BPGE-VERSN
               I_VERSN_TO LIKE BPGE-VERSN
               IT_QUELLE_COVOB TYPE IM_COVOB_TAB_TYPE
               I_FLG_ROLLUP TYPE C
               I_FLG_ADD TYPE C
     CHANGING
               ET_ZIEL_COVOB TYPE IM_COVOB_TAB_TYPE.

* Ab Releasestand 4.5A entsprechenden FB benutzen.
  IF RELEASE >= '45A'.
    if original <> space.                                   " n. 558016
      data: flg_ignore_stat_locked like sy-datar.           " n. 558016
      flg_ignore_stat_locked = 'X'.                         " n. 558016
      export flg_ignore_stat_locked to memory               " n. 558016
             id 'IM_FLG_IGNORE_STAT_LOCKED'.                " n. 558016
    endif.                                                  " n. 558016
    CALL FUNCTION 'AIPA_BUDGPROC_INPUT_CREATE'
         EXPORTING
              I_IMTP          = I_IMTP
              I_VORGA         = I_VORGA
              I_TRGKZ_FROM_1  = I_TRGKZ_FROM_1
              I_TRGKZ_FROM_2  = I_TRGKZ_FROM_2
              I_TRGKZ_TO      = I_TRGKZ_TO
              I_WRTTP_FROM_1  = I_WRTTP_FROM_1
              I_WRTTP_FROM_2  = I_WRTTP_FROM_2
              I_WRTTP_TO      = I_WRTTP_TO
              I_POSIT_FROM    = I_POSIT_FROM
              I_POSIT_TO      = I_POSIT_TO
              I_VERSN_FROM    = I_VERSN_FROM
              I_VERSN_TO      = I_VERSN_TO
              IT_QUELLE_COVOB = IT_QUELLE_COVOB
              I_FLG_ROLLUP    = I_FLG_ROLLUP
              I_FLG_ADD       = I_FLG_ADD  " I_ROLLUPCORR_COVOB =
         IMPORTING
              ET_ZIEL_COVOB =
                 ET_ZIEL_COVOB " I_FLG_ADD_ON_LOCKED_LEAFS = ' '
         TABLES
              IT_ZIEL_IMPR    = IT_ZIEL_IMPR.
*
    EXIT.
  ENDIF.

* IT_ZIEL_IMPR enth?t den zu bearbeitenden Teilbaum
* (mit oder ohne den Pfad zu dem der Einstiegsposition
* ?ergeordneten Top-Position).

* Ist I_FLG_ROLLUP <> SPACE, so werden aus IT_QUELLE_COVOB
* nur S?ze zu Bl?tern ber?ksichtigt.

* Positionen mit Plan-/Budgetsperre werden wie folgt behandelt:
* o  Bei I_FLG_ROLLUP =  SPACE werden Positionen mit Plan-/Budgetsperre
*    einfach ignoriert, d.h. f? diese wird kein Satz in
*    ET_ZIEL_COVOB erzeugt.
* o  Bei I_FLG_ROLLUP <> SPACE so werden alle Nicht-Blattpositionen
*    automatisch durch Hochrollen gef?lt, auch dann, wenn
*    sie eine Plan-/Budgetsperre haben (siehe HW 70720)!
*    Blattpositionen tragen wie folgt zu ET_ZIEL_COVOB und zum
*    Hochrollen bei:
*    a) Mit den BPGE/BPJA-S?ze zu den FROM-Parametern wenn
*       die Blattposition keine Budget-/Plansperre tr?t (Normalfall).
*    b) Mit BPGE/BPJA-S?ze zu den TO-Parametern wenn
*       die Blattposition eine Budget-/Plansperre tr?t
*       und ?erschrieben wird (I_FLG_ADD = SPACE).
*       Achtung: In diesem Falle wird der Eintrag in ET_ZIEL_COVOB
*       nur f?'s Hochrollen gemerkt. Blattpositionen mit
*       Budget-/Plansperre werden in T_OBJ_STAT_LOCKED
*       gemerkt, ET_ZIEL_COVOB wird nach dem Hochrollen wieder
*       um die gesperrten Blattpositionen T_OBJ_STAT_LOCKED
*       ausged?nt.
*    c) Gar nicht, wenn
*       die Blattposition eine Budget-/Plansperre tr?t
*       und addiert wird (I_FLG_ADD = 'X').

  DATA: L_YEAR_START        LIKE BPDY-GJAHR,
        L_YEAR_BEG          LIKE BPDY-GJAHR,
        L_YEAR_ACT          LIKE BPDY-GJAHR,
        L_YEAR_END          LIKE BPDY-GJAHR,
        COVOB_WA            TYPE IM_COVOB_TYPE.

  DATA: T_OBJ_STAT_LOCKED   LIKE RAIP_OBJ   OCCURS  10 WITH HEADER LINE,
        T_OBJ               LIKE RAIP_OBJ   OCCURS  10 WITH HEADER LINE,
        T_BPJA              LIKE BPJA       OCCURS  10 WITH HEADER LINE,
        T_BPGE              LIKE BPGE       OCCURS  10 WITH HEADER LINE,
        T_BPJA_TMP          LIKE BPJA       OCCURS  10 WITH HEADER LINE,
        T_BPGE_TMP          LIKE BPGE       OCCURS  10 WITH HEADER LINE.

  DATA: T_BPCUR             LIKE BPCU       OCCURS   1 WITH HEADER LINE,
        T_TAI08             LIKE TAI08      OCCURS  10 WITH HEADER LINE.

  DATA: L_FLG_LEAFS_ONLY(1) TYPE C,
        L_SUBRC             LIKE SY-SUBRC,
        L_TABIX             LIKE SY-TABIX,
        L_OBJNR             LIKE IMZO-OBJNR,
        L_PROFIL            LIKE TAI05-PROFIL.

  DATA: BEGIN OF T_PARENTS                  OCCURS 100,
          PARNR             LIKE IMPR-PARNR,
        END   OF T_PARENTS.

  DATA: BEGIN OF T_PAROB                    OCCURS 100,
          OBJNR             LIKE IMPR-OBJNR,
          OBJNR_PAR         LIKE IMPR-OBJNR,
        END   OF T_PAROB.

  DATA: BEGIN OF T_POS                      OCCURS 100,
          POSNR             LIKE IMPR-POSNR,
          OBJNR             LIKE IMPR-OBJNR,
        END   OF T_POS.

* Ausgangssituation herstellen.
  REFRESH ET_ZIEL_COVOB.
  SORT IT_QUELLE_COVOB BY OBJNR.

* Mitgegebene POSNRs/OBJNRs merken.
  LOOP AT IT_ZIEL_IMPR.
    CLEAR T_POS.
    T_POS-POSNR = IT_ZIEL_IMPR-POSNR.
    T_POS-OBJNR = IT_ZIEL_IMPR-OBJNR.
    APPEND T_POS.
  ENDLOOP.
  SORT T_POS BY POSNR.

* Tabelle mit OBJNR und Parent-OBJNR des Zielteilbaums
* aufbauen.
  REFRESH T_PAROB.
  LOOP AT IT_ZIEL_IMPR.
    CLEAR T_PAROB.
    T_PAROB-OBJNR = IT_ZIEL_IMPR-OBJNR.
    IF NOT IT_ZIEL_IMPR-PARNR IS INITIAL.
      READ TABLE T_POS
           WITH KEY POSNR = IT_ZIEL_IMPR-PARNR
           BINARY SEARCH.
      CHECK SY-SUBRC = 0.
      T_PAROB-OBJNR_PAR = T_POS-OBJNR.
    ENDIF.
    APPEND T_PAROB.
  ENDLOOP.
  SORT T_PAROB BY OBJNR.

* Hochrollen gew?scht ...
  IF I_FLG_ROLLUP <> SPACE.
*  ... dann wird Tabelle der Parents ben?igt
    LOOP AT IT_ZIEL_IMPR
      WHERE NOT PARNR IS INITIAL.
      APPEND IT_ZIEL_IMPR-PARNR TO T_PARENTS.
    ENDLOOP.
    SORT T_PARENTS BY PARNR.
    DELETE ADJACENT DUPLICATES FROM T_PARENTS.
  ENDIF.

* Tabelle der Positionen mit Budget-/Plansperre
* bestimmen.
* Wenn hochgerollt wird, werden Budget-/Plansperren
* nur auf Blattpositionen akzeptiert.
  IF I_FLG_ROLLUP <> SPACE.
    L_FLG_LEAFS_ONLY = 'X'.
  ELSE.
    L_FLG_LEAFS_ONLY = SPACE.
  ENDIF.

  if rollup <> space.                                       " n. 558016
    PERFORM AIPA_GET_LOCKED_POSITIONS
      TABLES
        IT_ZIEL_IMPR                       " IT_IMPR
        T_OBJ_STAT_LOCKED                  " ET_OBJ_STAT_LOCKED
      USING
        I_VORGA                            " I_VORGA
        L_FLG_LEAFS_ONLY.                  " I_FLG_LEAFS_ONLY
  endif.                                                    " n. 558016
  SORT T_OBJ_STAT_LOCKED BY OBJNR.                          " n. 354459

* KOKRS lesen.
  READ TABLE IT_ZIEL_IMPR INDEX 1.
  SELECT SINGLE * FROM TKA01
    WHERE KOKRS EQ IT_ZIEL_IMPR-KOKRS.

* Programmart lesen.
  SELECT SINGLE * FROM TAI05
    WHERE PRART EQ I_IMTP-PRART.

* Richtiges Profil w?len, je nachdem
* ob Budgetierung oder Planung.
  IF     I_WRTTP_TO EQ L_WIPLAN.
    L_PROFIL = TAI05-PPROFIL.
  ELSEIF I_WRTTP_TO EQ L_WIBUDGET.
    L_PROFIL = TAI05-PROFIL.
  ENDIF.

* Budget-/Planprofil lesen.
  SELECT SINGLE * FROM TBP1C
    WHERE PROFIL  EQ L_PROFIL
    AND   APPLIK  EQ 'I'
    AND   WRTTP   EQ I_WRTTP_TO.

* Eventuell Budgetarten zur Verf?ung stellen.
  REFRESH T_TAI08.
  IF I_IMTP-CAPEX NE SPACE.
    SELECT * FROM TAI08 INTO TABLE T_TAI08
      WHERE PRART = I_IMTP-PRART
      ORDER BY PRIMARY KEY.
    IF SY-SUBRC NE 0.
      MESSAGE A100 WITH SY-REPID 'TAI08_NOT_FOUND'.
    ENDIF.
* Keine Budgetarten vorhanden ...
  ELSE.
*  ... dann Budgetart = SPACE.
    APPEND INITIAL LINE TO T_TAI08.
  ENDIF.

* Aktuelles GJ ermitteln.
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
       EXPORTING
            I_DATE  = SY-DATLO
            I_PERIV = I_IMTP-PERIV
       IMPORTING
            E_GJAHR = L_YEAR_ACT.

* Startjahr f? Budgetprozessor.
  L_YEAR_START = L_YEAR_ACT + TBP1C-Y_START.
* Beginngesch?tsjahr f? Budgetprozessor.
  L_YEAR_BEG   = L_YEAR_START - TBP1C-Y_PAST.
* Endegesch?tsjahr f? Budgetprozessor.
  L_YEAR_END   = L_YEAR_START + TBP1C-Y_FUTURE.

* Gesamtwerte gem? BP-Profil erlaubt?
  IF TBP1C-BPGE NE SPACE .

*  Position f? Position abarbeiten.
    LOOP AT IT_ZIEL_IMPR.

*    Hochrollen?
      IF I_FLG_ROLLUP <> SPACE.
*       Ist Position unter den V?ern?
        READ TABLE T_PARENTS
             TRANSPORTING NO FIELDS
             WITH KEY PARNR = IT_ZIEL_IMPR-POSNR
             BINARY SEARCH.
*       Ja ...
        IF SY-SUBRC = 0.
*          ... dann ist sie kein Blatt und mu?ignoriert
*          werden, da sie dann sp?er durch Hochrollen
*          gef?lt wird.
          CONTINUE.
        ENDIF.
      ENDIF.

*    Quellwerte in T_BPGE stellen.
      REFRESH T_BPGE.
      READ TABLE IT_QUELLE_COVOB INTO COVOB_WA
           WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
           BINARY SEARCH.
      CHECK     SY-SUBRC        =  0       AND
            NOT COVOB_WA-BPGE[] IS INITIAL .
      T_BPGE[] = COVOB_WA-BPGE[].

*    Relevante Quellwerte in T_BPGE_TMP stellen.
      REFRESH T_BPGE_TMP.

*    Vorgang erlaubt f? Programmposition?
      READ TABLE T_OBJ_STAT_LOCKED
           TRANSPORTING NO FIELDS
           WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
           BINARY SEARCH.
*    Vorgang erlaubt ...
      IF SY-SUBRC <> 0.
*       ... dann sind relevante Quellwerte = FROM-Werte.
        LOOP AT T_BPGE
          WHERE ( WRTTP EQ I_WRTTP_FROM_1 OR
                  WRTTP EQ I_WRTTP_FROM_2 )
          AND   ( TRGKZ EQ I_TRGKZ_FROM_1 OR
                  TRGKZ EQ I_TRGKZ_FROM_2 )
          AND     POSIT EQ I_POSIT_FROM   .
*         Planungstransaktion ...
          IF I_WRTTP_TO EQ L_WIPLAN.
*            ... dann Version checken.
            CHECK T_BPGE-VERSN EQ I_VERSN_FROM.
          ENDIF.
*
          CLEAR T_BPGE_TMP.

          T_BPGE_TMP       = T_BPGE.
          T_BPGE_TMP-OBJNR = IT_ZIEL_IMPR-OBJNR.
          T_BPGE_TMP-VORGA = I_VORGA.
          T_BPGE_TMP-WRTTP = I_WRTTP_TO.
          T_BPGE_TMP-POSIT = I_POSIT_TO.
          T_BPGE_TMP-TRGKZ = I_TRGKZ_TO.
          CLEAR T_BPGE_TMP-GEBER.
          IF I_WRTTP_TO EQ L_WIPLAN.
            T_BPGE_TMP-VERSN = I_VERSN_TO.
          ENDIF.
*         Felder ohne Bedeutung f? IM m?sen einheitlich
*         sein, sonst f?ren sie beim anschlie?nden
*         COLLECT zu unterschiedlichen S?zen mit gleichem
*         Key, was sp?er zum DUPREC f?ren kann!
          CLEAR: T_BPGE_TMP-KALNR,
                 T_BPGE_TMP-KLVAR,
                 T_BPGE_TMP-BELTP.
*
          T_BPGE_TMP-WLGEV = 0.
          T_BPGE_TMP-WTGEV = 0.
          COLLECT T_BPGE_TMP.
        ENDLOOP.
*    Vorgang nicht erlaubt ...
      ELSE.
*       ... und es soll hochgerollt und
*       ?erschrieben werden ...
        IF I_FLG_ROLLUP <> SPACE AND
           I_FLG_ADD    =  SPACE .
*          ... dann sind relevante Quellwerte = TO-Werte.
          LOOP AT T_BPGE
            WHERE WRTTP EQ I_WRTTP_TO
            AND   TRGKZ EQ I_TRGKZ_TO
            AND   POSIT EQ I_POSIT_TO.
*            Planungstransaktion ...
            IF I_WRTTP_TO EQ L_WIPLAN.
*               ... dann Version checken.
              CHECK T_BPGE-VERSN EQ I_VERSN_TO.
            ENDIF.
*
            CLEAR T_BPGE_TMP.
            T_BPGE_TMP       = T_BPGE.
            T_BPGE_TMP-OBJNR = IT_ZIEL_IMPR-OBJNR.
            T_BPGE_TMP-VORGA = I_VORGA.
*            Felder ohne Bedeutung f? IM m?sen einheitlich
*            sein, sonst f?ren sie beim anschlie?nden
*            COLLECT zu unterschiedlichen S?zen mit gleichem
*            Key, was sp?er zum DUPREC f?ren kann!
            CLEAR: T_BPGE_TMP-KALNR,
                   T_BPGE_TMP-KLVAR,
                   T_BPGE_TMP-BELTP.
*
            T_BPGE_TMP-WLGEV = 0.
            T_BPGE_TMP-WTGEV = 0.
            COLLECT T_BPGE_TMP.
          ENDLOOP.
        ENDIF.
      ENDIF.

*    S?ze ?ergeben, wenn welche ermittelt wurden.
      IF NOT T_BPGE_TMP[] IS INITIAL.
        READ TABLE ET_ZIEL_COVOB INTO COVOB_WA
             WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
             BINARY SEARCH.
        L_SUBRC = SY-SUBRC.
        L_TABIX = SY-TABIX.
        IF L_SUBRC = 0.
          LOOP AT T_BPGE_TMP.
            COLLECT T_BPGE_TMP INTO COVOB_WA-BPGE.
          ENDLOOP.
          MODIFY ET_ZIEL_COVOB FROM COVOB_WA INDEX L_TABIX
                               TRANSPORTING BPGE.
        ELSE.
          CLEAR COVOB_WA.
          COVOB_WA-OBJNR  = IT_ZIEL_IMPR-OBJNR.
          COVOB_WA-BPGE[] = T_BPGE_TMP[].
          INSERT COVOB_WA INTO ET_ZIEL_COVOB INDEX L_TABIX.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.


* Jahreswerte gem? BP-Profil erlaubt?
  IF TBP1C-BPJA NE SPACE .

*  Position f? Position abarbeiten.
    LOOP AT IT_ZIEL_IMPR.

*    Hochrollen?
      IF I_FLG_ROLLUP <> SPACE.
*       Ist Position unter den V?ern?
        READ TABLE T_PARENTS
             TRANSPORTING NO FIELDS
             WITH KEY PARNR = IT_ZIEL_IMPR-POSNR
             BINARY SEARCH.
*       Ja ...
        IF SY-SUBRC = 0.
*          ... dann ist sie kein Blatt und mu?ignoriert
*          werden, da sie dann sp?er durch Hochrollen
*          gef?lt wird.
          CONTINUE.
        ENDIF.
      ENDIF.

*    Quellwerte in T_BPJA stellen.
      REFRESH T_BPJA.
      READ TABLE IT_QUELLE_COVOB INTO COVOB_WA
           WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
           BINARY SEARCH.
      CHECK     SY-SUBRC        =  0       AND
            NOT COVOB_WA-BPJA[] IS INITIAL .
      T_BPJA[] = COVOB_WA-BPJA[].

*    Relevante Quellwerte in T_BPJA_TMP stellen.
      REFRESH T_BPJA_TMP.

*    Vorgang erlaubt f? Programmposition?
      READ TABLE T_OBJ_STAT_LOCKED
           TRANSPORTING NO FIELDS
           WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
           BINARY SEARCH.
*    Vorgang erlaubt ...
      IF SY-SUBRC <> 0.
*       ... dann sind relevante Quellwerte = FROM-Werte.
        LOOP AT T_BPJA
          WHERE   GJAHR BETWEEN L_YEAR_BEG AND L_YEAR_END
          AND   ( WRTTP EQ I_WRTTP_FROM_1 OR
                  WRTTP EQ I_WRTTP_FROM_2 )
          AND   ( TRGKZ EQ I_TRGKZ_FROM_1 OR
                  TRGKZ EQ I_TRGKZ_FROM_2 )
          AND     POSIT EQ I_POSIT_TO     .
*         Planungstransaktion ...
          IF I_WRTTP_TO EQ L_WIPLAN.
*            ... dann Version checken.
            CHECK T_BPJA-VERSN EQ I_VERSN_FROM.
          ENDIF.
*
          CLEAR T_BPJA_TMP.
          T_BPJA_TMP       = T_BPJA.
          T_BPJA_TMP-OBJNR = IT_ZIEL_IMPR-OBJNR.
          T_BPJA_TMP-VORGA = I_VORGA.
          T_BPJA_TMP-WRTTP = I_WRTTP_TO.
          T_BPJA_TMP-POSIT = I_POSIT_TO.
          T_BPJA_TMP-TRGKZ = I_TRGKZ_TO.
          CLEAR T_BPJA_TMP-GEBER.
          IF I_WRTTP_TO EQ L_WIPLAN.
            T_BPJA_TMP-VERSN = I_VERSN_TO.
          ENDIF.
*         Felder ohne Bedeutung f? IM m?sen einheitlich
*         sein, sonst f?ren sie beim anschlie?nden
*         COLLECT zu unterschiedlichen S?zen mit gleichem
*         Key, was sp?er zum DUPREC f?ren kann!
          CLEAR: T_BPJA_TMP-KALNR,
                 T_BPJA_TMP-KLVAR,
                 T_BPJA_TMP-SPRED,
                 T_BPJA_TMP-BELTP.
*
          T_BPJA_TMP-WLJHV = 0.
          T_BPJA_TMP-WTJHV = 0.
          COLLECT T_BPJA_TMP.
        ENDLOOP.
*    Vorgang nicht erlaubt ...
      ELSE.
*       ... und es soll hochgerollt und
*       ?erschrieben werden ...
        IF I_FLG_ROLLUP <> SPACE AND
           I_FLG_ADD    =  SPACE .
*          ... dann sind relevante Quellwerte = TO-Werte.
          LOOP AT T_BPJA
            WHERE GJAHR BETWEEN L_YEAR_BEG AND L_YEAR_END
            AND   WRTTP EQ I_WRTTP_TO
            AND   TRGKZ EQ I_TRGKZ_TO
            AND   POSIT EQ I_POSIT_TO.
*            Planungstransaktion ...
            IF I_WRTTP_TO EQ L_WIPLAN.
*               ... dann Version checken.
              CHECK T_BPJA-VERSN EQ I_VERSN_TO.
            ENDIF.
*
            CLEAR T_BPJA_TMP.
            T_BPJA_TMP       = T_BPJA.
            T_BPJA_TMP-OBJNR = IT_ZIEL_IMPR-OBJNR.
            T_BPJA_TMP-VORGA = I_VORGA.
*            Felder ohne Bedeutung f? IM m?sen einheitlich
*            sein, sonst f?ren sie beim anschlie?nden
*            COLLECT zu unterschiedlichen S?zen mit gleichem
*            Key, was sp?er zum DUPREC f?ren kann!
            CLEAR: T_BPJA_TMP-KALNR,
                   T_BPJA_TMP-KLVAR,
                   T_BPJA_TMP-SPRED,
                   T_BPJA_TMP-BELTP.
*
            T_BPJA_TMP-WLJHV = 0.
            T_BPJA_TMP-WTJHV = 0.
            COLLECT T_BPJA_TMP.
          ENDLOOP.
        ENDIF.
      ENDIF.

*    S?ze ?ergeben, wenn welche ermittelt wurden.
      IF NOT T_BPJA_TMP[] IS INITIAL.
        READ TABLE ET_ZIEL_COVOB INTO COVOB_WA
             WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
             BINARY SEARCH.
        L_SUBRC = SY-SUBRC.
        L_TABIX = SY-TABIX.
        IF L_SUBRC = 0.
          LOOP AT T_BPJA_TMP.
            COLLECT T_BPJA_TMP INTO COVOB_WA-BPJA.
          ENDLOOP.
          MODIFY ET_ZIEL_COVOB FROM COVOB_WA INDEX L_TABIX
                               TRANSPORTING BPJA.
        ELSE.
          CLEAR COVOB_WA.
          COVOB_WA-OBJNR  = IT_ZIEL_IMPR-OBJNR.
          COVOB_WA-BPJA[] = T_BPJA_TMP[].
          INSERT COVOB_WA INTO ET_ZIEL_COVOB INDEX L_TABIX.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDIF.

* Hochsummieren?
  IF I_FLG_ROLLUP NE SPACE.
    SORT ET_ZIEL_COVOB BY OBJNR.
*  Position f? Position abarbeiten und vorhandene
*  Werte auf ?erliegenden Pfad hochrollen.
    LOOP AT IT_ZIEL_IMPR.
      READ TABLE T_PARENTS                                  " n. 133491
           TRANSPORTING NO FIELDS                           " n. 133491
           WITH KEY PARNR = IT_ZIEL_IMPR-POSNR              " n. 133491
           BINARY SEARCH.                                   " n. 133491
      CHECK SY-SUBRC <> 0.                                  " n. 133491
*    Zielwerte besorgen.
      READ TABLE ET_ZIEL_COVOB INTO COVOB_WA
           WITH KEY OBJNR = IT_ZIEL_IMPR-OBJNR
           BINARY SEARCH.
      CHECK SY-SUBRC = 0.
      T_BPGE[] = COVOB_WA-BPGE[].
      T_BPJA[] = COVOB_WA-BPJA[].
*    In T_OBJ Pfad (echt) oberhalb bestimmen.
      REFRESH T_OBJ.
      L_OBJNR = IT_ZIEL_IMPR-OBJNR.
      DO.
        READ TABLE T_PAROB
             WITH KEY OBJNR = L_OBJNR
             BINARY SEARCH.
        IF     SY-SUBRC          =  0       AND
           NOT T_PAROB-OBJNR_PAR IS INITIAL .
          APPEND T_PAROB-OBJNR_PAR TO T_OBJ.
          L_OBJNR = T_PAROB-OBJNR_PAR.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
*    Pfad (echt) oberhalb abturnen und hochrollen.
      CHECK NOT T_OBJ[] IS INITIAL.
      LOOP AT T_OBJ.
        READ TABLE ET_ZIEL_COVOB INTO COVOB_WA
             WITH KEY OBJNR = T_OBJ-OBJNR
             BINARY SEARCH.
        L_SUBRC = SY-SUBRC.
        L_TABIX = SY-TABIX.
        IF L_SUBRC <> 0.
          CLEAR COVOB_WA.
          COVOB_WA-OBJNR = T_OBJ-OBJNR.
        ENDIF.
        LOOP AT T_BPGE.
          T_BPGE-OBJNR = T_OBJ-OBJNR.
          COLLECT T_BPGE INTO COVOB_WA-BPGE.
        ENDLOOP.
        LOOP AT T_BPJA.
          T_BPJA-OBJNR = T_OBJ-OBJNR.
          COLLECT T_BPJA INTO COVOB_WA-BPJA.
        ENDLOOP.
        IF L_SUBRC = 0.
          MODIFY ET_ZIEL_COVOB FROM COVOB_WA INDEX L_TABIX.
        ELSE.
          INSERT COVOB_WA INTO ET_ZIEL_COVOB INDEX L_TABIX.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

* Hochsummieren und ?erschreiben ...
  IF I_FLG_ROLLUP <> SPACE AND
     I_FLG_ADD    =  SPACE .
*  ... dann ET_ZIEL_COVOB wieder um S?ze zu Blattpositionen
*  mit Budget-/Plansperre ausd?nen, da diese nur zum
*  Hochrollen ben?igt wurden, jedoch nicht upgedatet werden
*  sollen.
    LOOP AT ET_ZIEL_COVOB INTO COVOB_WA.
      READ TABLE T_OBJ_STAT_LOCKED
           TRANSPORTING NO FIELDS
           WITH KEY OBJNR = COVOB_WA-OBJNR
           BINARY SEARCH.
      IF SY-SUBRC = 0.
        DELETE ET_ZIEL_COVOB.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM AIPA_BUDGPROC_INPUT_POST                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_COVOB                                                      *
*  -->  I_VORGA                                                       *
*  -->  I_WRTTP                                                       *
*  -->  I_TRGKZ                                                       *
*  -->  I_VERSN                                                       *
*  -->  I_POSIT                                                       *
*  -->  I_SGTXT                                                       *
*  -->  I_FLG_COMPUTE_DELTA                                           *
*  -->  I_FLG_COMMIT                                                  *
*---------------------------------------------------------------------*
FORM AIPA_BUDGPROC_INPUT_POST
     USING
               IT_COVOB TYPE IM_COVOB_TAB_TYPE
               I_VORGA LIKE BPGE-VORGA
               I_WRTTP LIKE BPGE-WRTTP
               I_TRGKZ LIKE BPGE-TRGKZ
               I_VERSN LIKE BPGE-VERSN
               I_POSIT LIKE BPGE-POSIT
               I_SGTXT LIKE BPIN-SGTEXT
               I_FLG_COMPUTE_DELTA LIKE SY-DATAR
               I_FLG_COMMIT LIKE IMPR-XAKTB.

  DATA: L_COVOB    TYPE IM_COVOB_TYPE,
        L_BPIN     LIKE BPIN,
        T_OBJ      LIKE RAIP_OBJ OCCURS 100 WITH HEADER LINE,
        T_BPGE     LIKE BPGE OCCURS 100 WITH HEADER LINE,
        T_BPGE_DB  LIKE BPGE OCCURS 100 WITH HEADER LINE,
        T_BPGE_ID  LIKE BPGE_ID OCCURS 100 WITH HEADER LINE,
        T_BPJA     LIKE BPJA OCCURS 100 WITH HEADER LINE,
        T_BPJA_DB  LIKE BPJA OCCURS 100 WITH HEADER LINE,
        T_BPJA_ID  LIKE BPJA_ID OCCURS 100 WITH HEADER LINE,
        T_BPTR     LIKE BPTR OCCURS 100 WITH HEADER LINE,
        T_BPTR_DB  LIKE BPTR OCCURS 100 WITH HEADER LINE,
        T_BPTR_ID  LIKE BPTR_ID OCCURS 100 WITH HEADER LINE.

  DATA: BEGIN OF T_DUMMY OCCURS 1,
          DUMMYFIELD,
        END   OF T_DUMMY.

  RANGES: L_RG_VERSN FOR BPGE-VERSN.

* Ab Releasestand 4.5A entsprechenden FB benutzen.
  IF RELEASE >= '45A'.
    CALL FUNCTION 'AIPA_BUDGPROC_INPUT_POST'
         EXPORTING
              IT_COVOB            = IT_COVOB
              I_VORGA             = I_VORGA
              I_WRTTP             = I_WRTTP
              I_TRGKZ             = I_TRGKZ
              I_VERSN             = I_VERSN
              I_POSIT             = I_POSIT
              I_SGTXT             = I_SGTXT
              I_FLG_COMPUTE_DELTA = I_FLG_COMPUTE_DELTA
              I_FLG_COMMIT        = I_FLG_COMMIT.
*
    EXIT.
  ENDIF.

  IF I_WRTTP = L_WIPLAN.
    CLEAR L_RG_VERSN.
    L_RG_VERSN-SIGN   = 'I'.
    L_RG_VERSN-OPTION = 'EQ'.
    L_RG_VERSN-LOW    = I_VERSN.
    APPEND L_RG_VERSN.
  ENDIF.

* Unn?ige S?ze wegwerfen.
  LOOP AT IT_COVOB INTO L_COVOB.
    DELETE L_COVOB-BPGE
      WHERE NOT ( WRTTP =  I_WRTTP    AND
                  VORGA =  I_VORGA    AND
                  TRGKZ =  I_TRGKZ    AND
                  POSIT =  I_POSIT    AND
                  VERSN IN L_RG_VERSN ).
    DELETE L_COVOB-BPJA
      WHERE NOT ( WRTTP =  I_WRTTP    AND
                  VORGA =  I_VORGA    AND
                  TRGKZ =  I_TRGKZ    AND
                  POSIT =  I_POSIT    AND
                  VERSN IN L_RG_VERSN ).
    MODIFY IT_COVOB FROM L_COVOB.
  ENDLOOP.

* Alle beteiligten Objektnummern feststellen.
* Alle beteiligten BPTR-S?ze feststellen.
  LOOP AT IT_COVOB INTO L_COVOB.
    LOOP AT L_COVOB-BPGE INTO T_BPGE.
      COLLECT T_BPGE.
      T_OBJ-OBJNR = T_BPGE-OBJNR. COLLECT T_OBJ.
      CLEAR T_BPTR.
      MOVE-CORRESPONDING T_BPGE TO T_BPTR.
      COLLECT T_BPTR.
    ENDLOOP.
    LOOP AT L_COVOB-BPJA INTO T_BPJA.
      COLLECT T_BPJA.
      T_OBJ-OBJNR = T_BPJA-OBJNR. COLLECT T_OBJ.
      CLEAR T_BPTR.
      MOVE-CORRESPONDING T_BPJA TO T_BPTR.
      COLLECT T_BPTR.
    ENDLOOP.
  ENDLOOP.

* Wenn das Delta erst noch berechnet werden soll, mu?
* der DB-Stand aller Objektnummern in IT_COVOB eingelesen
* werden, um S?ze von Objektnummern auf der DB auf 0
* zu bringen, f? die IT_COVOB-BPGE/BPJA initial.
  IF I_FLG_COMPUTE_DELTA <> SPACE.
    LOOP AT IT_COVOB INTO L_COVOB.
      T_OBJ-OBJNR = L_COVOB-OBJNR. COLLECT T_OBJ.
    ENDLOOP.
  ENDIF.

* BPGE/BPJA und BPTR im Zustand
* vor dem Update von der DB lesen
* und nach Primary Key sortiert vorhalten.
  REFRESH: T_BPGE_DB, T_BPJA_DB, T_BPTR_DB.
  SELECT * FROM BPGE INTO TABLE T_BPGE_DB
    FOR ALL ENTRIES IN T_OBJ
    WHERE OBJNR =  T_OBJ-OBJNR
    AND   WRTTP =  I_WRTTP
    AND   VORGA =  I_VORGA
    AND   TRGKZ =  I_TRGKZ
    AND   POSIT =  I_POSIT
    AND   VERSN IN L_RG_VERSN
    AND   LEDNR = '0001'
    ORDER BY PRIMARY KEY.
  SELECT * FROM BPJA INTO TABLE T_BPJA_DB
    FOR ALL ENTRIES IN T_OBJ
    WHERE OBJNR = T_OBJ-OBJNR
    AND   WRTTP =  I_WRTTP
    AND   VORGA =  I_VORGA
    AND   TRGKZ =  I_TRGKZ
    AND   POSIT =  I_POSIT
    AND   VERSN IN L_RG_VERSN
    ORDER BY PRIMARY KEY.
  SELECT * FROM BPTR INTO TABLE T_BPTR_DB
    FOR ALL ENTRIES IN T_OBJ
    WHERE OBJNR = T_OBJ-OBJNR
    AND   WRTTP =  I_WRTTP
    AND   TRGKZ =  I_TRGKZ
    AND   POSIT =  I_POSIT
    AND   VERSN IN L_RG_VERSN
    ORDER BY PRIMARY KEY.

* Update-S?ze bestimmen.
  PERFORM AIPA_COMPUTE_UPDATE_BP
    TABLES
      T_BPGE_DB                          " IT_BPGE_DB
      T_BPGE                             " IT_BPGE
      T_BPJA_DB                          " IT_BPJA_DB
      T_BPJA                             " IT_BPJA
      T_BPTR_DB                          " IT_BPTR_DB
      T_BPTR                             " IT_BPTR
      T_BPGE_ID                          " ET_BPGE_ID
      T_BPJA_ID                          " ET_BPJA_ID
      T_BPTR_ID                          " ET_BPTR_ID
    USING
      I_FLG_COMPUTE_DELTA.               " I_FLG_COMPUTE_DELTA

* ?erhaupt was zu tun?
  CHECK NOT T_BPGE_ID[] IS INITIAL OR
        NOT T_BPJA_ID[] IS INITIAL OR
        NOT T_BPTR_ID[] IS INITIAL .

* Verbuchung vorbereiten.
  CLEAR L_BPIN.
  CASE I_VORGA.
    WHEN L_VPLAN.
      L_BPIN-TCODE = 'IM35'.
    WHEN L_VBUD.
      L_BPIN-TCODE = 'IM32'.
    WHEN L_VBR0.
      L_BPIN-TCODE = 'IM38'.
    WHEN L_VBN0.
      L_BPIN-TCODE = 'IM30'.
  ENDCASE.
  L_BPIN-DELTA = 'X'.
  L_BPIN-EPOS  = 'X'.

  CALL FUNCTION 'KBPS_INIT'
       EXPORTING
            BP_IMPORT = L_BPIN
       IMPORTING
            BP_IN     = L_BPIN
       EXCEPTIONS
            OTHERS    = 1.

  L_BPIN-SGTEXT = I_SGTXT.

* Verbuchung.
  CALL FUNCTION 'KBPV_POST_DATA'
       EXPORTING
            DELTA_UPDATE  = 'X'
            DIALOG_UPDATE = 'X'  " Nicht in Update-Task!
            IM_BPIN       = L_BPIN
       TABLES
            TAB_BPCH      = T_DUMMY
            TAB_BPGE      = T_BPGE_ID
            TAB_BPHI      = T_DUMMY
            TAB_BPIG      = T_DUMMY
            TAB_BPIJ      = T_DUMMY
            TAB_BPJA      = T_BPJA_ID
            TAB_BPPE      = T_DUMMY
            TAB_BPTR      = T_BPTR_ID
       EXCEPTIONS
            OTHERS        = 1.

* COMMIT wenn gew?scht.
  IF I_FLG_COMMIT <> SPACE.
    COMMIT WORK.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM AIPA_RECALC_DIST                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  I_PRNAM                                                       *
*  -->  I_POSID                                                       *
*  -->  I_GJAHR                                                       *
*  -->  I_FLG_NO_LEAFS                                                *
*  -->  I_BUT_CALC                                                    *
*---------------------------------------------------------------------*
FORM AIPA_RECALC_DIST
     USING
               I_PRNAM LIKE IMTP-PRNAM
               I_POSID LIKE IMPR-POSID
               I_GJAHR LIKE IMTP-GJAHR
               I_FLG_NO_LEAFS LIKE IMPR-XAKTB
               I_BUT_CALC LIKE IMPR-POSID.

  SUBMIT RAIMDNEW AND RETURN
          WITH BUT_CALC = I_BUT_CALC
          WITH NO_LEAFS = I_FLG_NO_LEAFS
          WITH PA_GJAHR = I_GJAHR
          WITH PA_POSID = I_POSID
          WITH PA_PRNAM = I_PRNAM.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM AIPA_GET_LOCKED_POSITIONS                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_IMPR                                                       *
*  -->  ET_OBJ_STAT_LOCKED                                            *
*  -->  I_VORGA                                                       *
*  -->  I_FLG_LEAFS_ONLY                                              *
*---------------------------------------------------------------------*
FORM AIPA_GET_LOCKED_POSITIONS
     TABLES
               IT_IMPR STRUCTURE IMPR
               ET_OBJ_STAT_LOCKED STRUCTURE  RAIP_OBJ
     USING
               I_VORGA LIKE BPGE-VORGA
               I_FLG_LEAFS_ONLY TYPE C.

* Ab Releasestand 4.5A entsprechenden FB benutzen.
  IF RELEASE >= '45A'.
    CALL FUNCTION 'AIPA_GET_LOCKED_POSITIONS'
         EXPORTING
              I_VORGA            = I_VORGA
              I_FLG_LEAFS_ONLY   = I_FLG_LEAFS_ONLY
         TABLES
              IT_IMPR            = IT_IMPR
              ET_OBJ_STAT_LOCKED = ET_OBJ_STAT_LOCKED.
*
    EXIT.
  ENDIF.

* IT_IMPR enth?t den zu bearbeitenden Teilbaum
* (mit oder ohne den Pfad zu dem der Einstiegsposition
* ?ergeordneten Top-Position).

  DATA:  L_NOT_ALLOWED(1) TYPE C.

  TYPES: BEGIN OF L_PARENTS_TYPE,
           PARNR          LIKE IMPR-PARNR,
         END   OF L_PARENTS_TYPE.

  DATA:  BEGIN OF T_PARENTS OCCURS 100,
           PARNR LIKE IMPR-PARNR,
         END   OF T_PARENTS.
  DATA:  BEGIN OF T_OBJ OCCURS 100,
           OBJNR LIKE IMPR-OBJNR,
         END   OF T_OBJ.

* Ausgangssituation herstellen.
  REFRESH ET_OBJ_STAT_LOCKED.

* Wenn nur gesperrte Blattpositionen bestimmt werden
* sollen: Tabelle der Parents bilden.
  IF I_FLG_LEAFS_ONLY <> SPACE.
    LOOP AT IT_IMPR
      WHERE NOT PARNR IS INITIAL.
      APPEND IT_IMPR-PARNR TO T_PARENTS.
    ENDLOOP.
    SORT T_PARENTS BY PARNR.
    DELETE ADJACENT DUPLICATES FROM T_PARENTS.
  ENDIF.

* Statusse puffern.
  LOOP AT IT_IMPR.
    CLEAR T_OBJ.
    T_OBJ-OBJNR = IT_IMPR-OBJNR.
    APPEND T_OBJ.
  ENDLOOP.
  CALL FUNCTION 'STATUS_PRE_READ'
       TABLES
            JSTO_PRE_TAB = T_OBJ.

* Position f? Position abarbeiten.
  LOOP AT IT_IMPR.

* Sollen nur Blattpositionen bestimmt werden?
    IF I_FLG_LEAFS_ONLY <> SPACE.
*    Ist Position unter den V?ern?
      READ TABLE T_PARENTS
           TRANSPORTING NO FIELDS
           WITH KEY PARNR = IT_IMPR-POSNR
           BINARY SEARCH.
*    Ja ...
      IF SY-SUBRC = 0.
*       ... dann ist sie kein Blatt und soll
*       ignoriert werden.
        CONTINUE.
      ENDIF.
    ENDIF.

* Vorgang erlaubt f? Programmposition?
    L_NOT_ALLOWED = SPACE.
    CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
         EXPORTING
              CHECK_ONLY           = 'X'
              OBJNR                = IT_IMPR-OBJNR
              VRGNG                = I_VORGA
         IMPORTING
              ACTIVITY_NOT_ALLOWED = L_NOT_ALLOWED
         EXCEPTIONS
              OTHERS               = 4.
* Vorgang erlaubt ...
    IF SY-SUBRC      = 0     AND
       L_NOT_ALLOWED = SPACE .
*       ... dann nichts tun.
      " DO NOTHING.
* Vorgang nicht erlaubt ...
    ELSE.
*    Blattposition merken, auf der
*    Budget-/Plansperre sitzt.
      APPEND IT_IMPR-OBJNR TO ET_OBJ_STAT_LOCKED.
    ENDIF.

  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM AIPA_COMPUTE_UPDATE_BP                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_BPGE_DB                                                    *
*  -->  IT_BPGE                                                       *
*  -->  IT_BPJA_DB                                                    *
*  -->  IT_BPJA                                                       *
*  -->  IT_BPTR_DB                                                    *
*  -->  IT_BPTR                                                       *
*  -->  ET_BPGE_ID                                                    *
*  -->  ET_BPJA_ID                                                    *
*  -->  ET_BPTR_ID                                                    *
*  -->  I_FLG_COMPUTE_DELTA                                           *
*---------------------------------------------------------------------*
FORM AIPA_COMPUTE_UPDATE_BP
     TABLES
                IT_BPGE_DB STRUCTURE BPGE
                IT_BPGE STRUCTURE BPGE
                IT_BPJA_DB STRUCTURE BPJA
                IT_BPJA STRUCTURE BPJA
                IT_BPTR_DB STRUCTURE BPTR
                IT_BPTR STRUCTURE BPTR
                ET_BPGE_ID STRUCTURE BPGE_ID
                ET_BPJA_ID STRUCTURE BPJA_ID
                ET_BPTR_ID STRUCTURE BPTR_ID
     USING
                I_FLG_COMPUTE_DELTA LIKE SY-DATAR.

* Ab Releasestand 4.5A entsprechenden FB benutzen.
  IF RELEASE >= '45A'.
    CALL FUNCTION 'AIPA_COMPUTE_UPDATE_BP'
         EXPORTING
              I_FLG_COMPUTE_DELTA = I_FLG_COMPUTE_DELTA
         TABLES
              IT_BPGE_DB          = IT_BPGE_DB
              IT_BPGE             = IT_BPGE
              IT_BPJA_DB          = IT_BPJA_DB
              IT_BPJA             = IT_BPJA
              IT_BPTR_DB          = IT_BPTR_DB
              IT_BPTR             = IT_BPTR
              ET_BPGE_ID          = ET_BPGE_ID
              ET_BPJA_ID          = ET_BPJA_ID
              ET_BPTR_ID          = ET_BPTR_ID.
*
    EXIT.
  ENDIF.

* In IT_BPGE_DB, IT_BPJA_DB, IT_BPTR_DB wird der
* Stand auf der DB ?ergeben.
* In IT_BPGE, IT_BPJA, IT_BPTR wird ?ergeben
* -  wenn I_FLG_COMPUTE_DELTA = ' '
*    der Stand, der auf den DB-Stand dazu kommt
*    (Delta mu?nicht eigens berechnet werden),
* -  wenn I_FLG_COMPUTE_DELTA = 'X'
*    der Stand, der nach dem Sichern auf
*    der DB stehen soll
*    (Delta soll erst noch berechnet werden).
* In den Tabellen ET_BPGE_ID, ET_BPJA_ID, ET_BPTR_ID
* werden in jedem Falle Delta-S?ze zur?kgegeben
* mit den richtigen Update-Kennzeichen (Feld ID).

* Nur einen bestimmten Vorgang bearbeiten?
* IF NOT I_VORGA IS INITIAL.
*    DELETE IT_BPGE
*      WHERE VORGA <> I_VORGA.
*    DELETE IT_BPJA
*      WHERE VORGA <> I_VORGA.
*    DELETE IT_BPGE_DB
*      WHERE VORGA <> I_VORGA.
*    DELETE IT_BPJA_DB
*      WHERE VORGA <> I_VORGA.
* ENDIF.

  REFRESH: ET_BPGE_ID, ET_BPJA_ID, ET_BPTR_ID.

* Was soll an BPGE-S?zen neu hinzukommen?
  SORT IT_BPGE_DB BY LEDNR
                     OBJNR
                     POSIT
                     TRGKZ
                     WRTTP
                     GEBER
                     VERSN
                     VORGA
                     TWAER.
  LOOP AT IT_BPGE.
    CLEAR ET_BPGE_ID.
    ET_BPGE_ID = IT_BPGE.
    READ TABLE IT_BPGE_DB
         WITH KEY LEDNR = IT_BPGE-LEDNR
                  OBJNR = IT_BPGE-OBJNR
                  POSIT = IT_BPGE-POSIT
                  TRGKZ = IT_BPGE-TRGKZ
                  WRTTP = IT_BPGE-WRTTP
                  GEBER = IT_BPGE-GEBER
                  VERSN = IT_BPGE-VERSN
                  VORGA = IT_BPGE-VORGA
                  TWAER = IT_BPGE-TWAER
         BINARY SEARCH.
    IF SY-SUBRC = 0.
      ET_BPGE_ID-ID = 'U'.
*      Delta bilden?
      IF I_FLG_COMPUTE_DELTA <> SPACE.
        ET_BPGE_ID-WLGES = ET_BPGE_ID-WLGES - IT_BPGE_DB-WLGES.
        ET_BPGE_ID-WTGES = ET_BPGE_ID-WTGES - IT_BPGE_DB-WTGES.
      ENDIF.
    ELSE.
      ET_BPGE_ID-ID = 'I'.
    ENDIF.
    ET_BPGE_ID-EPOS = 'X'.
    APPEND ET_BPGE_ID.
  ENDLOOP.

* Delta berechnen ...
  IF I_FLG_COMPUTE_DELTA <> SPACE.
*    ... dann auch eventuell auf der DB
*    vorhandene Werte auf 0 bringen, wenn
*    sie nach dem Update 0 sein sollen.
    SORT IT_BPGE BY LEDNR
                    OBJNR
                    POSIT
                    TRGKZ
                    WRTTP
                    GEBER
                    VERSN
                    VORGA
                    TWAER.
    LOOP AT IT_BPGE_DB.
      READ TABLE IT_BPGE
           WITH KEY LEDNR = IT_BPGE_DB-LEDNR
                    OBJNR = IT_BPGE_DB-OBJNR
                    POSIT = IT_BPGE_DB-POSIT
                    TRGKZ = IT_BPGE_DB-TRGKZ
                    WRTTP = IT_BPGE_DB-WRTTP
                    GEBER = IT_BPGE_DB-GEBER
                    VERSN = IT_BPGE_DB-VERSN
                    VORGA = IT_BPGE_DB-VORGA
                    TWAER = IT_BPGE_DB-TWAER
           BINARY SEARCH.
      CHECK SY-SUBRC <> 0.
      CLEAR ET_BPGE_ID.
      ET_BPGE_ID       = IT_BPGE_DB.
      ET_BPGE_ID-WLGES = 0 - IT_BPGE_DB-WLGES.
      ET_BPGE_ID-WTGES = 0 - IT_BPGE_DB-WTGES.
      ET_BPGE_ID-ID    = 'U'.
      ET_BPGE_ID-EPOS  = 'X'.
      APPEND ET_BPGE_ID.
    ENDLOOP.
  ENDIF.

* Was soll an BPJA-S?zen neu hinzukommen?
  SORT IT_BPJA_DB BY LEDNR
                     OBJNR
                     POSIT
                     TRGKZ
                     WRTTP
                     GJAHR
                     GEBER
                     VERSN
                     VORGA
                     TWAER.
  LOOP AT IT_BPJA.
    CLEAR ET_BPJA_ID.
    ET_BPJA_ID = IT_BPJA.
    READ TABLE IT_BPJA_DB
         WITH KEY LEDNR = IT_BPJA-LEDNR
                  OBJNR = IT_BPJA-OBJNR
                  POSIT = IT_BPJA-POSIT
                  TRGKZ = IT_BPJA-TRGKZ
                  WRTTP = IT_BPJA-WRTTP
                  GJAHR = IT_BPJA-GJAHR
                  GEBER = IT_BPJA-GEBER
                  VERSN = IT_BPJA-VERSN
                  VORGA = IT_BPJA-VORGA
                  TWAER = IT_BPJA-TWAER
         BINARY SEARCH.
    IF SY-SUBRC = 0.
      ET_BPJA_ID-ID = 'U'.
*      Delta bilden?
      IF I_FLG_COMPUTE_DELTA <> SPACE.
        ET_BPJA_ID-WLJHR = ET_BPJA_ID-WLJHR - IT_BPJA_DB-WLJHR.
        ET_BPJA_ID-WTJHR = ET_BPJA_ID-WTJHR - IT_BPJA_DB-WTJHR.
      ENDIF.
    ELSE.
      ET_BPJA_ID-ID = 'I'.
    ENDIF.
    ET_BPJA_ID-EPOS  = 'X'.
    APPEND ET_BPJA_ID.
  ENDLOOP.

* Delta berechnen ...
  IF I_FLG_COMPUTE_DELTA <> SPACE.
*    ... dann auch eventuell auf der DB
*    vorhandene Werte auf 0 bringen, wenn
*    sie nach dem Update 0 sein sollen.
    SORT IT_BPJA BY LEDNR
                    OBJNR
                    POSIT
                    TRGKZ
                    WRTTP
                    GJAHR
                    GEBER
                    VERSN
                    VORGA
                    TWAER.
    LOOP AT IT_BPJA_DB.
      READ TABLE IT_BPJA
           WITH KEY LEDNR = IT_BPJA_DB-LEDNR
                    OBJNR = IT_BPJA_DB-OBJNR
                    POSIT = IT_BPJA_DB-POSIT
                    TRGKZ = IT_BPJA_DB-TRGKZ
                    WRTTP = IT_BPJA_DB-WRTTP
                    GJAHR = IT_BPJA_DB-GJAHR
                    GEBER = IT_BPJA_DB-GEBER
                    VERSN = IT_BPJA_DB-VERSN
                    VORGA = IT_BPJA_DB-VORGA
                    TWAER = IT_BPJA_DB-TWAER
           BINARY SEARCH.
      CHECK SY-SUBRC <> 0.
      CLEAR ET_BPJA_ID.
      ET_BPJA_ID       = IT_BPJA_DB.
      ET_BPJA_ID-WLJHR = 0 - IT_BPJA_DB-WLJHR.
      ET_BPJA_ID-WTJHR = 0 - IT_BPJA_DB-WTJHR.
      ET_BPJA_ID-ID    = 'U'.
      ET_BPJA_ID-EPOS  = 'X'.
      APPEND ET_BPJA_ID.
    ENDLOOP.
  ENDIF.

* Was soll an BPTR-S?zen neu hinzukommen?
  SORT IT_BPTR_DB BY OBJNR
                     POSIT
                     TRGKZ
                     WRTTP
                     GEBER
                     VERSN.
  LOOP AT IT_BPTR.
    CLEAR ET_BPTR_ID.
    ET_BPTR_ID = IT_BPTR.
    CLEAR: ET_BPTR_ID-ERNAM,
           ET_BPTR_ID-ERDAT,
           ET_BPTR_ID-AENAM,
           ET_BPTR_ID-AEDAT.
    READ TABLE IT_BPTR_DB
         TRANSPORTING NO FIELDS
         WITH KEY OBJNR = IT_BPTR-OBJNR
                  POSIT = IT_BPTR-POSIT
                  TRGKZ = IT_BPTR-TRGKZ
                  WRTTP = IT_BPTR-WRTTP
                  GEBER = IT_BPTR-GEBER
                  VERSN = IT_BPTR-VERSN
         BINARY SEARCH.
    IF SY-SUBRC = 0.
      ET_BPTR_ID-AENAM = SY-UNAME.
      ET_BPTR_ID-AEDAT = SY-DATLO.
      ET_BPTR_ID-ID    = 'U'.
    ELSE.
      ET_BPTR_ID-ERNAM = SY-UNAME.
      ET_BPTR_ID-ERDAT = SY-DATLO.
      ET_BPTR_ID-AENAM = SY-UNAME.
      ET_BPTR_ID-AEDAT = SY-DATLO.
      ET_BPTR_ID-ID    = 'I'.
    ENDIF.
    APPEND ET_BPTR_ID.
  ENDLOOP.

  DELETE ET_BPGE_ID
    WHERE WLGES = 0
    AND   WTGES = 0
    AND   WLGEV = 0
    AND   WTGEV = 0.

  DELETE ET_BPJA_ID
    WHERE WLJHR = 0
    AND   WTJHR = 0
    AND   WLJHV = 0
    AND   WTJHV = 0.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM DETERMINE_VAL_CARRYING_SUBTREE                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DETERMINE_VAL_CARRYING_SUBTREE.

  DATA: L_POSNR LIKE IMPR-POSNR.

  DATA: BEGIN OF T_HIER OCCURS 10,
          POSNR LIKE IMPR-POSNR,
          PARNR LIKE IMPR-PARNR,
        END   OF T_HIER.

  DATA: BEGIN OF T_VALUE OCCURS 10,
          POSIT LIKE BPJA-POSIT,
          VORGA LIKE BPJA-VORGA,
          VERSN LIKE BPJA-VERSN,
          GJAHR LIKE BPJA-GJAHR,
          WLJHR LIKE BPJA-WLJHR,
        END   OF T_VALUE.

  DATA: BEGIN OF T_POS_VALUE_CARRIER OCCURS 10,
          POSNR LIKE IMPR-POSNR,
        END   OF T_POS_VALUE_CARRIER.

  DATA: BEGIN OF T_POS_REMAINING OCCURS 10,
          POSNR LIKE IMPR-POSNR,
        END   OF T_POS_REMAINING.

* Alle Parent-Beziehungen ermitteln.
  LOOP AT T_IMPR
    WHERE NOT PARNR IS INITIAL.
    CLEAR T_HIER.
    T_HIER-POSNR = T_IMPR-POSNR.
    T_HIER-PARNR = T_IMPR-PARNR.
    APPEND T_HIER.
  ENDLOOP.
  SORT T_HIER BY POSNR.

* Positionen abloopen und zun?hst bestimmen,
* ob Position wertetragend oder nicht.
  LOOP AT T_IMPR.
*   Werte je Vorgang, Gesch?tsjahr, Version und
*   Budgetart feststellen.
    REFRESH T_VALUE.
    FREE    T_VALUE.
*   Gesamtwerte (Gesch?tsjahr '0000').
    READ TABLE T_BPGE
         TRANSPORTING NO FIELDS
         WITH KEY OBJNR = T_IMPR-OBJNR
         BINARY SEARCH.
    L_SUBRC = SY-SUBRC.
    L_TABIX = SY-TABIX.
    IF L_SUBRC = 0.
      LOOP AT T_BPGE
        FROM L_TABIX.
        IF T_BPGE-OBJNR <> T_IMPR-OBJNR.
          EXIT.
        ENDIF.
        CLEAR T_VALUE.
        T_VALUE-POSIT = T_BPGE-POSIT.
        T_VALUE-VORGA = T_BPGE-VORGA.
        T_VALUE-VERSN = T_BPGE-VERSN.
        T_VALUE-GJAHR = '0000'.
        T_VALUE-WLJHR = T_BPGE-WLGES.
        COLLECT T_VALUE.
      ENDLOOP.
    ENDIF.
*   Jahreswerte.
    READ TABLE T_BPJA
         TRANSPORTING NO FIELDS
         WITH KEY OBJNR = T_IMPR-OBJNR
         BINARY SEARCH.
    L_SUBRC = SY-SUBRC.
    L_TABIX = SY-TABIX.
    IF L_SUBRC = 0.
      LOOP AT T_BPJA
        FROM L_TABIX.
        IF T_BPJA-OBJNR <> T_IMPR-OBJNR.
          EXIT.
        ENDIF.
        CLEAR T_VALUE.
        T_VALUE-POSIT = T_BPJA-POSIT.
        T_VALUE-VORGA = T_BPJA-VORGA.
        T_VALUE-VERSN = T_BPJA-VERSN.
        T_VALUE-GJAHR = T_BPJA-GJAHR.
        T_VALUE-WLJHR = T_BPJA-WLJHR.
        COLLECT T_VALUE.
      ENDLOOP.
    ENDIF.
*   Ist Position wertetragend?
    LOOP AT T_VALUE
      WHERE WLJHR <> 0.
      EXIT.
    ENDLOOP.
*   Ja, ...
    IF SY-SUBRC = 0.
*      ... dann merken.
      APPEND T_IMPR-POSNR TO T_POS_VALUE_CARRIER.
    ENDIF.
  ENDLOOP.
  SORT T_POS_VALUE_CARRIER BY POSNR.

* Alle Positionen abloopen.
  LOOP AT T_IMPR.
*   Ist Position wertetragend?
    READ TABLE T_POS_VALUE_CARRIER
         TRANSPORTING NO FIELDS
         WITH KEY POSNR = T_IMPR-POSNR
         BINARY SEARCH.
*   Ja, ...
    CHECK SY-SUBRC = 0.
*   ... dann verbleiben alle Positionen ihres
*   Pfades in der Hierarchie.
    L_POSNR = T_IMPR-POSNR.
    DO.
      READ TABLE T_POS_REMAINING
           TRANSPORTING NO FIELDS
           WITH KEY POSNR = L_POSNR
           BINARY SEARCH.
      L_SUBRC = SY-SUBRC.
      L_TABIX = SY-TABIX.
      IF L_SUBRC <> 0.
        INSERT L_POSNR INTO T_POS_REMAINING
               INDEX L_TABIX.
      ENDIF.
      READ TABLE T_HIER
           WITH KEY POSNR = L_POSNR
           BINARY SEARCH.
      IF SY-SUBRC = 0.
        L_POSNR = T_HIER-PARNR.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

* Jetzt T_IMPR um die Positionen ausd?nen, die
* nicht in der Hierarchie verbleiben sollen.
  LOOP AT T_IMPR.
    READ TABLE T_POS_REMAINING
         TRANSPORTING NO FIELDS
         WITH KEY POSNR = T_IMPR-POSNR
         BINARY SEARCH.
    IF SY-SUBRC <> 0.
      DELETE T_IMPR.
    ENDIF.
  ENDLOOP.

ENDFORM.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*
