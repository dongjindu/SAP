* ZKEVRK2E
* Vorlage f? RK2Exxxx
* Generierung in ZKEREO31

REPORT RK2E%ERK
    MESSAGE-ID KF
    LINE-SIZE 254 .

***********************************************************************
*                                                                     *
*  Template program to rebuild CO-PA segment level from line items.   *
*                                                                     *
*  Sequence number 57 released Sep 23, 1999                           *
*  Suitable for R/3 systems of releases 4.X only.                     *
*  If you implement this source code in a 3.X system you will         *
*  encounter syntax errors.                                           *
*                                                                     *
***********************************************************************

*---------------------------------------------------------------------
*
*                 constant data
*
*---------------------------------------------------------------------

DATA: G_C_PLIKZ_IST   LIKE CEST1-PLIKZ     VALUE '0',
      G_C_PLIKZ_PLAN  LIKE CEST1-PLIKZ     VALUE '1',

      G_C_FILE_READ                        VALUE 'R',
      G_C_FILE_WRITE                       VALUE 'W',

      G_C_CE1_NAME    LIKE DD03P-TABNAME   VALUE 'CE1%ERK',
      G_C_CE2_NAME    LIKE DD03P-TABNAME   VALUE 'CE2%ERK',
      G_C_CE3_NAME    LIKE DD03P-TABNAME   VALUE 'CE3%ERK',
      G_C_CE4_NAME    LIKE DD03P-TABNAME   VALUE 'CE4%ERK'.

*---------------------------------------------------------------------
*
*                 parameter data
*
*---------------------------------------------------------------------

* relevante Parameter von RKEREO31.
DATA: BEGIN OF G
    ,   ERKRS LIKE TKEBB-ERKRS
    ,   GJAHR LIKE CEST1-GJAHR
    ,   PLIKZ LIKE CEST3-PLIKZ
    ,   VRGAR LIKE CEST3-VRGAR
    ,   TEMPFILE(80) TYPE C
    ,   RAMAVAIL TYPE I
    ,   VERBOSE
    ,   SINGLDEL
    ,   DONTWRIT
    ,   PERBL_LOW  LIKE CEST3-PERBL VALUE 'xxxx000'
    ,   PERBL_HIGH LIKE CEST3-PERBL VALUE 'xxxx999'
    ,   PERIO_LOW  LIKE CEST1-PERIO VALUE 'xxxx000'
    ,   PERIO_HIGH LIKE CEST1-PERIO VALUE 'xxxx999'
    ,   SUBRC LIKE SY-SUBRC
    ,   REC_WAERS LIKE CEST0-REC_WAERS
    , END OF G
    .
RANGES: G-R_VRGAR FOR CEST3-VRGAR.

*---------------------------------------------------------------------
*
*                 global data
*
*---------------------------------------------------------------------

TABLES: CE1%ERK,                       " Ist-Einzelposten
        CE2%ERK,                       " Plan-Einzelposten
        CE3%ERK, *CE3%ERK,             " Objektebene
        CE4%ERK.                       " Objekttabelle

* Definition von STAT
INCLUDE RKEREOI1.

* Pufferbereiche f? Treffermengen und aufgesammeltes f? Insert
* bzw. Update.

DATA: G_C_TABSIZ TYPE I VALUE 100.

DATA: BEGIN OF G_TH_CE1 OCCURS 100.
        INCLUDE STRUCTURE CE1%ERK.
DATA: END OF G_TH_CE1.

DATA: BEGIN OF G_TH_CE2 OCCURS 100.
        INCLUDE STRUCTURE CE2%ERK.
DATA: END OF G_TH_CE2.

DATA: BEGIN OF G_TH_CE3 OCCURS 100.
        INCLUDE STRUCTURE CE3%ERK.
DATA: END OF G_TH_CE3.

* Zeitmessung
DATA: BEGIN OF G_STOPW
    ,   START_TIME TYPE T
    ,   START_DATE TYPE D
    ,   STOP_TIME TYPE T
    ,   STOP_DATE TYPE D
    ,   ANZAHL TYPE I
    , END OF G_STOPW
    .

*-----------------------------------------------------------------------
*
*            Neuaufbau der Objektebene aus den Einzelposten.
*
*-----------------------------------------------------------------------

FORM BUILD_CE3_FROM_LINE_ITEMS
      USING VALUE(I_GJAHR)
            VALUE(I_PLIKZ)
            VALUE(I_VRGAR)
            VALUE(I_TEMPFILE)
            VALUE(I_RAMAVAIL)
            VALUE(I_VERBOSE)
            VALUE(I_SINGLDEL)
            VALUE(I_DONTWRIT)
            X_STAT STRUCTURE STAT
*           Testhilfen !
            I_XXSELCE1
            I_XXDELCE3
            I_XXINSCE3
            E_RC
            .

  DATA: L_SUBRC LIKE SY-SUBRC
      , L_LINE_ITEM_COUNTER TYPE I
      , L_OBJ_LEV_COUNTER TYPE I
      , L_WRITE_CE3_FILE TYPE I
      , L_WRITE_CE3_DB TYPE I
      .

  PERFORM DUMP_TIME USING '> build_ce3_from_line_items'.

  STAT = X_STAT.
  G-SUBRC = 0.

  G-ERKRS         = '%ERK'.
  G-GJAHR         = I_GJAHR.
  G-PLIKZ         = I_PLIKZ.
  G-VRGAR         = I_VRGAR.
  G-TEMPFILE      = I_TEMPFILE.
  G-RAMAVAIL      = I_RAMAVAIL.

  G-VERBOSE       = I_VERBOSE.
  G-SINGLDEL      = I_SINGLDEL.
  G-DONTWRIT      = I_DONTWRIT.

  G-PERBL_LOW(4)  = G-GJAHR.
  G-PERBL_HIGH(4) = G-GJAHR.
  G-PERIO_LOW(4)  = G-GJAHR.
  G-PERIO_HIGH(4) = G-GJAHR.

* begin dh990809
  DATA: LL_ERKRS LIKE V_TKEBL-ERKRS VALUE '%ERK'
      , LL_PALEDGER LIKE V_TKEBL-PALEDGER VALUE '01'
      , LL_BUKRS LIKE T001-BUKRS VALUE '    '
      .
  CALL FUNCTION 'RKE_GET_REC_WAERS'
       EXPORTING
            I_ERKRS     = LL_ERKRS
            I_PALEDGER  = LL_PALEDGER
            I_BUKRS     = LL_BUKRS
       IMPORTING
            E_REC_WAERS = G-REC_WAERS
       EXCEPTIONS
            FAILED      = 1
            OTHERS      = 2
            .
* end dh990809

* globale ranges f? Vorgamgsart besetzen
  CLEAR: G-R_VRGAR.
  REFRESH: G-R_VRGAR.
  IF NOT G-VRGAR IS INITIAL.
    G-R_VRGAR-SIGN = 'I'.
    G-R_VRGAR-OPTION = 'EQ'.
    G-R_VRGAR-LOW = G-VRGAR.
    APPEND G-R_VRGAR.
  ENDIF.

* Einzelposten entladen
  IF I_XXSELCE1 = 'X'.
    PERFORM FILE_OPEN USING G_C_FILE_WRITE L_SUBRC.
    IF L_SUBRC = 0.
      IF G-PLIKZ = G_C_PLIKZ_IST.
        PERFORM EXTRACT_CE1
              USING L_LINE_ITEM_COUNTER
                    L_OBJ_LEV_COUNTER
                    L_SUBRC.
      ELSEIF G-PLIKZ = G_C_PLIKZ_PLAN.
        PERFORM EXTRACT_CE2
              USING L_LINE_ITEM_COUNTER
                    L_OBJ_LEV_COUNTER
                    L_SUBRC.
      ENDIF.
      L_WRITE_CE3_FILE = L_OBJ_LEV_COUNTER.
      PERFORM FILE_CLOSE.
    ENDIF.                                   " file_openw erfolreich
  ENDIF.                                     " xxselce1

* Objektebene aufbauen
  IF L_SUBRC = 0.
    IF I_XXDELCE3 = 'X'.
      PERFORM DELETE_CE3 USING L_SUBRC.
    ENDIF.
    IF L_SUBRC = 0.
      IF I_XXINSCE3 = 'X'.
        PERFORM FILE_OPEN USING G_C_FILE_READ L_SUBRC.
        IF L_SUBRC = 0.
          PERFORM IMPORT_CE3 USING L_OBJ_LEV_COUNTER L_SUBRC.
          L_WRITE_CE3_DB = L_OBJ_LEV_COUNTER.
        ENDIF.                               " file_openr erfolgreich
        PERFORM FILE_CLOSE.
      ENDIF.                                 " xxinsce3
    ENDIF.                                   " delete_ce3 erfolgreich
  ENDIF.                                     " entladen erfolgreich


  IF ( L_WRITE_CE3_FILE NE L_WRITE_CE3_DB ).
*   AND ( L_WRITE_CE3_FILE > 0 ) AND ( L_WRITE_CE3_DB > 0 ).
    WRITE: / '   Export/Impoert mismatch'.
    WRITE: / '   Operation unsuccessful ?'.
    G-SUBRC = 1.
  ENDIF.

  X_STAT = STAT.
  E_RC = G-SUBRC.

  PERFORM DUMP_TIME USING '< build_ce3_from_line_items'.

ENDFORM.

*---------------------------------------------------------------------
*
*     Lesen Ist-Einzelposten
*
*     Steuerlogik: Verwaltung der globalen Tabellen
*                  Gruppenwechsel
*     Die aufgerufenen Forms loopen immer nur die G_TH_CE* ab.
*
*---------------------------------------------------------------------

FORM EXTRACT_CE1
      USING E_LINE_ITEM_COUNTER
            E_OBJ_LEV_COUNTER
            E_RC.

  DATA: L_PAOBJNR LIKE CEST4-PAOBJNR
      , L_TMP_INTEGER TYPE I
      .

  PERFORM DUMP_TIME USING '> extract_ce1'.
  PERFORM STOPW_START.

  CLEAR: E_RC
       , E_LINE_ITEM_COUNTER, E_OBJ_LEV_COUNTER
       , G_TH_CE1, G_TH_CE3
       .
  REFRESH: G_TH_CE1, G_TH_CE3.

  SELECT *
        FROM CE1%ERK
        CLIENT SPECIFIED
        WHERE PERIO BETWEEN G-PERIO_LOW AND G-PERIO_HIGH
          AND VRGAR IN G-R_VRGAR
        ORDER BY MANDT PAOBJNR .

    CHECK CE1%ERK-MANDT = SY-MANDT.

*   Gruppenwechsel
    IF CE1%ERK-PAOBJNR NE L_PAOBJNR.

*     leere Gruppe zu Anfang rausnehmen
      IF NOT ( L_PAOBJNR IS INITIAL ).

        DESCRIBE TABLE G_TH_CE1 LINES L_TMP_INTEGER.
        IF L_TMP_INTEGER > 0.
          ADD L_TMP_INTEGER TO E_LINE_ITEM_COUNTER.
          PERFORM PROCESS_CE1_PAOBJNR.
        ENDIF.

        DESCRIBE TABLE G_TH_CE3 LINES L_TMP_INTEGER.
        IF L_TMP_INTEGER > 0.
          ADD L_TMP_INTEGER TO E_OBJ_LEV_COUNTER.
          PERFORM FILE_WRITE.
        ENDIF.

        CLEAR: G_TH_CE1, G_TH_CE3.
        REFRESH: G_TH_CE1, G_TH_CE3.

      ENDIF. " leere Gruppe

      L_PAOBJNR = CE1%ERK-PAOBJNR.

    ENDIF. " Gruppenwechsel

    G_TH_CE1 = CE1%ERK.
    APPEND G_TH_CE1.

  ENDSELECT. " CE1%ERK

* Schlu?ruppe

  DESCRIBE TABLE G_TH_CE1 LINES L_TMP_INTEGER.
  IF L_TMP_INTEGER > 0.
    ADD L_TMP_INTEGER TO E_LINE_ITEM_COUNTER.
    PERFORM PROCESS_CE1_PAOBJNR.
  ENDIF.

  DESCRIBE TABLE G_TH_CE3 LINES L_TMP_INTEGER.
  IF L_TMP_INTEGER > 0.
    ADD L_TMP_INTEGER TO E_OBJ_LEV_COUNTER.
    PERFORM FILE_WRITE.
  ENDIF.

  IF G-VERBOSE = 'X'.
    WRITE: / '   sy-dbcnt                        ', SY-DBCNT.
   WRITE: / '   line items read (CE1%ERK)       ', E_LINE_ITEM_COUNTER.
    WRITE: / '   datafile lines written (CE3%ERK)', E_OBJ_LEV_COUNTER.
  ENDIF.

  STAT-CE1 = E_LINE_ITEM_COUNTER.
  STAT-CE3 = E_OBJ_LEV_COUNTER.

  CALL FUNCTION 'DB_COMMIT'.

  PERFORM STOPW_STOP USING E_LINE_ITEM_COUNTER 100000 '100'''.
  PERFORM DUMP_TIME USING '< extract_ce1'.

ENDFORM. " extract_ce1


*--------------------------------------------
*
*     Verarbeiten von G_TH_CE1 f? eine Objektnummer
*     und Collecten in G_TH_CE3.
*
*--------------------------------------------

FORM PROCESS_CE1_PAOBJNR.

DATA: PERBL LIKE CEST3-PERBL
    , POS   LIKE CEST3-PERBL
    .

  LOOP AT G_TH_CE1.

    CALL FUNCTION 'RKE_CONVERT_LOGNR_TO_PHYSNR'
          EXPORTING
            ERKRS  = G-ERKRS
            LEDGER = G_TH_CE1-PALEDGER
            PERIO  = G_TH_CE1-PERIO
            PERIO2 = G_TH_CE1-ALTPERIO
          IMPORTING
            PERBL  = PERBL
            POS    = POS.

    CLEAR G_TH_CE3.

    CALL FUNCTION 'RKE_TRANSFORM_STRUCTURE'
         EXPORTING
              ERKRS          = G-ERKRS
              FROM_AREA      = G_TH_CE1
              FROM_TABNAME   = G_C_CE1_NAME
              POS            = POS
              TO_AREA_IMPORT = G_TH_CE3
              TO_TABNAME     = G_C_CE3_NAME
         IMPORTING
              TO_AREA_EXPORT = G_TH_CE3.

    G_TH_CE3-PLIKZ           = G_C_PLIKZ_IST.
    G_TH_CE3-TIMESTMP        = 0.
    G_TH_CE3-UPDAT           = SY-DATUM.
    G_TH_CE3-USNAM           = 'RKEREO31'.

*   begin dh990809
    IF G_TH_CE3-REC_WAERS IS INITIAL.
      G_TH_CE3-REC_WAERS = G-REC_WAERS.
    ENDIF.
*   end dh990809

    COLLECT G_TH_CE3.

  ENDLOOP.

ENDFORM. " process_ce1_paobjnr


*--------------------------------------------
*
*     Konsistenzcheck f? Einzelposten.
*     Operiert auf Work-Area von CE1%ERK und beschr?kt sich
*     zun?hst auf's Z?len von "schlechten" Feldinhalten.
*
*--------------------------------------------

FORM CHECK_CE1_CONSISTENT.

  IF CE1%ERK-PALEDGER NE '01'.                         " Ledger = 01
    ADD 1 TO STAT-ALERT_PALEDGER.
  ENDIF.
  IF CE1%ERK-PLIKZ NE G_C_PLIKZ_IST.                   " Plikz = 0
*   CE1%ERK-PLIKZ = G_C_PLIKZ_IST.
    ADD 1 TO STAT-ALERT_PLIKZ.
  ENDIF.
  IF CE1%ERK-PASUBNR NE '0001'.                        " Subnr = 1
*   CE1%ERK-PASUBNR = '0001'.
    ADD 1 TO STAT-ALERT_PASUBNR.
  ENDIF.
  IF NOT ( CE1%ERK-PAPAOBJNR IS INITIAL ) .            " PAPA = 0
*   CLEAR CE1%ERK-PAPAOBJNR.
    ADD 1 TO STAT-ALERT_PAPAOBJNR.
  ENDIF.
  IF NOT ( CE1%ERK-PAPASUBNR IS INITIAL ) .            " PAPAsub= 0
*   CLEAR CE1%ERK-PAPASUBNR.
    ADD 1 TO STAT-ALERT_PAPASUBNR.
  ENDIF.
  IF NOT ( CE1%ERK-HRKFT IS INITIAL ) .                " Herkunft = Sp
    ADD 1 TO STAT-ALERT_HRKFT.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------
*
*     Lesen Plan-Einzelposten
*
*     Steuerlogik: Verwaltung der globalen Tabellen
*                  Gruppenwechsel
*     Die aufgerufenen Forms loopen immer nur die G_TH_CE* ab.
*
*---------------------------------------------------------------------

FORM EXTRACT_CE2
      USING E_LINE_ITEM_COUNTER
            E_OBJ_LEV_COUNTER
            E_RC.

  DATA: L_PAOBJNR LIKE CEST4-PAOBJNR
      , L_TMP_INTEGER TYPE I
      .

  PERFORM DUMP_TIME USING '> extract_ce2'.
  PERFORM STOPW_START.

  CLEAR: E_RC
       , E_LINE_ITEM_COUNTER, E_OBJ_LEV_COUNTER
       , G_TH_CE2, G_TH_CE3
       .
  REFRESH: G_TH_CE2, G_TH_CE3.

  SELECT *
        FROM CE2%ERK
        CLIENT SPECIFIED
        WHERE PERBL BETWEEN G-PERBL_LOW AND G-PERBL_HIGH
          AND VRGAR IN G-R_VRGAR
        ORDER BY MANDT PAOBJNR .

    CHECK CE2%ERK-MANDT = SY-MANDT.

*   Gruppenwechsel
    IF CE2%ERK-PAOBJNR NE L_PAOBJNR.

*     leere Gruppe zu Anfang rausnehmen
      IF NOT ( L_PAOBJNR IS INITIAL ).

        DESCRIBE TABLE G_TH_CE2 LINES L_TMP_INTEGER.
        IF L_TMP_INTEGER > 0.
          ADD L_TMP_INTEGER TO E_LINE_ITEM_COUNTER.
          PERFORM PROCESS_CE2_PAOBJNR.
        ENDIF.

        DESCRIBE TABLE G_TH_CE3 LINES L_TMP_INTEGER.
        IF L_TMP_INTEGER > 0.
          ADD L_TMP_INTEGER TO E_OBJ_LEV_COUNTER.
          PERFORM FILE_WRITE.
        ENDIF.

        CLEAR: G_TH_CE2, G_TH_CE3.
        REFRESH: G_TH_CE2, G_TH_CE3.

      ENDIF. " leere Gruppe

      L_PAOBJNR = CE2%ERK-PAOBJNR.

    ENDIF. " Gruppenwechsel

    G_TH_CE2 = CE2%ERK.
    APPEND G_TH_CE2.

  ENDSELECT. " CE2%ERK

* Schlu?ruppe

  DESCRIBE TABLE G_TH_CE2 LINES L_TMP_INTEGER.
  IF L_TMP_INTEGER > 0.
    ADD L_TMP_INTEGER TO E_LINE_ITEM_COUNTER.
    PERFORM PROCESS_CE2_PAOBJNR.
  ENDIF.

  DESCRIBE TABLE G_TH_CE3 LINES L_TMP_INTEGER.
  IF L_TMP_INTEGER > 0.
    ADD L_TMP_INTEGER TO E_OBJ_LEV_COUNTER.
    PERFORM FILE_WRITE.
  ENDIF.

  IF G-VERBOSE = 'X'.
    WRITE: / '   sy-dbcnt                        ', SY-DBCNT.
   WRITE: / '   line items read (CE2%ERK)       ', E_LINE_ITEM_COUNTER.
    WRITE: / '   datafile lines written (CE3%ERK)', E_OBJ_LEV_COUNTER.
  ENDIF.

  STAT-CE1 = E_LINE_ITEM_COUNTER.
  STAT-CE3 = E_OBJ_LEV_COUNTER.

  CALL FUNCTION 'DB_COMMIT'.

  PERFORM STOPW_STOP USING E_LINE_ITEM_COUNTER 100000 '100'''.
  PERFORM DUMP_TIME USING '< extract_ce2'.

ENDFORM. " extract_ce2


*--------------------------------------------
*
*     Verarbeiten von G_TH_CE2 f? eine Objektnummer
*     und Collecten in G_TH_CE3.
*
*--------------------------------------------

FORM PROCESS_CE2_PAOBJNR.

  LOOP AT G_TH_CE2.

    CLEAR G_TH_CE3.

    CALL FUNCTION 'RKE_TRANSFORM_STRUCTURE'
         EXPORTING
              ERKRS          = G-ERKRS
              FROM_AREA      = G_TH_CE2
              FROM_TABNAME   = G_C_CE2_NAME
              TO_AREA_IMPORT = G_TH_CE3
              TO_TABNAME     = G_C_CE3_NAME
         IMPORTING
              TO_AREA_EXPORT = G_TH_CE3.

    G_TH_CE3-PLIKZ           = G_C_PLIKZ_PLAN.
    G_TH_CE3-TIMESTMP        = 0.
    G_TH_CE3-UPDAT           = SY-DATUM.
    G_TH_CE3-USNAM           = 'RKEREO31'.

*   begin dh990817
    IF G_TH_CE3-REC_WAERS IS INITIAL.
      G_TH_CE3-REC_WAERS = G-REC_WAERS.
    ENDIF.
*   end dh990817

    COLLECT G_TH_CE3.

  ENDLOOP.

ENDFORM. " process_ce2_paobjnr


*--------------------------------------------
*
*     Konsistenzcheck f? Einzelposten.
*     Operiert auf Work-Area von CE2%ERK und beschr?kt sich
*     zun?hst auf's Z?len von "schlechten" Feldinhalten.
*
*--------------------------------------------

FORM CHECK_CE2_CONSISTENT.

  IF CE2%ERK-PALEDGER NE '01'.                         " Ledger = 01
    ADD 1 TO STAT-ALERT_PALEDGER.
  ENDIF.

* kein PLIKZ in CE2.

  IF CE2%ERK-PASUBNR NE '0001'.                        " Subnr = 1
*   CE2%ERK-PASUBNR = '0001'.
    ADD 1 TO STAT-ALERT_PASUBNR.
  ENDIF.
  IF NOT ( CE2%ERK-PAPAOBJNR IS INITIAL ) .            " PAPA = 0
*   CLEAR CE2%ERK-PAPAOBJNR.
    ADD 1 TO STAT-ALERT_PAPAOBJNR.
  ENDIF.
  IF NOT ( CE2%ERK-PAPASUBNR IS INITIAL ) .            " PAPAsub= 0
*   CLEAR CE2%ERK-PAPASUBNR.
    ADD 1 TO STAT-ALERT_PAPASUBNR.
  ENDIF.
  IF NOT ( CE2%ERK-HRKFT IS INITIAL ) .                " Herkunft = Sp
    ADD 1 TO STAT-ALERT_HRKFT.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------
*
*                       L?chen CE3
*
*---------------------------------------------------------------------

FORM DELETE_CE3
      USING E_RC.

  DATA: L_DEL_RECS TYPE I.

  PERFORM DUMP_TIME USING '> delete_ce3'.
  PERFORM STOPW_START.

  CLEAR E_RC.

  IF G-DONTWRIT = ' '.

    DELETE FROM CE3%ERK
        WHERE
          PLIKZ   = G-PLIKZ            AND
          PERBL   >= G-PERBL_LOW       AND
          PERBL   <= G-PERBL_HIGH      AND
          VRGAR   IN G-R_VRGAR.

    E_RC = 0.                              " sy-subrc = 4 -> notfound
    L_DEL_RECS = SY-DBCNT.
    ADD L_DEL_RECS TO STAT-DEL_CE3.

    CALL FUNCTION 'DB_COMMIT'.

  ELSE.

    SELECT COUNT(*) INTO L_DEL_RECS FROM CE3%ERK
        WHERE
          PLIKZ   = G-PLIKZ            AND
          PERBL   >= G-PERBL_LOW       AND
          PERBL   <= G-PERBL_HIGH      AND
          VRGAR   IN G-R_VRGAR.
    ADD L_DEL_RECS TO STAT-DEL_CE3.

  ENDIF. " testmode

  IF G-VERBOSE = 'X'.
    WRITE: / '   database records deleted (CE3%ERK)', L_DEL_RECS.
  ENDIF.

  PERFORM STOPW_STOP USING L_DEL_RECS 100000 '100'''.
  PERFORM DUMP_TIME USING '< delete_ce3'.

ENDFORM. " delete_ce3


*---------------------------------------------------------------------
*
*     Schreiben Objektebene aus datafile
*
*---------------------------------------------------------------------

FORM IMPORT_CE3
      USING E_OBJ_LEV_COUNTER
            E_RC.

  DATA: L_MAX_LINES TYPE I
      , L_INS_LINES TYPE I
      , L_SUM_DBCNT TYPE I
      , L_INS_ERRORS TYPE I
      , L_INS_OPS TYPE I
      , L_EOF
      , L_MAX_LINES_0 TYPE I VALUE 20000
      .

  PERFORM DUMP_TIME USING '> import_ce3'.
  PERFORM STOPW_START.

  DO.

    CLEAR G_TH_CE3.
    REFRESH G_TH_CE3.
    L_MAX_LINES = L_MAX_LINES_0.
    PERFORM FILE_READ USING L_MAX_LINES L_EOF E_RC.

    IF E_RC = 0 AND L_MAX_LINES > 0.
      ADD L_MAX_LINES TO L_INS_LINES.
      ADD 1 TO L_INS_OPS.
      IF G-DONTWRIT EQ ' '.
        INSERT CE3%ERK FROM TABLE G_TH_CE3.
        ADD SY-DBCNT TO L_SUM_DBCNT.
        IF SY-SUBRC NE 0.
          ADD 1 TO L_INS_ERRORS.
          G-SUBRC = 2.
        ENDIF.
        CALL FUNCTION 'DB_COMMIT'.
        IF G-VERBOSE = 'X'.
          IF SY-DBCNT <> L_MAX_LINES.
            WRITE: / '   insert CE3%ERK:'
                 , SY-DBCNT, 'vs', L_MAX_LINES.
          ENDIF.                                " cnt mismatch
        ENDIF.                                  " verbose

      ELSE.
        DATA: LL_S_CE3 LIKE LINE OF G_TH_CE3
            , LL_TABIX LIKE SY-TABIX.
        SORT G_TH_CE3.
        LOOP AT G_TH_CE3.
          LL_TABIX = SY-TABIX.
          IF     G_TH_CE3-MANDT     = LL_S_CE3-MANDT
             AND G_TH_CE3-PAOBJNR   = LL_S_CE3-PAOBJNR
             AND G_TH_CE3-PALEDGER  = LL_S_CE3-PALEDGER
             AND G_TH_CE3-VRGAR     = LL_S_CE3-VRGAR
             AND G_TH_CE3-PLIKZ     = LL_S_CE3-PLIKZ
             AND G_TH_CE3-VERSI     = LL_S_CE3-VERSI
             AND G_TH_CE3-PERBL     = LL_S_CE3-PERBL
             AND G_TH_CE3-PASUBNR   = LL_S_CE3-PASUBNR
             AND G_TH_CE3-PAPAOBJNR = LL_S_CE3-PAPAOBJNR
             AND G_TH_CE3-PAPASUBNR = LL_S_CE3-PAPASUBNR
             AND G_TH_CE3-HRKFT     = LL_S_CE3-HRKFT.
            WRITE: / 'DUPREC in package',  L_INS_OPS
                 ,   'line', LL_TABIX.
            IF SY-BATCH = ' '.
              BREAK-POINT.
            ENDIF.
          ENDIF.
          LL_S_CE3 = G_TH_CE3.
        ENDLOOP.
      ENDIF.                                    " testmode
    ENDIF.                                      " valid read done

    IF L_EOF = 'X'.
      EXIT.
    ENDIF.

  ENDDO.

  E_OBJ_LEV_COUNTER = L_INS_LINES.

  IF G-VERBOSE = 'X'.
    WRITE: / '   datafile lines read (CE3%ERK)     ', L_INS_LINES.
   WRITE: / '   database records written (CE3%ERK)', E_OBJ_LEV_COUNTER.
  ENDIF.

  STAT-CE3 = E_OBJ_LEV_COUNTER.
  STAT-CE3_INS = L_INS_OPS.
  STAT-CE3_INS_ERR = L_INS_ERRORS.

  PERFORM STOPW_STOP USING E_OBJ_LEV_COUNTER 100000 '100'''.
  PERFORM DUMP_TIME USING '< import_ce3'.

ENDFORM.


*---------------------------------------------------------------------
*
*                       Dateioperationen
*
*---------------------------------------------------------------------

* Aller Datei-I/O passiert in CE3-Struktur !
* Als Kommunikationsmedium wird die G_TH_CE3 verwendet.
* Als Pfad wird immer G-TEMPFILE verwendet.

FORM FILE_OPEN
      USING VALUE(I_MODE)        " R / W
            E_RC.                " 0=ok, 4=err

  DATA: L_MSG(100).

  E_RC = 0.
  IF I_MODE = G_C_FILE_WRITE.
    OPEN DATASET G-TEMPFILE
          FOR OUTPUT
          IN BINARY MODE
          MESSAGE L_MSG.
    IF SY-SUBRC NE 0.
      E_RC = 4.
      WRITE: / '   error: open for write', G-TEMPFILE.
      Write: / '  ', L_MSG.
      G-SUBRC = 3.
    ENDIF.
  ELSEIF I_MODE = G_C_FILE_READ.
    OPEN DATASET G-TEMPFILE
          FOR INPUT
          IN BINARY MODE
          MESSAGE L_MSG.
    IF SY-SUBRC NE 0.
      E_RC = 4.
      WRITE: / '   error: open for read', G-TEMPFILE.
      Write: / '     ', L_MSG.
      G-SUBRC = 4.
    ENDIF.
  ENDIF.

ENDFORM. " file_open

*--------------------------------------------

FORM FILE_CLOSE.

  CLOSE DATASET G-TEMPFILE.

ENDFORM. " file_close

*--------------------------------------------

FORM FILE_WRITE.

  LOOP AT G_TH_CE3.
    TRANSFER G_TH_CE3 TO G-TEMPFILE.
  ENDLOOP.

ENDFORM. " file_write.

*--------------------------------------------

FORM FILE_READ
      USING X_MAX_LINES
            E_EOF                " SPACE=kein EOF, X=EOF
            E_RC.                " 0=ok, >0=error

  DATA: L_CNT TYPE I.

  CLEAR: E_EOF, E_RC.
  WHILE L_CNT < X_MAX_LINES.
    READ DATASET G-TEMPFILE INTO G_TH_CE3.
    IF SY-SUBRC EQ 0.
      APPEND G_TH_CE3.
      ADD 1 TO L_CNT.
    ELSEIF SY-SUBRC = 4. " EOF
      E_EOF = 'X'.
      EXIT.
    ELSEIF SY-SUBRC = 8. " error at open
      E_EOF = 'X'.
      E_RC = 8.
      WRITE: / '   error reading', G-TEMPFILE.
      G-SUBRC = 5.
      EXIT.
    ENDIF.
  ENDWHILE.
  X_MAX_LINES = L_CNT.

ENDFORM. " file_read



*-----------------------------------------------------------------------
*
*                       String mit Zeitangabe ausdrucken
*
*-----------------------------------------------------------------------

FORM DUMP_TIME
      USING VALUE(ZKETTE).

  GET TIME.
  WRITE: / SY-DATUM, SY-UZEIT, ZKETTE.

ENDFORM.



*-----------------------------------------------------------------------
*
*            Durchsatzmessung
*
*-----------------------------------------------------------------------

FORM STOPW_START.
  GET TIME.
  G_STOPW-START_TIME = SY-UZEIT.
  G_STOPW-START_DATE = SY-DATUM.
ENDFORM.

FORM STOPW_STOP
      USING ANZ BEZUG BLA.

DATA: F TYPE F
    , T TYPE T
    , D(1) TYPE N
    .

  GET TIME.
  G_STOPW-STOP_TIME = SY-UZEIT.
  G_STOPW-STOP_DATE = SY-DATUM.
  G_STOPW-ANZAHL = ANZ.

  IF G_STOPW-STOP_DATE > G_STOPW-START_DATE.
    F = ( G_STOPW-STOP_DATE - G_STOPW-START_DATE - 1 ) * 86400.
    F = F + ( 86400 - G_STOPW-START_TIME ).
    F = F + G_STOPW-STOP_TIME.
  ELSE.
    F = G_STOPW-STOP_TIME - G_STOPW-START_TIME.
  ENDIF.
  IF ANZ > 0.
    F = F / G_STOPW-ANZAHL * BEZUG.
  ENDIF.
  D = 0.
  WHILE F > 86400.
    D = D + 1.
    F = F - 86400.
  ENDWHILE.
  T = F.
  WRITE: / '     '.
  IF D > 0.
    WRITE: D NO-GAP, 'd'.
  ENDIF.
  WRITE: (11) T USING EDIT MASK '__h __m __s'.
  IF ANZ > 0.
    WRITE: '/', BLA.
  ENDIF.
ENDFORM.
