*----------------------------------------------------------------------*
*   INCLUDE RAKOPL_CALL_ALV                                            *
*----------------------------------------------------------------------*
   DATA: LS_SEL_INFO TYPE SLIS_SEL_HIDE_ALV.                "n.202306

   G_REPID = SY-REPID.
   PERFORM INIT_FIELDCAT USING GT_FIELDCAT[].
   PERFORM LAYOUT_INIT USING GS_LAYOUT.
   PERFORM SELINFO_INIT1 CHANGING LS_SEL_INFO.              "n.202306

**  Überschriften für die AfA und AHK-Wertfelder
*   PERFORM CREATE_HEADINGS USING HEADING_TYPE HEADING_GJAHR ANZ_PERI
*                                 GT_FIELDCAT[].

*  Varianten
   PERFORM VARIANT_INIT USING P_VARI.
   PERFORM INIT_GT_EXCLUDING USING GT_EXCLUDING.

   GT_EVENTS-NAME = 'TOP_OF_PAGE'.
   GT_EVENTS-FORM = 'TOP_OF_PAGE'.
   APPEND GT_EVENTS.
   GT_EVENTS-NAME = 'END_OF_LIST'.
   GT_EVENTS-FORM = 'ERROR_PROTOCOL'.
   APPEND GT_EVENTS.

*  Wenn Summenbericht muß outtab_sum nach outtab
   IF PA_SUMMB = 'X'.
     LOOP AT GT_OUTTAB_SUM.
       CLEAR GT_OUTTAB.
       MOVE-CORRESPONDING GT_OUTTAB_SUM TO GT_OUTTAB.
       APPEND GT_OUTTAB.
     ENDLOOP.
   ENDIF.

   IF NOT GT_OUTTAB[] IS INITIAL.
     CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
          EXPORTING
               I_CALLBACK_PROGRAM       = G_REPID
               I_CALLBACK_PF_STATUS_SET = G_STATUS_SET
               I_CALLBACK_USER_COMMAND  = G_USER_COMMAND
               IS_LAYOUT                = GS_LAYOUT
               IT_FIELDCAT              = GT_FIELDCAT[]
               IT_EXCLUDING             = GT_EXCLUDING
               IS_SEL_HIDE              = LS_SEL_INFO
               I_SAVE                   = 'A'
               I_DEFAULT                = 'X'
               IT_EVENTS                = GT_EVENTS[]
               IS_VARIANT               = G_VARIANT
          TABLES
               T_OUTTAB                 = GT_OUTTAB.
   ELSE.
     MESSAGE S020(AB).
   ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SELINFO_INIT1
*&---------------------------------------------------------------------*
   FORM SELINFO_INIT1 CHANGING SEL_INFO TYPE SLIS_SEL_HIDE_ALV.

     DATA: LS_ENTRIES      TYPE SLIS_SELENTRY_HIDE_ALV,
           LT_DFIES        TYPE STANDARD TABLE OF DFIES,
           LS_DFIES        TYPE DFIES,
           BEGIN OF LT_FIELDS OCCURS 15,
             SELNAME       LIKE RSPARAMS-SELNAME,
             VALUE         LIKE RSPARAMS-LOW,
             TABNAME       LIKE DCOBJDEF-NAME,
             FIELDNAME     TYPE FIELDNAME,
           END   OF LT_FIELDS.

     LT_FIELDS-SELNAME   = 'PA_XANLG'.         "Anlagen selektieren
     LT_FIELDS-VALUE     =  PA_XANLG.
     LT_FIELDS-FIELDNAME = 'XANLG'.
     LT_FIELDS-TABNAME   = 'RBADA'.
     APPEND LT_FIELDS.

     LT_FIELDS-SELNAME   = 'PA_XBUDG'.          "Sim. auf Budgetbasis
     LT_FIELDS-VALUE     =  PA_XBUDG.
     LT_FIELDS-FIELDNAME = 'XBUDGBAS'.
     APPEND LT_FIELDS.

  LT_FIELDS-SELNAME   = 'PA_XINVP'.       "ProgPositionen selektieren
     LT_FIELDS-VALUE     =  PA_XINVP.
     LT_FIELDS-FIELDNAME = 'XINVP'.
     APPEND LT_FIELDS.

   LT_FIELDS-SELNAME   = 'PA_PGSEL'.          "Selektiv für Auft/PSP/Anf
     LT_FIELDS-VALUE     =  PA_PGSEL.
     LT_FIELDS-TABNAME   = 'IM_PGSEL'.
     CLEAR LT_FIELDS-FIELDNAME.
     APPEND LT_FIELDS.

     LT_FIELDS-SELNAME   = 'PA_XMANF'.          "Manfen selektieren
     LT_FIELDS-VALUE     =  PA_XMANF.
     LT_FIELDS-FIELDNAME = 'XMANF'.
     LT_FIELDS-TABNAME   = 'RBADA'.
     APPEND LT_FIELDS.

     LT_FIELDS-SELNAME   = 'PA_XPROJ'.          "Projekte selektieren
     LT_FIELDS-VALUE     =  PA_XPROJ.
     LT_FIELDS-FIELDNAME = 'XPROJ'.
     APPEND LT_FIELDS.

     LT_FIELDS-SELNAME   = 'PA_XAUFT'.          "Aufträge selektieren
     LT_FIELDS-VALUE     =  PA_XAUFT.
     LT_FIELDS-FIELDNAME = 'XAUFT'.
     APPEND LT_FIELDS.

     IF NOT PA_XSETL IS INITIAL.
       LT_FIELDS-SELNAME   = 'PA_XSETL'.        "Kürzen um Aktivierungen
       LT_FIELDS-VALUE     =  PA_XSETL.
       LT_FIELDS-FIELDNAME = 'XSETL'.
       APPEND LT_FIELDS.
     ELSE.
    LT_FIELDS-SELNAME   = 'PA_XGJBG'.        "Anlagenbestand GJahrBeginn
       LT_FIELDS-VALUE     =  PA_XGJBG.
       LT_FIELDS-FIELDNAME = 'XGJBG'.
       APPEND LT_FIELDS.
     ENDIF.

* Einträge in der Liste der Selektionskriterien können nicht
* modifiziert, sondern nur gelöscht und neu aufgenommen werden
* 1.Schritt: Löschen
     SEL_INFO-MODE      = 'S'.
     LS_ENTRIES-MODE    = 'D'.
     LOOP AT LT_FIELDS.
       LS_ENTRIES-SELNAME = LT_FIELDS-SELNAME.
       APPEND LS_ENTRIES TO SEL_INFO-T_ENTRIES.
     ENDLOOP.

* 2.Schritt: Felder mit Kurztext aus DDIC in Liste aufnehmen
     LS_ENTRIES-MODE    = 'A'.
     LS_ENTRIES-SIGN0   = 'I'.
     LS_ENTRIES-OPTIO   = 'EQ'.

     LOOP AT LT_FIELDS.
       REFRESH LT_DFIES.
       CLEAR LS_DFIES.
       CALL FUNCTION 'DDIF_FIELDINFO_GET'
            EXPORTING
                 TABNAME        = LT_FIELDS-TABNAME
                 FIELDNAME      = LT_FIELDS-FIELDNAME
                 ALL_TYPES      = 'X'
            IMPORTING
                 DFIES_WA       = LS_DFIES
            TABLES
                 DFIES_TAB      = LT_DFIES
            EXCEPTIONS
                 NOT_FOUND      = 1
                 INTERNAL_ERROR = 2
                 OTHERS         = 3.
       IF SY-SUBRC = 0.
         IF LS_DFIES IS INITIAL.
           READ TABLE LT_DFIES INDEX 1 INTO LS_DFIES.
         ENDIF.
       ENDIF.
       LS_ENTRIES-STEXT   = LS_DFIES-FIELDTEXT.
       LS_ENTRIES-VALUF   = LT_FIELDS-VALUE.

       APPEND LS_ENTRIES TO SEL_INFO-T_ENTRIES.
     ENDLOOP.

   ENDFORM.                    " SELINFO_INIT1
