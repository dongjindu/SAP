*XLZL9CK109099 110902 note 0000553811
*XPXL9CK056755 290600 note 412813
* 220101 note 375711 - only read infotypes within date selection
*XPXL9CK012591 120600 Hinweis 309613 (Drucken der org. Zuordnung)
*XPXL9CK010530 230500 Hinweis 305453 (markiert mit L9BK018343)
*XPXL9CK006744 250400 Hinweis 214782(fehlender refresh auf ZL,ZES,SALDO)
*4.6C
*XPXPH0K002850 030100 Hinweis 192331
*4.6A
*XHRAHRK031576 141298 Listanzeige P0001/P0007-Splitgenau
*4.5A
*XPXPH4K013099 130798 Texte zu Org.einheiten und AZPregel
*XOQPH4K005691 150698 verwirrender Text


* COPY of rptbal00.
REPORT ZACO03U_HRMH MESSAGE-ID 72.


*-----Globale Daten
INCLUDE RPTBAL01.
*-----Reportparamter/Selektionsbedingungen
INCLUDE RPTBAL02.

*----------------------------------------------------------------------*
*                       INITIALIZATION                                 *
*----------------------------------------------------------------------*
INITIALIZATION.
*-----Füllen der Tabellen für RP_OPTIONS_INTO_STRING
  PERFORM FILL_SEL_TAB.
  PERFORM FILL_FDTAB.
*-----Keine Berücksichtigung von Grenzwerten mittels Merkmal LIMIT
  SEL_LIM = NO.
*-----Variante initialisieren
  IS_VARIANT-REPORT = 'SAPLHRB2'.
  ES_VARIANT = IS_VARIANT.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE        = 'A'
       CHANGING
            CS_VARIANT    = ES_VARIANT
       EXCEPTIONS
            WRONG_INPUT   = 1
            NOT_FOUND     = 2
            PROGRAM_ERROR = 3
            OTHERS        = 4.
  IF SY-SUBRC EQ 0.
    VARIANT = ES_VARIANT-VARIANT.
  ENDIF.

*----------------------------------------------------------------------*
*                    START-OF-SELECTION                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.
*-----Entfernung der nicht benötigten Infotypen aus Tabelle $RINFO$
  PERFORM REMOVE_UNUSED_INFOTYPES.
*-----Restrict infotypes to reporting interval
  RP_SET_DATA_INTERVAL 'ALL' PN-BEGDA PN-ENDDA. "<<<< INSERT NOTE 375711
*-----Zuweisung des Zeigers auf aktuelle Datentabelle
  PERFORM GET_DATA_TAB.
*-----Bestimmung der Auswertungsperioden
  PERFORM FILL_I549Q USING PN-BEGDA PN-ENDDA.
*-----Füllen der SORTFIELDTAB
  PERFORM FILL_SORTFIELDTAB USING SEL_ORG.
*-----Listlayout
  GS_LAYOUT-ZEBRA             = 'X'.
  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GS_LAYOUT-COLTAB_FIELDNAME  = 'COLOR'.
  GS_LAYOUT-GROUP_CHANGE_EDIT  = 'X'.
*-----Varianten
  ALV_VARIANT-REPORT = 'SAPLHRB2'.

*----------------------------------------------------------------------*
*                       GET PERNR                                      *
*----------------------------------------------------------------------*
GET PERNR.
*-----Ausgabe einer Status-Information
  PERFORM PROGRESS_INDICATOR USING TEXT-BEG.
*-----Import Zeitauswertungsergebnisse und Füllen der Datentabellen
  LOOP AT I549Q.
* blocks switched - NOTE 375711
    IF I549Q-BEGDA LT PN-BEGDA.
      BEGDA = PN-BEGDA.
    ELSE.
      BEGDA = I549Q-BEGDA.
    ENDIF.
    IF I549Q-ENDDA GT PN-ENDDA.
      ENDDA = PN-ENDDA.
    ELSE.
      ENDDA = I549Q-ENDDA.
    ENDIF.
* blocks switched - NOTE 375711
*-----Lesen der Infotypdaten
    PERFORM GET_INFTY_DATA USING BEGDA HIGH-DATE. "<<<< NOTE 375711
*-----Füllen der Struktur PME51 und Auswerten
    PERFORM FILL_PME51.
*-----Import der Tabellen ZES, SALDO und ZL aus Cluster B2
    CALL FUNCTION 'HR_TIME_RESULTS_IN_INTERVAL'
         EXPORTING
              INT_PERNR             = PERNR-PERNR
              INT_BEGDA             = BEGDA
              INT_ENDDA             = ENDDA
         TABLES
              INT_TIME_RESULTS      = TIME_RESULTS
         EXCEPTIONS
              WRONG_CLUSTER_VERSION = 1
              NO_READ_AUTHORITY     = 2
              CLUSTER_ARCHIVED      = 3
              TECHNICAL_ERROR       = 4
              OTHERS                = 5.
    CASE SY-SUBRC.
      WHEN 1.                          "falsche Version Cluster B2
        PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '101'
                                     SPACE SPACE SPACE SPACE.
      WHEN 2.  "keine Leseberechtigung Cluster B2
        PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '102'
                                     SPACE SPACE SPACE SPACE.
      WHEN 3.                          "Cluster archiviert
        PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '139'
                                     SPACE SPACE SPACE SPACE.
      WHEN 4.                          "technischer Fehler
        PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '140'
                                     SPACE SPACE SPACE SPACE.
      WHEN OTHERS.
        CLEAR: ZL[], SALDO[], ZES[].                        "L9CK006744
        CLEAR: ZL, SALDO, ZES.                              "L9CK006744
        LOOP AT TIME_RESULTS INTO TIME_RESULTS_WA.
          APPEND LINES OF TIME_RESULTS_WA-ZES   TO ZES.
          APPEND LINES OF TIME_RESULTS_WA-SALDO TO SALDO.
          APPEND LINES OF TIME_RESULTS_WA-ZL    TO ZL.
        ENDLOOP.
    ENDCASE.
*-----Füllen der internen Datentabellen
    CHECK NOT ZES[]   IS INITIAL OR
          NOT SALDO[] IS INITIAL OR
          NOT ZL[]    IS INITIAL.
    PERFORM FILL_TIME_DATA.
  ENDLOOP.

*----------------------------------------------------------------------*
*                      END-OF-SELECTION                                *
*----------------------------------------------------------------------*
END-OF-SELECTION.
*-----Ausgabe einer Status-Information
  PERFORM PROGRESS_INDICATOR USING TEXT-DIS.
*-----Überarbeiten der Tabelle GRAF_TAB für Grafikbaustein
  PERFORM COMPLETE_GRAF_TAB.
*-----Ausgabe der Fehler
  PERFORM DISPLAY_ERRORS.
*-----Ausgabe der Auswertungsergebnisse
  PERFORM DISPLAY_LIST.

*----------------------------------------------------------------------*
*                      AT SELECTION-SCREEN                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
    WHEN 'ORGE'.
      CALL FUNCTION 'RP_OPTIONS_INTO_STRING'
           EXPORTING
                MAX_CHOSEN_NUMBER       = 7
                DELIMITER_SIGN          = '/'
*       text_title = 'Organisationseinheiten'(or0)          "XOQK005691
*       text_left  = 'Organisationseinheit'(or1)            "XOQK005691
        TEXT_TITLE = 'Gruppierung nach Org. Zuordnung'(OR0) "XOQK005691
        TEXT_LEFT  = 'Org. Zuordnung'(OR1)                  "XOQK005691
                TEXT_RIGHT              = 'ausgewählt'(OR2)
                STATUS                  = 'ORDER'
           IMPORTING
                RETURN_CODE             = RETCD
           TABLES
                TEXT_SYMBOL_RELATION_TAB   = SEL_TAB
           CHANGING
                STRING_VALUE               = SEL_ORG
           EXCEPTIONS
                TABLE_STRING_INCONSISTENCY = 01
                UNKNOWN_STATUS             = 02
                STRING_VALUE_OVERFLOW      = 03.
    WHEN 'LIMI'.
      IF SEL_LIM EQ NO.
        SEL_LIM = YES.
        IF NOT SW_ZES IS INITIAL.
          PME51-ORIGS = 'E'.
          SEL_FEATURE = 'LIMIE'.
        ELSEIF NOT SW_SAL IS INITIAL.
          PME51-ORIGS = 'S'.
          SEL_FEATURE = 'LIMIS'.
        ELSEIF NOT SW_ZL IS INITIAL.
          PME51-ORIGS = 'Z'.
          SEL_FEATURE = 'LIMIZ'.
        ENDIF.
        SUBMIT RPUMKS00 WITH MERKMAL EQ SEL_FEATURE
                        WITH EXPAND  EQ 'X'
                        AND RETURN.
      ELSE.
        SEL_LIM = NO.
      ENDIF.
  ENDCASE.
  IF NOT VARIANT IS INITIAL.
    ES_VARIANT-VARIANT = VARIANT.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              I_SAVE        = 'A'
         CHANGING
              CS_VARIANT    = ES_VARIANT
         EXCEPTIONS
              WRONG_INPUT   = 1
              NOT_FOUND     = 2
              PROGRAM_ERROR = 3
              OTHERS        = 4.
    IF SY-SUBRC <> 0.
      MESSAGE E225(72).
    ENDIF.
  ELSE.
    CLEAR ES_VARIANT.
    ES_VARIANT-REPORT = 'SAPLHRB2'.
  ENDIF.

*----------------------------------------------------------------------*
*                      AT SELECTION-SCREEN OUTPUT                      *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-----Festsetzung von Stundengrenzwerten
** {* 2011/08/24 Paul Change not use RPTBAL00 (Origin Program)
**  WRITE 'Stundengrenzwerte'(LIM) TO FEAT+4(25).
**  IF SEL_LIM EQ YES.
**    WRITE ICON_DISPLAY_MORE AS ICON TO FEAT+0(4).
**  ELSE.
**    WRITE ICON_ENTER_MORE AS ICON TO FEAT+0(4).
**  ENDIF.
**  *}

*-----Auswahl Organisationseinheiten
* write 'Organisationseinheiten'(or0) to orge+4(25).        "XOQK005691
* WRITE 'Organisationseinheiten'(OR0) TO ORGE+4(34).        "XOQK005691
  IF SEL_ORG NE SPACE.
*   WRITE ICON_DISPLAY_MORE AS ICON TO ORGE+0(4).
    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 EQ 'SUB'.
      SCREEN-INPUT     = '1'.
      SCREEN-INVISIBLE = '0'.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
*   WRITE ICON_ENTER_MORE AS ICON TO ORGE+0(4).
    LOOP AT SCREEN.
      CHECK SCREEN-GROUP1 EQ 'SUB'.
      SCREEN-INPUT     = '0'.
      SCREEN-INVISIBLE = '1'.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN ON VALUE-REQUEST FOR VARIANT               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR VARIANT.
  IS_VARIANT-REPORT = 'SAPLHRB2'.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT    = IS_VARIANT
            I_SAVE        = 'A'
       IMPORTING
            ES_VARIANT    = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND     = 1
            PROGRAM_ERROR = 2
            OTHERS        = 3.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    VARIANT = ES_VARIANT-VARIANT.
  ENDIF.

*----------------------------------------------------------------------*
*                      Unterprogramme                                  *
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR USING PROG_TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT   = PROG_TEXT
       EXCEPTIONS
            OTHERS = 1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_i549q                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  fill_begda                                                    *
*  -->  fill_endda                                                    *
*---------------------------------------------------------------------*
FORM FILL_I549Q USING FILL_BEGDA FILL_ENDDA.
  IF FILL_BEGDA EQ LOW-DATE AND FILL_ENDDA EQ HIGH-DATE.
    FILL_BEGDA = FILL_ENDDA = SY-DATUM.
  ENDIF.

  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
       EXPORTING
            GET_BEGDA       = FILL_BEGDA
            GET_ENDDA       = FILL_ENDDA
            GET_PERMO       = RPTIME_PERIOD
       TABLES
            GET_PERIODS     = I549Q
       EXCEPTIONS
            NO_VALID_PERMO  = 1
            NO_PERIOD_FOUND = 2.

  CASE SY-SUBRC.
    WHEN 1.
      PERFORM ERROR_HANDLING USING SPACE         '72'  'E'  '025'
                                   RPTIME_PERIOD SPACE SPACE SPACE.
      STOP.
    WHEN 2. STOP.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.

*ORM get_infty_data USING get_begda get_endda.              "AHRK031576
FORM GET_INFTY_DATA USING VALUE(GET_BEGDA)                  "AHRK031576
                          VALUE(GET_ENDDA).                 "AHRK031576
*-----Texte zur org. Zuordnung                            "XPXPH4K013099
  DATA: TEXT(30).                        "XPXPH4K013099
*-----organisatorische Zuordnung
  RP_PROVIDE_FROM_FRST P0001 SPACE GET_BEGDA GET_ENDDA.
  IF PNP-SW-FOUND NE '1'.
    PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '103'
                                 PERNR-PERNR 'P0001' SPACE SPACE.
  ENDIF.
*----- Lesen der Texte zur org. Zuord.                    "XPXPH4K013099
PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'BUKRS'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-BUKRS_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099
PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'WERKS'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-WERKS_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'BTRTL'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-BTRTL_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'PERSG'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-PERSG_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'PERSK'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099

  P0001_TEXT-PERSK_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'KOSTL'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099

  P0001_TEXT-KOSTL_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'ABKRS'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-ABKRS_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'ANSVH'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-ANSVH_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'GSBER'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-GSBER_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'ORGEH'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-ORGEH_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'PLANS'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-PLANS_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'SACHA'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-SACHA_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'SACHP'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-SACHP_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'SACHZ'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-SACHZ_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

PERFORM GET_TEXT_FOR_P0001_FIELDS(RPTXTX00)               "XPXPH4K013099
                              USING P0001 'STELL'"XPXPH4K013099
                              CHANGING TEXT.     "XPXPH4K013099
  P0001_TEXT-STELL_TXT = TEXT.           "XPXPH4K013099
  CLEAR TEXT.                            "XPXPH4K013099

  PERFORM RE001P USING P0001-WERKS P0001-BTRTL.
*-----Daten zur Person
  RP_PROVIDE_FROM_FRST P0002 SPACE GET_BEGDA GET_ENDDA.
  IF PNP-SW-FOUND NE '1'.
    PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '103'
                                 PERNR-PERNR 'P0002' SPACE SPACE.
  ENDIF.
*-----Sollarbeitszeit
  RP_PROVIDE_FROM_FRST P0007 SPACE GET_BEGDA GET_ENDDA.
  IF PNP-SW-FOUND NE '1'.
    PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '103'
                                 PERNR-PERNR 'P0007' SPACE SPACE.
  ENDIF.
  PERFORM FILL_SCHKZ_TEXT USING P0007-SCHKZ               "XPXPH4K013099
                          CHANGING SCHKZ_TEXT-S_TEXT.     "XPXPH4K013099
*-----Basisbezüge
  IF SEL_LIM EQ YES.
    RP_PROVIDE_FROM_FRST P0008 SPACE GET_BEGDA GET_ENDDA.
    IF PNP-SW-FOUND NE '1'.
      PERFORM ERROR_HANDLING USING PERNR-PERNR '72' 'E' '103'
                                   PERNR-PERNR 'P0008' SPACE SPACE.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_pme51                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_PME51.
  MOVE-CORRESPONDING P0001 TO PME51.
  MOVE-CORRESPONDING P0008 TO PME51.
  PME51-GESCH = P0002-GESCH.
  IF NOT SW_ZES IS INITIAL.
    PME51-ORIGS = 'E'.
  ELSEIF NOT SW_SAL IS INITIAL.
    PME51-ORIGS = 'S'.
  ELSEIF NOT SW_ZL IS INITIAL.
    PME51-ORIGS = 'Z'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_time_data                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_TIME_DATA.
  ACT_PERIOD+0(4) = I549Q-PABRJ.
  ACT_PERIOD+4(2) = I549Q-PABRP.
*-----Verarbeiten der Einzelsalden
  IF NOT SW_ZES IS INITIAL.
    PERFORM PROCESS_ZES.
  ENDIF.
*-----Verarbeiten der Periodensalden
  IF NOT SW_SAL IS INITIAL.
    PERFORM PROCESS_SALDO.
  ENDIF.
*-----Verarbeiten der Zeitlohnarten
  IF NOT SW_ZL IS INITIAL.
    PERFORM PROCESS_ZL.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM process_zes                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROCESS_ZES.
  PROVIDE * FROM P0001                                      "AHRK031576
          * FROM P0007                                      "AHRK031576
          BETWEEN BEGDA AND ENDDA.      "AHRK031576 "<<<< NOTE 375711
    PERFORM GET_INFTY_DATA USING P0001-BEGDA P0001-ENDDA.   "AHRK031576

    MOVE-CORRESPONDING P0001 TO TIME_DATA_ZES.
  MOVE-CORRESPONDING P0001_TEXT TO TIME_DATA_ZES.         "XPXPH4K013099
    MOVE-CORRESPONDING P0007 TO TIME_DATA_ZES.
  MOVE-CORRESPONDING SCHKZ_TEXT TO TIME_DATA_ZES.         "XPXPH4K013099
    TIME_DATA_ZES-GESCH = P0002-GESCH.
    PERFORM TRANSFORM_GENDER USING TIME_DATA_ZES-GESCH.
    TIME_DATA_ZES-CLSTB2_PER = ACT_PERIOD.
    LOOP AT ZES WHERE ZTART IN ZTART.
      TIME_DATA_ZES-DATUM = I549Q-BEGDA + ZES-REDAY - 1.
*   CHECK time_data_zes-datum GE pn-begda                   "AHRK031576
*     AND time_data_zes-datum LE pn-endda.                  "AHRK031576
      CHECK TIME_DATA_ZES-DATUM GE P0001-BEGDA              "AHRK031576
        AND TIME_DATA_ZES-DATUM LE P0001-ENDDA.             "AHRK031576
      CHECK TIME_DATA_ZES-DATUM >= P0007-BEGDA              "L9BK018343
        AND TIME_DATA_ZES-DATUM <= P0007-ENDDA.             "L9BK018343
      TIME_DATA_ZES-PERNR = PERNR-PERNR.
      TIME_DATA_ZES-ENAME = P0001-ENAME.
      TIME_DATA_ZES-ZTART = ZES-ZTART.
      PERFORM RE555B USING T001P-MOBDE ZES-ZTART TIME_DATA_ZES-ZTEXT.
      TIME_DATA_ZES-ANZHL = ZES-ANZHL.
*-----Verprobung gegen Grenzwert laut Merkmal LIMIT
      PERFORM GET_LIMIT USING ZES-ZTART ZES-ANZHL LIMIT.
      IF LIMIT EQ YES.
        APPEND COLOR TO TIME_DATA_ZES-COLOR.
      ELSE.
        REFRESH TIME_DATA_ZES-COLOR.
      ENDIF.
      APPEND TIME_DATA_ZES.
*-----Bereitstellen der Informationen für Präsentationsgrafik
      PERFORM FILL_GRAF_DATA USING ZES-ZTART ACT_PERIOD ZES-ANZHL.
    ENDLOOP.

  ENDPROVIDE.                                               "AHRK031576
ENDFORM.

*---------------------------------------------------------------------*
*       FORM process_saldo                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROCESS_SALDO.
  MOVE-CORRESPONDING P0001 TO TIME_DATA_SALDO.
  MOVE-CORRESPONDING P0001_TEXT TO TIME_DATA_SALDO.       "XPXPH4K013099
  MOVE-CORRESPONDING P0007 TO TIME_DATA_SALDO.
  MOVE-CORRESPONDING SCHKZ_TEXT TO TIME_DATA_SALDO.       "XPXPH4K013099
  TIME_DATA_SALDO-GESCH = P0002-GESCH.
  PERFORM TRANSFORM_GENDER USING TIME_DATA_SALDO-GESCH.
  TIME_DATA_SALDO-CLSTB2_PER = ACT_PERIOD.
  LOOP AT SALDO WHERE ZTART IN SALDI.
    TIME_DATA_SALDO-PERNR = PERNR-PERNR.
    TIME_DATA_SALDO-ENAME = P0001-ENAME.
    TIME_DATA_SALDO-ZTART = SALDO-ZTART.
    PERFORM RE555B USING T001P-MOBDE SALDO-ZTART TIME_DATA_SALDO-ZTEXT.
    TIME_DATA_SALDO-ANZHL = SALDO-ANZHL.
*-----Verprobung gegen Grenzwert laut Merkmal LIMIT
    PERFORM GET_LIMIT USING SALDO-ZTART SALDO-ANZHL LIMIT.
    IF LIMIT EQ YES.
      APPEND COLOR TO TIME_DATA_SALDO-COLOR.
    ELSE.
      REFRESH TIME_DATA_SALDO-COLOR.
    ENDIF.
    APPEND TIME_DATA_SALDO.
*-----Bereitstellen der Informationen für Präsentationsgrafik
    PERFORM FILL_GRAF_DATA USING SALDO-ZTART ACT_PERIOD SALDO-ANZHL.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM process_zl                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM PROCESS_ZL.
  PROVIDE * FROM P0001                                      "AHRK031576
          * FROM P0007                                      "AHRK031576
          BETWEEN BEGDA AND ENDDA.       "AHRK031576  "<<<< NOTE 375711
    PERFORM GET_INFTY_DATA USING P0001-BEGDA P0001-ENDDA.   "AHRK031576

    MOVE-CORRESPONDING P0001 TO TIME_DATA_ZL.
  MOVE-CORRESPONDING P0001_TEXT TO TIME_DATA_ZL.          "XPXPH4K013099
    MOVE-CORRESPONDING P0007 TO TIME_DATA_ZL.
  MOVE-CORRESPONDING SCHKZ_TEXT TO TIME_DATA_ZL.          "XPXPH4K013099
    TIME_DATA_ZL-GESCH = P0002-GESCH.
    PERFORM TRANSFORM_GENDER USING TIME_DATA_ZL-GESCH.
    TIME_DATA_ZL-CLSTB2_PER = ACT_PERIOD.

*// 2011.09.16   find error     by yn.kim ================= //*
*// Variable[LGART]  Was declared in two ways.   ===>  Must check.
*// 1.  data  Declaration/ 2. SELECT-OPTIONS: LGART FOR T512W-LGART.

    LOOP AT ZL WHERE LGART in LGART
*            AND   datum GE pn-begda                        "AHRK031576
*            AND   datum LE pn-endda.                       "AHRK031576
              AND   DATUM >= P0007-BEGDA                    "L9BK018343
              AND   DATUM <= P0007-ENDDA                    "L9BK018343
              AND   DATUM GE P0001-BEGDA                    "AHRK031576
              AND   DATUM LE P0001-ENDDA.                   "AHRK031576
      TIME_DATA_ZL-PERNR = PERNR-PERNR.
      TIME_DATA_ZL-ENAME = P0001-ENAME.
      TIME_DATA_ZL-DATUM = ZL-DATUM.
      TIME_DATA_ZL-LGART = ZL-LGART.
      PERFORM RE512T USING T001P-MOLGA ZL-LGART TIME_DATA_ZL-LGTXT.
      TIME_DATA_ZL-ANZHL = ZL-ANZHL.
*-----Verprobung gegen Grenzwert laut Merkmal LIMIT
      PERFORM GET_LIMIT USING ZL-LGART ZL-ANZHL LIMIT.
      IF LIMIT EQ YES.
        APPEND COLOR TO TIME_DATA_ZL-COLOR.
      ELSE.
        REFRESH TIME_DATA_ZL-COLOR.
      ENDIF.
      APPEND TIME_DATA_ZL.
*-----Bereitstellen der Informationen für Präsentationsgrafik
      PERFORM FILL_GRAF_DATA USING ZL-LGART ACT_PERIOD ZL-ANZHL.
    ENDLOOP.

  ENDPROVIDE.                                               "AHRK031576
ENDFORM.

*---------------------------------------------------------------------*
*       FORM transform_gender                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  tr_gender                                                     *
*---------------------------------------------------------------------*
FORM TRANSFORM_GENDER USING TR_GENDER.
  CASE TR_GENDER.
    WHEN '1'.
      TR_GENDER = 'männlich'(GE1) .
    WHEN '2'.                          "weiblich
      TR_GENDER = 'weiblich'(GE2).
    WHEN OTHERS.                       "unbestimmt
      CLEAR TR_GENDER.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM get_limit                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  get_ident                                                     *
*  -->  get_anzhl                                                     *
*  -->  get_limit                                                     *
*---------------------------------------------------------------------*
FORM GET_LIMIT USING GET_IDENT GET_ANZHL GET_LIMIT.
  DATA: LIM_NUM TYPE P DECIMALS 2,
        RETCD   LIKE SY-SUBRC.
*-----Initialisierung einiger globaler Felder
  CLEAR COLOR.
  GET_LIMIT = NO.
*-----Ist Grenzwertbestimmung gewünscht?
  CHECK SEL_LIM EQ YES.
*-----Auswertung des Merkmals LIMIT
  PME51-ZTART = GET_IDENT.
  PME51-LGART = GET_IDENT.
  PERFORM RE549D USING 'LIMIT' SPACE LIM_NUM RETCD.
*-----Wurde der Schwellwert überschritten?
  CHECK RETCD EQ 0 AND GET_ANZHL GT LIM_NUM.
  GET_LIMIT = YES.
*-----Füllen der Farbinformation
  COLOR-FIELDNAME = 'ANZHL'.
  COLOR-COLOR-COL = '6'.
  COLOR-COLOR-INT = '1'.
  COLOR-COLOR-INV = '0'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_graf_data                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  fgd_ident                                                     *
*  -->  fgd_perio                                                     *
*  -->  fgd_anzhl                                                     *
*---------------------------------------------------------------------*
FORM FILL_GRAF_DATA USING FGD_IDENT FGD_PERIO FGD_ANZHL.
  READ TABLE GRAF_TAB WITH KEY PERIO = FGD_PERIO
                               IDENT = FGD_IDENT.
  IF SY-SUBRC EQ 0.
    GRAF_TAB-ANZHL = GRAF_TAB-ANZHL + FGD_ANZHL.
    MODIFY GRAF_TAB INDEX SY-TABIX.
  ELSE.
    GRAF_TAB-PERIO = FGD_PERIO.
    GRAF_TAB-IDENT = FGD_IDENT.
    GRAF_TAB-ANZHL = FGD_ANZHL.
    APPEND GRAF_TAB.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM complete_graf_tab                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM COMPLETE_GRAF_TAB.
  DATA: BEGIN OF ALL_IDENT OCCURS 0,
          IDENT(4),
        END OF ALL_IDENT.

  IF NOT SW_ZES IS INITIAL.
    GRAF_TITLE = TEXT-ZES.
    GRAF_TYPE  = 'Tageseinzelsalden'(GTZ).
  ELSEIF NOT SW_SAL IS INITIAL.
    GRAF_TITLE = TEXT-SAL.
    GRAF_TYPE  = 'Periodensalden'(GTS).
  ELSEIF NOT SW_ZL IS INITIAL.
    GRAF_TITLE = TEXT-ZLA.
    GRAF_TYPE  = 'Zeitlohnarten'(GTL).
  ENDIF.

  LOOP AT GRAF_TAB.
    READ TABLE ALL_IDENT WITH KEY IDENT = GRAF_TAB-IDENT.
    IF SY-SUBRC NE 0.
      ALL_IDENT-IDENT = GRAF_TAB-IDENT.
      APPEND ALL_IDENT.
    ENDIF.
  ENDLOOP.

  LOOP AT I549Q.
    ACT_PERIOD+0(4) = I549Q-PABRJ.
    ACT_PERIOD+4(2) = I549Q-PABRP.
    LOOP AT ALL_IDENT.
      READ TABLE GRAF_TAB WITH KEY PERIO = ACT_PERIOD
                                   IDENT = ALL_IDENT-IDENT.
      CHECK SY-SUBRC NE 0.
      GRAF_TAB-PERIO = ACT_PERIOD.
      GRAF_TAB-IDENT = ALL_IDENT-IDENT.
      GRAF_TAB-ANZHL = 0.
      APPEND GRAF_TAB.
    ENDLOOP.
  ENDLOOP.

  SORT GRAF_TAB BY PERIO IDENT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM append_field_tab                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  subtotal                                                      *
*---------------------------------------------------------------------*
FORM APPEND_FIELD_TAB USING SUBTOTAL.
  COL_POS = COL_POS + 1.
  FIELD_TAB_HEADER-COL_POS = COL_POS.
  APPEND FIELD_TAB_HEADER TO FIELD_TAB.
  IF NOT SUBTOTAL IS INITIAL.
    GT_ZSUM-TABNAME   = FIELD_TAB_HEADER-TABNAME.
    GT_ZSUM-FIELDNAME = FIELD_TAB_HEADER-FIELDNAME.
    GT_ZSUM-SPOS      = SY-TABIX.
    GT_ZSUM-SUBTOT    = 'X'.
    APPEND GT_ZSUM.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_sel_tab                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_SEL_TAB.
  REFRESH NAMETAB.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            LANGU               = SY-LANGU
            TABNAME             = 'PS0001'
       TABLES
            NAMETAB             = NAMETAB
       EXCEPTIONS
            INTERNAL_ERROR      = 1
            TABLE_HAS_NO_FIELDS = 2
            TABLE_NOT_ACTIV     = 3
            NO_TEXTS_FOUND      = 4
            OTHERS              = 5.

  LOOP AT NAMETAB WHERE FIELDNAME+1(4) NE 'NAME'.
    SEL_TAB-SHORTSTRG  = NAMETAB-FIELDNAME.
    SEL_TAB-OPTIONTEXT = NAMETAB-FIELDTEXT.
    SEL_TAB-TABNAME    = 'P0001'.
    SEL_TAB-FIELDNAME  = NAMETAB-FIELDNAME.
    APPEND SEL_TAB.
  ENDLOOP.
  SORT SEL_TAB BY OPTIONTEXT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_fdtab                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_FDTAB.
  DESCRIBE TABLE NAMETAB LINES SY-TFILL.
  IF SY-TFILL EQ 0.
    CALL FUNCTION 'NAMETAB_GET'
         EXPORTING
              TABNAME = 'PS0001'
              LANGU   = SY-LANGU
         TABLES
              NAMETAB = NAMETAB.
  ENDIF.
  LOOP AT NAMETAB.
    MOVE-CORRESPONDING NAMETAB TO FDTAB.
    APPEND FDTAB.
  ENDLOOP.
  FREE NAMETAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM remove_unused_infotypes                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM REMOVE_UNUSED_INFOTYPES.
  CHECK SEL_LIM EQ NO.
  DELETE $RINFO$ WHERE NUMBER EQ '0008'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM get_data_tab                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA_TAB.
  IF NOT SW_ZES IS INITIAL.
    DATA_TAB = 'TIME_DATA_ZES'.
    ASSIGN TIME_DATA_ZES[] TO <DATA_TAB>.
  ELSEIF NOT SW_SAL IS INITIAL.
    DATA_TAB = 'TIME_DATA_SALDO'.
    ASSIGN TIME_DATA_SALDO[] TO <DATA_TAB>.
  ELSEIF NOT SW_ZL IS INITIAL.
    DATA_TAB = 'TIME_DATA_ZL'.
    ASSIGN TIME_DATA_ZL[] TO <DATA_TAB>.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_sortfieldtab                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  assign_string                                                 *
*---------------------------------------------------------------------*
FORM FILL_SORTFIELDTAB USING ASSIGN_STRING.
  DATA: ASSIGNFIELD(11) VALUE 'P0001-'.
  DATA: HELP_STRING LIKE SEL_ORG,
        COUNT TYPE I.

  HELP_STRING = ASSIGN_STRING.

  SORTFIELDTAB-TABNAME = 'P0001'.
  SORTFIELDTAB-KEY     = 'X'.
  WHILE NOT HELP_STRING+0(5) IS INITIAL.
*-----Füllen der SORTFIELDTAB
    READ TABLE FDTAB WITH KEY HELP_STRING+0(5).
    SORTFIELDTAB-FIELDNAME = FDTAB-FIELDNAME.
    SORTFIELDTAB-FIELDTEXT = FDTAB-FIELDTEXT.
    APPEND SORTFIELDTAB.
    SHIFT HELP_STRING BY 6 PLACES.
  ENDWHILE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM display_list                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_LIST.
  DATA: L_FIELDINFO TYPE TABLE OF DFIES,            "note 412813
        L_FIELDINFO_LINE TYPE DFIES.                "note 412813
*-----Sind Ausgabedaten vorhanden?
  DESCRIBE TABLE <DATA_TAB> LINES SY-TFILL.
  IF SY-TFILL EQ 0.
    MESSAGE ID 'PN' TYPE 'S' NUMBER '006'.
  ENDIF.
  CHECK SY-TFILL GT 0.
*-----ausgewählte Organisationseinheiten
  LOOP AT SORTFIELDTAB.

    CLEAR FIELD_TAB_HEADER.
* begin of deletion (PH0K002850)
*    IF sortfieldtab-fieldname EQ 'VDSK1'                 "XPXPH4K013099
*      OR sortfieldtab-fieldname EQ 'JUPER'               "XPXPH4K013099
*      OR sortfieldtab-fieldname EQ 'MSTBR'               "XPXPH4K013099
*      OR sortfieldtab-fieldname EQ 'OTYPE'               "XPXPH4K013099
*      OR sortfieldtab-fieldname EQ 'SBMOD'               "XPXPH4K013099
*      OR sortfieldtab-fieldname EQ 'KOKRS'               "XPXPH4K013099
*       OR sortfieldtab-fieldname EQ 'FISTL'              "XPXPH4K013099
*       OR sortfieldtab-fieldname EQ 'GEBER'.             "XPXPH4K013099
*      field_tab_header-ref_tabname = sortfieldtab-tabname.
*      field_tab_header-fieldname   = sortfieldtab-fieldname.
*    ELSE.
*     field_tab_header-ref_tabname = sortfieldtab-tabname."XPXPH4K013099
*     field_tab_header-ref_fieldname = sortfieldtab-fieldname"XPXPH4K013
*     CONCATENATE sortfieldtab-fieldname 'TXT' INTO       "XPXPH4K013099
*         field_tab_header-fieldname SEPARATED BY '_'.    "XPXPH4K013099
*     field_tab_header-outputlen = 30.  "XPXPH4K013099
*    ENDIF.                             "XPXPH4K013099
* end of deletion (PH0K002850)

* begin of insertion (PH0K002850)
* Aufzählung aller Felder im IT 1 für die eine Textleseroutine existiert
    IF    SORTFIELDTAB-FIELDNAME EQ 'BUKRS'
       OR SORTFIELDTAB-FIELDNAME EQ 'WERKS'
       OR SORTFIELDTAB-FIELDNAME EQ 'BTRTL'
       OR SORTFIELDTAB-FIELDNAME EQ 'PERSG'
       OR SORTFIELDTAB-FIELDNAME EQ 'PERSK'
       OR SORTFIELDTAB-FIELDNAME EQ 'GSBER'
       OR SORTFIELDTAB-FIELDNAME EQ 'ABKRS'
       OR SORTFIELDTAB-FIELDNAME EQ 'ANSVH'
       OR SORTFIELDTAB-FIELDNAME EQ 'KOSTL'
       OR SORTFIELDTAB-FIELDNAME EQ 'ORGEH'
       OR SORTFIELDTAB-FIELDNAME EQ 'PLANS'
       OR SORTFIELDTAB-FIELDNAME EQ 'STELL'
       OR SORTFIELDTAB-FIELDNAME EQ 'SACHA'
       OR SORTFIELDTAB-FIELDNAME EQ 'SACHP'
       OR SORTFIELDTAB-FIELDNAME EQ 'SACHZ'.


* start of deletion note 412813
*     field_tab_header-ref_tabname = sortfieldtab-tabname.
*     field_tab_header-ref_fieldname = sortfieldtab-fieldname.
* end of deletion note 412813
      CONCATENATE SORTFIELDTAB-FIELDNAME 'TXT' INTO
        FIELD_TAB_HEADER-FIELDNAME SEPARATED BY '_'.
      FIELD_TAB_HEADER-OUTPUTLEN = 30.
* start of insertion note 412813
      FIELD_TAB_HEADER-DATATYPE = 'CHAR'.
      FIELD_TAB_HEADER-INTLEN = '30'.
      FIELD_TAB_HEADER-LOWERCASE   = 'X'.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          TABNAME              = SEL_TAB-TABNAME
          FIELDNAME            = SEL_TAB-FIELDNAME
        TABLES
          DFIES_TAB            = L_FIELDINFO
*      EXCEPTIONS
*        NOT_FOUND            = 1
*        INTERNAL_ERROR       = 2
*        OTHERS               = 3
                .
      READ TABLE L_FIELDINFO INTO L_FIELDINFO_LINE INDEX 1.
      FIELD_TAB_HEADER-ROLLNAME = L_FIELDINFO_LINE-ROLLNAME.
* end of insertion note 412813

* für diese Felder im IT 1 existiert keine Textleseroutine
    ELSE.
      FIELD_TAB_HEADER-REF_TABNAME = SORTFIELDTAB-TABNAME.
      FIELD_TAB_HEADER-FIELDNAME   = SORTFIELDTAB-FIELDNAME.
    ENDIF.
* end of insertion (PH0K002850)
    PERFORM APPEND_FIELD_TAB USING SW_SUBTO.
  ENDLOOP.
*-----übrige Organisationseinheiten
  LOOP AT SEL_TAB.
    READ TABLE SORTFIELDTAB WITH KEY TABNAME   = SEL_TAB-TABNAME
                                     FIELDNAME = SEL_TAB-FIELDNAME.
    CHECK SY-SUBRC NE 0.
    CLEAR FIELD_TAB_HEADER.
* begin of deletion (PH0K002850)
*    IF sel_tab-fieldname EQ 'VDSK1'                "XPXPH4K013099
*      OR sel_tab-fieldname EQ 'JUPER'               "XPXPH4K013099
*      OR sel_tab-fieldname EQ 'MSTBR'               "XPXPH4K013099
*      OR sel_tab-fieldname EQ 'OTYPE'               "XPXPH4K013099
*      OR sel_tab-fieldname EQ 'SBMOD'               "XPXPH4K013099
*      OR sel_tab-fieldname EQ 'KOKRS'               "XPXPH4K013099
*       OR sel_tab-fieldname EQ 'FISTL'              "XPXPH4K013099
*       OR sel_tab-fieldname EQ 'GEBER'.             "XPXPH4K013099
*      field_tab_header-ref_tabname = sel_tab-tabname.
*      field_tab_header-fieldname   = sel_tab-fieldname.
*      field_tab_header-no_out      = 'X'.
*    ELSE.
*     field_tab_header-ref_tabname = sel_tab-tabname."XPXPH4K013099
*     field_tab_header-ref_fieldname = sel_tab-fieldname"XPXPH4K013
*     CONCATENATE sel_tab-fieldname 'TXT' INTO       "XPXPH4K013099
*         field_tab_header-fieldname SEPARATED BY '_'.    "XPXPH4K013099
*     field_tab_header-outputlen = 30.  "XPXPH4K013099
*     field_tab_header-no_out      = 'X'.
*    ENDIF.                             "XPXPH4K013099
* end of deletion (PH0K002850)

* begin of insertion (PH0K002850)
* Aufzählung aller Felder im IT 1 für die eine Textleseroutine existiert
    IF    SEL_TAB-FIELDNAME EQ 'BUKRS'
       OR SEL_TAB-FIELDNAME EQ 'WERKS'
       OR SEL_TAB-FIELDNAME EQ 'BTRTL'
       OR SEL_TAB-FIELDNAME EQ 'PERSG'
       OR SEL_TAB-FIELDNAME EQ 'PERSK'
       OR SEL_TAB-FIELDNAME EQ 'GSBER'
       OR SEL_TAB-FIELDNAME EQ 'ABKRS'
       OR SEL_TAB-FIELDNAME EQ 'ANSVH'
       OR SEL_TAB-FIELDNAME EQ 'KOSTL'
       OR SEL_TAB-FIELDNAME EQ 'ORGEH'
       OR SEL_TAB-FIELDNAME EQ 'PLANS'
       OR SEL_TAB-FIELDNAME EQ 'STELL'
       OR SEL_TAB-FIELDNAME EQ 'SACHA'
       OR SEL_TAB-FIELDNAME EQ 'SACHP'
       OR SEL_TAB-FIELDNAME EQ 'SACHZ'.

* start of deletion note 412813
*     field_tab_header-ref_tabname = sel_tab-tabname.
*     field_tab_header-ref_fieldname = sel_tab-fieldname.
* end of deletion note 412813
      CONCATENATE SEL_TAB-FIELDNAME 'TXT' INTO
        FIELD_TAB_HEADER-FIELDNAME SEPARATED BY '_'.
      FIELD_TAB_HEADER-OUTPUTLEN = 30.
      FIELD_TAB_HEADER-NO_OUT      = 'X'.
* start of insertion note 412813
      FIELD_TAB_HEADER-DATATYPE = 'CHAR'.
      FIELD_TAB_HEADER-INTLEN = '30'.
      FIELD_TAB_HEADER-LOWERCASE   = 'X'.
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING
          TABNAME              = SEL_TAB-TABNAME
          FIELDNAME            = SEL_TAB-FIELDNAME
        TABLES
          DFIES_TAB            = L_FIELDINFO
*      EXCEPTIONS
*        NOT_FOUND            = 1
*        INTERNAL_ERROR       = 2
*        OTHERS               = 3
                .
      READ TABLE L_FIELDINFO INTO L_FIELDINFO_LINE INDEX 1.
      FIELD_TAB_HEADER-ROLLNAME = L_FIELDINFO_LINE-ROLLNAME.
* end of insertion note 412813
* für diese Felder im IT 1 existiert keine Textleseroutine
    ELSE.
      FIELD_TAB_HEADER-REF_TABNAME = SEL_TAB-TABNAME.
      FIELD_TAB_HEADER-FIELDNAME   = SEL_TAB-FIELDNAME.
      FIELD_TAB_HEADER-NO_OUT      = 'X'.
    ENDIF.
* end of insertion (PH0K002850)

    PERFORM APPEND_FIELD_TAB USING SPACE.
  ENDLOOP.

  CLEAR FIELD_TAB_HEADER.                                   "PH0K002850
*-----Organisationseinheiten (tech. Kürzel)               "XPXPH4K013099
*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'BUKRS'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-BUK.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'WERKS'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-WER.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'BTRTL'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-BTR.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099


*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'PERSG'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-PES.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'PERSK'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-PER.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099


*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'GSBER'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-GSB.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099


*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'ABKRS'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-ABK.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'ANSVH'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-ANS.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'KOSTL'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-KOS.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'ORGEH'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-ORG.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'PLANS'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-PLA.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'STELL'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-STE.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099


*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'SACHA'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-SAA.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'SACHP'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-SAP.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*  field_tab_header-tabname = 'P0001'.  "XPXPH4K013099   "note 309613
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.                "note 309613
  FIELD_TAB_HEADER-FIELDNAME = 'SACHZ'."XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-SAZ.               "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099

*-----Standardfelder
  CLEAR FIELD_TAB_HEADER.              "XPXPH4K013099
  FIELD_TAB_HEADER-REF_TABNAME = 'T508S'.                 "XPXPH4K013099
  FIELD_TAB_HEADER-REF_FIELDNAME = 'RTEXT'.               "XPXPH4K013099
  FIELD_TAB_HEADER-FIELDNAME = 'S_TEXT'.                  "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT = 'X'.       "XPXPH4K013099
  PERFORM APPEND_FIELD_TAB USING SPACE."XPXPH4K013099
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'P0007'.
  FIELD_TAB_HEADER-FIELDNAME   = 'SCHKZ'.
  FIELD_TAB_HEADER-REF_FIELDNAME = 'SCHKZ'.               "XPXPH4K013099
  FIELD_TAB_HEADER-REPTEXT_DDIC = TEXT-SCH.               "XPXPH4K013099
  FIELD_TAB_HEADER-OUTPUTLEN = 30.     "XPXPH4K013099
  FIELD_TAB_HEADER-NO_OUT      = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'P0002'.
  FIELD_TAB_HEADER-FIELDNAME   = 'GESCH'.
  FIELD_TAB_HEADER-OUTPUTLEN   = 10.
  FIELD_TAB_HEADER-NO_OUT      = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.
  FIELD_TAB_HEADER-FIELDNAME   = 'PERNR'.
  FIELD_TAB_HEADER-KEY         = 'X'.
  FIELD_TAB_HEADER-KEY_SEL     = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'P0001'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ENAME'.
  FIELD_TAB_HEADER-OUTPUTLEN   = 20.
  FIELD_TAB_HEADER-KEY         = 'X'.
  FIELD_TAB_HEADER-KEY_SEL     = 'X'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'RPCLXXXX'.
  FIELD_TAB_HEADER-FIELDNAME   = 'CLSTB2_PER'.
  PERFORM APPEND_FIELD_TAB USING SPACE.

*-----Export der Grafikdaten in's memory
*  EXPORT graf_title graf_type graf_tab TO MEMORY ID 'RPTBAL00_GRAF'.
  PERFORM FILL_GRAPH_TABLES.
*-----Listausgabe
  IF NOT SW_ZES IS INITIAL.
*-----Zeitarten
    HEADER_ALV_WA-INFO = 'Tagessalden'(HD1).
    PERFORM FILL_ZES_HEADER.
  ELSEIF NOT SW_SAL IS INITIAL.
*-----Zeitsalden
    HEADER_ALV_WA-INFO = 'Kumulierte Salden'(HD2).
    PERFORM FILL_SALDO_HEADER.
  ELSEIF NOT SW_ZL IS INITIAL.
*-----Zeitlohnarten
    HEADER_ALV_WA-INFO = 'Zeitlohnarten'(HD3).
    PERFORM FILL_ZL_HEADER.
  ENDIF.

*-----Listheader
  DATA: BEGDA(10) TYPE C,
        ENDDA(10) TYPE C.
  HEADER_ALV_WA-TYP  = 'H'.
  APPEND HEADER_ALV_WA TO HEADER_ALV.
  HEADER_ALV_WA-TYP  = 'S'.
  HEADER_ALV_WA-KEY  = 'Datenauswahlzeitraum'(HD4).
  WRITE PN-BEGDA TO BEGDA DD/MM/YYYY.
  WRITE PN-ENDDA TO ENDDA DD/MM/YYYY.
  CONCATENATE BEGDA '-' ENDDA INTO HEADER_ALV_WA-INFO
              SEPARATED BY SPACE.
  APPEND HEADER_ALV_WA TO HEADER_ALV.

**** -- test
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*       EXPORTING
*            I_CALLBACK_PROGRAM       = SY-CPROG
*            I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
*            IS_LAYOUT                = GS_LAYOUT
*            IT_FIELDCAT              = FIELD_TAB
*            IT_SORT                  = GT_ZSUM[]
*            I_SAVE                   = 'A'
**           IS_VARIANT               = ALV_VARIANT
*            IS_VARIANT               = ES_VARIANT
*       TABLES
*            T_OUTTAB                 = <DATA_TAB>
*       EXCEPTIONS
*            PROGRAM_ERROR            = 1
*            OTHERS                   = 2.

*TIME_DATA_ZES
*TIME_DATA_SALDO
*TIME_DATA_ZL
*DATA_TAB

*  FREE MEMORY ID 'HRM'.

  EXPORT TIME_DATA_ZES   = TIME_DATA_ZES
         TIME_DATA_SALDO = TIME_DATA_SALDO
         TIME_DATA_ZL    = TIME_DATA_ZL
         DATA_TAB        = DATA_TAB
         TO MEMORY ID 'HRM'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM set_pf_status                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  extab                                                         *
*---------------------------------------------------------------------*
FORM SET_PF_STATUS USING EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD_GRID' EXCLUDING EXTAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = HEADER_ALV.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ucomm                                                         *
*  -->  selfield                                                      *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.
  CASE UCOMM.
    WHEN '1GRF'.
      CALL FUNCTION 'GRAPH_MATRIX'
           EXPORTING
                DIM1       = GRAF_TYPE
                DIM2       = 'Zeitabrechnungsperioden'(ZAP)
                DIM3       = SPACE
                MAX1       = MAX1
                MAX2       = MAX2
                MAX3       = 10
                TITL       = GRAF_TITLE
                VALT       = 'Anzahl in Stunden'(AIS)
                WINPOS     = 5
                WINSZX     = 80
                WINSZY     = 90
                MAIL_ALLOW = 'X'
           TABLES
                DATA       = DATA
                OPTS       = OPTS
                TDIM1      = DIM1
                TDIM2      = DIM2
                TDIM3      = DIM3.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_graph_tables                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_GRAPH_TABLES.
*-----Import der Auswertungsergebnisse
  LOOP AT GRAF_TAB.
*-----Füllen der Anzahltabelle
    DATA-ANZHL = GRAF_TAB-ANZHL.
    APPEND DATA.
*-----Füllen der 1. Dimension - Zeit-/Lohnarten
    DIM1-IDENT = GRAF_TAB-IDENT.
    COLLECT DIM1.
*-----Füllen der 2. Dimension - Perioden
    IF DIM2-PERIO NE GRAF_TAB-PERIO.
      DIM2-PERIO = GRAF_TAB-PERIO.
      APPEND DIM2.
    ENDIF.
  ENDLOOP.
*-----maximale Spaltenzahlen der Dimnesionen 1 und 2
  DESCRIBE TABLE DIM1 LINES MAX1.
  DESCRIBE TABLE DIM2 LINES MAX2.
*-----Grundeinstellungen für die Diagramm-Darstellung
  OPTS-OPKEY = 'P3CTYP = CO'. APPEND OPTS.  "3D - Farben nach Spalten
  OPTS-OPKEY = 'P2TYPE = VB'. APPEND OPTS.  "2D - Senkrechte Säulen
  OPTS-OPKEY = 'P2TYPE = TO'. APPEND OPTS.                  "3D - Türme
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_zes_header                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_ZES_HEADER.
*-----Datum
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'SYST'.
  FIELD_TAB_HEADER-FIELDNAME   = 'DATUM'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Zeitart
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2B6'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ZTART'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Zeitartentext
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'T555B'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ZTEXT'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Stundenanzahl
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2B6'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ANZHL'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '11'.
  FIELD_TAB_HEADER-DO_SUM      = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_saldo_header                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_SALDO_HEADER.
*-----Zeitart
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2B5'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ZTART'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '0'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Zeitartentext
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'T555B'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ZTEXT'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '0'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Stundenanzahl
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2B5'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ANZHL'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '11'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_zl_header                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FILL_ZL_HEADER.
*-----Datum
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'SYST'.
  FIELD_TAB_HEADER-FIELDNAME   = 'DATUM'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '0'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Lohnart
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2BF'.
  FIELD_TAB_HEADER-FIELDNAME   = 'LGART'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '0'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Lohnartentext
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'T512T'.
  FIELD_TAB_HEADER-FIELDNAME   = 'LGTXT'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '0'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = SPACE.
  PERFORM APPEND_FIELD_TAB USING SPACE.
*-----Stundenanzahl
  CLEAR FIELD_TAB_HEADER.
  FIELD_TAB_HEADER-REF_TABNAME = 'PC2BF'.
  FIELD_TAB_HEADER-FIELDNAME   = 'ANZHL'.
  FIELD_TAB_HEADER-OUTPUTLEN   = '11'.
  FIELD_TAB_HEADER-NO_OUT      = SPACE.
  FIELD_TAB_HEADER-DO_SUM      = 'X'.
  PERFORM APPEND_FIELD_TAB USING SPACE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM display_errors                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_ERRORS.
*-----Sind Fehler aufgetreten?
  DESCRIBE TABLE ERROR_TAB LINES SY-TFILL.
  CHECK SY-TFILL GT 0.
*-----Ausgabe einer Fehlerliste
  CALL FUNCTION 'HR_DISPLAY_ERROR_LIST'
       TABLES
            ERROR  = ERROR_TAB
       EXCEPTIONS
            OTHERS = 1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM error_handling                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  pernr                                                         *
*  -->  arbgb                                                         *
*  -->  msgty                                                         *
*  -->  msgno                                                         *
*  -->  msgv1                                                         *
*  -->  msgv2                                                         *
*  -->  msgv3                                                         *
*  -->  msgv4                                                         *
*---------------------------------------------------------------------*
FORM ERROR_HANDLING USING PERNR ARBGB MSGTY MSGNO
                          MSGV1 MSGV2 MSGV3 MSGV4.
  CLEAR ERROR_TAB.
  ERROR_TAB-PERNR = PERNR.
  ERROR_TAB-ARBGB = ARBGB.
  ERROR_TAB-MSGTY = MSGTY.
  ERROR_TAB-MSGNO = MSGNO.
  ERROR_TAB-MSGV1 = MSGV1.
  ERROR_TAB-MSGV2 = MSGV2.
  ERROR_TAB-MSGV3 = MSGV3.
  ERROR_TAB-MSGV4 = MSGV4.
  APPEND ERROR_TAB.
  REJECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM fill_schkz_text                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  value(f_p0007-schkz)                                          *
*  -->  value(f_schkz_text)                                           *
*---------------------------------------------------------------------*
FORM FILL_SCHKZ_TEXT USING VALUE(F_P0007-SCHKZ)           "XPXPH4K013099
                                    LIKE P0007-SCHKZ      "XPXPH4K013099
CHANGING VALUE(F_SCHKZ_TEXT) LIKE SCHKZ_TEXT-S_TEXT.      "XPXPH4K013099

*----Grp. Mitarbeiterkreise für AZPregel                  "XPXPH4K013099
PERFORM RE503 USING P0001-PERSG P0001-PERSK.              "XPXPH4K013099

*----Lesen des AZPregeltextes                             "XPXPH4K013099
PERFORM RE508S USING T503-ZEITY T001P-MOFID               "XPXPH4K013099
                     T001P-MOSID F_P0007-SCHKZ.           "XPXPH4K013099
*----Füllen der Struktur                                  "XPXPH4K013099
  F_SCHKZ_TEXT = T508S-RTEXT.            "XPXPH4K013099
ENDFORM.

INCLUDE RPTBAL03.  "Routinen zum Lesen von Datenbanktabellen
INCLUDE RPUMKC00.  "Aufruf von Merkmalen und Rückgabe ihrer Werte
