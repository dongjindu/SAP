*&--------------------------------------------------------------------
*& Author                 : HS.Jeong
*& Creation Date          : 25/03/2004
*& Specification By       : hs.jeong
*& Pattern                : Report 1-2
*& Development Request No : UD1K908701
*& Addl documentation     :
*& Description  : KPI Report
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zcfii14 MESSAGE-ID  zmfi.

*FUNCTION AIAW_MANF_PROFITABILITY.
tables: imav, imak, impr.
*IMPORTING
DATA:
I_OBJNR  LIKE  IMAV-OBJNR,
I_IVART  LIKE  IMAK-IVART,
I_KOKRS  LIKE  TKA01-KOKRS,
I_ONLINE TYPE  XFELD.


*EXPORTING
DATA:
E_KAPITALWERT TYPE  F,
E_INTERNER_ZINSFUSS TYPE  F,
E_AMORTISATIONSDAUER LIKE  IMAV-AMORT,
E_KAPITALWERTRATE TYPE  F,
E_WIDAT LIKE  TAIF1-WIDAT.

tables: tka01.
*"  EXCEPTIONS
*"      DATA_READ_FAULT
*"      DATA_CALCULATE_FAULT
*"      DATA_MISSING
*"      PAY_OFF_TIME_ERROR
*"----------------------------------------------------------------------
*********************************************************************
*                        globale Konstanten
*********************************************************************
CONSTANTS:
  G_ZINSKURVE1 TYPE I VALUE '9990',
  G_ZINSKURVE2 TYPE I VALUE '9991',
  G_CONVERT_FACTOR type i value '100'.   "Konvertierung für Treasury

*********************************************************************
*                        globale Variablen
*********************************************************************
DATA:
  G_OKCODE(10) TYPE C.

*********************************************************************
*                        lokale Konstanten
*********************************************************************
CONSTANTS: C_EVAL_TYPE(4) TYPE C VALUE 'IM01'.

*********************************************************************
*                        lokale Variablen
*********************************************************************
DATA: L_DFAELL LIKE JBRBEWEG-DFAELL, "Hilfsfeld für das Datum
      L_IND LIKE SY-TABIX,           "Index-Zähler
      L_DATUM LIKE TAIF1-WIDAT,      "Datum der Wi.Rechnung
      L_SUMME_KOSTEN TYPE F,         "Summe der Kosten
      L_AMORTISATION LIKE SY-DATUM,  "Amortisationszeitpunkt
      L_MONAT TYPE MONAT,            "Monatsfeld
      L_BEGIN_OF_INVEST LIKE SY-DATUM,    "Start der Investition
      L_UNIT LIKE T006-MSEHI VALUE 'MON', "Für Berechnung der Amor.
      L_DURATION TYPE P,
      L_FLG_DB LIKE SY-SUBRC,
      L_FLG_DIALOG LIKE SY-SUBRC,
      L_SUBRC_IZF LIKE SY-SUBRC,                            "QJS270599
      L_SUBRC_AD LIKE SY-SUBRC,                             "QJS270599
      L_MIDDLE_DAY LIKE SY-DATUM,                           "QJS200599
      L_GJAHR LIKE T009B-BDATJ,                             "QJS200599
      L_ANZAHL_PERIODEN LIKE T009-ANZBP,                    "QJS200599
      L_ANZAHL_PERIODEN_NUM TYPE I,                         "QJS200599
      L_LFD_PERIODE TYPE T009B-POPER,                       "QJS200599
      L_IZFUS   like imav-izfus,                            "NOTE195207
      L_KAPWRT  like imav-kapwrt,                           "NOTE195207
      L_KAPRAT  like imav-kaprat,                           "NOTE195207
      L_AMORT   like imav-amort,                            "NOTE195207
      L_ERROR TYPE XFELD.            "Plandaten überschreiten
"Treasury-Format


TYPE-POOLS AIAW.
*********************************************************************
*                        lokale Strukturen
*********************************************************************
DATA: LSX_PLANINGVALUE_INSERT TYPE AIAW_TYPE_SX_PLANINGVALUE,
      LSX_PLANINGVALUE_UPDATE TYPE AIAW_TYPE_SX_PLANINGVALUE,
      LSX_PLANINGVALUE_DB     TYPE AIAW_TYPE_SX_PLANINGVALUE,
      LSX_PLANINGVALUE        TYPE AIAW_TYPE_SX_PLANINGVALUE.

*********************************************************************
*                        lokale Tabellen
*********************************************************************
DATA: BEGIN OF LT_JA_KOSTEN OCCURS 0."Tabelle für die Jahreswerte
        INCLUDE STRUCTURE BPJA.      "Kosten
DATA: END OF LT_JA_KOSTEN.

DATA: BEGIN OF LT_JA_ERTRAG OCCURS 0."Tabelle für die Jahreswerte
        INCLUDE STRUCTURE BPJA.      "Erträge
DATA: END OF LT_JA_ERTRAG.

DATA: BEGIN OF LT_DATA OCCURS 0.     "Schnittstellentabelle
        INCLUDE STRUCTURE JBRBEWEG.  "für den Kapitalwert
DATA: END OF LT_DATA.

DATA: BEGIN OF LT_DATA1 OCCURS 0.    "Schnittstellentabelle
        INCLUDE STRUCTURE JBRBEWEG.  "für das eingesetzte Kapital
DATA: END OF LT_DATA1.
****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS : s_prnam   FOR  impr-prnam OBLIGATORY.
selection-screen  skip 1.
Parameters: p_POSNR like imav-posnr,
            P_VARNT like imav-varnt,
            p_datum like sy-datum.
SELECT-OPTIONS:
  s_ivart   FOR   imak-ivart,
  s_posnr   FOR   imak-posnr,
  s_vkostl  FOR   imak-vkostl,      "res c c
  s_VPRCTR  for   imak-vprctr.     "res p c
SELECTION-SCREEN END OF BLOCK b0.

*********************************************************************
*                        Ablauflogik
*********************************************************************
select single * from imak
  where posnr = p_posnr.

select single * from imav
  where posnr = p_posnr
    and varnt = p_varnt.

check sy-subrc = 0.
I_OBJNR  = IMAV-OBJNR.
I_IVART  = IMAK-IVART.
I_KOKRS  = IMAK-VKOKRS.
I_ONLINE = ' '.


*--------------------------------------------------------------------
*                SOURCE DATA reading in
*--------------------------------------------------------------------
perform read_source_data.
*--------------------------------------------------------------------
*                       Default Data
*--------------------------------------------------------------------
*-Appropriation request types : determine reference date
SELECT SINGLE WIDAT FROM TAIF1 INTO L_DATUM
                               WHERE IVART = I_IVART. "Ar request type

*-if initial, use today.
IF L_DATUM IS INITIAL.
  L_DATUM = SY-DATUM.
ENDIF.

* use parameter
if not p_datum is initial.
  l_datum = p_datum.
endif.

*--------------------------------------------------------------------
*                       PRESENT VALUE compute
*--------------------------------------------------------------------
perform compute_NPV.


*---------------------------------------------------------------------
*                 PRESENT VALUE RATE compute
*---------------------------------------------------------------------
perform compute_DCFR.


*--------------------------------------------------------------------
*               INTERNAL INTEREST RATE compute
*--------------------------------------------------------------------
perform compute_IRR.

*--------------------------------------------------------------------
*              CASH RECOVERY PERIOD compute
*--------------------------------------------------------------------
perform compute_cash_recovery.



*-reference date
E_WIDAT = L_DATUM.


perform display_result.


*&---------------------------------------------------------------------*
*&      Form  read_source_data
*&---------------------------------------------------------------------*
FORM read_source_data.
*-Kostenrechnungskreiswährung ermitteln
IF TKA01-KOKRS IS INITIAL.
  SELECT SINGLE * FROM TKA01 WHERE KOKRS = I_KOKRS.
ENDIF.

**-first check whether the variant of one copies on the data base is
** preesent already.
CALL FUNCTION 'AIA9_DB_PLANINGVALUE'
     EXPORTING
          I_ACTION                = AIAW_CON_S_ACTION-READ
          I_OBJNR                 = I_OBJNR
          I_VERSN                 = AIAW_CON_S_VERSN-VARIANT
          IS_TKA01                = TKA01
     IMPORTING
          E_SUBRC                 = L_FLG_DB
     CHANGING
          CSX_DIALOG_PLANINGVALUE = LSX_PLANINGVALUE_DB
     EXCEPTIONS
          DB_READ_FAULT           = 1
          OTHERS                  = 2.

IF SY-SUBRC EQ 1.                    "Lesefehler aufgetreten
  write:/ 'Data read fault!!!'.
  exit.
*   RAISE DATA_READ_FAULT.
ENDIF.

* if this variant was changed by planning from master data once,
* and/or is the variant a copied variant, then an entry exists
* in glob. Tab.
CALL FUNCTION 'AIA9_DIALOG_PLANINGVALUE'
     EXPORTING
          I_ACTION                       = AIAW_CON_S_ACTION-READ
          I_OBJNR                        = I_OBJNR
          I_VERSN                        = AIAW_CON_S_VERSN-VARIANT
     IMPORTING
          E_SUBRC                        = L_FLG_DIALOG
     CHANGING
          CSX_DIALOG_PLANINGVALUE_INSERT = LSX_PLANINGVALUE_INSERT
          CSX_DIALOG_PLANINGVALUE_UPDATE = LSX_PLANINGVALUE_UPDATE.

*-the updates and INSERT table are added to the railway table,
* thus one receives the current conditions of the budget values
CALL FUNCTION 'AIA9_PLANINGVALUE_ADD'
     EXPORTING
          I_OBJTYP                = AIAW_CON_S_OBJECT-VARIANT
          ISX_PLANINGVALUE_INSERT = LSX_PLANINGVALUE_INSERT
          ISX_PLANINGVALUE_UPDATE = LSX_PLANINGVALUE_UPDATE
          ISX_PLANINGVALUE_DB     = LSX_PLANINGVALUE_DB
     IMPORTING
          ESX_PLANINGVALUE        = LSX_PLANINGVALUE.


*-Check whether budget values fit into the reduced Currency format
* of the Treasury.
CALL FUNCTION 'AIAW_CHECK_PLANNING_VALUES'
     EXPORTING
          I_LSX_PLANINGVALUE = LSX_PLANINGVALUE
     IMPORTING
          E_ERROR            = L_ERROR.


*-If the budget values are too large,
* they are converted into the Treasury format.
IF L_ERROR = 'X'.

  CALL FUNCTION 'AIAW_CONVERT_PLANNING_VALUES'
       CHANGING
            LSX_PLANINGVALUE = LSX_PLANINGVALUE.
  .

ENDIF.


*-Costs and yields take over (yearly)
LT_JA_KOSTEN[] = LSX_PLANINGVALUE-GESKOS_BPJA[].
LT_JA_ERTRAG[] = LSX_PLANINGVALUE-ERTRAG_BPJA[].

*-Nullen entfernen
DELETE LT_JA_KOSTEN[] WHERE WLJHR = 0.
DELETE LT_JA_ERTRAG[] WHERE WLJHR = 0.

*-if no values are there, break off then the FB and spend message
IF LT_JA_ERTRAG[] IS INITIAL AND LT_JA_KOSTEN[] IS INITIAL.
  write:/ 'No plan values exist for combined costs or revenue'.
*    MESSAGE ID 'AO' TYPE 'I' NUMBER '258' RAISING DATA_MISSING.
ENDIF.

ENDFORM.                    " read_source_data
*&---------------------------------------------------------------------*
*&      Form  compute_NPV
*&---------------------------------------------------------------------*
FORM compute_NPV.
*-Currency
LT_DATA-SBWHR = TKA01-WAERS.

*-convert costs: yearly data into daily data
LOOP AT LT_JA_ERTRAG.
  CLEAR LT_DATA.

*---middle due date determine with fiscal year variant 2003/07/02
*    CONCATENATE lt_ja_ertrag-gjahr '0702' INTO lt_data-dfaell.
  CALL FUNCTION 'AIAW_CALCULATE_DFAELL'
       EXPORTING
            I_VARI            = TKA01-LMONA
            I_GJAHR           = LT_JA_ERTRAG-GJAHR
       IMPORTING
            E_DFAELL          = LT_DATA-DFAELL
       EXCEPTIONS
            WRONG_CUSTOMIZING = 1
            OTHERS            = 2.

  IF LT_JA_ERTRAG-WLJHR GE 0.
    LT_DATA-SSIGN = '+'.
  ELSE.
    LT_DATA-SSIGN = '-'.
  ENDIF.

  LT_DATA-BBWHR = LT_JA_ERTRAG-WLJHR. "amount
  LT_DATA-SBWHR = TKA01-WAERS.

  APPEND LT_DATA.
ENDLOOP.


*-convert revenue: yearly data into daily data
CLEAR L_SUMME_KOSTEN.
LOOP AT LT_JA_KOSTEN.

*    CONCATENATE lt_ja_kosten-gjahr '0702' INTO l_dfaell.
  CALL FUNCTION 'AIAW_CALCULATE_DFAELL'
       EXPORTING
            I_VARI            = TKA01-LMONA
            I_GJAHR           = LT_JA_KOSTEN-GJAHR
       IMPORTING
            E_DFAELL          = L_DFAELL
       EXCEPTIONS
            WRONG_CUSTOMIZING = 1
            OTHERS            = 2.


*-check in the yield table whether for this year already
* a yield exists.
  READ TABLE LT_DATA WITH KEY DFAELL = L_DFAELL.
  L_IND = SY-TABIX.

*---if, then take from it the costs off
  IF SY-SUBRC IS INITIAL.
    LT_DATA-BBWHR = LT_DATA-BBWHR - LT_JA_KOSTEN-WLJHR.
    IF LT_DATA-BBWHR < 0.
      LT_DATA-SSIGN = '-'.
    ELSE.
      LT_DATA-SSIGN = '+'.
    ENDIF.
    MODIFY LT_DATA INDEX L_IND.

*---if no, then write the costs simply into the table
  ELSE.
    IF LT_JA_KOSTEN-WLJHR < 0.       "in diesem Fall dann Ertrag
      LT_DATA-SSIGN = '+'.
    ELSE.
      LT_DATA-SSIGN = '-'.
    ENDIF.
    LT_DATA-BBWHR = LT_JA_KOSTEN-WLJHR * ( -1 ).
    LT_DATA-DFAELL = L_DFAELL.
    LT_DATA-SBWHR = TKA01-WAERS.
    APPEND LT_DATA.
  ENDIF.

*---Datentabelle für die Berechnung des eingesetzten
*---Kapitals (=ab- bzw. aufgezinste Kosten-Reihe) füllen
  IF LT_JA_KOSTEN-WLJHR < 0.         "in diesem Fall Ertrag
    LT_DATA1-SSIGN = '+'.
  ELSE.
    LT_DATA1-SSIGN = '-'.
  ENDIF.
*---Der Betrag wird negativiert, damit die Soll-zinsen bei der Auf
*---bzw. Abzinsung verwendet wird.
  LT_DATA1-BBWHR = LT_JA_KOSTEN-WLJHR * ( -1 ).
  LT_DATA1-DFAELL = L_DFAELL.
  LT_DATA1-SBWHR = TKA01-WAERS.
  APPEND LT_DATA1.

ENDLOOP.

SORT LT_DATA BY DFAELL.
SORT LT_DATA1 BY DFAELL.

*-Call of the FB from the Treasury for the economy calculation
* present value computation
CALL FUNCTION 'RM_OP_KAPITALWERT_IM'
     EXPORTING
          DATE                 = L_DATUM
          EVAL_TYPE            = C_EVAL_TYPE                "IM01
          RESULT_CURRENCY      = TKA01-WAERS
     IMPORTING
          KAPITALWERT          = E_KAPITALWERT
     TABLES
          FLOWS                = LT_DATA  "Cash Flow Tabelle
     EXCEPTIONS
          ERROR_IN_MARKET_DATA = 1
          NO_FLOWS             = 2
          OTHERS               = 3.

IF NOT SY-SUBRC IS INITIAL.
  write:/ 'Error while calculating the net present value'.
ENDIF.

* if error flag for to large budget values is flown a flag,
* the present value by the global conversion actuator multiply
IF L_ERROR = 'X'.
  E_KAPITALWERT = E_KAPITALWERT *  G_CONVERT_FACTOR .
ENDIF.

catch system-exceptions CONVT_OVERFLOW = 1.
  L_KAPWRT = e_kapitalwert.
endcatch.
if sy-subrc = 1.
  clear e_kapitalwert.
endif.

ENDFORM.                    " compute_NPV
*&---------------------------------------------------------------------*
*&      Form  compute_DCFR
*&---------------------------------------------------------------------*
FORM compute_DCFR.
*Call of the FB from the Treasury for the economy calculation
* computation of the assigned capital
CALL FUNCTION 'RM_OP_KAPITALWERT_IM'
     EXPORTING
          DATE                 = L_DATUM
          EVAL_TYPE            = C_EVAL_TYPE
          RESULT_CURRENCY      = TKA01-WAERS
     IMPORTING
          KAPITALWERT          = L_SUMME_KOSTEN
     TABLES
          FLOWS                = LT_DATA1  "Cost table
     EXCEPTIONS
          ERROR_IN_MARKET_DATA = 1
          NO_FLOWS             = 2
          OTHERS               = 3.

IF NOT SY-SUBRC IS INITIAL.
  write:/ 'Error while calculating the net present value'.
*   MESSAGE ID 'AO' TYPE 'E' NUMBER '215'
*           RAISING DATA_CALCULATE_FAULT.
ENDIF.

IF L_ERROR = 'X'.
  L_SUMME_KOSTEN = L_SUMME_KOSTEN *  G_CONVERT_FACTOR .
ENDIF.

*-The sum of the costs is seen positive with the present value rate
L_SUMME_KOSTEN = L_SUMME_KOSTEN * ( -1 ).


*-Present value rate -------------------------------------------------
IF L_SUMME_KOSTEN > 0.
  E_KAPITALWERTRATE = E_KAPITALWERT / L_SUMME_KOSTEN.
*---Test auf Überlauf                                  "NOTE195207
  catch system-exceptions CONVT_OVERFLOW = 1.               "NOTE195207
    L_KAPRAT  = e_kapitalwertrate.                          "NOTE195207
  endcatch.                                                 "NOTE195207
  if sy-subrc = 1.                                          "NOTE195207
    clear e_kapitalwertrate.                                "NOTE195207
  endif.                                                    "NOTE195207

ELSE.
  E_KAPITALWERTRATE = 0.
ENDIF.

ENDFORM.                    " compute_DCFR
*&---------------------------------------------------------------------*
*&      Form  compute_IRR
*&---------------------------------------------------------------------*
FORM compute_IRR.
CALL FUNCTION 'RM_OP_INT_ZINSFUSS_IM'
     IMPORTING
          INT_ZINSFUSS         = E_INTERNER_ZINSFUSS
     TABLES
          FLOWS                = LT_DATA
     EXCEPTIONS
          ERROR_IN_MARKET_DATA = 1
          NO_FLOWS             = 2
          NOT_UNIQUE_CURRENCY  = 3
          NOT_SOLVABLE         = 4
          ERROR_IN_FLOWS       = 5
          OTHERS               = 6.
IF SY-SUBRC <> 0 AND NOT SY-SUBRC = 4.
  write:/ 'Error when calculating the internal rate of return'.
*   MESSAGE ID 'AO' TYPE 'E' NUMBER '216'
*           RAISING DATA_CALCULATE_FAULT.
ELSEIF SY-SUBRC = 4.
  CLEAR E_INTERNER_ZINSFUSS.
  L_SUBRC_IZF = SY-SUBRC.
ENDIF.

*NOTE195207
catch system-exceptions CONVT_OVERFLOW = 1.
  L_IZFUS  = e_interner_zinsfuss.
endcatch.
if sy-subrc = 1.
  clear e_interner_zinsfuss.
endif.


ENDFORM.                    " compute_IRR
*&---------------------------------------------------------------------*
*&      Form  compute_cash_recovery
*&---------------------------------------------------------------------*
FORM compute_cash_recovery.
* AD is time, at that the present value function zero crossover
* exhibits (monthlyscaled) for this purpose the annual cash-flow
* (acceptance in accordance with in the center of the yearly to
* result is), on the accounting periods is evenly distributed

* 1. : Number of accounting periods determine (first year as reference!)
* 2. : middle period date of the first period secure
* 3. : Loop over all periods and distribute the total value evenly
*      on all periods
*--------------------------------------------------------------------

*--------------------------------------------------------------------
*           1. : Number of accounting periods determine
*--------------------------------------------------------------------
* Date of the first cost accumulation read, this date on the center
* of the first financial year monthly this yearly one dates.

*--Special case: only yields planned! consider

READ TABLE LT_DATA1 INDEX 1.
* if no costs are there, the yields take
IF SY-SUBRC NE 0.
  READ TABLE LT_DATA INDEX 1.
  LT_DATA1 = LT_DATA.
ENDIF.

CALL FUNCTION 'AIAW_CALCULATE_DFAELL'
     EXPORTING
          I_VARI            = TKA01-LMONA
*         I_GJAHR           =
          I_DFAELL          = LT_DATA1-DFAELL
     IMPORTING
*         E_DFAELL          =
*         E_FIRSTDAY        =
          E_GJAHR           = L_GJAHR
*    EXCEPTIONS
*         WRONG_CUSTOMIZING = 1
*         OTHERS            = 2
.

CALL FUNCTION 'GET_ACCOUNT_OF_PERIODS'
     EXPORTING
          I_GJAHR        = L_GJAHR
*         I_MONMIT       = 00
          I_PERIV        = TKA01-LMONA
   IMPORTING
        E_ANZBUPER     = L_ANZAHL_PERIODEN
   EXCEPTIONS
        INPUT_FALSE    = 1
        T009B_NOTFOUND = 2
        T009_NOTFOUND  = 3
        OTHERS         = 4
        .
IF SY-SUBRC <> 0.
  write:/ 'Errors occurred during calculation of figures'.
*   MESSAGE ID 'AO' TYPE 'E' NUMBER '479' WITH TKA01-LMONA TKA01-KOKRS
*                    RAISING DATA_CALCULATE_FAULT.
ENDIF.

*----------------------------------------------------------------------
*     2. middle period date of the first accounting period secure
*----------------------------------------------------------------------

CALL FUNCTION 'AIAW_CALCULATE_MIDDLEOFPERIOD'
     EXPORTING
          I_GJAHR           = L_GJAHR
          I_VARI            = TKA01-LMONA
          I_POPER           = '001'
     IMPORTING
          E_MIDDLE_DAY      = L_MIDDLE_DAY
     EXCEPTIONS
          WRONG_CUSTOMIZING = 1
          OTHERS            = 2.
IF SY-SUBRC <> 0.
  write:/ 'Errors occurred during calculation of figures'.
*   MESSAGE ID 'AO' TYPE 'E' NUMBER '479' WITH TKA01-LMONA TKA01-KOKRS
*                    RAISING DATA_CALCULATE_FAULT.
ENDIF.

L_BEGIN_OF_INVEST = L_MIDDLE_DAY.


CLEAR LT_DATA1[].

*----------------------------------------------------------------------
*     3. Loop over all periods and distribute the total value evenly
*        on all periods
*----------------------------------------------------------------------

MOVE L_ANZAHL_PERIODEN TO L_ANZAHL_PERIODEN_NUM.

LOOP AT LT_DATA.

*--Geschäftsjahr ermitteln
  CALL FUNCTION 'AIAW_CALCULATE_DFAELL'
        EXPORTING
             I_VARI            = TKA01-LMONA
*         I_GJAHR           =
             I_DFAELL          = LT_DATA-DFAELL
        IMPORTING
*         E_DFAELL          =
*         E_FIRSTDAY        =
             E_GJAHR           = L_GJAHR
*    EXCEPTIONS
*         WRONG_CUSTOMIZING = 1
*         OTHERS            = 2
.

*--Date on center of the monthly set
  DO L_ANZAHL_PERIODEN_NUM TIMES.
    IF LT_DATA-BBWHR  GE 0.
      LT_DATA1-SSIGN = '+'.
    ELSE.
      LT_DATA1-SSIGN = '-'.
    ENDIF.


    MOVE SY-INDEX TO L_LFD_PERIODE.

    CALL FUNCTION 'AIAW_CALCULATE_MIDDLEOFPERIOD'
         EXPORTING
              I_GJAHR           = L_GJAHR
              I_VARI            = TKA01-LMONA
              I_POPER           = L_LFD_PERIODE
         IMPORTING
              E_MIDDLE_DAY      = L_MIDDLE_DAY
         EXCEPTIONS
              WRONG_CUSTOMIZING = 1
              OTHERS            = 2.
    IF SY-SUBRC <> 0.
      write:/ 'Errors occurred during calculation of figures'.

*    MESSAGE ID 'AO' TYPE 'E' NUMBER '479' WITH TKA01-LMONA TKA01-KOKRS
*                                RAISING DATA_CALCULATE_FAULT.
    ENDIF.


    LT_DATA1-DFAELL = L_MIDDLE_DAY.
    LT_DATA1-BBWHR = LT_DATA-BBWHR / L_ANZAHL_PERIODEN_NUM.
    LT_DATA1-SBWHR = TKA01-WAERS.
    APPEND LT_DATA1.
  ENDDO.
ENDLOOP.

* >>> END OF INSERTION <<<                                     QJS200599

*-Date calculate, when the Inv. amortizes.
CALL FUNCTION 'RM_OP_AMORTISATION_IM'
     EXPORTING
          DATE                 = L_DATUM
          EVAL_TYPE            = C_EVAL_TYPE
          RESULT_CURRENCY      = TKA01-WAERS
     IMPORTING
          DATE_OF_AMORTIZATION = L_AMORTISATION
     TABLES
          FLOWS                = LT_DATA1
     EXCEPTIONS
          NOT_UNIQUE           = 1
          ERROR_IN_MARKET_DATA = 2
          NO_FLOWS             = 3
          NO_AMORTISATION      = 4
          OTHERS               = 5.

*QJS270599
*emergency unique is falsely then released, if egg VZW von Plus is
*present after minus in the unkumulierten payment row.
IF SY-SUBRC =  4.
  CLEAR L_AMORTISATION.
ELSEIF SY-SUBRC = 1.                                        "QJS270599
  CLEAR L_AMORTISATION.                                     "QJS270599
  L_SUBRC_AD = SY-SUBRC.                                    "QJS270599
ELSEIF SY-SUBRC <> 0.
  write:/ 'Error in the calculation of the payback period'.
ENDIF.

*-Duration between start of the Inv. and the point of
* amortization period determine
IF L_AMORTISATION GE L_BEGIN_OF_INVEST.
  CALL FUNCTION 'DURATION_DETERMINE'
       EXPORTING
            UNIT                       = L_UNIT
       IMPORTING
            DURATION                   = L_DURATION
       CHANGING
            START_DATE                 = L_BEGIN_OF_INVEST
            END_DATE                   = L_AMORTISATION
       EXCEPTIONS
            FACTORY_CALENDAR_NOT_FOUND = 1
            DATE_OUT_OF_CALENDAR_RANGE = 2
            DATE_NOT_VALID             = 3
            UNIT_CONVERSION_ERROR      = 4
            SI_UNIT_MISSING            = 5
            PARAMETERS_NOT_VALID       = 6
            OTHERS                     = 7.
  IF SY-SUBRC IS INITIAL.
*        E_AMORTISATIONSDAUER = L_DURATION  / 12.

    E_AMORTISATIONSDAUER = L_DURATION  / L_ANZAHL_PERIODEN_NUM.

*-----Test auf Überlauf                                  "NOTE195207
    catch system-exceptions CONVT_OVERFLOW = 1.             "NOTE195207
      L_AMORT  = e_amortisationsdauer.                      "NOTE195207
    endcatch.                                               "NOTE195207
    if sy-subrc = 1.                                        "NOTE195207
      clear e_amortisationsdauer.                           "NOTE195207
    endif.                                                  "NOTE195207

  ENDIF.
ENDIF.


ENDFORM.                    " compute_cash_recovery


*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
FORM display_result.
* Success-Messages ausgeben
*-message 'Wirt.Kennzahlen wurden berechnet' ausgeben, falls kein
* untergeordneter Fehler aufgetreten ist.
DATA: LS_IMAV TYPE IMAV.

      LS_IMAV-KAPWRT = E_KAPITALWERT.
      LS_IMAV-IZFUS  = E_INTERNER_ZINSFUSS.
      LS_IMAV-AMORT  = E_AMORTISATIONSDAUER.
      LS_IMAV-KAPRAT = E_KAPITALWERTRATE.


  IF I_ONLINE = 'X'.
    IF ( NOT L_SUBRC_IZF IS INITIAL ) AND ( NOT L_SUBRC_AD IS INITIAL ).
      MESSAGE ID 'AO' TYPE 'S' NUMBER '483'.
    ELSEIF ( NOT L_SUBRC_IZF IS INITIAL ).
      MESSAGE ID 'AO' TYPE 'S' NUMBER '298'.
    ELSEIF ( NOT L_SUBRC_AD IS INITIAL ).
      MESSAGE ID 'AO' TYPE 'S' NUMBER '482'.
    ELSE.
      MESSAGE ID 'AO' TYPE 'S' NUMBER '245'.
    ENDIF.
  ELSE.
    Write:/ '*** Result ***'.
    write:/ 'Net present value....', LS_IMAV-KAPWRT.
    write:/ 'IRR..................', LS_IMAV-IZFUS.
    write:/ 'Payback period.......', LS_IMAV-AMORT.
    write:/ 'Disc. cash flow rate.', LS_IMAV-KAPRAT.
    write:/ 'Reference Date', E_WIDAT.


  ENDIF.

ENDFORM.                    " display_result
