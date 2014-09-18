REPORT coruafwp MESSAGE-ID ru
                LINE-SIZE 132
                NO STANDARD PAGE HEADING.
*
* Modified by Andy Choi
*
* - check with SM66, SM12
*
*----------------------------------------------------------------------*
* Change converted to ALV by C5053302, dated 01-03-2004
* Short description of the program: ALV Conversation of the Report
*    - Function/flow of the program
*       'REUSE_ALV_COMMENTARY_WRITE'
*       'REUSE_ALV_FIELDCATALOG_MERGE'
*       'REUSE_ALV_LIST_DISPLAY'
*       'REUSE_ALV_EVENTS_GET'
*---------------------------------------------------------------------*

DATA: affw   TYPE affw,
      affwv0 TYPE affwv0,
      imkpf  TYPE imkpf.

TABLES: resb,
        t156n.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK affw WITH FRAME TITLE text-007.
SELECT-OPTIONS: paautyp FOR affwv0-autyp no-display.
SELECT-OPTIONS: pawerks FOR affwv0-werks no-display.
SELECT-OPTIONS: palgort FOR affwv0-lgort no-display.
SELECTION-SCREEN SKIP 1.
PARAMETER: paanzahl(3) TYPE n DEFAULT 100,
         pamxlock(3) TYPE n DEFAULT 300,
         pagroup     TYPE rzllitab-classname DEFAULT space,
         parallel(2) TYPE n DEFAULT 0,
         parows(5)   TYPE n DEFAULT 0.
SELECTION-SCREEN SKIP 1.
PARAMETER: pafehl   AS CHECKBOX.
PARAMETER: paweonly AS CHECKBOX.
PARAMETER: pawdhl   AS CHECKBOX.
PARAMETER: panoprot AS CHECKBOX.
PARAMETER: paseriel AS CHECKBOX.
PARAMETER: pasimul  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK affw.


*ANDY-start
SELECTION-SCREEN BEGIN OF BLOCK affw1 WITH FRAME TITLE text-007.
SELECT-OPTIONS:
  s_werks FOR affw-werks MEMORY ID wrk,
  s_lgort FOR affw-lgort,
  s_matnr FOR affw-matnr MATCHCODE OBJECT mat1,
  s_dispo FOR affw-dispo,
  s_bwart FOR affw-bwart,
  s_belnr FOR affw-weblnr no-display.

SELECTION-SCREEN BEGIN OF BLOCK para WITH FRAME TITLE text-906.
SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(30) text-001.
SELECTION-SCREEN POSITION 31.
PARAMETERS:
  p_kdauf LIKE affw-kdauf no-display,
  p_kdpos LIKE affw-kdpos no-display.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-920. "Fehler: Id / Nummer
SELECTION-SCREEN POSITION 31.
PARAMETERS:
  p_msgid LIKE t100-arbgb.
SELECTION-SCREEN COMMENT 52(1) text-921. "/
PARAMETERS:
  p_msgno LIKE t100-msgnr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-922. "Fehlerdatum von
SELECTION-SCREEN POSITION 31.
PARAMETERS:
  p_datuv LIKE affw-fwdat,
  p_uzeiv LIKE affw-fwzet.
SELECTION-SCREEN COMMENT 51(5) text-923.                    "bis
PARAMETERS:
  p_datub LIKE affw-fwdat,
  p_uzeib LIKE affw-fwzet.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) text-924. "Buchungsdatum von
SELECTION-SCREEN POSITION 31.
PARAMETERS:
  p_datpv LIKE affw-budat.
SELECTION-SCREEN COMMENT 51(5) text-923.                    "bis
PARAMETERS:
  p_datpb LIKE affw-budat.
SELECTION-SCREEN END OF LINE.

select-options: s_RSNUM for affw-rsnum,
                s_RSPOS for affw-rspos.

SELECTION-SCREEN END OF BLOCK para.

PARAMETERS:
  p_maxrow(4) TYPE n,
  p_debug     type XFELD,
  p_time(5) type n default '10'.
SELECTION-SCREEN END OF BLOCK affw1.
*ANDY-end

* gemeinsame Datendefinitionen für Hintergrundjob und
* Verarbeitung im V2-Verbucher in Funktionsgruppe CORV.
INCLUDE coruafwd.

CONSTANTS:
  ymaxpos TYPE affw-weblpos VALUE 99999999. "max. Positions-Nr

AT SELECTION-SCREEN.

  IF NOT pafehl IS INITIAL AND
     NOT parows IS INITIAL.
*   Schalter 'mit fehlerhaften WB' ist eingeschaltet und für
*   'Max. Anzahl selektierter WB' wurde ein Wert eingegeben
    IF NOT pawdhl IS INITIAL.
*     Schalter 'Job wiederholen' ist eingeschaltet
*     --> ERROR wegen Endlosschleife
      MESSAGE e493.
    ELSE.
*     --> INFORMATION
      IF pasimul IS INITIAL.
        MESSAGE i492.
      ENDIF.
    ENDIF.
  ENDIF.


START-OF-SELECTION.

  IMPORT paautyp pawerks palgort paanzahl parows pafehl pasimul paseriel
         FROM MEMORY ID 'ZCORUAFWP'.
  GET TIME.
  start_dat = sy-datlo.
  start_tim = sy-timlo.
* Warenbewegungen sind aus der AFFW-Tab einzulesen
  CLEAR modus_backgr.
* Für Warenbewegungen aus Prozesskette der Rückmeldung wurden bereits
* im aufrufenden Programm die Sperren abgesetzt
  IF pasimul IS INITIAL.
*   Dummysperreintrag für alle fehlerfreien AFFW-Sätze absetzen
    CLEAR affw-weblnr.
    affw-weblpos = ymaxpos.
    CALL FUNCTION 'ENQUEUE_ESAFFW'
      EXPORTING
        weblnr  = affw-weblnr
        weblpos = affw-weblpos
        _scope  = '3'  "Freigabe Sperre bei Ende TA & VB
      EXCEPTIONS
        OTHERS  = 01.
    IF sy-subrc <> 0.
      MESSAGE e702.
      EXIT.
    ENDIF.
*   Rückmeldeprozess "Wareneingang" exclusiv sperren
    CALL FUNCTION 'ENQUEUE_ESCORUPROC'
      EXPORTING
        przid  = proz-przid_wein
        _scope = '3'  "Freigabe Sperre bei Ende TA & VB
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      MESSAGE e702.
      EXIT.
    ENDIF.
    IF paweonly IS INITIAL.
*   Rückmeldeprozess "Warenausgang" exclusiv sperren
      CALL FUNCTION 'ENQUEUE_ESCORUPROC'
        EXPORTING
          przid  = proz-przid_waus
          _scope = '3'  "Freigabe Sperre bei Ende TA & VB
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        MESSAGE e702.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
* alle Dummy-Fehlermeldungen bereitstellen
  SELECT * FROM tafwd INTO TABLE tafwd_tab.
  SORT tafwd_tab.

*ANDY for PCC only
  data: w_st_time like sy-uzeit.
  w_st_time = sy-uzeit.
  p_time = p_time * 60.

  IF tafwd_tab[] IS INITIAL.

    data: affw_tt like affw_tab occurs 1000 with header line.
    SELECT * FROM affw
           INTO TABLE affw_tt UP TO p_maxrow ROWS
           WHERE weblnr <> space
             AND   wablnr = space
             AND   ( inact = space OR
                     inact IS NULL )
             AND   weblnr IN s_belnr
             AND   werks  IN s_werks
             AND   matnr  IN s_matnr
             AND   lgort  IN s_lgort
             AND   dispo  IN s_dispo
             AND   msgno <> space
             and   rsnum  in s_rsnum
             and   rspos  in s_rspos.
    loop at affw_tt into affw_tab.
      PERFORM select_entries USING sy-subrc.
      if sy-subrc = 0.
        append affw_tab.
      endif.
    endloop.
  ENDIF.

  debug = p_debug.
*ANDY - end

* Aufruf der Routinen zum Buchen der Warenbewegungen
  PERFORM post_goods_movements.

* Nicht selektiert, dann trotzdem Érgebnis ausgeben
  IF gt_affw_prt IS INITIAL AND
     panoprot IS INITIAL.
    PERFORM output_alv.
  ENDIF.

END-OF-SELECTION.
*eject
* gemeinsame Form-Routinen zum Buchen der Warenbewegungen
* Einstieg erfolgt über die Form-Routine POST_GOODS_MOVEMENTS
  INCLUDE ZCORUAFWJ.


*----------------------------------------------------------------------*
*       FORM SELECT_ENTRIES                                            *
*----------------------------------------------------------------------*
*       aus eingelesenen Fehlersätzen selektieren                      *
*----------------------------------------------------------------------*
FORM select_entries USING sel_subrc LIKE sy-subrc.

  sel_subrc = 4.
  CHECK NOT affw_tab-msgno IS INITIAL OR
        affw_tab-autyp = auftyp-corp.
  CHECK affw_tab-werks IN s_werks.
  CHECK affw_tab-matnr IN s_matnr.
  CHECK affw_tab-lgort IN s_lgort.
  CHECK affw_tab-dispo IN s_dispo.
  CHECK affw_tab-weblnr IN s_belnr.
  IF NOT p_kdauf IS INITIAL.
    CHECK affw_tab-kdauf = p_kdauf.
    IF NOT p_kdpos IS INITIAL.
      CHECK affw_tab-kdpos = p_kdpos.
    ENDIF.
  ENDIF.
  IF NOT p_msgid IS INITIAL.
    CHECK affw_tab-msgid = p_msgid.
    IF NOT p_msgno IS INITIAL.
      CHECK affw_tab-msgno = p_msgno.
    ENDIF.
  ENDIF.
  IF NOT p_datuv IS INITIAL.
    CHECK affw_tab-fwdat >= p_datuv.
    IF NOT p_uzeiv IS INITIAL.
      CHECK affw_tab-fwdat > p_datuv OR
            affw_tab-fwzet >= p_uzeiv.
    ENDIF.
  ENDIF.
  IF NOT p_datub IS INITIAL.
    CHECK affw_tab-fwdat <= p_datub.
    IF NOT p_uzeib IS INITIAL.
      CHECK affw_tab-fwdat < p_datub OR
            affw_tab-fwzet <= p_uzeib.
    ENDIF.
  ENDIF.
  IF NOT p_datpv IS INITIAL.
    CHECK affw_tab-budat >= p_datpv.
  ENDIF.
  IF NOT p_datpb IS INITIAL.
    CHECK affw_tab-budat <= p_datpb.
  ENDIF.
  sel_subrc = 0.

ENDFORM.                    "select_entries
