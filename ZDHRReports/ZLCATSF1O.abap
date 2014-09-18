* XZQ 260402    Hinweis 515741 Einstiegsdatum bei Profilwechsel
* XZQP9CK071753 Hinweis 331797
* XZQP9CK036546 Hinweis 306620
* 4.0C
* XQPALRK084279 230498 Änderungen für verbesserte Kostenrechnung
* XQPALRK097720 260398 Kein Abzug von Pausen bei Berechnung der Stunden
* XQPALRK076418 200298 Set-/Get-Parameter für das Einstiegsdatum
***INCLUDE LCATSFC1.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_INTERPRETE_VARIANT
*&---------------------------------------------------------------------*
*       Variante interpretieren
*----------------------------------------------------------------------*
* form check_and_interprete_variant.                         "XQPK076418
FORM check_and_interprete_variant USING pai.                "XQPK076418

  DATA: tc_kontext LIKE tcview-tckontext.

  SELECT SINGLE * FROM tcats WHERE variant = tcatst-variant.
  IF sy-subrc NE 0.
    SET CURSOR FIELD 'TCATST-VARIANT'.
    MESSAGE e001 WITH tcatst-variant.
  ENDIF.
* set input date
  IF old_variant NE tcats-variant.
    IF catsfields-inputdate CO ' 0' OR pai = yx.            "XQPK076418
*----------XZQ Beg Hinweis 515741----------------------
* Falls das Profil ohne 'Enter' zu drücken gewechselt wurde,
* soll mit dem dem Benutzer zuletzt auf dem Einsteigsbild
* gezeigten Einstiegsdatum gearbeitet werden.
* Bei Profileinstellung wird weiterhin das Einstiegsdatum
* angepaßt, weil dem Benutzer das neu berechnete Datum
* auch noch angezeigt wird.
      IF catsfields-inputdate CO ' 0' OR
      NOT sy-ucomm = fc-time AND
      NOT sy-ucomm = fc-show.
*----------XZQ End Hinweis 515741----------------------
        IF tcats-reldate IS INITIAL.
          catsfields-inputdate = sy-datlo.
          SET PARAMETER ID 'CID' FIELD catsfields-inputdate."lux
        ELSE.
          PERFORM get_inputdate USING catsfields-inputdate.
          SET PARAMETER ID 'CID' FIELD catsfields-inputdate."note331797
        ENDIF.
      ENDIF.                           "XZQ Hinweis 515741
    ENDIF.                                                  "XQPK076418
    old_variant = tcats-variant.
  ENDIF.
* Set screen-number for worklist.
  PERFORM set_screen_for_worklist.
* fill datefrom and dateto and set number of days on screen
  PERFORM fill_dayfrom_and_dayto USING catsfields-dateleft
                                       catsfields-dateright
                                       yx yx.

* fill table with status
  PERFORM determine_view_on_data.
* fill status itable
  PERFORM change_status_tab.
* fill structure ICATSBEGEND with spaces
  PERFORM fill_icatsbegend.
* set context for settings of table control
  tc_kontext = tcats-variant.
  CALL FUNCTION 'SET_TC_KONTEXT'
       EXPORTING
            programname = 'SAPLZCATS'
            controlname = 'TC_CATSD'
            kontext     = tc_kontext.

  CALL FUNCTION 'SET_TC_KONTEXT'                            "note306620
       EXPORTING                                            "note306620
            programname = 'SAPLZCATS'                       "note306620
            controlname = 'TC_CATSW'                        "note306620
            kontext     = tc_kontext.                       "note306620


ENDFORM.                               " INTERPRETE_VARIANT
