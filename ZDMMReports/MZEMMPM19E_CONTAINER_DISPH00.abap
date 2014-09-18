*----------------------------------------------------------------------*
*   INCLUDE ML01SH00                                                   *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       Modulpool ML01SH00: Process-on value Request zu SAPML01S      *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         value_lagerplatz     "Eingabehilde Lagerplatz auf dynpro 100
*---------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       MODULE value_lagerplatz                                       *
*---------------------------------------------------------------------*
*       Eingabehilfe Lagerplatz auf Dynpro D0100                      *
*---------------------------------------------------------------------*
MODULE VALUE_LAGERPLATZ.


  REFRESH GETFIELDS.
  REFRESH UPDFIELDS.


* Felder die selektiv wirken in Tabelle getfields aufnehmen
  GETFIELDS-TABNAME = 'LAGP'.
  GETFIELDS-FIELDNAME = 'LGNUM'.
  APPEND GETFIELDS.

  GETFIELDS-TABNAME = 'LAGP'.
  GETFIELDS-FIELDNAME = 'LGTYP'.
  APPEND GETFIELDS.


* Felder die per Doppelklick übernommen werden können in Tabelle
* updfields aufnehmen
  UPDFIELDS-TABNAME = 'LAGP'.
  UPDFIELDS-FIELDNAME = 'LGPLA'.
  APPEND UPDFIELDS.

  UPDFIELDS-TABNAME = 'LAGP'.
  UPDFIELDS-FIELDNAME = 'LGTYP'.
  APPEND UPDFIELDS.

  UPDFIELDS-TABNAME = 'LAGP'.
  UPDFIELDS-FIELDNAME = 'LGNUM'.
  APPEND UPDFIELDS.


* Popup zur Anzeige der Werte
CALL FUNCTION 'HELP_VALUES_GET_EXTEND'
     EXPORTING
*         DISPLAY      = ' '
          DYNAME       = SY-CPROG
          DYNUMB       = SY-DYNNR
          FIELDNAME    = 'LGPLA'
*         INPUT_VALUE  = ' '
          TABNAME      = 'LAGP'
          SHRINK       = 'X'
*    importing
*         returncode   =
*         selection    =
*         select_value =
*         select_index =
     TABLES
          GETFIELDS    =  GETFIELDS
          UPDFIELDS    =  UPDFIELDS
     EXCEPTIONS
          OTHERS       = 1.




ENDMODULE.
