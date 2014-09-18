*
*        Allgemeine PBO- und PAI-Module zum RM-LVS
*

*---------------------------------------------------------------------*
*                                                                     *
*       Inhalt: PBO                                                   *
*                                                                     *
*         TSTCT_LESEN             Lesen eines TSTCT Eintrags          *
*         T340_FELDAUSWAHL        Bildschirmmodifikation nach T340    *
*         T340_LESEN              Lesen eines T340 Eintrags           *
*         T341_FELDAUSWAHL        Bildschirmmodifikation nach T341    *
*                                                                     *
*---------------------------------------------------------------------*
*                                                                     *
*       Inhalt: PAI                                                   *
*                                                                     *
*         CURSORPOSITION          Bestimmen der Cursorposition        *
*         FCODE                   Fcode Bearbeitung                   *
*         SAV_DATAR_SCHREIBEN     Schreibt SAV_DATAR fort             *
*         T341_LESEN              Lesen eines T341  Eintrags          *
*---------------------------------------------------------------------*

***********************************************************************
*                                                                     *
*         P B O                                                       *
*                                                                     *
***********************************************************************

*---------------------------------------------------------------------*
*       MODULE TSTCT_LESEN                                            *
*---------------------------------------------------------------------*
*       Lesen der Bezeichnung zur aktuellen Transaktion               *
*---------------------------------------------------------------------*
MODULE TSTCT_LESEN OUTPUT.

  IF CALL_DIALOG_TCODE IS INITIAL.
    LOCAL_HELP_TCODE = SY-TCODE.
  ELSE.
    LOCAL_HELP_TCODE = CALL_DIALOG_TCODE.
  ENDIF.

  SELECT SINGLE * FROM TSTCT
  WHERE SPRSL = SY-LANGU AND
        TCODE = LOCAL_HELP_TCODE.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T340_FELDAUSWAHL                                       *
*---------------------------------------------------------------------*
*       Feldauswahl fuer das aktuelle Dynpro anhand T340              *
*---------------------------------------------------------------------*
MODULE T340_FELDAUSWAHL OUTPUT.
  PERFORM FELDAUSWAHL(SAPFL000) USING T340-FA016.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T340_LESEN                                             *
*---------------------------------------------------------------------*
*       Lesen der Feldauswahlleiste der aktuellen Transaktion         *
*---------------------------------------------------------------------*
MODULE T340_LESEN OUTPUT.

  IF CALL_DIALOG_TCODE IS INITIAL.
    LOCAL_HELP_TCODE = SY-TCODE.
  ELSE.
    LOCAL_HELP_TCODE = CALL_DIALOG_TCODE.
  ENDIF.

  IF SY-TCODE = 'LT1B' OR SY-TCODE = 'LT1C'.
    LOCAL_HELP_TCODE = 'LT11'.
  ELSEIF
    SY-TCODE = 'LT1D' OR SY-TCODE = 'LT1E'.
    LOCAL_HELP_TCODE = 'LT12'.
  ELSEIF
    SY-TCODE = 'LT1F' OR SY-TCODE = 'LT1G'.
    LOCAL_HELP_TCODE = 'LT13'.
  ENDIF.


*........Tabelle lesen.................................................
  LOCAL_HELP_TCODE = 'LS03N'.
  PERFORM T340_LESEN USING LOCAL_HELP_TCODE.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE T341_FELDAUSWAHL                                       *
*---------------------------------------------------------------------*
*       Feldauswahl fuer das aktuelle Dynpro anhand T341              *
*---------------------------------------------------------------------*
MODULE T341_FELDAUSWAHL OUTPUT.
  PERFORM FELDAUSWAHL(SAPFL000) USING T341-FA064.
ENDMODULE.

***********************************************************************
*                                                                     *
*         P A I                                                       *
*                                                                     *
***********************************************************************

*---------------------------------------------------------------------*
*       MODULE CURSORPOSITION                                         *
*---------------------------------------------------------------------*
*       Bestimmen der Cursorposition auf dem Bildschirm               *
*---------------------------------------------------------------------*
MODULE CURSORPOSITION.
  GET CURSOR FIELD FELD LINE ZEILE.
ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE SAV_DATAR_SCHREIBEN                                    *
*---------------------------------------------------------------------*
*       SAV_DATAR mit X belegen, falls eine Änderung im laufenden     *
*       Dynpro geschehen (POPUP Steuerung)                            *
*---------------------------------------------------------------------*
MODULE SAV_DATAR_SCHREIBEN.

  IF NOT SY-DATAR IS INITIAL.
    SAV_DATAR = SY-DATAR.
  ENDIF.

ENDMODULE.

*---------------------------------------------------------------------*
*       MODULE FCODE                                                  *
*---------------------------------------------------------------------*
*       Das Feld OK-CODE wird ins Feld FCODE gemoved, sofern es nicht *
*       initial ist. Danach wird der FCODE-Eintrag mit der Prozedur   *
*       FCODE bearbeitet.                                             *
*---------------------------------------------------------------------*
MODULE FCODE.
* IF OK-CODE <> INIT_OKCODE.
*   FCODE = OK-CODE.
*   CLEAR OK-CODE.
* ENDIF.
  PERFORM FCODE.
ENDMODULE.


*---------------------------------------------------------------------*
*       MODULE T341_LESEN.                                            *
*---------------------------------------------------------------------*
*       Lesen der Feldauswahlleiste fuer die aktuelle Transaktion     *
*---------------------------------------------------------------------*
MODULE T341_LESEN.

  IF CALL_DIALOG_TCODE IS INITIAL.
    LOCAL_HELP_TCODE = SY-TCODE.
  ELSE.
    LOCAL_HELP_TCODE = CALL_DIALOG_TCODE.
  ENDIF.

  SELECT SINGLE * FROM T341
  WHERE TCODE = LOCAL_HELP_TCODE.
  IF SY-SUBRC NE 0.
    MESSAGE ID 'L3' TYPE 'E' NUMBER '704' WITH LOCAL_HELP_TCODE.
  ENDIF.

ENDMODULE.
