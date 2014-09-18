*---------------------------------------------------------------------*
*       Modulpool SAPML01S: Feldauswahlroutinen                       *
*---------------------------------------------------------------------*
*       Inhalt:                                                       *
*                                                                     *
*         BR001            "Initiale Felder ausblenden                *
*         BR002            "Initiale Felder ausblenden beim Anz.+Hinz.*
*         BR003            "Feld ausblenden wenn MHD nicht aktiv      *
*         BR004            "Mussfeld wenn LET-Prüfung aktiv           *
*         BR005            "Mussfeld abhängig von Kapazitätsprüfung   *
*         BR006            "Mussfeld abhängig von Kapazitätsprüfung   *
*---------------------------------------------------------------------*
*eject
*---------------------------------------------------------------------*
*       FORM BR001    Basisregel 1                                    *
*---------------------------------------------------------------------*
*       Wenn das Feld initial ist, wird es ausgeblendet               *
*---------------------------------------------------------------------*
FORM BR001.

  ASSIGN (SCREEN-NAME) TO <FELDNAME>.

  IF <FELDNAME> IS INITIAL.
    SCREEN-REQUIRED  = CON_OFF.
    SCREEN-INPUT  =    CON_OFF.
    SCREEN-OUTPUT =    CON_OFF.
    SCREEN-INVISIBLE = CON_ON.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BR002    Basisregel 2                                    *
*---------------------------------------------------------------------*
*       Wenn das Feld initial ist, wird es ausgeblendet,              *
*       jedoch nur im Anzeige und Hinzufüge-Modus.                    *
*---------------------------------------------------------------------*
FORM BR002.

  ASSIGN (SCREEN-NAME) TO <FELDNAME>.

  IF <FELDNAME> IS INITIAL
     AND T342-TRTYP NE CON_VERAENDERN.
    SCREEN-REQUIRED  = CON_OFF.
    SCREEN-INPUT  =    CON_OFF.
    SCREEN-OUTPUT =    CON_OFF.
    SCREEN-INVISIBLE = CON_ON.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BR003    Basisregel 3                                    *
*---------------------------------------------------------------------*
*       Wenn der Schalter "Mindesthaltbarkeitsdatum" in der Lager-    *
*       nummer nicht sitzt wird das Feld ausgeblendet                 *
*---------------------------------------------------------------------*
FORM BR003.


  IF T340D-MHDKZ IS INITIAL.
    SCREEN-REQUIRED  = CON_OFF.
    SCREEN-INPUT  =    CON_OFF.
    SCREEN-OUTPUT =    CON_OFF.
    SCREEN-INVISIBLE = CON_ON.
  ENDIF.

  IF NOT MLVS-XCHPF IS INITIAL.
    SCREEN-INPUT  =    CON_OFF.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM BR004    Basisregel 4                                    *
*---------------------------------------------------------------------*
*       Wenn im Lagertyp LET-Prüfung eingeschaltet ist, ist das Feld  *
*       Pflichtfeld.                                                  *
*---------------------------------------------------------------------*
FORM BR004.


  IF NOT T331-PRLET IS INITIAL AND
     T340-TRTYP NE CON_ANZEIGEN.
    SCREEN-REQUIRED  = CON_ON.
    SCREEN-INPUT  =    CON_ON.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BR005    Basisregel 5                                    *
*---------------------------------------------------------------------*
*       Wenn im Lagertyp Kapazitätsprüfung nach der Kapazitätskenn-  *
*       zahl aus dem Materialstamm und/oder dem Lagereinheitstyp      *
*       erfolgt, ist das Feld Pflichtfeld                             *
*---------------------------------------------------------------------*
FORM BR005.


IF T340-TRTYP NE CON_ANZEIGEN.
  IF T331-KAPAP EQ CON_KAPAZ_MAT OR        "Kapazitätspr. Material
     T331-KAPAP EQ CON_KAPAZ_LET OR        "Kapazitätspr. LET
     T331-KAPAP EQ CON_KAPAZ_MAT_LET.      "Kapazitätspr. MAT+LET
    SCREEN-REQUIRED  = CON_ON.
    SCREEN-INPUT  =    CON_ON.
  ENDIF.
ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM BR006    Basisregel 6                                    *
*---------------------------------------------------------------------*
*       Wenn im Lagertyp Kapazitätsprüfung nach der Gewicht erfolgt,  *
*       ist das Feld Pflichtfeld                                      *
*---------------------------------------------------------------------*
FORM BR006.


IF T340-TRTYP NE CON_ANZEIGEN.
  IF T331-KAPAP EQ CON_KAPAZ_GEWICHT.      "Kapazitätsprüfung Gewicht
    SCREEN-REQUIRED  = CON_ON.
    SCREEN-INPUT  =    CON_ON.
  ENDIF.
ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BR002    Basisregel 7                                    *
*---------------------------------------------------------------------*
*       Wenn das Feld initial ist, wird es ausgeblendet,              *
*       jedoch nur im Anzeige-Modus.                                  *
*---------------------------------------------------------------------*
FORM BR007.

  ASSIGN (SCREEN-NAME) TO <FELDNAME>.

  IF <FELDNAME> IS INITIAL
     AND ( T342-TRTYP NE CON_VERAENDERN
          AND T342-TRTYP NE CON_HINZUFUEGEN ).
    SCREEN-REQUIRED  = CON_OFF.
    SCREEN-INPUT  =    CON_OFF.
    SCREEN-OUTPUT =    CON_OFF.
    SCREEN-INVISIBLE = CON_ON.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BR008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BR008.


  IF screen-name EQ 'RL01S-ESTAT' or
     screen-name EQ 'RL01S-ESICO'.
    if lagp-skzse is initial and
       lagp-skzue is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

  IF screen-name EQ 'RL01S-ASTAT' or
     screen-name EQ 'RL01S-ASICO'.
    if lagp-skzsa is initial and
       lagp-skzua is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

  IF screen-name EQ 'RL01S-ISTAT' or
     screen-name EQ 'RL01S-ISICO'.
    if lagp-skzsi is initial and
       lagp-ivivo is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

ENDFORM.                    " BR008
*&---------------------------------------------------------------------*
*&      Form  BR009
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BR009.

  IF screen-name EQ 'LAGP-KZLER' or
     screen-name EQ 'RL01S-LKICO'.
    if lagp-kzler is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

  IF screen-name EQ 'LAGP-KZVOL' or
     screen-name EQ 'RL01S-VKICO'.
    if lagp-kzvol is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

  IF screen-name EQ 'LAGP-KZDYN' or
     screen-name EQ 'RL01S-DPICO'.
    if lagp-kzdyn is initial.
      SCREEN-REQUIRED  = CON_OFF.
      SCREEN-INPUT  =    CON_OFF.
      SCREEN-OUTPUT =    CON_OFF.
      SCREEN-INVISIBLE = CON_ON.
    endif.
  endif.

ENDFORM.                    " BR009
*&---------------------------------------------------------------------*
*&      Form  BR010
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BR010.

  if t340-trtyp eq con_anzeigen.
    screen-input = con_off.
  endif.

ENDFORM.                    " BR010
*&---------------------------------------------------------------------*
*&      Form  BR011
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BR011.

  if t340-trtyp eq con_hinzufuegen.
    screen-input = con_off.
    screen-invisible = con_on.
  endif.

ENDFORM.                    " BR011
*&---------------------------------------------------------------------*
*&      Form  BR012
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BR012.

  screen-input = con_off.
  screen-invisible = con_on.

ENDFORM.                    " BR012
