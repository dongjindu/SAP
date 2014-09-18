* XZQP9CK095919 221100 Hinweis 360450

* 4.6C
* 4.6A
* XQPALRK169209 170198 Überspringen Einstiegsbild
* ALRK157395    011298 Abfrage des Systemtyps
* 4.0C
* XQPALRK087783 040398 Personenauswahl über Report
* XQPALRK076418 200298 Set-/Get-Parameter für das Einstiegsdatum
***INCLUDE LCATSFI1.
*&---------------------------------------------------------------------*
*&      Form  INIT_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_TRANS.
 DATA: pernr_char(8) TYPE c.
  DATA: pai VALUE space.               " form called in PBO   XQPK076418

  CHECK trans_init IS INITIAL.         " only process once
  add_fields = yx.
  trans_init = yx.
* Systemtyp bestimmen                                        "ALRK157395
  PERFORM get_appl_sys_type CHANGING appl_sys_type.         "ALRK157395
* Transaktionstyp bestimmen
* select single * from tc10 where tcode = sy-tcode.
* if not sy-subrc is initial.
*   message a025 with sy-tcode.
* endif.
* IF SY-TCODE EQ TRANS-CAT2.                            "DEL XQPK169209
  IF sy-tcode EQ trans-cat2 OR                              "XQPK169209
     sy-tcode EQ 'ZCAT2' OR
     sy-tcode EQ trans-cat2_iscr.                           "XQPK169209
*   TRTYP = AENDERN.                                    "DEL XQPK193399
    mode = fmode-maintain.                                  "XQPK193399
* ELSEIF SY-TCODE EQ TRANS-CAT3.                        "DEL XQPK169209
  ELSEIF sy-tcode EQ trans-cat3 OR                          "XQPK169209
         sy-tcode EQ trans-cat3_iscr.                       "XQPK169209
*   TRTYP = ANZEIGEN.                                   "DEL XQPK193399
    mode  = fmode-display.                                  "XQPK193399
  ELSE.
*   TRTYP = ANZEIGEN.                                   "DEL XQPK193399
    mode = fmode-display.                                   "XQPK193399
  ENDIF.
* check authorization  ?????   Ersatz möglich?
* IF TRTYP EQ AENDERN.                         "BEGIN OF DEL XQPK193399
*   MODE = FMODE-MAINTAIN.
*   PERFORM CHECK_AUTHORIZATION USING ' ' ACTIV-CHANGE.
* ELSE.
*   MODE = FMODE-DISPLAY.
*   PERFORM CHECK_AUTHORIZATION USING ' ' ACTIV-DISPLAY.
* ENDIF.                                         "END OF DEL XQPK193399
* set start_values_for_cats
  PERFORM set_start_values.
* get parameter id for inputdate                            "XQPK076418
  IF catsfields-inputdate IS INITIAL.                       "XQPK076418
    GET PARAMETER ID 'CID' FIELD catsfields-inputdate.      "XQPK076418
  ENDIF.                                                    "XQPK076418

  IF catsfields-inputdate(4) < 1901 OR "Hinweis 360450
  catsfields-inputdate(4) > 2098.      "Hinweis 360450
    CLEAR catsfields-inputdate.        "Hinweis 360450
  ENDIF.                               "Hinweis 360450

* Text zur Variante lesen
  IF tcatst-variant IS INITIAL.
    GET PARAMETER ID 'CVR' FIELD tcatst-variant.
    TRANSLATE tcatst-variant TO UPPER CASE.              "#EC TRANSLANG
    IF NOT ( tcatst-variant IS INITIAL ).
      SELECT SINGLE * FROM tcatst WHERE variant = tcatst-variant AND
                                        langu = sy-langu.
      IF NOT sy-subrc IS INITIAL.
        SELECT SINGLE * FROM tcats WHERE variant = tcatst-variant.
        IF NOT sy-subrc IS INITIAL.
          CLEAR tcatst-text.
          CLEAR trans_init.
* set empty subscreen as default subscreen
          d1000_subscr = subscr-101.
          EXIT.
        ENDIF.
      ENDIF.
* Variante lesen
*     perform check_and_interprete_variant.
      PERFORM check_and_interprete_variant USING pai.       "XQPK076418
      catsfields-worksince = catsfields-inputdate - tcats-worksince.
                                       " note  119096
    ENDIF.
  ENDIF.
* use distribution key with equal distribution
  PERFORM equal_distribution_key_set.
* get parameter ids
  IF catsfields-pernr IS INITIAL.
    GET PARAMETER ID 'PER' FIELD pernr_char.
    catsfields-pernr = pernr_char.
  ENDIF.
  IF catsfields-sbmod IS INITIAL.
    GET PARAMETER ID 'SGR' FIELD catsfields-sbmod.
* change to capital letters.
    TRANSLATE catsfields-sbmod TO UPPER CASE.            "#EC TRANSLANG
  ENDIF.
  IF catsfields-sachz IS INITIAL.
    GET PARAMETER ID 'SAZ' FIELD catsfields-sachz.
* change to capital letters.
    SET LOCALE LANGUAGE sy-langu.
    TRANSLATE catsfields-sachz TO UPPER CASE.
  ENDIF.
  IF catsfields-otype IS INITIAL.
    GET PARAMETER ID 'POT' FIELD catsfields-otype.
  ENDIF.
  IF catsfields-objid IS INITIAL.
    GET PARAMETER ID 'PON' FIELD catsfields-objid.
  ENDIF.
  IF catsfields-kokrs IS INITIAL.
    GET PARAMETER ID 'CAC' FIELD catsfields-kokrs.
  ENDIF.
  IF catsfields-selkostl IS INITIAL.
    GET PARAMETER ID 'KOS' FIELD catsfields-selkostl.
  ENDIF.
  IF catsfields-selrepvar IS INITIAL.                       "XQPK087783
    GET PARAMETER ID 'VSR' FIELD catsfields-selrepvar.      "XQPK087783
  ENDIF.                                                    "XQPK087783
* initialize persons and set subscreen
  IF NOT tcatst-variant IS INITIAL.
    IF tcats-pernrlist IS INITIAL.
      PERFORM init_single_pernr.
      d1000_subscr = subscr-101.
    ELSE.
      PERFORM init_multi_pernr.
      d1000_subscr = subscr-102.
    ENDIF.
    pernrlist_flag = tcats-pernrlist.                       "XQPK078873
  ELSE.
*   set empty subscreen as default subscreen
    d1000_subscr = subscr-101.
  ENDIF.
* if possible, skip entry screen         "Begin of insertion XQPK169209
  IF tcats-skipiscr           =  yx
     AND     sy-tcode         <> trans-cat2_iscr        "initial screen
     AND     sy-tcode         <> trans-cat3_iscr        "initial screen
     AND NOT tcats-variant    IS INITIAL
     AND NOT catsfields-pernr IS INITIAL
     AND     tcats-pernrlist  IS INITIAL.
    SUPPRESS DIALOG.
    IF mode = fmode-maintain.
      ok_code = fc-time.
    ELSE.
      ok_code = fc-show.
    ENDIF.
  ENDIF.
ENDFORM.                    " INIT_TRANS
