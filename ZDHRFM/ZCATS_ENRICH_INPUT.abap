* DWI Note number 692661 - Clearing of data from infotype 0001 if the
* personel number is changed
* 4.6C HP
* YCY           110902 note 553591 Vorschlagswerte aus 0315 bei pernr
*                      Wechsel
* FWE           030602 Hinweis 524531 Existenz des Split immer prüfen
* XCF           140202 Hinweis 495439 CATS notebook
* XCF           131201 Hinweis 460163 Dump wegen Langtext, Exit 0002
* XZQ           121201 Hinweis 459438 Fehler bei Nicht-Stundeneinheiten
*                                     und Default-AWART
* XDEP9CK165392 050601 Note 409555, no sender when stat. key figure
* XDEP9CK161823 220501 Note 406857, sending business process
*                                   in costing scenarios D and E
* XCFP9CK157120 040501 Hinweis 401122 Vorschlagswerte bei mehereren 0315
* XDEP9CK130186 130201 Note 382051, Lohnart mit Bewertungsgrundlage
* XDEP9CK124577 020201 Note 378841, Bewertungsgrundlage
* XZQP9CK119085 240101 Hinweis 376352 (Vorschlagswerte MM-SRV)
* XQPP9CK045688 300600 note 313331
* 4.6C
* XQPP00K020148 270100 Stat. Kennzahl
* 4.6A
* XQPALRK186288 010399 Sendender Geschäftsprozeß
* 4.0C
* XQPALRK097720 260398 Kein Abzug von Pausen bei Berechnung der Stunden
* XQPALRK084279 230398 Änderungen für verbesserte Kostenrechnung
* 4.6C MATH     200104 Note 698454 - Duplication of records in CATW when
*                      exit 002 active and inserts additional records.
FUNCTION ZCATS_ENRICH_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(ENRICH_CATS) LIKE  TCATS STRUCTURE  TCATS
*"             VALUE(ENRICH_CATSFIELDS) LIKE  CATSFIELDS
*"  STRUCTURE  CATSFIELDS
*"             VALUE(I_EXT_CALL) TYPE  C DEFAULT SPACE
*"       TABLES
*"              ENRICH_CATSDB STRUCTURE  CATSDB_EXT
*"              TAB_MESSAGES STRUCTURE  MESG
*"              ENRICH_FLAG STRUCTURE  CATS_ENRIC
*"              ENRICH_PERNR STRUCTURE  CATS_PERIN
*"       EXCEPTIONS
*"              ENRICH_ERROR_OCCURRED
*"----------------------------------------------------------------------

  DATA: buffer_catsdb LIKE enrich_catsdb OCCURS 0 WITH HEADER LINE.
  DATA: customer_tab LIKE cats_comm OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF essential_fields.
          INCLUDE STRUCTURE cats_sec_k.
  DATA: counter LIKE catsdb-counter,
        longtext LIKE catsdb-longtext,
        END OF essential_fields.
  DATA: ucount LIKE sy-tabix.

  DATA: ucats_appli LIKE cats_appli,  "no sender bus.proc. for Logistics
        sprznr_entered_by_user TYPE c,
        skostl_entered_by_user TYPE c,
        lstar_entered_by_user  TYPE c.
  DATA: help_subrc LIKE sy-subrc.
  DATA: up0315 LIKE cats_0315.
  DATA: up0001 LIKE pernr_list_structure.

  DATA: subrc0315 LIKE sy-subrc,  "<> 0 ... reading IT0315 unsuccessful
        subrc0001 LIKE sy-subrc.  "<> 0 ... reading IT0001 unsuccessful
  DATA: amount_quantity_flag TYPE c.
* BAPI return structures to store error messages                  "LUX
  DATA: return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.     "LUX
  DATA: enriched_via_task TYPE c.      "XCF note 495439

  REFRESH buffer_catsdb.
  LOOP AT enrich_catsdb.

* Clear BEDID in case it's no longer valid due to changed data
    CLEAR enrich_catsdb-bedid.                              "note524531

    CLEAR lstar_entered_by_user.                            "note319624
    CLEAR amount_quantity_flag.                             "note319624
    CLEAR skostl_entered_by_user.                           "note319624
    CLEAR sprznr_entered_by_user.                           "note319624
    IF NOT ( enrich_catsdb-workdate >= up0315-begda         "note401122
*      AND   enrich_catsdb-workdate <= up0315-endda ).      "note401122
       AND   enrich_catsdb-workdate <= up0315-endda )       "note553591
       OR   enrich_catsdb-pernr <> up0315-pernr.            "note553591
      CLEAR up0315.                                         "note401122
      CLEAR subrc0315.                                      "note401122
    ENDIF.                                                  "note401122
* DWI Note number 692661---------------
    IF NOT ( enrich_catsdb-workdate >= up0001-begda
       AND   enrich_catsdb-workdate <= up0001-endda )
       OR   enrich_catsdb-pernr <> up0001-pernr.
      CLEAR up0001.
      CLEAR subrc0001.
    ENDIF.
* DWI Note number 692661--------------

    ucount = sy-tabix.
* read the corresponding enrich-flag
    READ TABLE enrich_flag INDEX ucount.
    IF sy-subrc NE 0.
      IF i_ext_call IS INITIAL.
        MESSAGE x030.
      ELSE.
        PERFORM add_message_row USING arbgb 'X' '030'
                                      space space space space
                                      enrich_catsdb-row.
      ENDIF.
    ENDIF.
* read the enrich fields
    READ TABLE enrich_pernr INDEX ucount.
    IF sy-subrc NE 0.
      IF i_ext_call IS INITIAL.
        MESSAGE x030.
      ELSE.
        PERFORM add_message_row USING arbgb 'X' '030'
                                      space space space space
                                      enrich_catsdb-row.
      ENDIF.
    ENDIF.
* begin insert xcf, note 495439
* before further enrich, check if there are derivatives for catsxt
    CLEAR enriched_via_task.
*   don't derivate for data from CATSXT!
    IF enrich_catsdb-taskcounter IS INITIAL.
*     is one of the three fields filled?
      IF      NOT enrich_catsdb-taskcomponent IS INITIAL
           OR NOT enrich_catsdb-tasktype IS INITIAL
           OR NOT enrich_catsdb-tasklevel IS INITIAL.
*       set the enriched via task flag always when one of the
*       fields is filled, also when the derivation has no success
*       or the fields are not all filled
        enriched_via_task = 'X'.
*     are all three fields filled? if not -> error message
        IF      NOT enrich_catsdb-taskcomponent IS INITIAL
            AND NOT enrich_catsdb-tasktype IS INITIAL
            AND NOT enrich_catsdb-tasklevel IS INITIAL.
          PERFORM get_derivatives CHANGING enrich_catsdb.
        ELSE.
          IF 1 = 2. MESSAGE e128. ENDIF.
          PERFORM add_message_row USING arbgb 'E' '128'
                                        space space space space
                                        enrich_catsdb-row.

        ENDIF.
      ENDIF.
    ENDIF.
* end insert xcf, note 495439
* enrich some basic fields anyway. this is necessary, because the fields
* are not in the list of fields on the dynpro and cannot be transported
* via catsd
* set the client
    enrich_catsdb-mandt = sy-mandt.
* set the quantity
    enrich_catsdb-meinh = enrich_catsfields-meinh.
* set the unit if absence or attendance
    IF      NOT enrich_catsdb-awart IS INITIAL             "Note 378841
        AND     enrich_catsdb-unit  IS INITIAL.            "Note 378841
      enrich_catsdb-unit = enrich_catsdb-meinh.            "Note 378841
    ENDIF.                                                 "Note 378841
* set the flag to specify the handling of cost assignment    "XQPK084279
*   data sets with only absence/attendance get the flag szenario_0
    CLEAR receiver.
    MOVE-CORRESPONDING enrich_catsdb TO receiver.
* begin of deletion note 313331
*   IF receiver IS INITIAL AND         "no receiver
*     enrich_catsdb-sebeln IS INITIAL AND        "and no MM-SRV data
*     enrich_catsdb-sebelp IS INITIAL AND        "--> szenario_0
*     enrich_catsdb-lstnr  IS INITIAL.
*     enrich_catsdb-hrcostasg = szenario_0.
*   ELSE.
*     enrich_catsdb-hrcostasg = enrich_cats-hrcostasg.
*   ENDIF.
* end of deletion note 313331
    enrich_catsdb-hrcostasg = enrich_cats-hrcostasg.       "note 313331
* set currency for alternate payment
    IF      NOT enrich_catsdb-bwgrl IS INITIAL
        AND     enrich_catsdb-waers IS INITIAL.
      CALL FUNCTION 'CATS_GET_ALP_CURRENCY'
           EXPORTING
                pernr  = enrich_catsdb-pernr
                date   = enrich_catsdb-workdate
           IMPORTING
                waers  = enrich_catsdb-waers
           EXCEPTIONS
                error  = 1
                OTHERS = 2.
      IF sy-subrc <> 0.
* Replace Add_Message by Add_Message_Row to insert the line number
* LUXALRK186289
*       PERFORM ADD_MESSAGE USING ARBGB 'E' '180'
*                        ENRICH_CATSDB-PERNR ENRICH_CATSDB-WORKDATE
*                        SPACE SPACE.
        PERFORM add_message_row USING arbgb 'E' '180'       "LUX
                         enrich_catsdb-pernr enrich_catsdb-workdate"LUX
                         space space                        "LUX
                         enrich_catsdb-row.                 "LUX
        IF 1 = 2.
          MESSAGE e180 WITH enrich_catsdb-pernr
                            enrich_catsdb-workdate.
        ENDIF.
        EXIT.
      ENDIF.
      CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
           EXPORTING
                currency    = enrich_catsdb-waers
                idoc_amount = enrich_catsdb-bwgrl
           IMPORTING
                sap_amount  = enrich_catsdb-bwgrl.
    ENDIF.

* find out, which senders the user has entered (never delete those!)
    IF NOT enrich_catsdb-sprznr IS INITIAL.
      sprznr_entered_by_user = yx.
    ENDIF.
    IF NOT enrich_catsdb-skostl IS INITIAL.
      skostl_entered_by_user = yx.
    ENDIF.
    IF NOT enrich_catsdb-lstar IS INITIAL.
      lstar_entered_by_user = yx.
    ENDIF.
* COMPANY CODE FROM INFOTYPE 0001
    IF enrich_cats-defkokrsco = yx AND
       enrich_catsdb-kokrs IS INITIAL.
      PERFORM get_pernr_list_0001 USING    enrich_catsdb-pernr
                                           enrich_catsdb-workdate
                                  CHANGING up0001
                                           subrc0001.
      IF subrc0001 IS INITIAL.
        IF up0001-kokrs IS INITIAL.
*       derive controlling area from company code           "XQP
          CALL FUNCTION 'BAPI_CONTROLLINGAREA_FIND'
               EXPORTING
                    companycodeid     = up0001-bukrs
               IMPORTING
                    controllingareaid = up0001-kokrs
*                    RETURN            =
               EXCEPTIONS
                    OTHERS            = 1.
        ENDIF.                                              "XQP
        enrich_catsdb-kokrs = up0001-kokrs.
      ENDIF.
    ENDIF.
* check if unit of currency key is default or filled        "lux
    IF NOT enrich_catsdb-statkeyfig IS INITIAL.
      PERFORM get_unit_from_stagr USING enrich_catsdb-kokrs
                                        enrich_catsdb-statkeyfig
                                        enrich_catsdb-row
                                  CHANGING enrich_catsdb-unit.
    ELSEIF enrich_catsdb-unit IS INITIAL AND
           enrich_catsdb-waers IS INITIAL.
      enrich_catsdb-unit = unit_of_hour.
    ENDIF.
* set the unit if wage type with valuation basis
    IF      NOT enrich_catsdb-lgart IS INITIAL             "Note 382051
        AND     enrich_catsdb-unit  IS INITIAL             "Note 382051
        AND NOT enrich_catsdb-bwgrl IS INITIAL.            "Note 382051
      enrich_catsdb-unit = enrich_catsdb-meinh.            "Note 382051
    ENDIF.                                                 "Note 382051

* set flag to indicate whether amount or quantity are processed.
*    IF NOT enrich_catsdb-catsquantity IS INITIAL OR
    IF enrich_catsdb-unit NE enrich_catsdb-meinh OR         "note319642
           NOT enrich_catsdb-catsamount IS INITIAL.
      amount_quantity_flag = yx.
    ENDIF.
* the following enrichment only for new data sets
    IF enrich_flag-flag       = yx.
* MM-SRV-data from infotype 0315
      IF ( enrich_cats-defpuror = yx OR
*           enrich_cats-deflstst = yx ) AND             "Hinweis 376352
*           amount_quantity_flag = yx.                  "Hinweis 376352
            enrich_cats-deflstst = yx ).                "Hinweis 376352


        PERFORM get_infotype_0315 USING     enrich_catsdb-pernr
                                            enrich_catsdb-workdate
                                  CHANGING  up0315
                                            subrc0315.
        IF subrc0315 IS INITIAL.
          IF enrich_cats-defpuror = yx AND
             enrich_catsdb-sebeln IS INITIAL.
            enrich_catsdb-sebeln = up0315-ebeln.
          ENDIF.
          IF enrich_cats-defpuror = yx AND
             enrich_catsdb-sebelp IS INITIAL.
            enrich_catsdb-sebelp = up0315-ebelp.
          ENDIF.
          IF enrich_cats-deflstst = yx AND
             enriched_via_task IS INITIAL AND          "XCF note 495439
             enrich_catsdb-lstnr IS INITIAL.
            enrich_catsdb-lstnr = up0315-lstnr.
          ENDIF.
        ENDIF.
      ENDIF.
*   attendances out of tcats
      IF enrich_catsdb-awart  IS INITIAL AND
         enrich_catsdb-lgart  IS INITIAL AND
*        amount_quantity_flag IS INITIAL AND"new YIK  (del)"Note 378841
*--------------Beginn XZQ Hinweis 459438-------------------
* Falls der Satz bereits eine UNIT hat, die sich von MEINH unterscheidet
* so darf die Einheit des Satzes nicht auf MEINH geändert werden.
* Die Default-AWART ist bei Einheiten <> MEINH natürlich auch unsinnig
         ( enrich_catsdb-unit IS INITIAL OR
         enrich_catsdb-unit = enrich_catsdb-meinh ) AND
*--------------Ende XZQ Hinweis 459438----------------------
         enriched_via_task IS INITIAL AND               "XCF note 495439
         enrich_cats-defatype  = yx.
        enrich_catsdb-awart  = enrich_cats-defawart.
* set the unit if absence or attendance
        enrich_catsdb-unit = enrich_catsdb-meinh.          "Note 378841
        CLEAR amount_quantity_flag.                        "Note 378841
      ENDIF.
*   wagetypes out of tcats  new YIK
      IF enrich_catsdb-lgart  IS INITIAL AND
         enrich_catsdb-awart  IS INITIAL AND
         enrich_cats-defwtype  = yx.
        enrich_catsdb-lgart  = enrich_cats-deflgart.
* set the unit if wage type with valuation basis
        IF      NOT enrich_catsdb-lgart IS INITIAL         "Note 382051
            AND     enrich_catsdb-unit  IS INITIAL         "Note 382051
            AND NOT enrich_catsdb-bwgrl IS INITIAL.        "Note 382051
          enrich_catsdb-unit = enrich_catsdb-meinh.        "Note 382051
        ENDIF.                                             "Note 382051
      ENDIF.
    ENDIF.

*  Value for field OTYPE has to be filled, if field 'Position' is input
*  on the screen. This is applicable for release 46C onwards as the
*  search help (H_T528B) parameters in structure CATSD have been
*  modified. The only allowed value for field OTYPE in CATS is 'S'.
    IF NOT enrich_catsdb-plans IS INITIAL.
      enrich_catsdb-otype = 'S'.                           "Note 638567
    ENDIF.

* the following enrichment only if a receiver is entered! (not only
* for new data sets, as maybe a receiver has been entered later!)
    IF NOT receiver IS INITIAL AND
       amount_quantity_flag IS INITIAL.
* sender business process or sender cost center and activity type
* from infotype 0315
      IF enrich_catsdb-hrcostasg <> szenario_a AND
         ( enrich_cats-defcostcc = yx          AND
           enrich_catsdb-skostl IS INITIAL     OR
           enrich_cats-defactty  = yx          AND
*        ENRICH_CATSDB-LSTAR IS INITIAL ).               "DEL XQPK186288
        enrich_catsdb-lstar IS INITIAL      OR              "XQPK186288
        enrich_catsdb-sprznr IS INITIAL     AND             "XQPK186288
        enrich_cats-defsprznr = yx ).                       "XQPK186288
        IF up0315 IS INITIAL AND subrc0315 IS INITIAL.
          PERFORM get_infotype_0315 USING     enrich_catsdb-pernr
                                              enrich_catsdb-workdate
                                    CHANGING  up0315
                                              subrc0315.
        ENDIF.
        IF subrc0315 IS INITIAL.
* sender bus.proc. only if there is no sender cc.           "XQPK186288
          IF enrich_cats-defsprznr = yx      AND             "...
             enrich_catsdb-skostl IS INITIAL AND
             enriched_via_task IS INITIAL AND           "XCF note 495439
             enrich_catsdb-sprznr IS INITIAL.
            enrich_catsdb-sprznr = up0315-sprznr.
          ENDIF.                                            "XQPK186288
* sender cc. and activity type
          IF enrich_cats-defcostcc = yx      AND
             enrich_catsdb-skostl IS INITIAL.
            enrich_catsdb-skostl = up0315-kostl.
          ENDIF.
          IF enrich_cats-defactty = yx       AND
             enriched_via_task IS INITIAL AND           "XCF note 495439
             enrich_catsdb-lstar IS INITIAL.
            enrich_catsdb-lstar = up0315-lstar.
          ENDIF.
        ENDIF.
      ENDIF.
* sender cost center from infotype 0001
      IF enrich_catsdb-hrcostasg <> szenario_a AND
         enrich_cats-defcostco = yx            AND
         enrich_catsdb-skostl IS INITIAL       AND
         enrich_catsdb-sprznr IS INITIAL.
        IF up0001 IS INITIAL AND subrc0001 IS INITIAL.
          PERFORM get_pernr_list_0001 USING    enrich_catsdb-pernr
                                               enrich_catsdb-workdate
                                      CHANGING up0001
                                               subrc0001.
        ENDIF.
        IF subrc0001 IS INITIAL.
          enrich_catsdb-skostl = up0001-kostl.
        ENDIF.
      ENDIF.
    ENDIF.
* determine hours/vtken out of beguz and enduz
*    IF ENRICH_CATS-BEGEND = YX.
    IF NOT ( enrich_catsdb-beguz CO ' 0'
         AND enrich_catsdb-enduz CO ' 0' ).
      IF enrich_catsdb-awart IS INITIAL.                    "YIK
        CALL FUNCTION 'CATS_COMPUTE_HOURS'
             EXPORTING
                  pernr              = enrich_catsdb-pernr
                  date               = enrich_catsdb-workdate
                  no_break_deduction =
                     enrich_cats-nobreakded                 "XQPK09772
                  row                = enrich_catsdb-row
             TABLES
                  return             = return
             CHANGING
                  catshours          = enrich_catsdb-catshours
                  beguz              = enrich_catsdb-beguz
                  enduz              = enrich_catsdb-enduz
                  vtken              = enrich_catsdb-vtken
             EXCEPTIONS
                  OTHERS             = 1.
        LOOP AT return.                                     "LUX
          PERFORM add_message_row USING                     "LUX
                                  return-id                 "LUX
                                  return-type               "LUX
                                  return-number             "LUX
                                  return-message_v1         "LUX
                                  return-message_v2         "LUX
                                  return-message_v3         "LUX
                                  return-message_v4         "LUX
                                  return-row.               "LUX
        ENDLOOP.                                            "LUX

      ENDIF.
*     else.                                        "YIK
*       VTKEN is senseless maybe not
*       clear enrich_catsdb-vtken.                 "YIK
    ENDIF.
*    endif.

* customer enrichment as last action before conversion extern intern
* this is necessary, if the customer substitutes a work center etc
* store primary key
    REFRESH customer_tab.
    MOVE-CORRESPONDING enrich_catsdb TO essential_fields.
    MOVE-CORRESPONDING enrich_catsdb TO customer_tab.
    APPEND customer_tab.
* customer checks
    PERFORM customer_enrichment TABLES customer_tab.
* could be that customer added lines
    LOOP AT customer_tab.
      MOVE-CORRESPONDING customer_tab TO enrich_catsdb.
* if more than one record treat all records except the first one as new
* restore primary key
      MOVE-CORRESPONDING essential_fields TO enrich_catsdb.
      IF sy-tabix GT 1.
        CLEAR enrich_catsdb-longtext.  "XCF note 460163
        CLEAR enrich_catsdb-counter.
        CLEAR enrich_catsdb-row.       "MATH Note 698454
* set the status
        enrich_catsdb-status = status-lock.
      ENDIF.

* conversion of input (AUFNR --> AUFPL) first
      PERFORM convert_ext_to_int USING enrich_catsdb
                                       i_ext_call.

* stat. key fig.: don't default LSTAR                       "XQPK020148
      IF NOT enrich_catsdb-statkeyfig IS INITIAL AND
         lstar_entered_by_user        IS INITIAL.
        CLEAR enrich_catsdb-lstar.
      ENDIF.
* stat. key fig.: don't default SKOSTL                     "Note 409555
      IF NOT enrich_catsdb-statkeyfig IS INITIAL AND
         skostl_entered_by_user       IS INITIAL.
        CLEAR enrich_catsdb-skostl.
      ENDIF.
* stat. key fig.: don't default LSTAR                      "Note 409555
      IF NOT enrich_catsdb-statkeyfig IS INITIAL AND
          sprznr_entered_by_user       IS INITIAL.
        CLEAR enrich_catsdb-sprznr.
      ENDIF.

* don't default sender bus.proc for logistics!              "XQPK186288
      CALL FUNCTION 'CATS_CHECK_APPLICATION'
           EXPORTING
                catsdb_imp     = enrich_catsdb
           IMPORTING
                cats_appli_exp = ucats_appli.
      IF ( ucats_appli-pm = yx OR
           ucats_appli-ps = yx ) AND NOT
         sprznr_entered_by_user = yx.
        CLEAR enrich_catsdb-sprznr.
      ENDIF.
* don't default sender cc / activity type if there is a sender bus.proc
* exception: bus.proc entered by user, target appl. is logistics
      IF NOT enrich_catsdb-sprznr IS INITIAL AND
             ucats_appli-pm <> yx            AND
             ucats_appli-ps <> yx            AND
         NOT enrich_catsdb-skostl IS INITIAL AND
             skostl_entered_by_user <> yx.
        CLEAR enrich_catsdb-skostl.
      ENDIF.
      IF NOT enrich_catsdb-sprznr IS INITIAL AND
             ucats_appli-pm <> yx            AND
             ucats_appli-ps <> yx            AND
         NOT enrich_catsdb-lstar  IS INITIAL AND
             lstar_entered_by_user <> yx.
        CLEAR enrich_catsdb-lstar.
      ENDIF.                                                "XQPK186288

* Szenario D/E                                      "shifted, XQP 4.6A
* get sender cc from infotype 0001 and PLSTA from infotype 0315;
* only possible after default of sender cost center from workcenter!
      CLEAR receiver.
      MOVE-CORRESPONDING enrich_catsdb TO receiver.
* Act. All.: SKOSTL or SPRZNR, LSTAR and RECEIVER!
      IF NOT receiver IS INITIAL.
*       Note 406857, IF condition reformulated
        IF          (    enrich_catsdb-hrcostasg = szenario_d
                      OR enrich_catsdb-hrcostasg = szenario_e )
            AND     enrich_catsdb-statkeyfig IS INITIAL
            AND NOT (     enrich_catsdb-skostl IS INITIAL
                      AND enrich_catsdb-sprznr IS INITIAL ).
          IF up0001 IS INITIAL AND subrc0001 IS INITIAL.
            PERFORM get_pernr_list_0001 USING    enrich_catsdb-pernr
                                                 enrich_catsdb-workdate
                                        CHANGING up0001
                                                 subrc0001.
          ENDIF.
*         master cc successfully read:
          IF NOT up0001-kostl IS INITIAL AND subrc0001 IS INITIAL.
            IF enrich_catsdb-skostl <> up0001-kostl.
*          get PLSTA from infotype 0315
              IF up0315 IS INITIAL AND subrc0315 IS INITIAL.
                PERFORM get_infotype_0315 USING    enrich_catsdb-pernr
                                                 enrich_catsdb-workdate
                                          CHANGING up0315
                                                   subrc0315.
              ENDIF.
*             PLSTA from infotype 0315 successfully read:
              IF NOT up0315-plsta IS INITIAL AND subrc0315 IS INITIAL.
                enrich_catsdb-hrkostl = up0001-kostl.
                IF enrich_catsdb-hrlstar IS INITIAL.
                  enrich_catsdb-hrlstar = up0315-plsta.
                ENDIF.
              ENDIF.
*             PLSTA not successfully read
              IF enrich_catsdb-hrlstar IS INITIAL.
*            Fehler beim Lesen der Stammleistungsart in Szenario D / E
* Replace by Add_Message_Row                             LUXALRK186289
*            PERFORM ADD_MESSAGE USING ARBGB 'E' '322' SPACE SPACE
*                                                      SPACE SPACE.
                PERFORM add_message_row USING arbgb 'E' '322'"LUX
                                             space space space space"LUX
                                              enrich_catsdb-row."LUX
                IF 1 = 2. MESSAGE e322(lr). ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
*         master cc not successfully read
          IF up0001-kostl IS INITIAL.
*     Fehler beim Lesen der Stammkostenstelle in Szenario D oder E
* Replace by Add_Message_Row                             LUXALRK186289
*       PERFORM ADD_MESSAGE USING ARBGB 'E' '321' SPACE SPACE
*                                                 SPACE SPACE.
            PERFORM add_message_row USING arbgb 'E' '321'   "LUX
                                          space space space space"LUX
                                          enrich_catsdb-row."LUX
            IF 1 = 2. MESSAGE e321(lr). ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*     clear sender and activity type in szenario A
      IF enrich_catsdb-hrcostasg = szenario_a.              "XQPK084279
        CLEAR enrich_catsdb-sprznr.                         "XQPK186288
        CLEAR enrich_catsdb-skostl.
        CLEAR enrich_catsdb-lstar.
      ENDIF.
*     clear hrkostl and hrlstar if they are not needed
      IF enrich_catsdb-hrcostasg <> szenario_d AND
         enrich_catsdb-hrcostasg <> szenario_e OR
        receiver IS INITIAL                    OR
        enrich_catsdb-skostl    = up0001-kostl.
        CLEAR: enrich_catsdb-hrkostl,
               enrich_catsdb-hrlstar.
      ENDIF.
* szenario_0: For these data sets, the business logic (defaults &
* checks) is the same, whatever the profile's szenario is!
* Begin of insertion note 313331
      MOVE-CORRESPONDING enrich_catsdb TO receiver.
      IF receiver IS INITIAL AND       "no receiver
         enrich_catsdb-sebeln IS INITIAL AND       "and no MM-SRV data
         enrich_catsdb-sebelp IS INITIAL AND
         enrich_catsdb-lstnr  IS INITIAL.
        enrich_catsdb-hrcostasg = szenario_0.
      ENDIF.
* End of insertion note 313331

      APPEND enrich_catsdb TO buffer_catsdb.
    ENDLOOP.
  ENDLOOP.

* move buffer to enrich
  REFRESH enrich_catsdb.
  enrich_catsdb[] = buffer_catsdb[].

* check for errors
  PERFORM get_errors TABLES tab_messages USING help_subrc.
  IF NOT help_subrc IS INITIAL.
    RAISE enrich_error_occurred.
  ENDIF.

ENDFUNCTION.
*eject
