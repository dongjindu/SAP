* DWI note 641940 - To distinguish between old records and new records
* in the user exit 003
* 4.70
* XZQ 20.12.02 Hinweis 583314 Logistik-Satz mit ALLDF und 0 Stunden
* XZQ 08.04.02 Hinweis 509814 Statistische Kennzahl in der
*                             Einheit MEINH
* FWE 19.03.02 correction of note 496445
* YCY 07.03.02 note 496445 beguz,enduz auf Plausibilität prüfen
* YCY 22.02.02 note 496840
* XCF 18.02.02 Note 495439, CATS notebook, CATSXT-fields in CAT2
* XDE 23.11.01 Note 454089, missing controlling area
* XDEP9CK209577 051101 Note 447720, multiple warnings
* XZQP9CK200753 011001 Hinweis 439497 Unberechtigte Meldung (LR214) bei
*                                     Zuschlagslohnart
* XDEP9CK165392 050601 Note 409555, no sender when stat. key figure
* QWKP9CK134196 230201 note 381207
* YCYP9CK107807 020201 Endgeldbelege und Verrechnungsschlüssel
*                      note 369192
* XZQP9CK076216 141200 Ergebnisobjekt in CATS
* 4.6C
* XQPP00K024269 020300 note 198712
* XQPP00K020654 260100 Kostenzuordnung: BUKRS ins HR übergeben
* 4.6A
* XQPALRK186288 010399 Sendender Geschäftsprozeß
* KMR ALRK176697 040299 Übernahme U_COBL_EX in CATS_CHECK_AC
* KMR ALRK171586 220199 Änderung Interface CATS_CHECK_AC
* XQPALRK169492 190199 Mehrarbeitsverrechnungsart
* KMR ALRK166983 120199 Entkopplung für HR-Only
* 4.5A
* XQPP45K044686 060898 Keine Erfassung von Sätzen mit 0 Stunden
* 4.0C
* XQPALRK097720 260398 Kein Abzug von Pausen bei Berechnung der Stunden
* XQPALRK084279 230398 Änderungen für verbesserte Kostenrechnung
FUNCTION ZCATS_CHECK_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(NO_BREAK_DEDUCTION) LIKE  TCATS-NOBREAKDED
*"         OPTIONAL
*"             VALUE(OLD_DATA) TYPE  BOOLEAN OPTIONAL
*"       TABLES
*"              CHECK_CATSDB STRUCTURE  CATSDB_EXT
*"              TAB_MESSAGES STRUCTURE  MESG
*"       EXCEPTIONS
*"              MESSAGE_OCCURRED
*"----------------------------------------------------------------------
  DATA: u_customer LIKE cats_comm.
  DATA: ucats_appli LIKE cats_appli.
  DATA: help_subrc LIKE sy-subrc.
  DATA: applic_sys TYPE devtype.       "KMR ALRK166983
  DATA: u_cobl_ex  TYPE cobl_ex.       "KMR ALRK166983
  DATA: itab_messages LIKE mesg OCCURS 0 WITH
        HEADER LINE.                                        "YIK
  DATA: subrc LIKE sy-subrc.

  DATA: BEGIN OF tmp_ps,
          rnplnr LIKE check_catsdb-rnplnr,
          vornr  LIKE check_catsdb-vornr,
          uvorn  LIKE check_catsdb-uvorn,
          raufpl LIKE check_catsdb-raufpl,
          raplzl LIKE check_catsdb-raplzl,
        END OF tmp_ps.

* copies of time fields, in case they shall not be changed
  DATA: i_beguz     LIKE catsdb-beguz,                      "XQPK097720
        i_enduz     LIKE catsdb-enduz,                      "XQPK097720
        i_vtken     LIKE catsdb-vtken,                      "XQPK097720
        i_catshours LIKE catsdb-catshours,                  "XQPK097720
        i_alldf     LIKE catsdb-alldf.                      "XQPK097720

  DATA: att_abs_in  LIKE bapi7011_1.                        "YIK
  DATA: att_abs_out LIKE bapi7011_1.                        "YIK
  DATA: remun_in    LIKE bapi7012_1.                        "YIK
  DATA: return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.     "YIK
  DATA: catsinitial TYPE catsh_initial.                     "YIK
  DATA: catstimes TYPE catsh_times.
  DATA: objnr       TYPE j_objnr.      "like aufk-objnr
  DATA: imessages TYPE mesg OCCURS 0 WITH HEADER LINE.      "YIK


  CALL FUNCTION 'RH_APPLICATION_SYSTEM_CHECK'           "KMR ALRK176697
      IMPORTING                        "KMR ALRK176697
           application_system_type = applic_sys.        "KMR ALRK176697
*      exceptions
*           system_type_unknown     = 1
*           basis_system            = 2
*           others                  = 3.

  LOOP AT check_catsdb.
* cobl has to be cleared
    CLEAR u_cobl_ex.                                        "YIK
    i_beguz     = check_catsdb-beguz.                       "XQPK097720
    i_enduz     = check_catsdb-enduz.                       "XQPK097720
    i_vtken     = check_catsdb-vtken.                       "XQPK097720
    i_catshours = check_catsdb-catshours.                   "XQPK097720
    i_alldf     = check_catsdb-alldf.                       "XQPK097720
* store primary key
    MOVE-CORRESPONDING check_catsdb TO u_customer.
* customer checks
    PERFORM customer_checks USING u_customer old_data.  "DWI note 641940
* extended checks
    PERFORM extended_checks USING check_catsdb.             "YIK
* simple plausability checks
    PERFORM simple_checks USING check_catsdb.
* check that employee is active
    PERFORM check_employee_active USING check_catsdb-pernr
                                        check_catsdb-workdate
                                        check_catsdb-row    "LUX
                                        subrc.
    IF subrc = 0.

* begin of note 496445
* check if clock times are valid
      IF check_catsdb-beguz > '240000' OR
         check_catsdb-enduz > '240000' OR
         check_catsdb-beguz+2(2) > 59 OR
         check_catsdb-enduz+2(2) > 59 OR
         check_catsdb-beguz+4(2) > 59 OR
         check_catsdb-enduz+4(2) > 59.
        IF 1 = 2. MESSAGE e295. ENDIF.
        PERFORM add_message_row USING arbgb 'E' '295'
                                      space space space space
                                      check_catsdb-row.
      ENDIF.
* end of note 496445

*   begin insert XCF note 495439
*   First check, if catsxt-fields are used correctly
      IF      NOT check_catsdb-taskcomponent IS INITIAL
           OR NOT check_catsdb-tasktype IS INITIAL
           OR NOT check_catsdb-tasklevel IS INITIAL.
*       if one field is not filled -> error message
        IF    check_catsdb-taskcomponent IS INITIAL
           OR check_catsdb-tasktype IS INITIAL
           OR check_catsdb-tasklevel IS INITIAL.
          IF 1 = 2. MESSAGE e128. ENDIF.
          PERFORM add_message_row USING arbgb 'E' '128'
                                        space space space space
                                        check_catsdb-row.
        ENDIF.
      ENDIF.
* end insert XCF note 495439

* determin the active components
      CALL FUNCTION 'CATS_CHECK_APPLICATION'
           EXPORTING
                catsdb_imp     = check_catsdb
           IMPORTING
                cats_appli_exp = ucats_appli
           EXCEPTIONS
                OTHERS         = 1.

* check, if one application is selected at all
      IF ucats_appli IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '079'       "LUX
                                  space space space space
                                  check_catsdb-row.         "LUX
      ENDIF.


* could be that old record doesnot have a unit.
* Problem occurs when old record from cats < 4.6 is checked

      IF check_catsdb-unit IS INITIAL AND                   "note325504
         check_catsdb-waers IS INITIAL.
        check_catsdb-unit = check_catsdb-meinh.
      ENDIF.
* Quantity: MM-SRV or stat. keyfig. or wage type required
      IF check_catsdb-unit NE check_catsdb-meinh
         AND ucats_appli-mm IS INITIAL
         AND check_catsdb-statkeyfig IS INITIAL
         AND check_catsdb-lgart IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '265'
                                      space space space space
                                      check_catsdb-row.
        IF 1 = 2. MESSAGE e265. ENDIF.
      ENDIF.
*---------XZQ Beginn Hinweis 583314-----------
* Bei einem Satz mit ALLDF = 'X' und 0 Stunden und einer
* Logistik-Zielanwendung wird das ALLDF gelöscht.
* Sonst: Satz kann nicht in Logistik-ZA übergeleitet werden
      IF NOT check_catsdb-alldf    IS INITIAL AND
         check_catsdb-catshours    IS INITIAL AND
         check_catsdb-catsquantity IS INITIAL AND
         check_catsdb-beguz CO ' 0'           AND
         check_catsdb-enduz CO ' 0'           AND
         ( NOT ucats_appli-co IS INITIAL OR
           NOT ucats_appli-ps IS INITIAL OR
           NOT ucats_appli-pm IS INITIAL OR
           NOT ucats_appli-mm IS INITIAL ).
        CLEAR check_catsdb-alldf.
      ENDIF.
*---------XZQ Ende Hinweis 583314--------------
*---------------------------------------------------------------------*
* no sender bus.proc for logistics!                       "XQPK186288 *
* this message should come before the 'no two senders' message!       *
*---------------------------------------------------------------------*
      IF ( ucats_appli-pm = yx OR
         ucats_appli-ps = yx ) AND
         NOT check_catsdb-sprznr IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '263'
                                    space space space space
                                    check_catsdb-row.
        IF 1 = 2. MESSAGE e263. ENDIF.
      ENDIF.
*---------------------------------------------------------------------*
* check CO                                                            *
*---------------------------------------------------------------------*
* check data against master
      IF ucats_appli-co = yx                                "XQPK084279
     AND ucats_appli-pm = space   "otherwise: szenario D/E    XQPK084279
        AND ucats_appli-ps = space.                         "XQPK084279

        IF NOT check_catsdb-catsamount IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '266'
                                        space space space space
                                        check_catsdb-row.
          IF 1 = 2. MESSAGE e266. ENDIF.
        ENDIF.
*       no sender when statistical key figure              "Note 409555
        IF     NOT check_catsdb-statkeyfig IS INITIAL
           AND (     NOT check_catsdb-sprznr IS INITIAL
                 OR  NOT check_catsdb-skostl IS INITIAL ).
* note 409555 describes an incompatibel change ->       "note 496840
*   allow deletion for old records                      "note 496840
          MOVE-CORRESPONDING check_catsdb TO catsinitial.  "note 496840
          MOVE-CORRESPONDING check_catsdb TO catstimes.    "note 496840
          IF   check_catsdb-counter IS INITIAL             "note 496840
            AND NOT ( catstimes CO ' 0'                    "note 496840
            AND catsinitial IS INITIAL ).                  "note 496840
            PERFORM add_message_row USING arbgb 'E' '334'
                                        space space space space
                                        check_catsdb-row.
            IF 1 = 2. MESSAGE e334. ENDIF.
          ENDIF.                                           "note 496840
          CLEAR: catsinitial,catstimes.                    "note 496840
        ENDIF.

        IF check_catsdb-statkeyfig IS INITIAL.
          PERFORM cats_check_ac        "KMR ALRK176697
                  USING applic_sys send_and_recv check_catsdb u_cobl_ex
                        space.                              "P9CK044565
          IF check_catsdb-unit NE check_catsdb-meinh.
            PERFORM add_message_row USING arbgb 'E' '270'
                                       check_catsdb-unit
                                       space space space
                                       check_catsdb-row.
            IF 1 = 2. MESSAGE e270. ENDIF.
          ENDIF.
        ELSE.                          "begin KMR ALRK254592
          PERFORM cats_check_ac        "KMR ALRK176697
                  USING applic_sys recver check_catsdb u_cobl_ex
                  space.                                    "P9CK044565
        ENDIF.                         "end KMR ALRK254592
* check statkeyfig and paobjnr
* Begin XZQP9CK076216----------------------------------------
        IF NOT check_catsdb-statkeyfig IS INITIAL
        AND NOT check_catsdb-paobjnr IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '288'
                                      space space
                                      space space
                                      check_catsdb-row.
          IF 1 = 2. MESSAGE e288. ENDIF.
        ENDIF.
* End XZQP9CK076216-------------------------------------------
* info, if PM order/network without operation --> is this by mistake?
* --> possibly wrong target application
        IF ( NOT u_cobl_ex-aufnr IS INITIAL
             AND u_cobl_ex-autyp = '30' ) OR
           ( NOT u_cobl_ex-nplnr IS INITIAL
             AND u_cobl_ex-vornr IS INITIAL ).
*------------Beginn XZQ Hinweis 439497---------------------
          IF NOT check_catsdb-vornr IS INITIAL AND
          NOT check_catsdb-lgart IS INITIAL.
* Die Meldung LR214 ist hier nicht angebracht.
* Hier würde bestenfalls die Meldung Sinn machen, daß das
* CO nun Zielapplikation ist, obwohl auf ein Inst-Auftrag/Netzplan
* mit Vorgang kontiert wird. Der Wechsel der Zielapplikation
* erfolgt durch die Lohnart, die implizit eine
* Zuschlagslohnart sein muß.
* Auf eine entsprechende Meldung wird verzichtet.
          ELSE.
* In diesem Fall ist die LR214 berechtigt, weil der Vorgang
* fehlt und deshalb keine Rückmeldung ins PM/PS, sondern nur
* eine LV ins CO erfolgt, und die fehlende VORNR u.U.
* ein Versehen sein könnte!
*------------Ende XZQ Hinweis 439497-----------------------
            PERFORM add_message_row                         "LUX
                    USING arbgb 'I' '214' u_cobl_ex-aufnr
                          space space space
                          check_catsdb-row.                 "LUX
         IF 1 = 2. MESSAGE i214 WITH u_cobl_ex-aufnr. ENDIF. "cross ref.
          ENDIF.                       "Hinweis 439497
        ENDIF.                         " '30' = auftragstyp-inst
*-------------XZQ Beginn Hinweis 509814-------------------
* Sätze mit stat. Kennzahl und UNIT = MEINH sollen auch nicht
* mehr als 99,99 Stunden pro Tag haben dürfen.
* Dies war bisher nur implizit abgefangen bei Sätzen, die auch
* eine AWART oder LGART haben (Siehe HR-CHECKS)
        IF NOT check_catsdb-statkeyfig IS INITIAL AND
        check_catsdb-unit = check_catsdb-meinh AND
        check_catsdb-catsquantity >= hour_max.
          PERFORM add_message_row USING '00' 'E'  '092'
                                           space space
                                           space space
                                           check_catsdb-row.
        ENDIF.
*-------------XZQ Ende Hinweis 509814---------------------
      ENDIF.
*---------------------------------------------------------------------*
* check HR                                                            *
*---------------------------------------------------------------------*
* Kostenrechnungsszenarium 'A' bei PAOBJNR nicht möglich
* Begin XZQP9CK076216 ----------------------------------------
      IF check_catsdb-hrcostasg = 'A'
      AND NOT check_catsdb-paobjnr IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '289'
                                    space space
                                    space space
                                    check_catsdb-row.
        IF 1 = 2. MESSAGE e289. ENDIF.
      ENDIF.
* End XZQP9CK076216-------------------------------------------
      IF ucats_appli-hr <> yx.
*     if not HR (no AWART) then clear ALLDF
        CLEAR check_catsdb-alldf.
      ELSE.
* neither awart nor lgart entered
        IF     check_catsdb-awart IS INITIAL AND            "XQPK169492
               check_catsdb-lgart IS INITIAL AND           "note 369192
           NOT check_catsdb-versl IS INITIAL.               "XQPK169492
          PERFORM add_message_row USING arbgb 'E' '258'     "LUX
                                  space space space space   "XQPK169492
                                    check_catsdb-row.       "LUX
          IF 1 = 2. MESSAGE e258. ENDIF.                    "XQPK169492
        ENDIF.                                              "XQPK169492
        IF check_catsdb-awart IS INITIAL AND
           check_catsdb-lgart IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '246'     "LUX
                                    space space space space
                                    check_catsdb-row.       "LUX
          IF 1 = 2. MESSAGE e246. ENDIF.

        ENDIF.
* both lgart and awart entered
        IF NOT check_catsdb-awart IS INITIAL AND
           NOT check_catsdb-lgart IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '245'     "LUX
                                    space space space space
                                    check_catsdb-row.       "LUX
          IF 1 = 2. MESSAGE e245. ENDIF.
        ENDIF.

        IF NOT check_catsdb-awart IS INITIAL.

* --------> Note 449025 <--------   ->start
          IF old_data = 'X'.
* The data to be checked is the old data. Threfore, checks can
* be less strong than for new entries (e.g. to allow deleting
* entries that would now be illigal to enter but that are in the
* data already). In fact, for HR-purposes, checking if the user
* has sufficient rights is enough.

            CALL FUNCTION 'CATS_CHECK_ATT_ABS_AUTHORITY'
                 EXPORTING
                      pernr           = check_catsdb-pernr
                      begda           = check_catsdb-workdate
                      endda           = check_catsdb-workdate
                      awart           = check_catsdb-awart
                      action          = 'E'
                 EXCEPTIONS
                      pernr_not_found = 1
                      invalid_action  = 2
                      awart_not_found = 3
                      no_authority    = 4
                      system_error    = 5
                      OTHERS          = 6.
            IF sy-subrc <> 0.

              PERFORM add_message_row
                        USING  sy-msgid sy-msgty
                               sy-msgno sy-msgv1 sy-msgv2
                               sy-msgv3 sy-msgv4
                               check_catsdb-row.

            ENDIF.  "--> sy-subrc <> 0. ('CATS_CHECK_ATT_ABS_AUTHORITY')

          ELSE.                        "--> old_data <> 'X'
* The data to be checked is the new data. Threfore, checks have
* to be stronger than e.g. when deleting entries.
* --------> Note 449025 <--------   <-end

            IF NOT check_catsdb-catsamount IS INITIAL.
              PERFORM add_message_row USING arbgb 'E' '269'
                                            space space space space
                                            check_catsdb-row.
              IF 1 = 2. MESSAGE e269. ENDIF.
            ENDIF.
            IF NOT check_catsdb-unit = unit_of_hour.
              PERFORM add_message_row USING arbgb 'E' '272'
                                            check_catsdb-unit
                                            space space space
                                            check_catsdb-row.
              IF 1 = 2. MESSAGE e272. ENDIF.
            ENDIF.
* could be that the entry is too long
            IF check_catsdb-catsquantity GT hour_max.
              PERFORM add_message_row USING '00' 'E'  '092'
                                         space space
                                         space space
                                         check_catsdb-row.  "LUX

            ELSE.

              PERFORM convert_cats_to_bapi7011_1 USING check_catsdb
                                                 CHANGING att_abs_in.


              CLEAR return.                                "note 381207
              CLEAR return[].                              "note 381207

              CALL FUNCTION 'CATS_ATTABS_CHECK'
                   EXPORTING
                        attendance_absence_in  = att_abs_in
                   IMPORTING
                        attendance_absence_out = att_abs_out
                   TABLES
                        return                 = return.

              LOOP AT return.
                PERFORM add_message_row USING return-id     "LUX
                                          return-type
                                          return-number
                                          return-message_v1
                                          return-message_v2
                                          return-message_v3
                                          return-message_v4
                                          check_catsdb-row. "LUX
              ENDLOOP.

              IF no_break_deduction = yx.
                CLEAR check_catsdb-alldf.                   "YIK
*             IF CHECK_CATSDB-BEGUZ CN ' 0' AND   "del note 198712
                IF check_catsdb-beguz CN ' 0' OR           "note 198712
                     check_catsdb-enduz CN ' 0'.
                  IF NOT check_catsdb-statkeyfig IS INITIAL.
                    check_catsdb-catshours = check_catsdb-catsquantity.
                  ENDIF.
                  PERFORM compute_hours_directly
                          USING    check_catsdb-beguz
                                   check_catsdb-enduz
                          CHANGING check_catsdb-catshours.
                  IF NOT check_catsdb-statkeyfig IS INITIAL.
                    check_catsdb-catsquantity = check_catsdb-catshours.
                    CLEAR check_catsdb-catshours.
                  ENDIF.
                ENDIF.
              ELSE.
                check_catsdb-beguz      = att_abs_out-start_time.
                check_catsdb-enduz      = att_abs_out-end_time.
                check_catsdb-vtken      = att_abs_out-previous_day.
                IF check_catsdb-statkeyfig IS INITIAL.
                  check_catsdb-catshours  = att_abs_out-abs_att_hours.
                ELSE.
                 check_catsdb-catsquantity  = att_abs_out-abs_att_hours.
                ENDIF.
                check_catsdb-alldf      = att_abs_out-all_day_flag.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF.
* begin of insertion yik
        IF NOT check_catsdb-lgart IS INITIAL.
* could be that the entry is too long
          IF check_catsdb-catsquantity GT hour_max_lgart.
            PERFORM add_message_row USING '00' 'E'  '092'
                                       space space
                                       space space
                                       check_catsdb-row.    "LUX

          ELSE.
            PERFORM convert_cats_to_bapi7012_1 USING check_catsdb
                                               CHANGING remun_in.

            CLEAR check_catsdb-alldf.                       "YIK
            CLEAR check_catsdb-vtken.                       "YIK
            IF check_catsdb-unit = unit_of_hour.            "YIK
              CLEAR remun_in-number.
            ENDIF.

            CLEAR return.                                  "note 381207
            CLEAR return[].                                "note 381207

            CALL FUNCTION 'CATS_REMUN_CHECK'
                 EXPORTING
                      remun_in = remun_in
                 TABLES
                      return   = return.

            LOOP AT return.
              PERFORM add_message_row USING return-id       "LUX
                                        return-type
                                        return-number
                                        return-message_v1
                                        return-message_v2
                                        return-message_v3
                                        return-message_v4
                                        check_catsdb-row.   "LUX
            ENDLOOP.
          ENDIF.
        ENDIF.

      ENDIF.
*---------------------------------------------------------------------*
* check MM                                                            *
*---------------------------------------------------------------------*
      IF ucats_appli-mm = yx.
        IF NOT check_catsdb-catsamount IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '268'
                                        space space space space
                                        check_catsdb-row.
          IF 1 = 2. MESSAGE e268. ENDIF.
        ENDIF.

        IF check_catsdb-hrcostasg <> szenario_space.        "XQPK084279
*       szenarios don't make sense for MM-SRV as there is no payroll!
          PERFORM add_message_row USING arbgb 'E' '320'     "LUX
                                  space space space space   "XQPK084279
                                    check_catsdb-row.       "LUX
          IF 1 = 2. MESSAGE e320. ENDIF.
        ENDIF.
        CALL FUNCTION 'CATS_MM_CHECK_001'
             EXPORTING
                  check_catsdb = check_catsdb.
*            exceptions
*                 others       = 1.
      ENDIF.
*---------------------------------------------------------------------*
* check PM and PS                                                     *
*---------------------------------------------------------------------*
      IF ucats_appli-pm = yx OR
         ucats_appli-ps = yx.
        IF check_catsdb-arbpl IS INITIAL OR
           check_catsdb-werks IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '244' space"LUX
                                                    space
                                                    space
                                                    space
                                                 check_catsdb-row."LUX
        ENDIF.
*       Is work center locked/flagged for deletion?
        IF NOT check_catsdb-arbpl IS INITIAL AND
           NOT check_catsdb-werks IS INITIAL.
          CALL FUNCTION 'CATS_CHECK_ARBPL_VALID'
               EXPORTING
                    appli_pm_imp       = ucats_appli
                    catsdb_imp         = check_catsdb
               EXCEPTIONS
                    locked             = 1
                    deleted            = 2
                    locked_and_deleted = 3.
          IF sy-subrc = 2 OR
             sy-subrc = 3.
            PERFORM add_message_row USING 'CO' 'E' '214'
                                          check_catsdb-arbpl
                                          check_catsdb-werks
                                          space
                                          space
                                          check_catsdb-row.
          ENDIF.
          IF sy-subrc = 1 OR
             sy-subrc = 3.
            PERFORM add_message_row USING 'CO' 'E' '624'
                                          check_catsdb-arbpl
                                          check_catsdb-werks
                                          space
                                          space
                                          check_catsdb-row.
          ENDIF.
        ENDIF.
        IF NOT check_catsdb-catsamount IS INITIAL.
          PERFORM add_message_row USING arbgb 'E' '267'
                                      space space space space
                                      check_catsdb-row.
          IF 1 = 2. MESSAGE e267. ENDIF.
        ENDIF.
        IF check_catsdb-unit NE check_catsdb-meinh.
          PERFORM add_message_row USING arbgb 'E' '271'
                                             check_catsdb-unit
                                             space space space
                                             check_catsdb-row.
          IF 1 = 2. MESSAGE e271. ENDIF.
        ENDIF.
* Decoupling CATS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Get table AUFNR
        IF appl_sys_type <> c_appl_sys_type_hro.
          CALL FUNCTION 'CATS_GET_TABLE_AUFNR'
               TABLES
                    aufnr_exp = aufnr.
* Determine objectnumber
          LOOP AT aufnr WHERE
            raufpl EQ check_catsdb-raufpl AND
            raplzl EQ check_catsdb-raplzl.
            EXIT.
          ENDLOOP.
          IF sy-subrc IS INITIAL.
* check status of receiver (Header)
            CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
                 EXPORTING
                      check_only = yx
                      no_check   = space
                      objnr      = aufnr-objnrh
                      vrgng      = 'RMRU'
                 EXCEPTIONS
                      OTHERS     = 99.
            IF NOT sy-subrc IS INITIAL.
              PERFORM add_message_row USING arbgb 'E' '666' 'OBJNR'"LUX
                                                          space
                                                          space
                                                          space
                                             check_catsdb-row."LUX
            ENDIF.
* check status of receiver (Activity)
            CALL FUNCTION 'STATUS_CHANGE_FOR_ACTIVITY'
                 EXPORTING
                      check_only = yx
                      no_check   = space
                      objnr      = aufnr-objnrv
                      vrgng      = 'RMVR'
                 EXCEPTIONS
                      OTHERS     = 99.
            IF NOT sy-subrc IS INITIAL.
              PERFORM add_message_row USING arbgb 'E' '666' 'OBJNR'"LUX
                                                          space
                                                          space
                                                          space
                                             check_catsdb-row."LUX
            ENDIF.
          ENDIF.
* Decoupling CATS <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

* check sender
          IF check_catsdb-hrcostasg <> szenario_a.          "XQPK084279

            PERFORM cats_check_ac      "KMR ALRK176697
                     USING applic_sys sender check_catsdb u_cobl_ex
                          space.                            "P9CK044565

          ENDIF.                                            "XQPK084279
        ELSE.
          PERFORM add_message_row USING arbgb 'E' '666' 'OBJNR'"LUX
                                                     space
                                                     space
                                                     space
                                        check_catsdb-row.   "LUX
        ENDIF.
*---------------------------------------------------------------------*
* check final confirmation indicator and flag 'Reset final confirm.'
*---------------------------------------------------------------------*
        IF NOT check_catsdb-aueru IS INITIAL AND
           NOT check_catsdb-eruzu IS INITIAL.
          IF ucats_appli-ps = yx.
            PERFORM add_message_row USING arbgb 'E' '243'   "LUX
                                      check_catsdb-rnplnr
                                      check_catsdb-vornr
                                      check_catsdb-workdate
                                      space
                                      check_catsdb-row.     "LUX
          ELSE.
            PERFORM add_message_row USING arbgb 'E' '243'   "LUX
                                      check_catsdb-raufnr
                                      check_catsdb-vornr
                                      check_catsdb-workdate
                                      space
                                      check_catsdb-row.     "LUX
          ENDIF.
        ENDIF.
*---------------------------------------------------------------------*
* check statistic accountings                                         *
*---------------------------------------------------------------------*
*     IF NOT CHECK_CATSDB-RKOSTL IS INITIAL OR              "XQPK084279
*        NOT CHECK_CATSDB-RPROJ  IS INITIAL OR              "XQPK084279
*        NOT CHECK_CATSDB-RAUFNR IS INITIAL.                "XQPK084279
*       MOVE-CORRESPONDING CHECK_CATSDB TO TMP_PS.
*       CLEAR CHECK_CATSDB-RNPLNR.
*       CLEAR CHECK_CATSDB-VORNR.
*       CLEAR CHECK_CATSDB-UVORN.
*       CLEAR CHECK_CATSDB-RAUFPL.
*       CLEAR CHECK_CATSDB-RAPLZL.
*       check receiver
*       U_COBL_EX-VORGN = ACTIVITY-RKL."KMR ALRK176697
*
*       PERFORM CATS_CHECK_AC
*               USING APPLIC_SYS RECVER CHECK_CATSDB U_COBL_EX.
*       MOVE-CORRESPONDING TMP_PS TO CHECK_CATSDB.
*     ENDIF.
*---------------------------------------------------------------------*
*     Receiver check for logistic PS/PM/SM                            *
*---------------------------------------------------------------------*
        MOVE-CORRESPONDING check_catsdb TO tmp_ps.
* PS
        IF ucats_appli-ps = yx.
* Decoupling CATS
          CALL FUNCTION 'K_ORDER_READ'
               EXPORTING
                    aufnr     = check_catsdb-rnplnr
               IMPORTING
                    objnr     = objnr
               EXCEPTIONS
                    not_found = 1.
          IF sy-subrc IS INITIAL.
            CALL FUNCTION 'STATUS_CHECK'
                 EXPORTING
                      objnr             = objnr
                      status            = 'I0053'
                 EXCEPTIONS
                      object_not_found  = 1
                      status_not_active = 2
                      OTHERS            = 3.
          ENDIF.
          CASE sy-subrc.
            WHEN 0.
            WHEN 2.
              CLEAR check_catsdb-vornr.
              CLEAR check_catsdb-uvorn.
              CLEAR check_catsdb-raplzl.
            WHEN OTHERS.
              PERFORM add_message USING arbgb 'E' '666' space
                                                        space
                                                        space
                                                        space.
              IF 1 = 2. MESSAGE e666. ENDIF.
          ENDCASE.
        ELSE.
* PM/SM
          CLEAR check_catsdb-vornr.
          CLEAR check_catsdb-uvorn.
          CLEAR check_catsdb-raplzl.
        ENDIF.
* check receiver
*     check receiver
        u_cobl_ex-vorgn = activity-rkl."KMR ALRK176697

        PERFORM cats_check_ac
                USING applic_sys recver check_catsdb u_cobl_ex
                     'RMRU'.                                "P9CK044565
        MOVE-CORRESPONDING tmp_ps TO check_catsdb.
      ENDIF.
*----------------------------------------------------------------------*
* check CO data for target application HR      - new for 4.0C XQPK084279
*----------------------------------------------------------------------*
      CASE check_catsdb-hrcostasg.
        WHEN szenario_a.
*       no CO activity allocation
          IF check_catsdb-skostl <> space OR
           check_catsdb-sprznr <> space OR                  "XQPK186288
             check_catsdb-lstar  <> space.
           PERFORM add_message_row USING arbgb 'E' '318' space space"LUX
                                                            space space
                                                   check_catsdb-row."LUX
            IF 1 = 2. MESSAGE e318. ENDIF.
          ENDIF.
*       following checks only if a receiver is entered
          CLEAR receiver.
          MOVE-CORRESPONDING check_catsdb TO receiver.
          IF NOT receiver IS INITIAL.
*         HR must be target application
            IF ucats_appli-hr <> yx.
              PERFORM add_message_row USING arbgb 'E' '317' space"LUX
                                            space space space"LUX
                                            check_catsdb-row."LUX
              IF 1 = 2. MESSAGE e317. ENDIF.
            ENDIF.
*         cost assignment on receiver object
           u_cobl_ex-vorgn = activity-rfbu.              "KMR ALRK176697
            u_cobl_ex-budat = check_catsdb-workdate.
*           READ TABLE ALLOWED_PERNR WITH KEY PERNR = CHECK_CATSDB-PERNR
*         U_COBL_EX-BUKRS = ALLOWED_PERNR-BUKRS.        "DEL XQPK020654
            IF check_catsdb-kokrs IS INITIAL.              "Note 454089
              READ TABLE    allowed_pernr                  "Note 454089
                   WITH KEY pernr = check_catsdb-pernr.    "Note 454089
*             derive controlling area from company code
              CALL FUNCTION 'BAPI_CONTROLLINGAREA_FIND'    "Note 454089
                 EXPORTING                                 "Note 454089
                   companycodeid     = allowed_pernr-bukrs "Note 454089
                 IMPORTING                                 "Note 454089
                   controllingareaid = check_catsdb-kokrs  "Note 454089
               EXCEPTIONS                                  "Note 454089
                   OTHERS            = 1.                  "Note 454089
            ENDIF.                                         "Note 454089
            u_cobl_ex-kokrs = check_catsdb-kokrs.           "XQPK020654
            PERFORM cats_check_ac
                    USING applic_sys recver check_catsdb u_cobl_ex
                        space.                              "P9CK044565
            check_catsdb-bukrs = u_cobl_ex-bukrs.           "XQPK020654
          ENDIF.

        WHEN szenario_b OR szenario_c.
*        following checks only if sender and activity type are entered
          IF  NOT check_catsdb-skostl IS INITIAL AND
              NOT check_catsdb-lstar  IS INITIAL OR
           NOT check_catsdb-sprznr IS INITIAL.              "XQPK186288
*         HR must be target application
            IF ucats_appli-hr <> yx.
              PERFORM add_message_row USING arbgb 'E' '317' space"LUX
                                                        space
                                                        space

                                                        space
                                            check_catsdb-row."LUX
              IF 1 = 2. MESSAGE e317. ENDIF.
            ENDIF.
*         cost assignment on sender
           READ TABLE allowed_pernr WITH KEY pernr = check_catsdb-pernr.
            PERFORM check_co_szenario_b
                 USING    check_catsdb-workdate
*                       ALLOWED_PERNR-BUKRS             "DEL XQPK020654
                          check_catsdb-kokrs
                          check_catsdb-skostl
                       check_catsdb-sprznr                  "XQPK186288
              CHANGING check_catsdb-bukrs.                  "XQPK020654

          ENDIF.
        WHEN szenario_d.
          PERFORM check_lgart   USING check_catsdb.         "YIK
          IF NOT check_catsdb-hrkostl IS INITIAL.
            PERFORM check_co_szenario_d      USING check_catsdb.
          ENDIF.
        WHEN szenario_e.
          PERFORM check_lgart   USING check_catsdb.         "YIK
          IF NOT check_catsdb-hrkostl IS INITIAL.
            PERFORM check_co_szenario_d      USING check_catsdb.
          ENDIF.
*       HR must be target application
          IF ucats_appli-hr <> yx.
            PERFORM add_message_row USING arbgb 'E' '317' space"LUX
                                                      space
                                                      space
                                                      space
                                          check_catsdb-row. "LUX
            IF 1 = 2. MESSAGE e317. ENDIF.
          ENDIF.
      ENDCASE.
*---------------------------------------------------------------------*
* check sender without receiver                                       *
*---------------------------------------------------------------------*
      IF ucats_appli-co IS INITIAL AND
         ucats_appli-pm IS INITIAL AND
         ucats_appli-ps IS INITIAL AND
        ( NOT check_catsdb-skostl IS INITIAL OR
          NOT check_catsdb-lstar  IS INITIAL OR
       NOT check_catsdb-sprznr IS INITIAL ).                "XQPK186288
        IF NOT check_catsdb-lstar  IS INITIAL OR
         NOT check_catsdb-sprznr IS INITIAL .              "note 335020
*     check sender
          PERFORM cats_check_ac        "KMR ALRK176697
                  USING applic_sys sender check_catsdb u_cobl_ex
                      space.                                "P9CK044565
* begin of note 335020
        ELSE.
          CALL FUNCTION 'K_HR_COSTCENTER_GETDETAIL'
               EXPORTING
                    controllingarea            = check_catsdb-kokrs
                    costcenter                 = check_catsdb-skostl
                    read_date                  = check_catsdb-workdate
               EXCEPTIONS
                    nothing_found              = 1
                    no_application_system      = 2
                    unknown_application_system = 3
                    OTHERS                     = 4.
          IF sy-subrc <> 0.
            PERFORM add_message_row USING sy-msgid sy-msgty sy-msgno
                                                sy-msgv1
                                                sy-msgv2
                                                sy-msgv3
                                                sy-msgv4
                                                check_catsdb-row.
          ENDIF.

        ENDIF.
* end of note 335020

      ENDIF.
*---------------------------------------------------------------------*
* General checks                                                      *
*---------------------------------------------------------------------*
*   data sets with 0 hours                                  "XQPK044686
      MOVE-CORRESPONDING check_catsdb TO catsinitial.       "YIK
      MOVE-CORRESPONDING check_catsdb TO catstimes.         "YIK

      IF    check_catsdb-counter IS INITIAL                 "XQPK044686
           AND catstimes CO ' 0'
           AND catsinitial IS INITIAL.                      "YIK
        PERFORM add_message_row USING arbgb 'E' '249' space "LUX
                                               space        "XQPK044686
                                               space        "XQPK044686
                                               space        "XQPK044686
                                      check_catsdb-row.     "LUX
        IF 1 = 2. MESSAGE e249. ENDIF.                      "XQPK044686
      ENDIF.                                                "XQPK044686
      IF NOT check_catsdb-arbpl IS INITIAL AND
             check_catsdb-werks IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '188' space "LUX
                                                  space
                                                  space
                                                  space
                                      check_catsdb-row.     "LUX

      ENDIF.
      IF NOT check_catsdb-price IS INITIAL AND
             check_catsdb-tcurr IS INITIAL.
        PERFORM add_message_row USING arbgb 'E' '189' space "LUX
                                                  space
                                                  space
                                                  space
                                      check_catsdb-row.     "LUX

      ENDIF.
      IF ( NOT check_catsdb-kapar IS INITIAL AND
             check_catsdb-split IS INITIAL ) OR
         ( NOT check_catsdb-split IS INITIAL AND
             check_catsdb-kapar IS INITIAL ).
        PERFORM add_message_row USING arbgb 'E' '234' space "LUX
                                                  space
                                                  space
                                                  space
                                      check_catsdb-row.     "LUX

      ENDIF.
    ENDIF.
* Begin of insertion YIK
*    PERFORM get_errors TABLES imessages USING help_subrc. note 381207

* begin of insertion note 381207.
*    IF help_subrc = 0. "XCF update note 381207

    CLEAR imessages[].
    CALL FUNCTION 'MESSAGES_GIVE'
         TABLES
              t_mesg = imessages
         EXCEPTIONS
              OTHERS = 1.

    IF sy-subrc <> 0.
      MESSAGE x030.
    ENDIF.

    IF NOT imessages[] IS INITIAL.
      help_subrc = 4.
    ENDIF.

*     ENDIF. "XCF update note 381207
* end of insertion note 381207

    LOOP AT imessages.
      MOVE-CORRESPONDING imessages TO tab_messages.
      IF tab_messages-zeile CO ' 0'.
        MOVE check_catsdb-row TO tab_messages-zeile.
      ENDIF.
      READ TABLE tab_messages FROM tab_messages            "Note 447720
           TRANSPORTING NO FIELDS.                         "Note 447720
      IF sy-subrc <> 0.                                    "Note 447720
        APPEND tab_messages.
      ENDIF.                                               "Note 447720
    ENDLOOP.

    PERFORM activate_messages TABLES imessages.

*---------------------------------------------------------------------*
* E N D - O F - C H E C K S                                           *
*---------------------------------------------------------------------*
    MODIFY check_catsdb.

  ENDLOOP.
* store messages in message handler
  LOOP AT tab_messages.
    PERFORM add_message_row USING tab_messages-arbgb tab_messages-msgty
                                  tab_messages-txtnr tab_messages-msgv1
                                  tab_messages-msgv2 tab_messages-msgv3
                                  tab_messages-msgv4 tab_messages-zeile.
  ENDLOOP.
* check for errors
*  PERFORM GET_ERRORS TABLES TAB_MESSAGES USING HELP_SUBRC.  YIK
  IF NOT help_subrc IS INITIAL.
    RAISE message_occurred.
  ENDIF.

ENDFUNCTION.
*eject
