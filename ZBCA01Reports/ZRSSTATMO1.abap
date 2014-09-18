*-----------------------------------------------------------------------
*
*   Formroutinen für Import/Export Datenbank Moni
*
*-----------------------------------------------------------------------

*
*  UPDATE-MONI: Akutalisieren aller Datenbestände der MONI
*  Beschreibung:
*
*  Diese Routine akutalisiert alle Datenbestände und stellt die
*  aktuellen Werte in den diversen internen Tabellen zur Verfügung.
*    RDATE            datum            default = Sy-Datum
*    RTSTART          Messbeginnzeit   default = '000000'.
*    RTSTOP           Messendezeit     default = '235959'.
*    RSYSTEM          cpuid            default = eigen CPUID
*    RSYSTEMID        systemid         default = eigenes System
*    RPERIOD          periode          default = 'DAY'.
*
*    COUNTLIMIT       max. anzahl lesen des Stat.records( 0 = unendlich)
*    INITFLAG         N: Parameter RDATE RSYSTEM.......... werden aus
*                        Memory importiert und standardbehandelt.
*                        Pfad PFAD der Statistikdatei wird geholt.
*                        INITFLAG Wird anschliessen auf 'Y' gesetzt.
*                     Y: Externe Bestimmung von RDATE RSYSTEM.........
*                        sowie des Statistik-Pfadnamens.

*  Wichtig:
*  A.)   RDATE bestimmt die Summensaetze, die am Ende der Routine
*        in den internen Tabellen stehen (SUMMU,SUMMT,HITL1,...)
*
*  B.)   RDATE bestimmt auch, wie das Update auf MONI geschieht.
*        Dazu wird die Tabelle PERIOD aus MONI gelesen, in der
*        vermerkt ist, wann der letzte Satz fuer die Kombination
*        CPUID/SYSTEMID gelesen wurde. Ist RDATE ein Tag kleiner
*        als der Last-Record-Tag, wird NICHT upgedatet.
*        Sonst wird immer upgedatet, aber bitte A.) beachten !
*
*  C.)   RPERIOD bestimmt den Zeitraum, fuer welchen die internen
*        Tabellen gefuellt werden sollen. Ist RPERIOD nicht
*        'DAY', so wird zwar ein Update ( in Abhaengigkeit von RDATE !)
*        auf Datei MONI durchgefuehrt, aber die internen Tabellen mit
*        entsprechenden Periodenwerten (Woche, Monat, Jahr)  gefuellt.
*
*-----------------------------------------------------------------------
*
*
*---Beginn Kommunikation mit MONI--------------------------------------
*---Aktuelle Saetze bearbeiten und in MONI ablegen---------------------
*
FORM update-moni.
************************************************************************
* Form for release >= 3.0C                                             *
************************************************************************

  DATA  wp_number                    LIKE sapwlpstrc-wp_number
                                     VALUE 'FFFF'.
  DATA  v2_normal_records            LIKE sapwlpfnrm OCCURS 0.
  DATA  v2_btc_step_records          LIKE sapwlpfbtc OCCURS 0.
  DATA  v2_table_records             LIKE sapwlpftab OCCURS 0.
  DATA  v2_rfc_client_records        LIKE sapwlpfrc  OCCURS 0.
  DATA  v2_rfc_server_records        LIKE sapwlpfrs  OCCURS 0.
  DATA  v2_rfc_client_dest_records   LIKE sapwlpfrcd OCCURS 0.
  DATA  v2_rfc_server_dest_records   LIKE sapwlpfrsd OCCURS 0.
  DATA  v2_spool_print_records       LIKE sapwlpfspp OCCURS 0.
  DATA  v2_spool_activity_records    LIKE sapwlpfspa OCCURS 0.
  DATA  norm_subrecord_index         LIKE sapwlsrcix OCCURS 0.
  DATA  file_error                   LIKE sapwlpstrc-file_error.
  DATA  statistic_version_used       LIKE sapwlpstrc-version.
  DATA  records_read                 LIKE sapwlsfidx-recordno.
  DATA  time_resolution              LIKE sapwlpstrc-time_res.
  DATA  date                         LIKE sy-datum.
  DATA  tasktypes_per_workprocess    LIKE sapwlpstrc-flag.
  DATA  tcodes_with_dynpro_number    LIKE sapwlpstrc-flag.
  DATA  terminal_instead_of_username LIKE sapwlpstrc-flag.
  DATA  eof_reached                  LIKE sapwlpstrc-flag.

* JS 08.09.99
  data: my_name         type   stuninst.

  CALL 'C_SAPGPARAM'   ID 'NAME'    FIELD 'rdisp/myname'
                       ID 'VALUE'   FIELD my_name.


* first get the parameters from global variables

  IF initflag NE 'Y'.
    IMPORT rdate rinstance rsystem rsystemid rperiod rcommand
                              FROM MEMORY ID 'RSSTATMO_PARAMETERS'.
    PERFORM check-parameters.
  ENDIF.

* get some data, but don't do an update

  IF snapshotflag IS INITIAL OR snapshotflag = 'N'.
    PERFORM get-summary-of-period USING 'ALL'.
  ELSE.
                                       "get snapshot data

    date = rdate.
    IF statl-swpid NE '*' AND statl-swpid CO ' 0123456789'.
      wp_number = statl-swpid.
    ENDIF.

    CALL FUNCTION 'SAPWL_STATREC_READ_FILE'
         EXPORTING
*             NO_OF_RECORDS               = -1
              read_client                 = statl-smandt
              read_end_date               = date
              read_end_time               = rtstop
              read_exclude_username       = 'SYSPERF'
*             READ_CONTINUE_RECORDNO      = -1
              read_start_date             = date
              read_start_time             = rtstart
              read_username               = statl-sbenu
              read_workprocess            = wp_number
*             READ_FORWARD                = 'X'
*             STATISTIC_FILE              =
*             NO_BUFFER_FLUSH             = ' '
         IMPORTING
              file_error                  = file_error
              records_read                = records_read
              eof_reached                 = eof_reached
*             CONTINUE_RECORDNO_FORWARD   =
*             CONTINUE_RECORDNO_BACKWARD  =
*             PROBLEMS                    =
           TABLES
              v2_normal_records           = v2_normal_records
              v2_btc_step_records         = v2_btc_step_records
              v2_table_records            = v2_table_records
              v2_rfc_client_records       = v2_rfc_client_records
              v2_rfc_server_records       = v2_rfc_server_records
              v2_rfc_client_dest_records  = v2_rfc_client_dest_records
              v2_rfc_server_dest_records  = v2_rfc_server_dest_records
              v2_spool_print_records      = v2_spool_print_records
              v2_spool_activity_records   = v2_spool_activity_records
              norm_subrecord_index        = norm_subrecord_index
           EXCEPTIONS
              wrong_parameter_combination = 1
              OTHERS                      = 2.

    IF file_error NE space.
      MESSAGE e076 WITH file_error.
*    Read problems with statistic file (OS error &)
    ENDIF.
    IF eof_reached = 'X'.
      IF records_read = 0.
        CALL FUNCTION 'SAPWL_INST_LONG'
             EXPORTING
                  inst_short                    = rsystem
             IMPORTING
                  inst_long                     = long_instance
*                 HOST_LONG                     =
             EXCEPTIONS
                  could_not_determine_long_name = 1
                  OTHERS                        = 2.
        IF sy-subrc <> 0.
          long_instance = rsystem.
        ENDIF.
        MESSAGE s006 WITH long_instance.
*    No statistical record for $ found with given criteria
      ELSE.
        MESSAGE s333 WITH 'End of statistic file reached.' .
*    End of statistic file reached.
      ENDIF.
    ENDIF.
    time_resolution = statl-scount3 DIV 60.
    IF NOT ( statl-scount2 IS INITIAL ).
      tasktypes_per_workprocess = 'X'.
    ENDIF.
    IF NOT ( statl-scount1 IS INITIAL ).
      tcodes_with_dynpro_number = 'X'.
    ENDIF.
    IF NOT ( statl-sseltime1 IS INITIAL ).
      terminal_instead_of_username = 'X'.
    ENDIF.
    CALL FUNCTION 'SAPWL_WORKLOAD_ADD_TO_SNAPSHOT'
         EXPORTING
*          statistic_version             = statistic_version_used
           time_resolution               = time_resolution
           tasktypes_per_workprocess     = tasktypes_per_workprocess
           tcodes_with_dynpro_number     = tcodes_with_dynpro_number
           terminal_instead_of_username  = terminal_instead_of_username
           instance_name              = my_name
         IMPORTING
              spool_print_statistic         = spool_print_statistic
         TABLES
*              v1_normal_records             = v1_normal_records
*              v1_table_records              = v1_table_records
              v2_normal_records             = v2_normal_records
              v2_btc_step_records           = v2_btc_step_records
              v2_table_records              = v2_table_records
              v2_rfc_client_records         = v2_rfc_client_records
              v2_rfc_server_records         = v2_rfc_server_records
              v2_rfc_client_dest_records    = v2_rfc_client_dest_records
              v2_rfc_server_dest_records    = v2_rfc_server_dest_records
              v2_spool_print_records        = v2_spool_print_records
              v2_spool_activity_records     = v2_spool_activity_records
              norm_subrecord_index          = norm_subrecord_index
              summary                       = summary
              hitlist_dbcalls               = tt_hitl_dbcalls
              hitlist_respti                = tt_hitl_respti
              accounting_statistic_tasktype = tt_summa
              time_statistic                = tt_summt
              table_record_statistic        = tt_summl
              application_statistic         = tt_summu
              function_code_statistic       = summcua
              terminal_io_statistic         = summy
              tasktype_statistic            = summr
              user_statistic                = tt_users_of_entry_id
              rfc_client_statistic          = rfc_client_statistic
              rfc_server_statistic          = rfc_server_statistic
              rfc_client_dest_statistic     = rfc_client_dest_statistic
              rfc_server_dest_statistic     = rfc_server_dest_statistic
              memory_statistic              = memory_statistic
              instances                     = instance_table
              spool_activity_statistic      = spool_activity_statistic.

  ENDIF.
ENDFORM.                               "UPDATE-MONI

*
*
* Diverse Unterprogramme zum MONI - Update & MONI-Kommunikation
*
*
*-----------------------------------------------------------------------
* FILL-PFAD: Ermitteln des Pfadnamens der Statistikdatei, System-ID.
*-----------------------------------------------------------------------
*
FORM fill-pfad.
  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'stat/file'
                     ID 'VALUE' FIELD pfad.
  CALL 'C_GET_SYSTEM_NUMBER' ID 'SYSTEM' FIELD systemid.
  initflag = 'Y'.
ENDFORM.
*
*
*-----------------------------------------------------------------------
*   GET-SUMMARY-OF-PERIOD:
*   In Abhaengigkeit von RPERIOD werden in den internen Tabellen
*   MONI-Daten bereitgestellt. Die jeweilige Periode (Tag,Woche,
*   Monat oder Jahr wird aus RDATE ermittelt.
*-----------------------------------------------------------------------
*
FORM get-summary-of-period USING option.
  tcodeflg1 = 'N'.                     "Neues Transakt.profil
  tcodeflg2 = 'N'.                     "Neues Time-profil
  tablsflag = 'N'.                     "Neues Tabellenprofil
  usersflag = 'N'.                     "Neues Benutzerprofil
  tasksflag = 'N'.                     "Neues Tasktyp-profil
  clientflg = 'N'.                     "Neues Client/Accounting-profil

  CASE rperiod.
    WHEN 'DAY     '.
*-----Tagessumme--------------------------------------------------------
      PERFORM get-summary-of-day USING option.
    WHEN OTHERS.
*-----Wochen-,Monats- oder Jahressumme----------------------------------
      PERFORM get-summary-of-longterms USING option.
  ENDCASE.
ENDFORM.
*
*-----------------------------------------------------------------------
*   GET-SUMMARY-OF-LONGTERMS:
*   Wochen-, Monats-, und Jahressaetze:
*   Holen der MONI-Summensaetze in abhaengigkeit von RPERIOD und RDATE.
*-----------------------------------------------------------------------
*
FORM get-summary-of-longterms USING option.
  DATA  date LIKE sy-datum.

  CHECK snapshotflag NE 'X'.           "no smapshot data

  date = rdate.

  FREE:    xummary, xhitl1, xhitl2, xummu, xumml, xummr, xummt,
           xummy, xumma, xummcua,
           tt_more_summt, tt_more_summu, tt_more_summa,
           tt_more_hitl_respti, tt_more_hitl_dbcalls,
           tt_more_users_of_entry_id, tt_more_summl,
           rfc_client_statistic, rfc_server_statistic,
           rfc_client_dest_statistic, rfc_server_dest_statistic,
           spool_print_statistic, spool_activity_statistic,
           memory_statistic, instance_table.

  readsummflag = readhitlflag = readsummlflag = readcuaflag = space.

  IF option = 'HITL' OR option = 'SUMM' OR option = 'SUMML'.
    CALL FUNCTION 'SAPWL_WORKLOAD_GET_SUMMARY'
         EXPORTING
              periodtype         = rperiod(1)
              hostid             = rsystem
              startdate          = date
         TABLES
              summary            = summary
         EXCEPTIONS
              unknown_periodtype = 1
              no_data_found      = 2
              OTHERS             = 3.
  ELSE.
    CALL FUNCTION 'SAPWL_WORKLOAD_GET_SUMMARY'
         EXPORTING
              periodtype         = rperiod(1)
              hostid             = rsystem
              startdate          = date
         TABLES
              summary            = xummary
         EXCEPTIONS
              unknown_periodtype = 1
              no_data_found      = 2
              OTHERS             = 3.
  ENDIF.

  CHECK sy-subrc = 0 AND option NE 'MAIN'.

  IF option = 'HITL' OR option = 'SUMM' OR option = 'SUMML'.
    CALL FUNCTION 'SAPWL_WORKLOAD_GET_STATISTIC'
         EXPORTING
              periodtype                    = rperiod(1)
              hostid                        = rsystem
              startdate                     = date
         IMPORTING
              spool_print_statistic         = spool_print_statistic
         TABLES
              hitlist_dbcalls               = tt_hitl_dbcalls
              hitlist_respti                = tt_hitl_respti
              accounting_statistic_tasktype = tt_summa
              time_statistic                = tt_summt
              table_record_statistic        = tt_summl
              application_statistic         = tt_summu
              function_code_statistic       = summcua
              terminal_io_statistic         = summy
              tasktype_statistic            = summr
              user_statistic                = tt_users_of_entry_id
              memory_statistic              = memory_statistic
              rfc_client_statistic          = rfc_client_statistic
              rfc_server_statistic          = rfc_server_statistic
              rfc_client_dest_statistic     = rfc_client_dest_statistic
              rfc_server_dest_statistic     = rfc_server_dest_statistic
              instances                     = instance_table
              spool_activity_statistic      = spool_activity_statistic
         EXCEPTIONS
              unknown_periodtype            = 1
              no_data_found                 = 2
              OTHERS                        = 3.
    IF sy-subrc = 0.
      readsummflag = readhitlflag = readsummlflag = 'X'.
      IF rsystem = 'TOTAL'.
        readcuaflag = 'X'.
      ENDIF.
      PERFORM tt_get_hitl_min TABLES tt_hitl_respti
                                     tt_hitl_dbcalls
                                     tt_hitl_respti_min
                                     tt_hitl_dbcalls_min.
      PERFORM convert_spoolbyte_into_kbyte.
    ELSE.
      CLEAR: tt_hitl_respti_min, tt_hitl_dbcalls_min,
             hit1minval, hit2minval.
    ENDIF.
  ELSE.
    CALL FUNCTION 'SAPWL_WORKLOAD_GET_STATISTIC'
         EXPORTING
              periodtype                    = rperiod(1)
              hostid                        = rsystem
              startdate                     = date
         IMPORTING
              spool_print_statistic         = spool_print_statistic
         TABLES
              hitlist_dbcalls               = tt_more_hitl_dbcalls
              hitlist_respti                = tt_more_hitl_respti
              accounting_statistic_tasktype = tt_more_summa
              time_statistic                = tt_more_summt
              table_record_statistic        = tt_more_summl
              application_statistic         = tt_more_summu
              function_code_statistic       = xummcua
              terminal_io_statistic         = xummy
              tasktype_statistic            = xummr
              user_statistic                = tt_more_users_of_entry_id
*             MEMORY_STATISTIC              =
*             RFC_CLIENT_STATISTIC          =
*             RFC_SERVER_STATISTIC          =
*             RFC_CLIENT_DEST_STATISTIC     =
*             RFC_SERVER_DEST_STATISTIC     =
              spool_activity_statistic      = spool_activity_statistic
         EXCEPTIONS
              unknown_periodtype            = 1
              no_data_found                 = 2
              OTHERS                        = 3.
    IF sy-subrc = 0.
      readsummflag = readhitlflag = readsummlflag = 'X'.
      IF rsystem = 'TOTAL'.
        readcuaflag = 'X'.
      ENDIF.
      PERFORM tt_get_hitl_min TABLES tt_more_hitl_respti
                                     tt_more_hitl_dbcalls
                                     tt_more_hitl_respti_min
                                     tt_more_hitl_dbcalls_min.
      PERFORM convert_spoolbyte_into_kbyte.
    ELSE.
      CLEAR: tt_more_hitl_respti_min, tt_more_hitl_dbcalls_min,
             xhit1minval, xhit2minval.
    ENDIF.
  ENDIF.
ENDFORM.                               "GET-SUMMARY-OF-LONGTERMS

*
*-----------------------------------------------------------------------
*   GET-SUMMARY-OF-DAY:
*   Laden der MONI-Saetze (Tagessaetze)  in interne Tabellen
*-----------------------------------------------------------------------
*
FORM get-summary-of-day USING option.
  DATA  date LIKE sy-datum.

  REFRESH stats.

  CHECK snapshotflag NE 'X'.           "not when snapshot is wanted
  CHECK quickrefrflag NE 'X'.          "not for 'quick-refresh'

  date = rdate.

  readsummflag = readhitlflag = readsummlflag = readcuaflag  = space.

  CLEAR:   hit1minval, hit2minval,
           tt_hitl_dbcalls_min, tt_hitl_respti_min.
  FREE:    summary, xummary, hitl1, xhitl1, hitl2, xhitl2, summu,
           xummu, summl, xumml, summr, xummr, summt, xummt,
           summy, xummy, summa, xumma, summcua, xummcua,
           tt_summt, tt_summu, tt_summa, tt_summl,
           tt_more_summt, tt_more_summu, tt_more_summa, tt_more_summl,
           tt_hitl_respti, tt_hitl_dbcalls,
           tt_more_hitl_respti, tt_more_hitl_dbcalls,
           tt_more_users_of_entry_id, tt_users_of_entry_id,
           rfc_client_statistic, rfc_server_statistic,
           rfc_client_dest_statistic, rfc_server_dest_statistic,
           spool_print_statistic, spool_activity_statistic,
           memory_statistic, instance_table.

  CALL FUNCTION 'SAPWL_WORKLOAD_GET_SUMMARY'
       EXPORTING
            periodtype         = 'D'
            hostid             = rsystem
            startdate          = date
       TABLES
            summary            = summary
       EXCEPTIONS
            unknown_periodtype = 1
            no_data_found      = 2
            OTHERS             = 3.

  CHECK sy-subrc = 0 AND option NE 'MAIN'.

  CALL FUNCTION 'SAPWL_WORKLOAD_GET_STATISTIC'
       EXPORTING
            periodtype                    = 'D'
            hostid                        = rsystem
            startdate                     = date
       IMPORTING
            spool_print_statistic         = spool_print_statistic
       TABLES
            hitlist_dbcalls               = tt_hitl_dbcalls
            hitlist_respti                = tt_hitl_respti
            accounting_statistic_tasktype = tt_summa
            time_statistic                = tt_summt
            table_record_statistic        = tt_summl
            application_statistic         = tt_summu
            function_code_statistic       = summcua
            terminal_io_statistic         = summy
            tasktype_statistic            = summr
            user_statistic                = tt_users_of_entry_id
            memory_statistic              = memory_statistic
            rfc_client_statistic          = rfc_client_statistic
            rfc_server_statistic          = rfc_server_statistic
            rfc_client_dest_statistic     = rfc_client_dest_statistic
            rfc_server_dest_statistic     = rfc_server_dest_statistic
            instances                     = instance_table
            spool_activity_statistic      = spool_activity_statistic
       EXCEPTIONS
            unknown_periodtype            = 1
            no_data_found                 = 2
            OTHERS                        = 3.

  IF sy-subrc = 0.
    readhitlflag = readsummflag = readsummlflag = 'X'.
    PERFORM tt_get_hitl_min TABLES tt_hitl_respti
                                   tt_hitl_dbcalls
                                   tt_hitl_respti_min
                                   tt_hitl_dbcalls_min.
    PERFORM convert_spoolbyte_into_kbyte.
    IF rsystem = 'TOTAL'.
      readcuaflag = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.

*
*-----------------------------------------------------------------------
* CHECK-PARAMETERS: Pruefen der Parameter  RDATE RSYSTEM RSYSTEMID
*                   und der Periode        RPERIOD
*-----------------------------------------------------------------------
*
FORM check-parameters.
*
  IF rperiod <> 'DAY     ' AND         "Wertebereich Periode
     rperiod <> 'WEEK    ' AND
     rperiod <> 'MONTH   ' AND
     rperiod <> 'YEAR    '.
    rperiod = 'DAY     '.
  ENDIF.
*
  IF rdate = '        ' OR rdate = '00000000'    "Datum nicht gesetzt
    OR rdate = '*       '.
    rdate     = sy-datum.              "aktuelles Datum
  ENDIF.
*
  IF rtstart = '      ' OR rtstart = '*'.
    rtstart = '000000'.
  ENDIF.
*
  IF rtstop = '      ' OR rtstop = '000000' OR rtstop = '*'.
    rtstop = '235959'.
  ENDIF.
  IF rtstop < rtstart.                 "tausche Zeiten !
    t1 = rtstop. rtstop = rtstart. rtstart = t1.
  ENDIF.
  IF rtstop <> '235959' OR rtstart <> '000000'.
    snapshotflag = 'X'.
    rperiod      = 'DAY'.
  ENDIF.
  sta1-rtstop  = rtstop.
  sta1-rtstart = rtstart.

*
  IF rcommand = '    ' OR rcommand = '0000'    "Kein externes Kommando
    OR rcommand = '*   '.
    rcommand  = 'INIT'.                "eigene Maschine
  ENDIF.
*
  IF rcommand = 'NOUP'.                "Kein Update gewuenscht
    rcommand  = 'INIT'.
    noupdateflag = 'X'.
  ELSE.
    noupdateflag = ' '.
  ENDIF.
*
  IF rcommand =  'RECE'.               "Recent (Time slot) analysis
    rcommand = 'INIT'.
    rperiod = 'DAY'.
    snapshotflag = 'X'.
  ENDIF.
*
  IF rcommand =  'RECS'.               "Recent (Time slot) analysis
    rcommand = 'RETS'.                 "mit direktem Return in Calling
    rperiod = 'DAY'.                   "Routine
    snapshotflag = 'X'.
  ENDIF.
*
  IF rsystem = '        ' OR rsystem = '00000000'  "CPU nicht gesetzt
    OR rsystem = '*       '.
    IF cpuid = space.
      PERFORM get-cpu-id.
    ENDIF.
    rsystem = cpuid.                   "eigene Maschine
  ENDIF.
*
  IF rsystemid = '  ' OR rsystemid = '*' OR     "Systemno.nicht gesetzt
     rsystemid = '**' OR rsystemid = ' *' OR     "Systemno.nicht gesetzt
     rsystemid BETWEEN '00' AND '99'.
    rsystemid = '--'.
  ENDIF.
*
ENDFORM.
*
*-----------------------------------------------------------------------
* GET-CPU-ID: Ermitteln der CPU-ID der SAP-Applikation
*-----------------------------------------------------------------------
*
FORM get-cpu-id.
*  cpuid = sy-host.
  IF myname = space.
    CALL 'C_SAPGPARAM'         ID 'NAME'   FIELD 'rdisp/myname'
                               ID 'VALUE'  FIELD myname.
  ENDIF.
  CALL FUNCTION 'SAPWL_INST_SHORT'
       EXPORTING
            inst_long          = myname
       IMPORTING
            inst_short         = cpuid
       EXCEPTIONS
            too_many_instances = 1
            OTHERS             = 2.
  IF sy-subrc <> 0.
    cpuid = myname(8).
  ENDIF.
ENDFORM.
*
*-----------------------------------------------------------------------
* DELETE-STATIFILE: Loeschen der Statistikdatei
*-----------------------------------------------------------------------
*
FORM delete-statfile.
  CALL FUNCTION 'PF_RESET_FILE'
       EXCEPTIONS
            OTHERS = 1.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM CHECK-SPECIAL-TCODE                                      *

*&---------------------------------------------------------------------*
*&      Form  TT_GET_HITL_MIN
*&---------------------------------------------------------------------*
*       Get the min values of the resp. times & DB calls per tasktype  *
*----------------------------------------------------------------------*
*  -->  RESPTIMES     table with response times hitlists/tasktype
*  -->  DBCALLS       table with DB calls hitlists/tasktype
*  <--  RESPTIMES_MIN table with min values for response times/tasktype
*  <--  DBCALLS_MIN   table with min values for DB calls/tasktype
*----------------------------------------------------------------------*
FORM tt_get_hitl_min TABLES resptimes     STRUCTURE tt_hitl_respti
                            dbcalls       STRUCTURE tt_hitl_dbcalls
                            resptimes_min STRUCTURE tt_hitl_respti_min
                            dbcalls_min   STRUCTURE tt_hitl_dbcalls_min.
  DATA ttcounter TYPE i.

  REFRESH: resptimes_min, dbcalls_min.
  CLEAR  : resptimes_min, dbcalls_min.
  SORT resptimes BY tasktype respti.
  SORT dbcalls   BY tasktype dbcalls.
  ttcounter = tt_min.
  WHILE ttcounter <= tt_max.
    READ TABLE resptimes WITH KEY tasktype = ttcounter BINARY SEARCH.
    IF sy-subrc = 0.
      resptimes_min-respti   = resptimes-respti.
      resptimes_min-tasktype = ttcounter.
      APPEND resptimes_min.
    ENDIF.
    READ TABLE dbcalls WITH KEY tasktype = ttcounter BINARY SEARCH.
    IF sy-subrc = 0.
      dbcalls_min-dbcalls  = dbcalls-dbcalls.
      dbcalls_min-tasktype = ttcounter.
      APPEND dbcalls_min.
    ENDIF.
    ADD 1 TO ttcounter.
    IF ttcounter = 9.           "nur Tasktypen 0-8 und 253-255 verhanden
      ttcounter = 253.
    ENDIF.
  ENDWHILE.
ENDFORM.                               " TT_GET_HITL_MIN

*&---------------------------------------------------------------------*
*&      Form  TT_ADD_HITL_RESPTI_RECORD
*&---------------------------------------------------------------------*
*       Add a record to the response time hitlist                      *
*----------------------------------------------------------------------*
*  -->  RESPTIMES table with response times topscorers/tasktype.
*                 Must be sorted by tasktype + response time (asc.)
*  -->  RECORD    new record to compare & insert
*----------------------------------------------------------------------*
FORM tt_add_hitl_respti_record
                          TABLES resptimes STRUCTURE tt_hitl_respti
                          USING  record    STRUCTURE tt_hitl_respti.
  DATA start LIKE sy-tabix.
  DATA size  TYPE i.
  DATA last  TYPE i.

                                       "duplicate check, very slow!
* PERFORM TT_REMOVE_HITL_DUPLICATES TABLES RESPTIMES.

  READ TABLE resptimes WITH KEY tasktype = record-tasktype
       BINARY SEARCH.
  IF sy-subrc NE 0.
                                       "no record for this tasktype yet
    INSERT record INTO resptimes INDEX sy-tabix.
  ELSE.
    start = sy-tabix.
                                       "find position for new record
    READ TABLE resptimes WITH KEY tasktype = record-tasktype
                                  respti   = record-respti
         BINARY SEARCH.
    IF sy-subrc = 0.
      IF record NE resptimes.          "no duplicates
        INSERT record INTO resptimes INDEX sy-tabix.
      ELSE.
        EXIT. "nothing happened -> no cut necessary
      ENDIF.
    ELSE.
      INSERT record INTO resptimes INDEX sy-tabix.
    ENDIF.
                                       "cut record if necessary
    DESCRIBE TABLE resptimes LINES size.
    last = tt_hitl_max_records + start.
    IF last <= size.
      READ TABLE resptimes INDEX last.
      IF resptimes-tasktype = record-tasktype.
        DELETE resptimes INDEX start.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " TT_ADD_HITL_RESPTI_RECORD

*&---------------------------------------------------------------------*
*&      Form  TT_ADD_HITL_DBCALLS_RECORD
*&---------------------------------------------------------------------*
*       Add a record to the DB calls hitlist                           *
*----------------------------------------------------------------------*
*  -->  DBCALLS   table with DB calls topscorers/tasktype.
*                 Must be sorted by tasktype + response time (asc.)
*  -->  RECORD    new record to compare & insert
*----------------------------------------------------------------------*
FORM tt_add_hitl_dbcalls_record
                          TABLES dbcalls STRUCTURE tt_hitl_dbcalls
                          USING  record  STRUCTURE tt_hitl_dbcalls.
  DATA start LIKE sy-tabix.
  DATA size  TYPE i.
  DATA last  TYPE i.

* "duplicate check, not used normally, because it's very slow
* PERFORM TT_REMOVE_HITL_DUPLICATES TABLES DBCALLS.

  READ TABLE dbcalls WITH KEY tasktype = record-tasktype
       BINARY SEARCH.
  IF sy-subrc NE 0.
                                       "no record for this tasktype yet
    INSERT record INTO dbcalls INDEX sy-tabix.
  ELSE.
    start = sy-tabix.
                                       "find position for new record
    READ TABLE dbcalls WITH KEY tasktype = record-tasktype
                                dbcalls  = record-dbcalls
         BINARY SEARCH.
    IF sy-subrc = 0.
      IF record NE dbcalls.            "no duplicates
        INSERT record INTO dbcalls INDEX sy-tabix.
      ELSE.
        EXIT. "nothing happened -> no cut necessary
      ENDIF.
    ELSE.
      INSERT record INTO dbcalls INDEX sy-tabix.
    ENDIF.
                                       "cut record if necessary
    DESCRIBE TABLE dbcalls LINES size.
    last = tt_hitl_max_records + start.
    IF last <= size.
      READ TABLE dbcalls INDEX last.
      IF dbcalls-tasktype = record-tasktype.
        DELETE dbcalls INDEX start.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " TT_ADD_HITL_DBCALLS_RECORD

*&---------------------------------------------------------------------*
*&      Form  TT_ADD_USER_FOR_ENTRY_ID
*&---------------------------------------------------------------------*
*       Add a name of a user using the specified transaction, report   *
*       or batchjob (used only for long-term statistics)               *
*----------------------------------------------------------------------*
*  <->  E_U_TAB   table with names of users using a TA, rep. or btcjob
*                 (should be sorted ascending)
*  -->  TTYPE     tasktype
*  -->  ENTRY_ID  object (name of TA, report or batchjob)
*  -->  ACCOUNT   user account
*----------------------------------------------------------------------*
FORM tt_add_user_for_entry_id
                      TABLES e_u_tab  STRUCTURE tt_users_of_entry_id
                      USING  ttype    LIKE      tt_summu-ttype
                             entry_id LIKE      tt_summu-entry_id
                             account  LIKE      tt_summu-account.
  READ TABLE e_u_tab WITH KEY ttype    = ttype
                              entry_id = entry_id
                              account  = account
       BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    e_u_tab-ttype    = ttype.
    e_u_tab-entry_id = entry_id.
    e_u_tab-account  = account.
    INSERT e_u_tab INDEX sy-tabix.
  ENDIF.
ENDFORM.                               " TT_ADD_USER_FOR_ENTRY_ID

*&---------------------------------------------------------------------*
*&      Form  TT_REMOVE_HITL_DUPLICATES
*&---------------------------------------------------------------------*
*       Remove duplicates from a hitlist table (only for repair of     *
*       hitlist tables, very slow!!)                                   *
*----------------------------------------------------------------------*
*  <->  HITL      hitlist table (sorted)
*----------------------------------------------------------------------*
FORM tt_remove_hitl_duplicates TABLES hitl STRUCTURE stats_cuml.
  DATA last      LIKE stats_cuml.
  DATA tabixsave LIKE sy-tabix.
  DATA removeit.

  LOOP AT hitl.
    last = hitl.
    tabixsave = sy-tabix.
    LOOP AT hitl.
      IF tabixsave NE sy-tabix AND last = hitl.
        removeit = 'X'.
        EXIT.                          "inner loop
      ENDIF.
      IF removeit = 'X'.
        DELETE hitl INDEX tabixsave.
        CLEAR removeit.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                               " TT_REMOVE_HITL_DUPLICATES

*&---------------------------------------------------------------------*
*&      Form  TT_SUMML_COLLECT
*&---------------------------------------------------------------------*
*       Make a "Pseudo"-collect on the sorted table statistic table    *
*----------------------------------------------------------------------*
*  -->  TABCALLS  table statistic table (global *SUMML*)
*  -->  RECORD    new record
*----------------------------------------------------------------------*
FORM tt_summl_collect TABLES tabcalls STRUCTURE tt_summl
                      USING  record   STRUCTURE tt_summl.
  DATA BEGIN OF sum.
          INCLUDE STRUCTURE stat_moni_4.
  DATA END   OF sum.
  DATA BEGIN OF wa.
          INCLUDE STRUCTURE tt_summl.
  DATA END   OF wa.

  READ TABLE tabcalls INTO wa
                      WITH KEY ttype    = record-ttype
                               entry_id = record-entry_id
                               tname    = record-tname
             BINARY SEARCH.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING wa TO sum.
    ADD-CORRESPONDING sum TO record.
    MODIFY tabcalls INDEX sy-tabix FROM record.
  ELSE.
    INSERT record INTO tabcalls INDEX sy-tabix.
  ENDIF.
ENDFORM.                               " TT_SUMML_COLLECT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_SPOOLBYTE_INTO_KBYTE
*&---------------------------------------------------------------------*
*       Converts bytes of job, processed bytes and transferred
*       bytes of spool_print_statistic into kbytes.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_spoolbyte_into_kbyte.
  spool_print_statistic-pjbytes = spool_print_statistic-pjbytes / 1000.
  spool_print_statistic-procbytes = spool_print_statistic-procbytes
                                                           / 1000.
  spool_print_statistic-trbytes = spool_print_statistic-trbytes / 1000.
ENDFORM.                               " CONVERT_SPOOLBYTE_INTO_KBYTE
