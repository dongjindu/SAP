*----------------------------------------------------------------------*
*       Program name: ZGHOCPZP_AR
*----------------------------------------------------------------------*
*       Archiving report for Reporting Points (Objekt)
*----------------------------------------------------------------------*

REPORT zghocpzp_ar MESSAGE-ID rm LINE-SIZE 83.
DATA:   it_cpzp         LIKE cpzp       OCCURS 0 WITH HEADER LINE,
        it_chzp         LIKE chzp       OCCURS 0 WITH HEADER LINE,
        it_cezp         LIKE cezp       OCCURS 0 WITH HEADER LINE.
DATA:
   handle         LIKE sy-tabix,
   i_counter      TYPE i,                     "temp. Objektz?ler
   i_commit_cnt   LIKE arch_usr-arch_comit.   "Commit Z?ler

CONSTANTS:
   k_loek           TYPE jest-stat VALUE 'I0013',
   k_chari          TYPE c VALUE 'I',
   k_chare          TYPE c VALUE 'E',
   k_charx          TYPE c VALUE 'X',
   k_chars          TYPE c VALUE 'S',
   k_msgid_rm       LIKE sy-msgid VALUE 'RM',
   k_archive_object LIKE arch_obj-object  VALUE 'ZPP_REPORT',
   k_str_delete     LIKE arc_buffer-flags VALUE 'DELETE  ',
   k_object_counter TYPE i VALUE 100,
*-> Tabellennamen
   k_tab_cpzp       LIKE arc_buffer-rname VALUE 'CPZP',
   k_tab_chzp       LIKE arc_buffer-rname VALUE 'CHZP',
   k_tab_cezp       LIKE arc_buffer-rname VALUE 'CEZP'.
*fl_tab_counter TYPE t_counter.
************************************************************************
*       Selection parameters                                           *
************************************************************************

TABLES: afpo, cezp.
SELECT-OPTIONS: s_aufnr  FOR afpo-aufnr,
                s_aendat FOR it_cezp-budat,
                s_aentim FOR it_cezp-buzei.
RANGES: r_objnr FOR jest-objnr.
*comment line to be added on the archive
PARAMETERS: p_text TYPE admi_run-comments.
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-022.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     pa_testr DEFAULT ' ' AS CHECKBOX.
PARAMETERS:     pa_creat DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK a1.
SELECTION-SCREEN SKIP 1.

************************************************************************
*       START OF SELECTION - Event                                     *
************************************************************************
START-OF-SELECTION.
  PERFORM generate_objects.

*-> Read table contents
  PERFORM select_archive_tables TABLES it_cpzp it_cezp it_chzp.

*-> Open archiving object for output
  PERFORM open_archive_object USING handle.

*-> Create new archiving object
  PERFORM get_new_archive_object USING handle.

*-> Save internal table IT_CPZP
  READ TABLE it_cezp INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM put_archive_record_tab TABLES it_cezp
                                   USING k_tab_cezp handle i_counter.
  ENDIF.
*-> Save internal table IT_CHZP
  READ TABLE it_cpzp INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM put_archive_record_tab TABLES it_cpzp
                                   USING k_tab_cpzp   handle i_counter.
  ENDIF.
*-> Save internal table IT_CHZP
  READ TABLE it_chzp INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM put_archive_record_tab TABLES it_chzp
                                   USING k_tab_chzp   handle i_counter.
  ENDIF.
  IF pa_testr EQ  k_charx.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

*-> Save archiving object
  PERFORM save_archive_object USING handle.

*-> Write statistics for archiving object
  PERFORM get_archive_statistic USING handle.

*-> Close archiving object
  PERFORM close_archive_object USING handle.


* ENDIF.                               "fl_tab_counter-i_count_mdbp > 0

************************************************************************
*                                                                      *
*       FORM - ROUTINES                                                *
*                                                                      *
************************************************************************



*&---------------------------------------------------------------------*
*&      Form  select_archive_tables
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PIT_CPZP   Internal table containing CPZP data
*      -->PIT_CHZP   Internal table containing CHZP data
*      -->PIT_CEZP   Internal table containing CEZP data
*----------------------------------------------------------------------*
FORM select_archive_tables TABLES pit_cpzp  LIKE it_cpzp[]
                                  pit_cezp  LIKE it_cezp[]
                                  pit_chzp  LIKE it_chzp[].
  DATA: l_lines LIKE sy-tabix,
         l_date(1),
         l_time(1).
  DATA:  t_jest_tab LIKE STANDARD TABLE OF jest,
         t_jest_wa  LIKE jest,
         l_cezp     LIKE STANDARD TABLE OF it_cezp,
         count      TYPE int4.

* Select orders per start date, time and object number
  READ TABLE s_aendat INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    l_date = k_charx.
  ENDIF.
  READ TABLE s_aentim INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    l_time  = k_charx.
  ENDIF.
*-> CHZP
  IF NOT l_date IS INITIAL AND NOT l_time IS INITIAL.
    SELECT * FROM cezp INTO TABLE pit_cezp
                       WHERE cpudt  IN s_aendat
                       AND   cputm  IN s_aentim
                       AND   objnr  IN r_objnr ORDER BY PRIMARY KEY.
* Select orders per start date and object number
  ELSEIF NOT l_date IS INITIAL .
    SELECT * FROM cezp INTO TABLE pit_cezp
                       WHERE cpudt  IN s_aendat
                       AND   objnr  IN r_objnr ORDER BY PRIMARY KEY.
  ELSE.
* Select orders per object number
    SELECT * FROM cezp INTO TABLE pit_cezp
               WHERE objnr   IN r_objnr ORDER BY PRIMARY KEY.
  ENDIF.
  READ TABLE pit_cezp INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    PERFORM read_by_cezp TABLES pit_cezp pit_cpzp pit_chzp.
  ELSE.
    PERFORM read_by_cpzp TABLES pit_cezp pit_cpzp pit_chzp
                         USING  l_date l_time.
  ENDIF.
ENDFORM.                               " SELECT_ARCHIVE_TABLES


*&---------------------------------------------------------------------*
*&      Form  OPEN_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*       Open archiving object
*----------------------------------------------------------------------*
*  <->  p_handle  Pointer to archiving file
*----------------------------------------------------------------------*
FORM open_archive_object USING p_handle LIKE sy-tabix.

  DATA:
     c_comment(60).

  IF p_text IS INITIAL.
    IF pa_testr <> k_charx.              "Kein Testlauf ?
      c_comment = text-002.
    ELSE.
      c_comment = text-001.
    ENDIF.                               "pa_testr <> charx
  ELSE.
    c_comment = p_text.
  ENDIF.

*-> Open new archive for output
  CALL FUNCTION 'ARCHIVE_OPEN_FOR_WRITE'
    EXPORTING
      call_delete_job_in_test_mode = pa_testr
      create_archive_file          = pa_creat
      comments                     = c_comment
      object                       = k_archive_object
    IMPORTING
      archive_handle               = p_handle
    EXCEPTIONS
      internal_error               = 1
      object_not_found             = 2
      open_error                   = 3
      not_authorized               = 4
      OTHERS                       = 5.

  IF sy-subrc <> 0.
*-> Message: 'Archiving object could not be created !'
    MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 705.
  ENDIF.                               "sy-subrc <> 0

ENDFORM.                               " OPEN_ARCHIVE_OBJECT


*&---------------------------------------------------------------------*
*&      Form  GET_NEW_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*       Creates new archiving object
*----------------------------------------------------------------------*
*  <->  p_handle  Pointer to archiving file                            *
*----------------------------------------------------------------------*
FORM get_new_archive_object USING p_handle LIKE sy-tabix.

  CALL FUNCTION 'ARCHIVE_NEW_OBJECT'
    EXPORTING
      archive_handle          = p_handle
      object_id               = k_archive_object
    EXCEPTIONS
      internal_error          = 1
      wrong_access_to_archive = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
*-> Message: 'Wrong access to archiving object !'
    MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 706.
  ENDIF.                               "sy-subrc <> 0

ENDFORM.                               " GET_NEW_ARCHIVE_OBJECT


*&---------------------------------------------------------------------*
*&      Form  PUT_ARCHIVE_RECORD_TAB
*&---------------------------------------------------------------------*
*       Archivierungssatz an das Archivsystem ?ergeben
*----------------------------------------------------------------------*
*  <->  p_tab       internal table with archiving data                 *
*  <->  p_tab_mame  Table name                                         *
*  <->  p_handle    Pointer to archiving file                          *
*  <->  p_i_counter Record counter                                     *
*----------------------------------------------------------------------*
FORM put_archive_record_tab TABLES p_tab
                            USING  p_tab_name LIKE arc_buffer-rname
                                   p_handle LIKE sy-tabix
                                   p_i_counter TYPE i.

  LOOP AT p_tab.
    p_i_counter = p_i_counter + 1.

    CALL FUNCTION 'ARCHIVE_PUT_RECORD'
      EXPORTING
        archive_handle           = p_handle
        record                   = p_tab
        record_flags             = k_str_delete
        record_structure         = p_tab_name
      EXCEPTIONS
        internal_error           = 1
        wrong_access_to_archive  = 2
        invalid_record_structure = 3
        OTHERS                   = 4.

    IF sy-subrc <> 0.
*-> Message: 'Wrong access to archiving object !'
      MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 707.
    ENDIF.                             "sy-subrc <> 0

*-> limit exceed?
    IF p_i_counter = k_object_counter.
*-> Objekt sichern
      PERFORM save_archive_object USING handle.
*-> provide new object
      PERFORM get_new_archive_object USING handle.
*-> reset counter
      p_i_counter = 0.
    ENDIF.                             "p_i_counter = k_object_counter

  ENDLOOP.

ENDFORM.                               " PUT_ARCHIVE_RECORD_TAB


*&---------------------------------------------------------------------*
*&      Form  SAVE_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*       Save of archiving object                                       *
*----------------------------------------------------------------------*
*  <->  p_handle    Pointer to archiving file                          *
*----------------------------------------------------------------------*
FORM save_archive_object USING p_handle LIKE sy-tabix.

  CALL FUNCTION 'ARCHIVE_SAVE_OBJECT'
    EXPORTING
      archive_handle          = p_handle
    EXCEPTIONS
      file_io_error           = 1
      internal_error          = 2
      open_error              = 3
      termination_requested   = 4
      wrong_access_to_archive = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
*-> Message: 'Error when accessing archiving object !'
    MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 708.
  ENDIF.                               "sy-subrc <> 0

ENDFORM.                               " SAVE_ARCHIVE_OBJECT


*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVE_STATISTIC
*&---------------------------------------------------------------------*
*       Update of statistic informationen.
*----------------------------------------------------------------------*
*  <->  p_handle    Pointer to archiving file
*----------------------------------------------------------------------*
FORM get_archive_statistic USING p_handle LIKE sy-tabix.

*-> Statistikinformationen
  CALL FUNCTION 'ARCHIVE_WRITE_STATISTICS'
    EXPORTING
      archive_handle          = p_handle
      statistics_only_per_run = ' '
    EXCEPTIONS
      internal_error          = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
*-> Message: 'Fehler beim Schreiben der Statistikinformationen !'
    MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 709.
  ENDIF.                               "sy-subrc <> 0

ENDFORM.                               " GET_ARCHIVE_STATISTIC
*&---------------------------------------------------------------------*
*&      Form  close_archive_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_handle    Pointer to archiving file
*----------------------------------------------------------------------*
FORM close_archive_object USING p_handle LIKE sy-tabix.

*-> close archiving object
  CALL FUNCTION 'ARCHIVE_CLOSE_FILE'
    EXPORTING
      archive_handle          = p_handle
    EXCEPTIONS
      internal_error          = 1
      wrong_access_to_archive = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
*-> Message: 'Error when closing archiving file !'
    MESSAGE ID k_msgid_rm TYPE k_chars NUMBER 710.
  ENDIF.                               "sy-subrc <> 0

ENDFORM.                               " CLOSE_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  generate_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_AUFNR  range for order number
*----------------------------------------------------------------------*
FORM generate_objects.
* Generate object number from order number
  LOOP AT s_aufnr.
    MOVE s_aufnr-sign   TO r_objnr-sign.
    MOVE s_aufnr-option TO r_objnr-option.
    IF NOT s_aufnr-low IS INITIAL.
      CONCATENATE 'OR' s_aufnr-low INTO r_objnr-low.
    ENDIF.
    IF NOT s_aufnr-high IS INITIAL.
      CONCATENATE 'OR' s_aufnr-high INTO r_objnr-high.
    ENDIF.
    APPEND r_objnr.
  ENDLOOP.
  FREE s_aufnr.
ENDFORM.                    " generate_objects
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_BY_CEZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_by_cezp TABLES lit_cezp LIKE it_cezp[]
                         lit_cpzp LIKE it_cpzp[]
                         lit_chzp LIKE it_chzp[].
  DATA: l_lines LIKE sy-tabix,
       l_date(1),
       l_time(1).
  DATA:  t_jest_tab LIKE STANDARD TABLE OF jest,
         t_jest_wa  LIKE jest,
         l_cezp     LIKE STANDARD TABLE OF it_cezp,
         count      TYPE int4.

  l_cezp[] = lit_cezp[].
* Remove duplicates for status selection
  DELETE ADJACENT DUPLICATES FROM l_cezp COMPARING mandt objnr .
*-> CPZP
  DESCRIBE TABLE l_cezp LINES l_lines.
  IF NOT l_lines IS INITIAL.
* Check for order existance
* All orders have at least one either acrive or inactive status
    SELECT * FROM jest INTO TABLE t_jest_tab
                           FOR ALL ENTRIES IN l_cezp
                           WHERE objnr = l_cezp-objnr
                           ORDER BY PRIMARY KEY.
    FREE l_cezp.
* Check orders
    LOOP AT lit_cezp.
* Does the object have a status?
      READ TABLE t_jest_tab WITH KEY mandt  = lit_cezp-mandt
                                     objnr  = lit_cezp-objnr
                                     BINARY SEARCH
                                     TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
* :-(
* Corresponding order does not exit any more ==> Archive records for
* that object number
        CONTINUE.
      ELSE.
* Has the object an active deletion status?
        READ TABLE t_jest_tab WITH KEY mandt  = lit_cezp-mandt
                                       objnr  = lit_cezp-objnr
                                       stat   = k_loek
                                       inact  = space
                                       BINARY SEARCH
                                       TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
* :-(
* Corresponding order is not marked for deletion ==> Remove object
* number from archive tables
          DELETE lit_cezp "INDEX count .
            WHERE mandt = lit_cezp-mandt
            AND   objnr = lit_cezp-objnr.
        ENDIF.
      ENDIF.
    ENDLOOP.
    FREE t_jest_tab.
* Check for results
    READ TABLE lit_cezp INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
* :-(
      MESSAGE s001(00) WITH 'No data found'.
      LEAVE PROGRAM.
    ENDIF.

* Read correspnding CPZP entries
    SELECT * FROM cpzp INTO TABLE lit_cpzp
                       FOR ALL ENTRIES IN lit_cezp
                       WHERE objnr = lit_cezp-objnr.
* Read correspnding CPZP entries
    SELECT * FROM chzp INTO TABLE lit_chzp
                       FOR ALL ENTRIES IN lit_cezp
                       WHERE objnr = lit_cezp-objnr.
  ELSE.
* No data selected
* :-(
    MESSAGE s001(00) WITH 'No data found'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " READ_BY_CEZP
*&---------------------------------------------------------------------*
*&      Form  READ_BY_CPZP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_by_cpzp TABLES lit_cezp LIKE it_cezp[]
                         lit_cpzp LIKE it_cpzp[]
                         lit_chzp LIKE it_chzp[]
                  USING  l_date
                         l_time.
  DATA: l_lines LIKE sy-tabix.
  DATA:  t_jest_tab LIKE STANDARD TABLE OF jest,
         t_jest_wa  LIKE jest,
         l_chzp  LIKE STANDARD TABLE OF it_chzp.

*-> CHZP

  IF NOT l_date IS INITIAL AND NOT l_time IS INITIAL.
    SELECT * FROM chzp INTO TABLE lit_chzp
                       WHERE aendat  IN s_aendat
                       AND   aentime IN s_aentim
                       AND   objnr   IN r_objnr.
* Select orders per start date and object number
  ELSEIF NOT l_date IS INITIAL .
    SELECT * FROM chzp INTO TABLE lit_chzp
                       WHERE aendat  IN s_aendat
                       AND   objnr   IN r_objnr.
  ELSE.
* Select orders per object number
    SELECT * FROM chzp INTO TABLE lit_chzp
               WHERE objnr   IN r_objnr.
  ENDIF.
  SORT lit_chzp BY mandt objnr status.
  l_chzp[] = lit_chzp[].
* Remove duplicates for status selection
  DELETE ADJACENT DUPLICATES FROM l_chzp COMPARING mandt objnr .
*-> CPZP
  DESCRIBE TABLE l_chzp LINES l_lines.
  IF NOT l_lines IS INITIAL.
* Check for order existance
* All orders have at least one either acrive or inactive status
    SELECT * FROM jest INTO TABLE t_jest_tab
                           FOR ALL ENTRIES IN l_chzp
                           WHERE objnr = l_chzp-objnr ORDER BY PRIMARY KEY.
    FREE l_chzp.

* Check orders
    LOOP AT lit_chzp.
* Does the object have a status?
      READ TABLE t_jest_tab WITH KEY mandt  = lit_chzp-mandt
                                     objnr  = lit_chzp-objnr BINARY SEARCH TRANSPORTING NO FIELDS.

      IF sy-subrc NE 0.
* :-(
* Corresponding order does not exit any more ==> Archive records for that object number
        CONTINUE.
      ELSE.
* Has the object an active deletion status?
        READ TABLE t_jest_tab WITH KEY mandt  = lit_chzp-mandt
                                       objnr  = lit_chzp-objnr
                                       stat   = k_loek
                                       inact  = space BINARY SEARCH TRANSPORTING NO FIELDS.
        IF sy-subrc NE 0.
* :-(
* Corresponding order is not marked for deletion ==> Remove object number from archive tables
          DELETE lit_chzp WHERE mandt = t_jest_wa-mandt
                          AND   objnr = t_jest_wa-objnr.
        ENDIF.
      ENDIF.
    ENDLOOP.
* Read correspnding CPZP entries
    SELECT * FROM cpzp INTO TABLE lit_cpzp
                       FOR ALL ENTRIES IN lit_chzp
                       WHERE objnr = lit_chzp-objnr.
* Read correspnding CEZP entries
    SELECT * FROM cezp INTO TABLE lit_cezp
                           FOR ALL ENTRIES IN lit_chzp
                           WHERE objnr = lit_chzp-objnr.
  ELSE.
* No data selected
    MESSAGE s000(00).
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " READ_BY_CHZP
