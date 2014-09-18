report zghocpzp_dl message-id rm line-size 86.

tables: dd02t.
types:
   begin of t_counter,                 "Table counter
      i_count_cpzp   type i,
      i_count_chzp   type i,
      i_count_cezp   type i,
      i_count_object type i,
   end of t_counter.

************************************************************************
*       Selection parameters                                           *
************************************************************************
selection-screen begin of block a1 with frame title text-022.
selection-screen skip 1.
parameters:     pa_testr default 'X' as checkbox.
selection-screen end   of block a1.
selection-screen skip 1.
data:   it_cezp         like cezp       occurs 0 with header line,
        it_cpzp         like cpzp       occurs 0 with header line,
        it_chzp         like chzp       occurs 0 with header line.
data:
   handle           like sy-tabix,
   i_counter        type i,                   "temp. object counter
   i_commit_cnt     like arch_usr-arch_comit, "Commit counter
   c_db_index       like arch_usr-arch_index. "Datenbankindex aufbauen ?
constants:
   k_chare          type c value 'E',
   k_charx          type c value 'X',
   k_chars          type c value 'S',
   k_msgid_rm       like sy-msgid value 'RM',
   k_archive_object like arch_obj-object value 'ZPP_REPORT',
   k_str_delete     like arc_buffer-flags value 'DELETE  ',
   k_object_counter type i value 100,
*-> Table names
   k_tab_cpzp       like arc_buffer-rname value 'CPZP',
   k_tab_chzp       like arc_buffer-rname value 'CHZP',
   k_tab_cezp       like arc_buffer-rname value 'CEZP' .
data:   fl_tab_counter   type t_counter.

************************************************************************
*       Internal tables
************************************************************************
data:   it_select_files like admi_files occurs 0.
************************************************************************
*       START OF SELECTION - Event
************************************************************************
start-of-selection.
*-> initialize data structure
  perform init_data tables it_cezp it_cpzp it_chzp
                    using  fl_tab_counter.

*-> Open archiving object for deletion
  perform open_archive_del_object using handle.

*-> Read customizing data
  perform get_customizing_data using i_commit_cnt c_db_index.

*-> Read archiving file into internal tables
  perform get_archive_data using handle i_commit_cnt                                  fl_tab_counter.

*-> Close archving object
  perform close_archive_object using handle.

*-> Statistic head line
  write: / text-001.

*-> Print protocol information
  perform print_information using fl_tab_counter.

*-> Release memory for internal tables
  perform free_tab_memory tables it_cezp it_cpzp it_chzp.

************************************************************************
*                                                                      *
*       FORM - ROUTINES                                                *
*                                                                      *
************************************************************************


*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CEZP         text
*      -->P_IT_CPZP         text
*      -->P_IT_CHZP         text
*      -->P_FL_TAB_COUNTER  text
*----------------------------------------------------------------------*
form init_data tables p_it_cezp  structure it_cezp
                      p_it_cpzp  structure it_cpzp
                      p_it_chzp  structure it_chzp
               using  p_fl_tab_counter type t_counter.

  refresh:
    p_it_cezp,
    p_it_cpzp,
    p_it_chzp  .
  clear:
    p_it_cezp,
    p_it_cpzp,
    p_it_chzp,
    p_fl_tab_counter.

endform.                    "init_data

*&---------------------------------------------------------------------*
*&      Form  OPEN_ARCHIVE_DEL_OBJECT
*&---------------------------------------------------------------------*
*       Open archiving object
*----------------------------------------------------------------------*
*  -->  P_HANDLE  Zeiger auf die Archivierungsdatei                    *
*----------------------------------------------------------------------*
form open_archive_del_object using p_handle like sy-tabix.
* Open archving object for deletion
  call function 'ARCHIVE_OPEN_FOR_DELETE'
    exporting
      archive_name       = ' '
      object             = k_archive_object
      test_mode          = pa_testr
    importing
      archive_handle     = p_handle
    tables
      selected_files     = it_select_files
    exceptions
      file_already_open  = 1
      file_io_error      = 2
      internal_error     = 3
      no_files_available = 4
      object_not_found   = 5
      open_error         = 6
      not_authorized     = 7
      others             = 8.
* Exception handling
  if sy-subrc <> 0.
*-> Message: 'Archivdatei konnte nicht zum L#schen ge#ffnert werden !'
    message id k_msgid_rm type k_chars number 711.
  endif.                               "sy-subrc <> 0

endform.                               " OPEN_ARCHIVE_DEL_OBJECT


*&---------------------------------------------------------------------*
*&      Form  GET_CUSTOMIZING_DATA
*&---------------------------------------------------------------------*
*       Lie#t die Customizing Informationen zu diesem Archivierungs-
*       objekt aus
*----------------------------------------------------------------------*
*  <--  p_i_commit_cnt  Anzahl der Objekte f#r ein Commit Work         *
*  <--  pc_db_index     Kennzeichen, ob Datenobjektindex angelegt wird *
*----------------------------------------------------------------------*
form get_customizing_data using pi_commit_cnt like arch_usr-arch_comit
                                pc_db_index   like arch_usr-arch_index.

  call function 'ARCHIVE_GET_CUSTOMIZING_DATA'
    exporting
      object                      = k_archive_object
    importing
      commit_count_for_delete_prg = pi_commit_cnt
      maintain_index              = pc_db_index
    exceptions
      object_not_found            = 1
      others                      = 2.

  if sy-subrc <> 0.
*-> Message: 'Customizingdaten konnten nicht gefunden werden !'
    message id k_msgid_rm type k_chars number 712.
  endif.                               "sy-subrc <> 0

endform.                               " GET_CUSTOMIZING_DATA


*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVE_DATA
*&---------------------------------------------------------------------*
*       Liest die Archivierungsdatei aus und #bergibt die eingelesenen
*       S#tze an die entsprechenden internen Tabellen
*----------------------------------------------------------------------*
*  <->  P_HANDLE         Zeiger auf die Archivierungsdatei             *
*  <->  P_I_COMMIT_CNT   Commit Z#hler                                 *
*  -->  P_C_DB_INDEX     Indexkennzeichen                              *
*  <->  P_FL_TAB_COUNTER Struktur der Tabellenz#hler                   *
*----------------------------------------------------------------------*
form get_archive_data using p_handle         like sy-tabix
                            p_i_commit_cnt   like arch_usr-arch_comit
                            p_fl_tab_counter type t_counter.

  data:
    i_count     like arch_usr-arch_comit,
    s_object_id like arch_idx-object_id,
    i_offset    like arch_idx-offset,
    s_arkey     like arch_idx-archivekey,
    i_return    like sy-subrc.

*-> Schleife #ber alle Archivierungsobjekte
  i_count = 0.
  do.
    i_count = i_count + 1.

*-> Archivierungsobjekt bereitstellen
    perform get_next_object using p_handle s_object_id s_arkey i_offset
                                   p_fl_tab_counter i_return.

    if i_return = 1.
      exit.
    endif.                             "i_return = 1

*-> Read records into internal tables
    perform get_archive_record_tab tables it_cezp it_cpzp it_chzp
                                   using  p_handle p_fl_tab_counter.

*-> Limit exceeded ?
    if i_count > p_i_commit_cnt.
*-> delete table entries
      perform delete_db_tables tables  it_cezp it_cpzp it_chzp.

      i_count = 0.                     "Z#hler zur#cksetzen
    endif.                             "i_count > p_i_commit_cnt
  enddo.

*--> delete remaining records
  perform delete_db_tables tables  it_cezp it_cpzp it_chzp.

endform.                               " GET_ARCHIVE_DATA


*&---------------------------------------------------------------------*
*&      Form  DELETE_DB_TABLES
*&---------------------------------------------------------------------*
*       Delete records on DB from internal tables
*----------------------------------------------------------------------*
*  -->  P_IT_BLPK   Internal table it_cezp                             *
*  -->  P_IT_BLPP   Internal table it_cpzp                             *
*  -->  P_IT_MKPF   Internal table it_chzp                             *
*----------------------------------------------------------------------*
form delete_db_tables tables   p_it_cezp  structure it_cezp
                               p_it_cpzp  structure it_cpzp
                               p_it_chzp  structure it_chzp.

*-> Nicht im Testmodus ?
  if pa_testr = space.
*-> CPZP
    delete cpzp from table p_it_cpzp.
*-> CHZP
    delete chzp from table p_it_chzp.
*-> CEZP
    delete cezp from table p_it_cezp.
    commit work.
  endif.                               "pa_testr = space

  refresh:
    p_it_cpzp, p_it_chzp, p_it_cezp.

endform.                               " DELETE_DB_TABLES
*&---------------------------------------------------------------------*
*&      Form  BUILD_UP_INDEX
*&---------------------------------------------------------------------*
*       Index erstellen
*----------------------------------------------------------------------*
*  -->  P_S_OBJECT_ID  Schl#ssel des Datenobjektes                     *
*  -->  P_S_ARKEY      eindeutiger Schl#ssel der Archivdatei           *
*  -->  P_I_OFFSET     Offset des Datenobjektes im Archiv              *
*----------------------------------------------------------------------*
form build_up_index using p_s_object_id like arch_idx-object_id
                          p_s_arkey     like arch_idx-archivekey
                          p_i_offset    like arch_idx-offset.

  call function 'ARCHIVE_ADMIN_SAVE_INDEX'
    exporting
      object        = k_archive_object
      object_id     = p_s_object_id
      archivekey    = p_s_arkey
      object_offset = p_i_offset
    exceptions
      others        = 0.

endform.                    "build_up_index
*&---------------------------------------------------------------------*
*&      Form  CLOSE_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*       Schliest das Archivierungsobjekt
*----------------------------------------------------------------------*
*  -->  p_handle   Zeiger auf die Archivierungsdatei                   *
*----------------------------------------------------------------------*
form close_archive_object using p_handle like sy-tabix.

*-> Archivierungsobjekt schliessen
  call function 'ARCHIVE_CLOSE_FILE'
    exporting
      archive_handle          = p_handle
    exceptions
      internal_error          = 1
      wrong_access_to_archive = 2
      others                  = 3.

  if sy-subrc <> 0.
*-> Message: 'Fehler beim Schliessen der Archivierungsdatei !'
    message id k_msgid_rm type k_chars number 710.
  endif.                               "sy-subrc <> 0

endform.                               " CLOSE_ARCHIVE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  free_tab_memory
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CPZP  text
*      -->P_IT_CHZP  text
*----------------------------------------------------------------------*
form free_tab_memory tables   p_it_cezp  structure it_cezp
                              p_it_cpzp  structure it_cpzp
                              p_it_chzp  structure it_chzp.

  free:
   p_it_cezp  ,
   p_it_cpzp  ,
   p_it_chzp.
endform.                    "free_tab_memory
*&---------------------------------------------------------------------*
*&      Form  get_archive_record_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BLPK  text
*      -->P_IT_BLPP  text
*      -->P_IT_MKPF  text
*      -->P_IT_MSEG  text
*      -->P_IT_AFRU  text
*      -->P_IT_PZPE  text
*      -->P_IT_AFWIS text
*      -->P_IT_AFFW  text
*      -->P_IT_CEZP  text
*      -->P_P_HANDLE text
*      -->P_COUNTER  text
*----------------------------------------------------------------------*
form get_archive_record_tab tables p_it_cezp  structure it_cezp
                                   p_it_cpzp  structure it_cpzp
                                   p_it_chzp  structure it_chzp
                            using  p_p_handle like sy-tabix
                                   p_counter  type t_counter.
  data: p_arc_buffer type arc_buffer.
  do.
    call function 'ARCHIVE_GET_NEXT_RECORD'
         exporting
              archive_handle          = p_p_handle
              get_real_structure_name = 'X'
              automatic_conversion    = 'X'
         importing
              record                  = p_arc_buffer-segment
*             record_cursor           =
              record_flags            = p_arc_buffer-flags
              record_structure        = p_arc_buffer-rname
*             record_length           =
         exceptions
              end_of_object           = 1
              internal_error          = 2
              wrong_access_to_archive = 3
              others                  = 4.

    if sy-subrc <> 0.
      case sy-subrc.
        when 1.
          exit.
        when others.
*-> Message: 'Fehler beim Zugriff auf die Archivdatei !'
          message id k_msgid_rm type k_chars number 713.
      endcase.                         "sy-subrc
    endif.                             "sy-subrc <> 0

*-> Kennzeichen zum L#schen des Datensatzes gesetzt ?
    if p_arc_buffer-flags = k_str_delete.
*-> ausgelesene Daten der entsprechenden internen Tabelle zuweisen
      case p_arc_buffer-rname.
        when k_tab_cpzp.
*          MOVE ARC_BUFFER-SEGMENT TO P_IT_CPZP.
          call method cl_abap_container_utilities=>read_container_c
            exporting
              im_container           = p_arc_buffer-segment
            importing
              ex_value               = p_it_cpzp
            exceptions
              illegal_parameter_type = 1
              others                 = 2.
          append p_it_cpzp.
          p_counter-i_count_cpzp = p_counter-i_count_cpzp + 1.
        when k_tab_cezp.
*          MOVE ARC_BUFFER-SEGMENT TO P_IT_CEZP.
          call method cl_abap_container_utilities=>read_container_c
            exporting
              im_container           = p_arc_buffer-segment
            importing
              ex_value               = p_it_cezp
            exceptions
              illegal_parameter_type = 1
              others                 = 2.
          append p_it_cezp.
          p_counter-i_count_cezp = p_counter-i_count_cezp + 1.

        when k_tab_chzp.
*         MOVE ARC_BUFFER-SEGMENT TO P_IT_CHZP.
          call method cl_abap_container_utilities=>read_container_c
            exporting
              im_container           = p_arc_buffer-segment
            importing
              ex_value               = p_it_chzp
            exceptions
              illegal_parameter_type = 1
              others                 = 2.
          append p_it_chzp.
          p_counter-i_count_chzp = p_counter-i_count_chzp + 1.

      endcase.
    endif.                             "arc_buffer-flags = k_str_delete
  enddo.

endform.                               " GET_ARCHIVE_RECORD_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_NEXT_OBJECT
*&---------------------------------------------------------------------*
*       Liefert das n#chste Archivierungsobjekt, sollte keines mehr
*       vorhanden sein, wird der Parameter p_i_return auf 1 gesetzt.
*----------------------------------------------------------------------*
*  -->  P_P_HANDLE     Zeiger auf die Archivierungsdatei               *
*  -->  P_S_OBJECT_ID  Identifikation des gelesenen Datenobjekts       *
*  -->  P_S_ARKEY      Archiv-Schl#ssel laut Archivverwaltung          *
*  -->  P_I_OFFSET     Offset des Datenobjektes im Archiv              *
*  -->  P_COUNTER      Objektz#hler                                    *
*  -->  P_I_RETURN     Returnwert (0 = keine Probleme, 1 = Ende)       *
*----------------------------------------------------------------------*
form get_next_object using p_p_handle    like sy-tabix
                           p_s_object_id like arch_idx-object_id
                           p_s_arkey     like arch_idx-archivekey
                           p_i_offset    like arch_idx-offset
                           p_counter     type t_counter
                           p_i_return    like sy-subrc.

  p_i_return = 0.
  call function 'ARCHIVE_GET_NEXT_OBJECT'
    exporting
      archive_handle          = p_p_handle
    importing
      object_id               = p_s_object_id
      object_offset           = p_i_offset
      archive_name            = p_s_arkey
    exceptions
      end_of_file             = 1
      file_io_error           = 2
      internal_error          = 3
      open_error              = 4
      wrong_access_to_archive = 5
      others                  = 6.

  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        p_i_return = 1.
      when others.
*-> Message: 'Fehler beim Zugriff auf die Archivdatei !'
        message id k_msgid_rm type k_chars number 713.
        p_i_return = 1.
    endcase.                           "sy-subrc
  else.
*-> Objektz#hler erh#hen
    p_counter-i_count_object = p_counter-i_count_object + 1.
  endif.                               "sy-subrc <> 0

endform.                               " GET_NEXT_OBJECT


*&---------------------------------------------------------------------*
*&      Form  PRINT_INFORMATION
*&---------------------------------------------------------------------*
*       Benutzerinformationen ausgeben
*----------------------------------------------------------------------*
*  -->  P_COUNTER  Feldleiste der Tabellenzeilenz#hler                 *
*----------------------------------------------------------------------*
form print_information using p_counter type t_counter.

  data:
    c_color type c.

  format color col_heading intensified." OFF.
  write: / sy-uline(80).
  skip.
  write: / text-006, p_counter-i_count_object color col_key, 80 ' '.

  write: / sy-uline(80).
  write: / sy-vline, 3 text-004, 15 text-005,
           26 text-008, 80 sy-vline.
  write: / sy-uline(80).

  perform print_table_info using k_tab_cpzp p_counter-i_count_cpzp
                                 c_color.

  perform print_table_info using k_tab_chzp p_counter-i_count_chzp
                                 c_color.
  perform print_table_info using k_tab_cezp p_counter-i_count_cezp
                                 c_color.
  write: / sy-uline(80).

endform.                               " PRINT_INFORMATION


*&---------------------------------------------------------------------*
*&      Form  PRINT_TABLE_INFO
*&---------------------------------------------------------------------*
*       Hintergrundfarbe der Tabellenzeilen umschalten
*----------------------------------------------------------------------*
*  -->  P_TAB_NAME  Tabellenname                                       *
*  -->  P_COUNT     Anzahl der Eintr#ge                                *
*  -->  P_C_COLOR   Farbmerker                                         *
*----------------------------------------------------------------------*
form print_table_info using p_tab_nam like arc_buffer-rname
                            p_count   type i
                            p_c_color type c.

  statics:
    c_zwerg(10).

*-> Anzahl Tabellens#tze > 0 ?
  if p_count > 0.

*-> Farbenhintergrund wechseln
    if p_c_color = space.
      format color col_normal intensified.
      p_c_color = 'X'.
    else.
      format color col_normal intensified off.
      p_c_color = ' '.
    endif.                             "p_c_color = space

*-> Tabellenname bestimmen
    select single * from dd02t
           where  tabname     = p_tab_nam
           and    ddlanguage  = sy-langu
           and    as4local    = 'A'
           and    as4vers     = '0000'.

    if sy-subrc = 0.
*-> Umsetzung aufgrund der Parameter#bergabe
      c_zwerg = p_tab_nam.
*-> Tabellenzeile ausgeben
      write: /   '|',
                 c_zwerg,
                 p_count,
                 dd02t-ddtext(55),
                 80 '|'.
    else.                              "kommt eigendlich nicht vor
      clear dd02t-ddtext.
*-> Tabellenzeile ausgeben
      write: /   '|',
                 c_zwerg,
                 p_count,
                 dd02t-ddtext(55),
                 80 '|'.
    endif.                             "sy-subrc <> 0
  endif.                               "p_count > 0

endform.                               " PRINT_TABLE_INFO
