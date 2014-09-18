report zgho_cpzp_rd message-id rm line-size 86.


************************************************************************
*       Includes                                                       *
************************************************************************

tables: dd02t, afko, jsto.
type-pools: slis. "Provides definitions for ALV usage

*--- Additional data for ALV usage
data: g_fieldcat_tab type slis_t_fieldcat_alv.
data: g_tabname      like tih01-tabname.
data: g_repid        like sy-repid.


************************************************************************
*       Selection parameter                                            *
************************************************************************
selection-screen begin of block a1 with frame title text-022.
selection-screen skip 1.
selection-screen end   of block a1.
selection-screen skip 1.
parameters: r_cezp   type c radiobutton group test,
            r_chzp   type c radiobutton group test,
            r_cpzp   type c radiobutton group test.

data:   it_cezp      like cezp       occurs 0 with header line,
        it_cpzp      like cpzp       occurs 0 with header line,
        it_chzp      like chzp       occurs 0 with header line.
data:
   handle            like sy-tabix,
   i_counter         type i.            "temp. Objektz#hler
constants:
   k_chara           type c value 'A',
   k_chare           type c value 'E',
   k_charx           type c value 'X',
   k_chars           type c value 'S',
   k_msgid_rm        like sy-msgid value 'RM',
   k_archive_object  like arch_obj-object value 'ZPP_REPORT',
   k_str_delete      like arc_buffer-flags value 'DELETE  ',
   k_object_counter  type i value 100,
*-> Tabellennamen
   k_tab_cpzp        like arc_buffer-rname value 'CPZP',
   k_tab_chzp        like arc_buffer-rname value 'CHZP',
   k_tab_cezp        like arc_buffer-rname value 'CEZP' .



************************************************************************
*       Internal tables                                                *
************************************************************************
data:   it_select_files like admi_files occurs 0.

************************************************************************
*       START OF SELECTION - Event                                     *
************************************************************************
start-of-selection.
*-> Initialize data structure
  perform init_data tables it_cezp it_cpzp it_chzp.

*-> Open archiving object for read
  perform open_archive_read_object using handle.

*-> Retrieve and store data in internal tables
  perform get_archive_data using handle.

*-> Close archiving object
  perform close_archive_object using handle.
* Output of data
  if r_cezp eq k_charx.
* Display of CEZP data
    read table it_cezp index 1 transporting no fields.
    if sy-subrc eq 0.
      perform get_fieldcat using k_tab_cezp.
      perform list_data_cezp tables it_cezp.
    else.
      message s742 with k_tab_cezp.
    endif.
  elseif r_chzp eq k_charx.
* Display of CHZP data
    read table it_chzp index 1 transporting no fields.
    if sy-subrc eq 0.
      perform get_fieldcat using k_tab_chzp.
      perform list_data_chzp tables it_chzp.
    else.
      message s742 with k_tab_chzp.
    endif.
  elseif r_cpzp eq k_charx.
* Display of CPZP data
    read table it_cpzp index 1 transporting no fields.
    if sy-subrc eq 0.
      perform get_fieldcat using k_tab_cpzp  .
      perform list_data_cpzp tables it_cpzp.
    else.
      message s742 with k_tab_cpzp.
    endif.
  endif.
*-> Release memory of internal tables
  perform free_tab_memory tables it_cezp it_cpzp it_chzp.



************************************************************************
*                                                                      *
*       FORM - ROUTINEN                                                *
*                                                                      *
************************************************************************
form init_data tables p_it_cezp  structure it_cezp
                      p_it_cpzp  structure it_cpzp
                      p_it_chzp  structure it_chzp.

  refresh:
    p_it_cezp,
    p_it_cpzp,
    p_it_chzp  .
  clear:
    p_it_cezp,
    p_it_cpzp,
    p_it_chzp.

endform.                    "init_data

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
form get_archive_data using p_handle         like sy-tabix.
  data:
    s_object_id like arch_idx-object_id,
    i_offset    like arch_idx-offset,
    s_arkey     like arch_idx-archivekey,
    i_return    like sy-subrc.
*-> Loop over all objects
    do.
*-> Provide archiving object
    perform get_next_object using p_handle s_object_id s_arkey i_offset
                                  i_return.
*-> Read data tinto internal tables
    perform get_archive_record_tab tables it_cezp it_cpzp it_chzp
                                   using  p_handle.
    if i_return ne 0.
      exit.
    endif.
  enddo.
endform.                               " GET_ARCHIVE_DATA

*&---------------------------------------------------------------------*
*&      Form  CLOSE_ARCHIVE_OBJECT
*&---------------------------------------------------------------------*
*       Schliest das Archivierungsobjekt
*----------------------------------------------------------------------*
*  -->  p_handle   Zeiger auf die Archivierungsdatei                   *
*----------------------------------------------------------------------*
form close_archive_object using p_handle like sy-tabix.

*-> Close archiving object
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
* Release entire memory for all tables
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
*      -->P_IT_CEZP  text
*      -->P_IT_CPZP  text
*      -->P_IT_CHZP  text
*      -->P_P_HANDLE text
*      -->P_COUNTER  text
*----------------------------------------------------------------------*
form get_archive_record_tab tables p_it_cezp  structure it_cezp
                                   p_it_cpzp  structure it_cpzp
                                   p_it_chzp  structure it_chzp
                            using  p_p_handle like sy-tabix.
  data: p_arc_buffer type arc_buffer.
* Retrieve individual archived records
  do.
    call function 'ARCHIVE_GET_NEXT_RECORD'
         exporting
              archive_handle          = p_p_handle
              get_real_structure_name = k_charx
              automatic_conversion    = k_charx
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
* Exception handling
    if sy-subrc <> 0.
      case sy-subrc.
        when 1.
          exit.
        when others.
*-> Message: 'Fehler beim Zugriff auf die Archivdatei !'
          message id k_msgid_rm type k_chare number 713.
          exit.
      endcase.                         "sy-subrc
    endif.                             "sy-subrc <> 0

*-> Assign read data to corresponding internal table
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
    endcase.
  enddo.
endform.                               " GET_ARCHIVE_RECORD_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_NEXT_OBJECT
*&---------------------------------------------------------------------*
*       Probides next archiving object. If there is no more object
*       parameter p_i_return is set to 1.
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
                           p_i_return    like sy-subrc.
* Open next object for reading
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
* Exception handling
  if sy-subrc <> 0.
    case sy-subrc.
      when 1.
        p_i_return = 1.
      when others.
*-> Message: 'Fehler beim Zugriff auf die Archivdatei !'
        message id k_msgid_rm type k_chars number 713.
        p_i_return = 1.
    endcase.                           "sy-subrc
  endif.                               "sy-subrc <> 0

endform.                               " GET_NEXT_OBJECT

*&---------------------------------------------------------------------*
*&      Form  list_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form get_fieldcat using g_tabname.
  data: l_events_wa type slis_alv_event.
* Setting up field catalog for ALV output
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = 'OBJECT_TAB'
      i_structure_name       = g_tabname
      i_client_never_display = ' '
    changing
      ct_fieldcat            = g_fieldcat_tab[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
endform.                    "get_fieldcat
*&---------------------------------------------------------------------*
*&      Form  list_data_cepz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CEPZ  text
*      -->TABLE      text
*      -->OF         text
*      -->CEPZ       text
*----------------------------------------------------------------------*
form  list_data_cezp tables p_it_cezp structure it_cezp.
  data:
  l_exit_caused_by_caller type c,
  l_exit_caused_by_user   type slis_exit_by_user.
* Display of CEPZ data in ALV list
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = sy-repid
      it_fieldcat             = g_fieldcat_tab[]
    importing
      e_exit_caused_by_caller = l_exit_caused_by_caller
      es_exit_caused_by_user  = l_exit_caused_by_user
    tables
      t_outtab                = p_it_cezp
    exceptions
      program_error           = 1
      others                  = 2.
endform.                    "list_data_cezp
*&---------------------------------------------------------------------*
*&      Form  list_data_chzp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CHZP  text
*----------------------------------------------------------------------*
form  list_data_chzp tables p_it_chzp structure it_chzp.
  data:
  l_exit_caused_by_caller type c,
  l_exit_caused_by_user   type slis_exit_by_user.
* Display of CHPZ data in ALV list
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = sy-repid
      it_fieldcat             = g_fieldcat_tab[]
    importing
      e_exit_caused_by_caller = l_exit_caused_by_caller
      es_exit_caused_by_user  = l_exit_caused_by_user
    tables
      t_outtab                = p_it_chzp
    exceptions
      program_error           = 1
      others                  = 2.
endform.                    "list_data_chzp
*&---------------------------------------------------------------------*
*&      Form  list_data_cpzp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CPZP  text
*----------------------------------------------------------------------*
form  list_data_cpzp tables p_it_cpzp structure it_cpzp.
  data:
  l_exit_caused_by_caller type c,
  l_exit_caused_by_user   type slis_exit_by_user.
* Display of CPZP data in ALV list
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = sy-repid
      it_fieldcat             = g_fieldcat_tab[]
    importing
      e_exit_caused_by_caller = l_exit_caused_by_caller
      es_exit_caused_by_user  = l_exit_caused_by_user
    tables
      t_outtab                = p_it_cpzp
    exceptions
      program_error           = 1
      others                  = 2.
endform.                    "list_data_cpzp

*&---------------------------------------------------------------------*
*&      Form  open_archive_del_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HANDLE   text
*----------------------------------------------------------------------*
form open_archive_read_object using p_handle like sy-tabix.
* Opens archive file for reading
  call function 'ARCHIVE_OPEN_FOR_READ'
    exporting
      object             = k_archive_object
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
  if sy-subrc <> 0.
*-> Message: 'Archivdatei konnte nicht zum L#schen ge#ffnert werden !'
    message id k_msgid_rm type k_chars number 711.
  endif.                               "sy-subrc <> 0

endform.                    "open_archive_del_object
