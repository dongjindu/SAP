FUNCTION Z_SE16N_INTERFACE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_TAB) TYPE  SE16N_TAB
*"     VALUE(I_EDIT) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_SAPEDIT) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_NO_TXT) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_MAX_LINES) TYPE  SYTABIX DEFAULT 500
*"     VALUE(I_LINE_DET) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_DISPLAY) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(I_CLNT_SPEZ) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_CLNT_DEP) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_VARIANT) TYPE  SLIS_VARI DEFAULT ' '
*"     VALUE(I_OLD_ALV) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_CHECKKEY) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_TECH_NAMES) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_CWIDTH_OPT_OFF) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_SCROLL) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_NO_CONVEXIT) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_LAYOUT_GET) TYPE  CHAR1 DEFAULT ' '
*"     VALUE(I_ADD_FIELD) TYPE  CHAR40 OPTIONAL
*"     VALUE(I_ADD_FIELDS_ON) TYPE  CHAR1 OPTIONAL
*"     VALUE(I_UNAME) TYPE  SY-UNAME OPTIONAL
*"  EXPORTING
*"     VALUE(E_LINE_NR) TYPE  SYTABIX
*"     VALUE(E_DREF)
*"  TABLES
*"      IT_SELFIELDS STRUCTURE  SE16N_SELTAB OPTIONAL
*"      IT_OUTPUT_FIELDS STRUCTURE  SE16N_OUTPUT OPTIONAL
*"      IT_OR_SELFIELDS TYPE  SE16N_OR_T OPTIONAL
*"      IT_CALLBACK_EVENTS TYPE  SE16N_EVENTS OPTIONAL
*"      IT_ADD_UP_CURR_FIELDS STRUCTURE  SE16N_OUTPUT OPTIONAL
*"      IT_ADD_UP_QUAN_FIELDS STRUCTURE  SE16N_OUTPUT OPTIONAL
*"  EXCEPTIONS
*"      NO_VALUES
*"--------------------------------------------------------------------
data: lt_where   type se16n_where_132 occurs 0 with header line.
data: ld_subrc   like sy-subrc.
data: ld_txt_tab type DD08V-TABNAME.
data: ld_partial(1).
data: ld_tabclass like dd02l-tabclass.

*..If only line number -> no texts
   if i_line_det = true.
      i_no_txt = true.
   endif.
   gd-no_txt = i_no_txt.

*..In batch or extern call, these are not filled before
   gd-tab         = i_tab.
   gd-max_lines   = i_max_lines.
   gd-variant     = i_variant.
   gd-edit        = i_edit.
   gd-sapedit     = i_sapedit.
   gd-checkkey    = i_checkkey.
   gd-scroll      = i_scroll.
   gd-no_convexit = i_no_convexit.
   gd-layout_get  = i_layout_get.
   gd-add_field   = i_add_field.
   gd-add_fields_on = i_add_fields_on.
   if i_uname = space.
      gd-uname = sy-uname.
   else.
      gd-uname = i_uname.
   endif.

*..fill global event table
   refresh: gt_callback_events.
   gt_callback_events[] = it_callback_events[].
*.........................................................
   select * from se16n_exit into table gt_cb_events
               where tab = gd-tab.
*..special logic for some events, because of *-entries
   perform read_exit_data using c_event_add_fields.
*..be compatible to new exit logic
*..caller can either hand over it_callback_events
*..or table SE16N_EXIT is permanently filled with exit
*.........................................................
   loop at gt_callback_events into gs_callback_events.
      move-corresponding gs_callback_events to gs_cb_events.
      gs_cb_events-tab = i_tab.
      append gs_cb_events to gt_cb_events.
   endloop.

*..fill global add-up tables
   refresh: gt_add_up_curr_fields.
   gt_add_up_curr_fields[] = it_add_up_curr_fields[].
   refresh: gt_add_up_quan_fields.
   gt_add_up_quan_fields[] = it_add_up_quan_fields[].

*..check if table is a pool-table
   select single tabclass from dd02l into ld_tabclass
                     where tabname = i_tab.
   if sy-subrc = 0 and
      ld_tabclass = 'POOL'.
      gd-pool = true.
   else.
      clear gd-pool.
   endif.

*..If back to first screen, delete all modifcations
   clear: gt_mod.
   refresh: gt_mod.

*..Check the authority of the user
*   perform authority_check using    i_tab
*                           changing i_edit.

*..First get time for display of runtime
   perform get_time changing gd-start_time
                             gd-start_date.

*..Check if there is a corresponding text table
   perform get_text_table using    i_tab
                          changing ld_txt_tab.
   gd-txt_tab = ld_txt_tab.

*..Now create a fieldcatalog of the table I have to display. After this
*..GT_FIELDCAT is filled and <ALL_TABLE> is created and defined
   perform create_fieldcat_standard tables   it_output_fields
                                    using    i_tab
                                             i_edit
                                             i_tech_names
                                             i_no_txt
                                             i_clnt_spez
                                             i_clnt_dep
                                             ld_txt_tab.

*..delete adjacent duplicates out of selection table
   perform delete_duplicates tables it_or_selfields.

*..find out if too many selection criteria are used
*..if so, do the partial select
   clear ld_partial.
*..Check if no multi select took place.
*..Then no partial select is possible.
   describe table it_or_selfields lines sy-tabix.
   if sy-tabix = 1.
      gt_or[] = it_or_selfields[].
      perform scan_selfields using    i_tab
                                      i_max_lines
                                      i_line_det
                                      i_display
                                      i_clnt_spez
                             changing ld_subrc
                                      ld_partial.
   endif.

*..if not, do the normal select
   if ld_partial <> true.
*.....Create selection table out of input selfields
      perform create_seltab tables lt_where
                                   it_selfields
                                   it_or_selfields.

*..Now do the select on one table with the created selection criteria
      perform select_standard tables   lt_where
                              using    i_tab
                                       i_max_lines
                                       i_line_det
                                       i_display
                                       i_clnt_spez
                              changing ld_subrc.
   endif.

*..Give number of found entries back to caller
   e_line_nr = gd-number.

*..If nothing has been found, exit
*..In case of edit -> Show empty table, because of insert
   if ld_subrc <> 0 and i_edit <> true and
      gd-number < 1.
      message e002(wusl) raising no_values.
   endif.

*..Select the texts
   if i_no_txt <> true.
      perform select_text_table using  ld_txt_tab
                                       i_clnt_spez.
   endif.

*..Now display the results in a fullscreen ALV-Grid
   if i_line_det <> true.
      if i_display = true.
         perform display_standard using i_old_alv
                                        i_cwidth_opt_off.
      else.
         e_dref = gd_dref.
      endif.
   else.
      perform display_line_nr.
   endif.


ENDFUNCTION.
