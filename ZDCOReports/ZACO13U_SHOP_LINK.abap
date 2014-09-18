*
* Spec: ANDY CHOI
* developed by Andy Choi
*
* t-code: SE93 / SM30 / VIEWNAME = table, UPDATE = X
*

report zaco13u_shop_link  no standard page heading message-id zmco.

include <icon>.

tables: ztco_shop_link.
data:   gt_data type table of zsco_shop_link with header line.
constants: gv_structure type dd02l-tabname value 'ZSCO_SHOP_LINK'.
*Structure
*FSC_MATNR	MATNR
*FSC_MAKTG	MAKTG
*ABP_MATNR	PLMAT
*ABP_MAKTG	MAKTG

data: gs_ztco_shop_link like ztco_shop_link,
      gt_ztco_shop_link type table of ztco_shop_link with header line.

*§1.Extend your output table for a field, e.g., CELLTAB, that holds
*   information about the edit status of each cell for the
*   corresponding row (the table type is SORTED!).
data: begin of gt_outtab occurs 0.  "with header line
data:   %_box(1) type c.
        include structure zsco_shop_link.
data: celltab type lvc_t_styl.
data: end of gt_outtab.


selection-screen begin of block b1 with frame title text-001.
parameters    :
  p_kokrs like tka01-kokrs memory id cac,
  p_gjahr like ztco_shop_link-bdatj memory id gjr.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
select-options:
  s_fmatnr for ztco_shop_link-fsc_matnr,
  s_amatnr for ztco_shop_link-abp_matnr.
selection-screen end of block b2.


* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class lcl_event_receiver definition deferred.
*
*********
data: ok_code like sy-ucomm,
      save_ok like sy-ucomm,
      g_container type scrfname value 'BCALV_GRID_DEMO_0100_CONT1',
      g_grid  type ref to cl_gui_alv_grid,
      g_custom_container type ref to cl_gui_custom_container,
      gt_fieldcat type lvc_t_fcat,
      gs_layout type lvc_s_layo,
      event_receiver type ref to lcl_event_receiver.

data: gt_row   type lvc_t_row,
      gs_row   type lvc_s_row,
      gt_roid  type lvc_t_roid.
data: gv_level(5),                       " Authorization Level
      gv_past(1).


*Refer: BCALV_EDIT_02.
*Refer: BCALV_EDIT_04
*BCALV_GRID_01                  Events for print processing
*BCALV_GRID_02                  Basic list and details list using one
*ALV control
*BCALV_GRID_03                  Detail list in screen of type dialog
*BCALV_GRID_04                  Display exceptions using LEDs or lights
*BCALV_GRID_05                  Add a self-defined button to the toolbar
*BCALV_GRID_06                  Define self-defined context menu
*BCALV_GRID_07                  Defining a toolbar menu
*BCALV_GRID_08                  Defining a toolbar menu with default
*button
*BCALV_GRID_09                  Control the saving options of layouts
*BCALV_GRID_10                  Load a layout before list display
*BCALV_GRID_11                  Test for new layout function modules
*BCALV_GRID_AND_POPUP           ALV Grid in dialog box
*BCALV_GRID_DEMO                Simple ALV Control Call Demo Program
*BCALV_GRID_DND_TREE            ALV Grid: Drag and Drop with ALV Tree
*BCALV_GRID_DND_TREE_SIMPLE     ALV GRID: Drag and drop with ALV tree
*(simple)
*BCALV_GRID_EDIT
*BCALV_GRID_F4_HELP             F4 Help: Test and Sample Program
*BCALV_GRID_VERIFY              ALV GridControl test program
*BCALV_GRID_VERIFY2             ALV GridControl test program

*se24 / CL_GUI_ALV_GRDI
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
class lcl_event_receiver definition.
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  public section.

    methods:
    handle_toolbar
        for event toolbar of cl_gui_alv_grid
            importing e_object e_interactive,

    handle_before_user_command
        for event MENU_BUTTON of cl_gui_alv_grid
            importing e_ucomm,

    handle_user_command
        for event user_command of cl_gui_alv_grid
            importing e_ucomm.
endclass.

*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
class lcl_event_receiver implementation.
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  method handle_toolbar.
* § 2.In event handler method for event TOOLBAR: Append own functions
*   by using event parameter E_OBJECT.
    data: ls_toolbar  type stb_button.
*....................................................................
* E_OBJECT of event TOOLBAR is of type REF TO CL_ALV_EVENT_TOOLBAR_SET.
* This class has got one attribute, namly MT_TOOLBAR, which
* is a table of type TTB_BUTTON. One line of this table is
* defined by the Structure STB_BUTTON (see data deklaration above).
*

* A remark to the flag E_INTERACTIVE:
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*         'e_interactive' is set, if this event is raised due to
*         the call of 'set_toolbar_interactive' by the user.
*         You can distinguish this way if the event was raised
*         by yourself or by ALV
*         (e.g. in method 'refresh_table_display').
*         An application of this feature is still unknown... :-)
* If the user klicks on the default button ALV raises
* directly event BEFORE_USER_COMMAND
* (then USER_COMMAND, AFTER_USER_COMMAND).
* If the user klicks on the menu button ALV raises event MENU_BUTTON.

* append a separator to normal toolbar
    clear ls_toolbar.
    move 3 to ls_toolbar-butn_type.
    append ls_toolbar to e_object->mt_toolbar.
* append an icon to show booking table
    clear ls_toolbar.
    move 'BOOKINGS' to ls_toolbar-function.
    move icon_employee to ls_toolbar-icon.
    move 'Show Bookings'(111) to ls_toolbar-quickinfo.
    move 'Detail'(112) to ls_toolbar-text.
    move ' ' to ls_toolbar-disabled.
    append ls_toolbar to e_object->mt_toolbar.

  endmethod.

*-------------------------------------------------------------------
  method handle_before_user_command.
    case e_ucomm.
       when others.
    endcase.
  endmethod.

*-------------------------------------------------------------------
  method handle_user_command.
* § 3.In event handler method for event USER_COMMAND: Query your
*   function codes defined in step 2 and react accordingly.

    data: lt_rows type lvc_t_row.

    case e_ucomm.
      when 'BOOKINGS'.
        call method g_grid->get_selected_rows
                 importing et_index_rows = lt_rows.
        call method cl_gui_cfw=>flush.
        if sy-subrc ne 0.
* add your handling, for example
        else.
        endif.
    endcase.
  endmethod.                           "handle_user_command

endclass.



*---------------------------------------------------------------------*
*       Start-of-selection
*---------------------------------------------------------------------*
start-of-selection.

  perform select_data.

*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*
  call screen 100.
*PROCESS BEFORE OUTPUT.
*  MODULE PBO.
**
*PROCESS AFTER INPUT.
*  MODULE PAI.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*

module pbo output.
  data: lt_exclude type ui_functions.

  set pf-status 'MAIN100'.
*  SET TITLEBAR 'MAIN100'.
  if g_custom_container is initial.
    create object g_custom_container
           exporting container_name = g_container.
    create object g_grid
           exporting i_parent = g_custom_container.
    perform init_style.

*§3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
    gs_layout-stylefname = 'CELLTAB'.

* Build fieldcat and set columns XXX edit enabled.
    perform build_fieldcat changing gt_fieldcat.
    perform exclude_tb_functions changing lt_exclude.

* set substate of editable cells to deactivated
    call method g_grid->set_ready_for_input
          exporting i_ready_for_input = 0.

    call method g_grid->set_table_for_first_display
         exporting "i_structure_name = gv_structure  -> fieldcatalog...
                   it_toolbar_excluding  = lt_exclude
                   is_layout             = gs_layout
         changing  it_fieldcatalog       = gt_fieldcat
                   it_outtab             = gt_outtab[].

********
* ->Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
*
    create object event_receiver.
    set handler event_receiver->handle_user_command for g_grid.
    set handler event_receiver->handle_toolbar for g_grid.
*
********

* § 4.Call method 'set_toolbar_interactive' to raise event TOOLBAR.
    call method g_grid->set_toolbar_interactive.

  endif.                               "IF grid1 IS INITIAL

  call method cl_gui_control=>set_focus exporting control = g_grid.

endmodule.
*---------------------------------------------------------------------*
*       MODULE PAI INPUT                                              *
*---------------------------------------------------------------------*
module pai input.
  save_ok = ok_code.
  clear ok_code.
  case save_ok.
    when 'EXIT'.
      perform exit_program.
    when 'SWITCH'.
      if gv_past = space or gv_level = 'ADM'.
        perform switch_edit_mode.
      endif.
    when 'SAVE'.
      if not g_grid->is_ready_for_input( ) eq 0.
        perform data_save.
      endif.
    when 'DELETE'.
      if not g_grid->is_ready_for_input( ) eq 0.
        perform data_delete.
        perform refresh_alv_disp.
      endif.

    when others.
*     do nothing
  endcase.
endmodule.
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
form exit_program.
  leave to screen 0.
*  leave PROGRAM.
endform.
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
form init_style.
  data: lt_celltab type lvc_t_styl,
        l_index type i.


*§2.After selecting data, set edit status for each row in a loop
*   according to field SEATSMAX.
  loop at gt_outtab.
    l_index = sy-tabix.
    refresh lt_celltab.
*    if gt_outtab-seatsmax ge 300.
    perform fill_celltab using 'RW'
                         changing lt_celltab.
*    else.
*        perform fill_celltab using 'RO'
*                             changing lt_celltab.
*    endif.

*§2c.Copy your celltab to the celltab of the current row of gt_outtab.
    insert lines of lt_celltab into table gt_outtab-celltab.
    modify gt_outtab index l_index.
  endloop.

endform.                               " SELECT_DATA_AND_INIT_STYLE
*&---------------------------------------------------------------------*
*&      Form  FILL_CELLTAB
*&---------------------------------------------------------------------*
form fill_celltab using value(p_mode)
                  changing pt_celltab type lvc_t_styl.
  data: ls_celltab type lvc_s_styl.
  data: l_mode1 type raw4,
        l_mode2 type raw4.
* This forms sets the style of column 'PRICE' editable
* according to 'p_mode' and the rest to read only either way.

  if p_mode eq 'RW'.
*§2a.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_ENABLED to set a cell
*    to status "editable".
    l_mode1 = cl_gui_alv_grid=>mc_style_enabled.
  else. "p_mode eq 'RO'
*§2b.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_DISABLED to set a cell
*    to status "non-editable".
    l_mode1 = cl_gui_alv_grid=>mc_style_disabled.
  endif.
  l_mode2 = cl_gui_alv_grid=>mc_style_disabled.

  ls_celltab-fieldname = 'ABP_MATNR'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

  ls_celltab-fieldname = 'FSC_MATNR'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

*  if gt_outtab-vvatage = 9999.
*    ls_celltab-fieldname = 'VVATAGE'.
*    ls_celltab-style = l_mode1.
*    insert ls_celltab into table pt_celltab.
*  endif.
endform.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
form switch_edit_mode.

  if g_grid->is_ready_for_input( ) eq 0.
* set edit enabled cells ready for input
    call method g_grid->set_ready_for_input
                     exporting i_ready_for_input = 1.

  else.
* lock edit enabled cells against input
    call method g_grid->set_ready_for_input
                    exporting i_ready_for_input = 0.
  endif.
endform.                               " SWITCH_EDIT_MODE


*CUST
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
form select_data.

  select fsc_matnr c~maktg as fsc_maktg abp_matnr
    into corresponding fields of table gt_data
    from ztco_shop_link as a
         inner join makt as c
            on c~matnr = a~fsc_matnr
           and c~spras = sy-langu
    where a~kokrs = p_kokrs
      and a~bdatj = p_gjahr
      and a~fsc_matnr   in s_fmatnr
      and a~abp_matnr   in s_amatnr.

  data: begin of lt_matnr occurs 0,
           matnr like makt-matnr,
           maktg like makt-maktg,
        end of lt_matnr.

  select * into corresponding fields of table lt_matnr
     from makt
     for all entries in gt_data
     where matnr = gt_data-abp_matnr
       and spras = sy-langu.
  sort lt_matnr by matnr.

  loop at gt_data.
    move-corresponding gt_data to gt_outtab.

    read table lt_matnr with key matnr = gt_data-abp_matnr
                        binary search.
    gt_outtab-abp_maktg = lt_matnr-maktg.

    append gt_outtab.
  endloop.

  sort gt_data by fsc_matnr.

endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
form build_fieldcat changing pt_fieldcat type lvc_t_fcat.

  data ls_fcat type lvc_s_fcat.

  call function 'LVC_FIELDCATALOG_MERGE'
       exporting
            i_structure_name = gv_structure
       changing
            ct_fieldcat      = pt_fieldcat.
  loop at pt_fieldcat into ls_fcat.
    if     ls_fcat-fieldname eq 'FSC_MATNR'.
      ls_fcat-edit = 'X'.
    elseif ls_fcat-fieldname eq 'ABP_MATNR'.
      ls_fcat-edit = 'X'.
      ls_fcat-auto_value = 'X'.
    else.
      ls_fcat-checktable = '!'.   "do not check foreign key relations
    endif.

    modify pt_fieldcat from ls_fcat.
  endloop.

endform.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  exclude_tb_functions
*&---------------------------------------------------------------------*
form exclude_tb_functions changing pt_exclude type ui_functions.
*
  data ls_exclude type ui_func.

  exit.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  append ls_exclude to pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  append ls_exclude to pt_exclude.

*  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
*  append ls_exclude to pt_exclude.

endform.                    " exclude_tb_functions
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV_DISP
*&---------------------------------------------------------------------*
*       Refresh ALV grid
*----------------------------------------------------------------------*
form refresh_alv_disp.
  call method g_grid->set_frontend_fieldcatalog
       exporting
         it_fieldcatalog = gt_fieldcat.

  call method g_grid->refresh_table_display.
  call method cl_gui_cfw=>flush.

endform.                    " REFRESH_ALV_DISP
*&---------------------------------------------------------------------*
*&      Form  update_database
*&---------------------------------------------------------------------*
form update_database.
*  DATA: lt_del_rows TYPE TABLE OF sflight,
*        lt_ins_keys TYPE g_verifier->sflight_keys,
*        l_ins_key TYPE g_verifier->sflight_key,
*        ls_sflight TYPE sflight,
*        ls_outtab LIKE LINE OF gt_outtab,
*        lt_instab TYPE TABLE OF sflight.
*
** §8.When all values are valid, use your internal tables
**    to update your table on the database.
*
** First delete lines in data base according to the
** keys you remembered within 'g_verifier'.
** Imagine a user deleted a row and then entered one with
** the same key fields like the deleted ones.
** Then both tables (deleted_rows, inserted_rows) have got
** an entry of this row.
** So if you first inserted the new rows in the data base
** and then deleted rows according to 'deleted_rows'
** The reentered rows would be lost.
**
** 1.Delete Lines:
*  call method g_verifier->get_deleted_rows
*          importing deleted_rows = lt_del_rows.
*
*  delete sflight from table lt_del_rows.
*
*
** 2.Insert Lines:
*  call method g_verifier->get_inserted_rows
*          importing inserted_rows = lt_ins_keys.
*
*

* §9.Refresh your internal tables.
*
*  call method g_verifier->refresh_delta_tables.

endform.                    " update_database
*&---------------------------------------------------------------------*
*&      Form  data_delete
*&---------------------------------------------------------------------*
form data_delete.
  data: l_valid type c.

  call method g_grid->check_changed_data
               importing e_valid = l_valid.

  if not l_valid is initial.

    call method g_grid->get_selected_rows
                importing et_index_rows = gt_row
                          et_row_no = gt_roid.

    loop at gt_row into gs_row.
      read table gt_outtab index gs_row-index.
      if sy-subrc = 0.
        move-corresponding gt_outtab to gt_ztco_shop_link.
        append gt_ztco_shop_link.

        delete gt_outtab index gs_row-index.
      endif.
    endloop.

    loop at gt_ztco_shop_link into gs_ztco_shop_link.
      delete from ztco_shop_link
        where kokrs = p_kokrs
          and bdatj = p_gjahr
          and fsc_matnr = gs_ztco_shop_link-fsc_matnr.
    endloop.
    if sy-dbcnt > 0.
      message s000(0k) with 'Data deleted:'  sy-dbcnt.
    endif.
  endif.

endform.                    " data_delete
*&---------------------------------------------------------------------*
*&      Form  data_save
*&---------------------------------------------------------------------*
form data_save.
  data: l_valid type c.

* §7.Check if any errors exist in protocol by using method
*    CHECK_CHANGED_DATA of your ALV Grid instance.

* The method CHECK_CHANGED_DATA checks all new cells syntactically,
* raises event DATA_CHANGED and looks then for any entries
* in the error protocol. If any exist the parameter e_valid
* is initial ('X' in the other case).

  call method g_grid->check_changed_data
               importing e_valid = l_valid.

  if not l_valid is initial.
*   perform update_database.

    call method g_grid->get_selected_rows
                importing et_index_rows = gt_row
                          et_row_no = gt_roid.

    loop at gt_row into gs_row.
      read table gt_outtab index gs_row-index.
      if sy-subrc = 0.
        check gt_outtab-fsc_matnr <> space.
        move-corresponding gt_outtab to gt_ztco_shop_link.

        append gt_ztco_shop_link.
      endif.
    endloop.

    loop at gt_ztco_shop_link into gs_ztco_shop_link.
      gs_ztco_shop_link-mandt = sy-mandt.
      gs_ztco_shop_link-kokrs = p_kokrs.
      gs_ztco_shop_link-bdatj = p_gjahr.

    read table gt_data with key fsc_matnr = gs_ztco_shop_link-fsc_matnr
                                            binary search.
      if sy-subrc = 0.
        gs_ztco_shop_link-aedat = sy-datum.
        gs_ztco_shop_link-aezet = sy-uzeit.
        gs_ztco_shop_link-aenam = sy-uname.



        update ztco_shop_link from gs_ztco_shop_link.
      else.
        gs_ztco_shop_link-aedat = sy-datum.
        gs_ztco_shop_link-aezet = sy-uzeit.
        gs_ztco_shop_link-aenam = sy-uname.
        insert ztco_shop_link from gs_ztco_shop_link.

      endif.
    endloop.
    if sy-dbcnt > 0.
      message s000(0k) with 'Data saved:'  sy-dbcnt.
    endif.
  endif.

endform.                    " data_save
