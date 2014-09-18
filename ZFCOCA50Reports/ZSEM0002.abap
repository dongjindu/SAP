*
* Spec: ANDY CHOI
* developed by Andy Choi
*
report zsem0002  no standard page heading message-id zmco.

tables: ztbw_pmei, marv.

selection-screen begin of block b1 with frame title text-001.
parameters    :
  p_bukrs like t001-bukrs default 'H201',
  p_gjahr like ztbw_pmei-gjahr memory id gjr,
  p_monat like ztbw_pmei-monat.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-002.
select-options:
  s_matnr for ztbw_pmei-matnr,
  s_bklas for ztbw_pmei-bklas,
  s_reasn for ztbw_pmei-reason,
  s_vvata for ztbw_pmei-vvatage,
  s_mstae for ztbw_pmei-mstae,
  s_mmsta for ztbw_pmei-mmsta,
  s_salk3 for ztbw_pmei-salk3,
  s_menm1 for ztbw_pmei-ZZBDMNG1,
  s_menm2 for ztbw_pmei-ZZBDMNG2,
  s_menm3 for ztbw_pmei-ZZBDMNG3,
  s_salm1 for ztbw_pmei-zzsalk1,
  s_salm2 for ztbw_pmei-zzsalk2,
  s_salm3 for ztbw_pmei-zzsalk3.
selection-screen end of block b2.


* Refer: BCALV_EDIT_02.
* - prepare STRUCTURE (se11)
* - copy screen, pf-status

*-----------------------------------------------------------------
* Essential steps (search for '§')
* ~~~~~~~~~~~~~~~
* 1.Extend your output table for a field, e.g., CELLTAB, that holds
*   information about the edit status of each cell for the
*   corresponding row (the table type is SORTED!).
* 2.After selecting data, set edit status for each row in a loop
*   according to field SEATSMAX.
* 2a.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_ENABLED to set a cell
*    to status "editable".
* 2b.Use attribute CL_GUI_ALV_GRID=>MC_STYLE_DISABLED to set a cell
*    to status "non-editable".
* 2c.Copy your celltab to the celltab of the current row of gt_outtab.
* 3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
*&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

data: ok_code like sy-ucomm,
      save_ok like sy-ucomm,
      g_container type scrfname value 'BCALV_GRID_DEMO_0100_CONT1',
      grid1  type ref to cl_gui_alv_grid,
      g_custom_container type ref to cl_gui_custom_container,
      gs_layout type lvc_s_layo.


*§1.Extend your output table for a field, e.g., CELLTAB, that holds
*   information about the edit status of each cell for the
*   corresponding row (the table type is SORTED!).
data: begin of gt_outtab occurs 0.  "with header line
        include structure zsem_excinv.
data: celltab type lvc_t_styl.
data: end of gt_outtab.

data: gt_row   type lvc_t_row,
      gs_row   type lvc_s_row,
      gt_roid  type lvc_t_roid.

data: gv_level(5),                       " Authorization Level
      gv_past(1).

get parameter id 'ZCOLV1' field gv_level.
clear gv_past.

perform check_period.
perform select_data.


*---------------------------------------------------------------------*
*       MAIN                                                          *
*---------------------------------------------------------------------*
call screen 100.

*---------------------------------------------------------------------*
*       MODULE PBO OUTPUT                                             *
*---------------------------------------------------------------------*
module pbo output.
  set pf-status 'MAIN100'.
*  SET TITLEBAR 'MAIN100'.
  if g_custom_container is initial.
    create object g_custom_container
           exporting container_name = g_container.
    create object grid1
           exporting i_parent = g_custom_container.
    perform init_style.

*§3.Provide the fieldname of the celltab field by using field
*   STYLEFNAME of the layout structure.
    gs_layout-stylefname = 'CELLTAB'.

* set substate of editable cells to deactivated
    call method grid1->set_ready_for_input
          exporting i_ready_for_input = 0.

    call method grid1->set_table_for_first_display
         exporting i_structure_name = 'ZSEM_EXCINV'
                   is_layout        = gs_layout
         changing  it_outtab        = gt_outtab[].

  endif.
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
      if not grid1->is_ready_for_input( ) eq 0.
        perform data_save.
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

  ls_celltab-fieldname = 'REASON'.
  ls_celltab-style = l_mode1.
  insert ls_celltab into table pt_celltab.

  if gt_outtab-vvatage = 9999.
    ls_celltab-fieldname = 'VVATAGE'.
    ls_celltab-style = l_mode1.
    insert ls_celltab into table pt_celltab.
  endif.
endform.                               " FILL_CELLTAB
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
form switch_edit_mode.

  if grid1->is_ready_for_input( ) eq 0.
* set edit enabled cells ready for input
    call method grid1->set_ready_for_input
                     exporting i_ready_for_input = 1.

  else.
* lock edit enabled cells against input
    call method grid1->set_ready_for_input
                    exporting i_ready_for_input = 0.
  endif.
endform.                               " SWITCH_EDIT_MODE


*CUST
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
form select_data.
  data: lt_data type table of zsem_excinv with header line.

  select * into corresponding fields of table lt_data
    from ztbw_pmei as a
         inner join mara as b
            on b~matnr = a~matnr
         inner join makt as c
            on c~matnr = a~matnr
    where a~bukrs = p_bukrs
      and a~gjahr = p_gjahr
      and a~monat = p_monat
      and a~matnr   in s_matnr
      and a~reason  in s_reasn
      and a~bklas   in s_bklas
      and a~vvatage in s_vvata
      and a~mstae   in s_mstae
      and a~mmsta   in s_mmsta
      and a~salk3   in s_salk3
      and a~zzsalk1 in s_salm1
      and a~zzsalk2 in s_salm2
      and a~zzsalk3 in s_salm3
      and a~ZZBDMNG1 in s_menm1
      and a~ZZBDMNG2 in s_menm2
      and a~ZZBDMNG3 in s_menm3

      and c~spras = sy-langu.

  loop at lt_data.
    move-corresponding lt_data to gt_outtab.
    append gt_outtab.
  endloop.

endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  data_save
*&---------------------------------------------------------------------*
form data_save.
  data: ls_ztbw_pmei like ztbw_pmei,
        lt_ztbw_pmei type table of ztbw_pmei with header line.

  call method grid1->get_selected_rows
              importing et_index_rows = gt_row
                        et_row_no = gt_roid.

  loop at gt_row into gs_row.
    read table gt_outtab index gs_row-index.
    if sy-subrc = 0.
      move-corresponding gt_outtab to lt_ztbw_pmei.

      lt_ztbw_pmei-aedat = sy-datum.
      lt_ztbw_pmei-aenam = sy-uname.

      lt_ztbw_pmei-mandt = sy-mandt.
      lt_ztbw_pmei-bukrs = p_bukrs.
      lt_ztbw_pmei-gjahr = p_gjahr.
      lt_ztbw_pmei-monat = p_monat.

      append lt_ztbw_pmei.
    endif.
  endloop.

  loop at lt_ztbw_pmei into ls_ztbw_pmei.
    update ztbw_pmei from ls_ztbw_pmei.
  endloop.

  message s000 with 'Data saved'.
endform.                    " data_save
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
form check_period.
  data: l_yymm1(6) type c,
        l_yymm2(6) type c.

  select single * from marv
    where bukrs = p_bukrs.

  concatenate marv-vmgja marv-vmmon  into l_yymm1.
  concatenate p_gjahr    p_monat     into l_yymm2.

  if l_yymm1 > l_yymm2.
    gv_past = 'X'.
  endif.

endform.                    " check_period
