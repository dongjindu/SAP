************************************************************************
* Program Name      : ZRCO_AALA_REPORT
* Author            : Byung Sung Bae
* Creation Date     : 2005.01.10.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K913724
* Addl Documentation:
* Description       : AALA Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/13/2007 Manju        UD1K940054   Program corrections
*
************************************************************************
report zrco_aala_report .
tables: t001,
        ztco_aala_model, ztco_aala_creation_9000.

data: zsco_aala_report_9001 like zsco_aala_report_9001,
      zsco_aala_report_9002 like zsco_aala_report_9002,
      zsco_aala_report_9003 like zsco_aala_report_9003,
      zsco_aala_report_9004 like zsco_aala_report_9004.

*---// Internal tables
data: it_parts  like ztco_aala_parts  occurs 0 with header line.
data: it_mip    like ztco_aala_mip    occurs 0 with header line.
data: it_fsc    like ztco_aala_fsc    occurs 0 with header line.
data: it_model  like ztco_aala_model  occurs 0 with header line.

data: it_9001 type standard table of zsco_aala_report_9001
                                     with header line,
      it_9002 type standard table of zsco_aala_report_9002
                                     with header line,
      it_9003 type standard table of zsco_aala_report_9003
                                     with header line,
      it_9004 type standard table of zsco_aala_report_9004
                                     with header line.

data: it_rows          type lvc_t_row with header line.

*---// Work areas
data: w_fsc(10),
      w_row_no        type lvc_t_roid. "/Numeric IDs of Selected rows

*---// Constants
constants: c_check                             value 'X',
           c_bukrs     like t001-bukrs         value 'H201'.

*---// Tabstrip
* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
constants: begin of c_tab_9000,
             tab1 like sy-ucomm value 'TAB_9000_FC1',
             tab2 like sy-ucomm value 'TAB_9000_FC2',
             tab3 like sy-ucomm value 'TAB_9000_FC3',
             tab4 like sy-ucomm value 'TAB_9000_FC4',
           end of c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
controls:  tab_9000 type tabstrip.
data:      begin of g_tab_9000,
             subscreen   like sy-dynnr,
             prog        like sy-repid value 'ZRCO_AALA_REPORT',
             pressed_tab like sy-ucomm value c_tab_9000-tab1,
           end of g_tab_9000.
data:      ok_code like sy-ucomm.


*-----/// ALV Control : START
* Control Framework Basic Class
class cl_gui_cfw      definition load.

* Declare reference variables, the container and internal table
data: wc_control_9001   type        scrfname value 'CC_9001_ALV',
      wc_alv_9001       type ref to cl_gui_alv_grid,
      wc_container_9001 type ref to cl_gui_custom_container.

data: wc_control_9002   type        scrfname value 'CC_9002_ALV',
      wc_alv_9002       type ref to cl_gui_alv_grid,
      wc_container_9002 type ref to cl_gui_custom_container.

data: wc_control_9003   type        scrfname value 'CC_9003_ALV',
      wc_alv_9003       type ref to cl_gui_alv_grid,
      wc_container_9003 type ref to cl_gui_custom_container.

data: wc_control_9004   type        scrfname value 'CC_9004_ALV',
      wc_alv_9004       type ref to cl_gui_alv_grid,
      wc_container_9004 type ref to cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class lcl_event_receiver definition deferred. "/ALV Event Handling

data : event_receiver type ref to lcl_event_receiver.

* Interal tables for ALV GRID
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line.

* Global variable for ALV GRID
data : w_is_layout type lvc_s_layo,
       w_variant   type disvariant,          "for parameter IS_VARIANT
       w_fieldname like line of it_fieldcat,
       w_repid     like sy-repid,
       w_cnt       type i,                   "Field count
       w_save      type c   value 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

data: w_container(100),
      w_control(100),
      w_alv(100),
      w_itab(100),
      w_structure like dd02l-tabname.

field-symbols: <container> type ref to   cl_gui_custom_container,
               <control>   type          scrfname,
               <alv>       type ref to   cl_gui_alv_grid,
               <itab>      type standard table.

constants: c_structure(100) value 'ZSCO_AALA_REPORT_'.

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
class lcl_event_receiver definition.
  public section.
    methods:

    handle_double_click
        for event double_click of cl_gui_alv_grid
            importing e_row
                      e_column
                      es_row_no.
endclass.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
class lcl_event_receiver implementation.
  method handle_double_click.
    perform dbl_click_9000 using e_column-fieldname
                                 es_row_no-row_id.

  endmethod.                           "handle_double_click
endclass.

*---// Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters: p_kokrs   like   tka01-kokrs modif id gr1,
            p_gjahr   like   sy-datum(4) obligatory default sy-datum(4).
select-options: s_model for ztco_aala_model-model.
selection-screen end of block bl1.

initialization.
  perform initialization.

at selection-screen output.
  perform screen_modify.

at selection-screen.
  check sy-ucomm eq 'ONLI'.
  perform get_data.
  perform set_screen_data using '9001'.

start-of-selection.
  call screen 9000.


*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
form dbl_click_9000 using    p_e_column_fieldname
                             p_es_row_no_row_id.

endform.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status output.
  case sy-dynnr.
    when 9000.
      set pf-status '9000'.
      set titlebar  '9000'.
  endcase.
endmodule.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.
  case sy-ucomm.
    when 'EXIT' or 'CANC'.
      clear: sy-ucomm.
      leave to screen 0.
  endcase.
endmodule.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_9000 input.
  case ok_code.
    when 'BACK'.
      clear sy-ucomm.
      leave to screen 0.
*    WHEN c_tab_9000-tab1.
*      CLEAR sy-ucomm.
*      PERFORM reset_subscreen USING '9001'.
*    WHEN c_tab_9000-tab2.
*      CLEAR sy-ucomm.
*      PERFORM reset_subscreen USING '9002'.
*    WHEN c_tab_9000-tab3.
*      CLEAR sy-ucomm.
*      PERFORM reset_subscreen USING '9003'.
*    WHEN c_tab_9000-tab4.
*      CLEAR sy-ucomm.
*      PERFORM reset_subscreen USING '9004'.
  endcase.
endmodule.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object using p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  concatenate: 'WC_CONTAINER_' p_dynnr into w_container,
               'WC_CONTROL_'   p_dynnr into w_control,
               'WC_ALV_'       p_dynnr into w_alv.

  assign: (w_container) to <container>,
          (w_control)   to <control>,
          (w_alv)       to <alv>.

  create object <container>
         exporting container_name = <control>
         exceptions
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object <alv>
         exporting i_parent      = <container>
                   i_appl_events = 'X'.
endform.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid using p_dynnr.
  case p_dynnr.
    when '9001'.
      perform set_attributes_alv_9001.
    when '9002'.
      perform set_attributes_alv_9002.
    when '9003'.
      perform set_attributes_alv_9003.
    when '9004'.
      perform set_attributes_alv_9004.
  endcase.
endform.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog using p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  perform set_fieldname using p_dynnr.
  perform set_screen_fields using p_dynnr.
endform.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9001.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_fieldname using p_dynnr.
  data: lw_itab type slis_tabname.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  move: sy-repid to w_repid.
  concatenate c_structure p_dynnr into lw_itab.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       changing
            ct_fieldcat        = it_fieldname.
endform.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0573   text
*      -->P_0574   text
*      -->P_0575   text
*----------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat structure it_fieldcat
                      using    p_gubun
                               p_field
                               p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_fieldcat.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check filed catalog'.
    endif.

    move: w_fieldname-fieldname to p_fieldcat-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_FIELDCAT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    if p_fieldcat-col_pos is initial.
      add 1 to w_cnt.
      p_fieldcat-col_pos = w_cnt.
    endif.
    append p_fieldcat.
  endif.
endform.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv using p_dynnr.
  data: lw_dynnr   like   sy-dynnr.

  concatenate: 'WC_ALV_'    p_dynnr      into w_alv,
               c_structure  p_dynnr      into w_structure,
               'IT_'        p_dynnr '[]' into w_itab.

  assign: (w_alv)       to <alv>,
          (w_itab)      to <itab>.

  call method <alv>->set_table_for_first_display
     exporting i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     changing  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
endform.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sssign_event.

endform.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Module  read_data_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module read_data_9000 input.
  perform read_aala_data.
  perform set_screen_data using '9001'.
  perform assign_itab_to_alv using '9001'.
endmodule.                 " read_data_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_initial_value output.
*  DATA: lw_dynnr LIKE sy-dynnr.

  check ztco_aala_creation_9000-kokrs is initial.

*----- Set Controlling area
  call function 'K_KOKRS_SET'
       importing
            e_kokrs   = ztco_aala_creation_9000-kokrs
       exceptions
            not_found = 1
            others    = 2.
  if sy-subrc <> 0.
    if sy-msgty = 'E' or sy-msgty = 'A' or sy-msgty = 'X'.
      message id sy-msgid type 'S' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      leave program.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

*---// Get company infomation
  select single * from t001 where bukrs = c_bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.

  do 4 times.
**    lw_dynnr = sy-dynnr + sy-index.
*    PERFORM create_alv_object USING lw_dynnr.
  enddo.
endmodule.                 " set_initial_value  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9002.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9002
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9003.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9003
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9004.
  clear : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9004
*&---------------------------------------------------------------------*
*&      Form  READ_AALA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_aala_data.

  if p_gjahr > '2008'.
    concatenate '_____' ztco_aala_creation_9000-model '%' into w_fsc.
  else.
    concatenate '______' ztco_aala_creation_9000-model '%' into w_fsc.
  endif.
  select *
    into corresponding fields of table it_model
    from ztco_aala_model
   where kokrs = ztco_aala_creation_9000-kokrs
     and model = ztco_aala_creation_9000-model
     and gjahr = ztco_aala_creation_9000-gjahr
     and versn = ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.

  read table it_model index 1.
  move it_model to ztco_aala_model.

  select *
    into corresponding fields of table it_fsc
    from ztco_aala_fsc
   where kokrs eq   ztco_aala_creation_9000-kokrs
     and matnr like w_fsc
     and gjahr eq   ztco_aala_creation_9000-gjahr
     and versn eq   ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message e000(zz) with text-m03.
  endif.

  select *
    into corresponding fields of table it_mip
    from ztco_aala_mip
   where kokrs eq   ztco_aala_creation_9000-kokrs
     and matnr like w_fsc
     and gjahr eq   ztco_aala_creation_9000-gjahr
     and versn eq   ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message e000(zz) with text-m04.
  endif.

  select *
    into corresponding fields of table it_parts
    from ztco_aala_parts
   where kokrs eq   ztco_aala_creation_9000-kokrs
     and matnr like w_fsc
     and gjahr eq   ztco_aala_creation_9000-gjahr
     and versn eq   ztco_aala_creation_9000-versn.
  if sy-subrc ne 0.
    message e000(zz) with text-m05.
  endif.

  move: c_tab_9000-tab1 to sy-ucomm.
endform.                    " READ_AALA_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_data using p_dynnr.
  case p_dynnr.
    when 9001.
      perform set_it_9001.
    when 9002.
      perform set_it_9002.
    when 9003.
      perform set_it_9003.
    when 9004.
      perform set_it_9004.
    when others.
*      DO NOTHING
  endcase.
endform.                    " SET_SCREEN_DATA

* OUTPUT MODULE FOR TABSTRIP 'TAB_9000': SETS ACTIVE TAB
module tab_9000_active_tab_set output.
  tab_9000-activetab = g_tab_9000-pressed_tab.
  case g_tab_9000-pressed_tab.
    when c_tab_9000-tab1.
      g_tab_9000-subscreen = '9001'.
    when c_tab_9000-tab2.
      g_tab_9000-subscreen = '9002'.
    when c_tab_9000-tab3.
      g_tab_9000-subscreen = '9003'.
    when c_tab_9000-tab4.
      g_tab_9000-subscreen = '9004'.
    when others.
*      DO NOTHING
  endcase.
endmodule.

* INPUT MODULE FOR TABSTRIP 'TAB_9000': GETS ACTIVE TAB
module tab_9000_active_tab_get input.
  ok_code = sy-ucomm.
  case ok_code.
    when c_tab_9000-tab1.
      perform reset_subscreen using '9001'.

      g_tab_9000-pressed_tab = c_tab_9000-tab1.
    when c_tab_9000-tab2.
      perform reset_subscreen using '9002'.

      g_tab_9000-pressed_tab = c_tab_9000-tab2.
    when c_tab_9000-tab3.
      perform reset_subscreen using '9003'.

      g_tab_9000-pressed_tab = c_tab_9000-tab3.
    when c_tab_9000-tab4.
      perform reset_subscreen using '9004'.

      g_tab_9000-pressed_tab = c_tab_9000-tab4.
    when others.
*      DO NOTHING
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Form  get_selected_row
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_selected_row using p_dynnr.
  data: lw_parent_scrn   like sy-dynnr.

  check p_dynnr ne '9001'.

  lw_parent_scrn = p_dynnr - 1.

  concatenate: 'WC_CONTAINER_' lw_parent_scrn into w_container.
  assign:      (w_container)                    to   <container>.

  if <container> is initial.          "/Not Created Control for ALV GRID
    message s000(zz) with text-m06.
    leave to screen sy-dynnr.
  endif.



  concatenate: 'WC_ALV_' lw_parent_scrn into w_alv.
  assign:      (w_alv)                  to   <alv>.

  clear: it_rows, it_rows[].
  call method <alv>->get_selected_rows
           importing et_index_rows = it_rows[]
                     et_row_no     = w_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.

  read table it_rows index 1.
  if sy-subrc ne 0 or it_rows-index eq 0.
    message s000(zz) with text-m06.
    leave to screen sy-dynnr.
  endif.
endform.                    " get_selected_row
*&---------------------------------------------------------------------*
*&      Form  set_it_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_9002.
  refresh: it_9002.
  loop at it_rows.
    read table it_model index it_rows-index.
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.

    if p_gjahr > '2008'.
      loop at it_fsc where kokrs      = it_model-kokrs
                       and matnr+5(2) = it_model-model
                       and gjahr      = it_model-gjahr
                       and versn      = it_model-versn.
        clear: it_9002.
        move-corresponding it_fsc to it_9002.
        append it_9002.
      endloop.
    else.
      loop at it_fsc where kokrs      = it_model-kokrs
                       and matnr+6(2) = it_model-model
                       and gjahr      = it_model-gjahr
                       and versn      = it_model-versn.
        clear: it_9002.
        move-corresponding it_fsc to it_9002.
        append it_9002.
      endloop.
    endif.

  endloop.

  perform create_alv_object using '9002'.
endform.                    " set_it_9002
*&---------------------------------------------------------------------*
*&      Form  set_it_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_9003.
  refresh: it_9003.
  loop at it_rows.
    read table it_9002 index it_rows-index.
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.

    loop at it_mip where kokrs = p_kokrs
                     and matnr = it_9002-matnr
                     and gjahr = p_gjahr
                     and versn = it_9002-versn.
      clear: it_9003.

      move-corresponding it_mip to it_9003.
      append it_9003.
    endloop.
  endloop.

  perform create_alv_object using '9003'.
endform.                    " set_it_9003
*&---------------------------------------------------------------------*
*&      Form  set_it_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_9004.
  refresh: it_9004.
  loop at it_rows.
    read table it_9003 index it_rows-index.
    if sy-subrc ne 0.
      message e000(zz) with text-m01.
    endif.
*    IF it_9003-shop+2(1) EQ 'E'.    " UD1K940054
    loop at it_parts where kokrs = p_kokrs
                       and matnr = it_9003-matnr
                       and gjahr = p_gjahr
                       and versn = it_9003-versn
                       and shop  = it_9003-shop
                       and mipcd = it_9003-mipcd.
      clear: it_9004.

      move-corresponding it_parts to it_9004.

      it_9004-amont = ( it_parts-stprs - it_parts-dduty ) *
                        it_parts-menge.
      append it_9004.
    endloop.
* Begin of changes - UD1K940054
*    ELSE.
*      LOOP AT it_parts WHERE kokrs = p_kokrs
*                         AND matnr = it_9003-matnr
*                         AND gjahr = p_gjahr
*                         AND versn = it_9003-versn
*                         AND shop  = it_9003-shop.
*        CLEAR: it_9004.
*
*        MOVE-CORRESPONDING it_parts TO it_9004.
*
*        it_9004-amont = ( it_parts-stprs - it_parts-dduty ) *
*                          it_parts-menge.
*        APPEND it_9004.
*      ENDLOOP.
*    ENDIF.
* End of changes - UD1K940054
  endloop.

  perform create_alv_object using '9004'.
endform.                    " set_it_9004
*&---------------------------------------------------------------------*
*&      Form  set_it_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_9001.
  refresh it_9001.
  loop at it_model.
    clear: it_9001.
    move-corresponding it_model to it_9001.
    append it_9001.
  endloop.

  perform create_alv_object using '9001'.
endform.                    " set_it_9001
*&---------------------------------------------------------------------*
*&      Form  reset_subscreen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reset_subscreen using p_dynnr.
  perform get_selected_row using p_dynnr.
  perform set_screen_data using p_dynnr.
endform.                    " reset_subscreen
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_DYNNR  text
*----------------------------------------------------------------------*
form create_alv_object using p_dynnr.
  concatenate: 'WC_CONTAINER_' p_dynnr into w_container.
  assign:      (w_container)           to   <container>.

  if <container> is initial.          "/Not Created Control for ALV GRID
    perform create_container_n_object using p_dynnr.
    perform set_attributes_alv_grid using p_dynnr.
    perform build_field_catalog using p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    perform assign_itab_to_alv using p_dynnr.
    perform sssign_event.
  else.
    perform set_attributes_alv_grid using p_dynnr.
    perform build_field_catalog using p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    perform assign_itab_to_alv using p_dynnr.
  endif.
endform.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
form set_screen_fields using p_dynnr.
  case p_dynnr.
    when '9001'.
      perform set_screen_fields_9001.
    when '9002'.
      perform set_screen_fields_9002.
    when '9003'.
      perform set_screen_fields_9003.
    when '9004'.
      perform set_screen_fields_9004.
  endcase.
endform.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9001.
  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MODEL'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'KEY'         'X'.
endform.                    " set_screen_fields_9001
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9002.
  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'KEY'         'X'.
endform.                    " set_screen_fields_9002
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9003.
  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'SHOP'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MIPCD'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'KEY'         'X'.
endform.                    " set_screen_fields_9003
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9004.
  perform setting_fieldcat tables it_fieldcat using :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VERSN'       ' ',
                                  'E' 'KEY'         'X'.
endform.                    " set_screen_fields_9004
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialization.
*----- Set Controlling area
  call function 'K_KOKRS_SET'
       importing
            e_kokrs   = p_kokrs
       exceptions
            not_found = 1
            others    = 2.
  if sy-subrc <> 0.
    if sy-msgty = 'E' or sy-msgty = 'A' or sy-msgty = 'X'.
      message id sy-msgid type 'S' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      leave program.
    else.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.

*---// Get company infomation
  select single * from t001 where bukrs = c_bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.
endform.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form screen_modify.
  loop at screen.
    if screen-group1 = 'GR1'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.
endform.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  select *
    into corresponding fields of table it_model
    from ztco_aala_model
   where kokrs eq p_kokrs
     and model in s_model
     and gjahr eq p_gjahr.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.

  loop at it_model.

    if p_gjahr > '2008'.
      concatenate '_____' it_model-model '%' into w_fsc.
    else.
      concatenate '______' it_model-model '%' into w_fsc.
    endif.

    select *
      appending corresponding fields of table it_fsc
      from ztco_aala_fsc
     where kokrs eq   p_kokrs
       and matnr like w_fsc
       and gjahr eq   p_gjahr
       and versn eq   it_model-versn.
  endloop.

  read table it_fsc index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m03.
  endif.

  select *
    into corresponding fields of table it_mip
    from ztco_aala_mip
     for all entries in it_fsc
   where kokrs eq   p_kokrs
     and matnr eq   it_fsc-matnr
     and gjahr eq   p_gjahr.
  if sy-subrc ne 0.
    message e000(zz) with text-m04.
  endif.

  select *
    into corresponding fields of table it_parts
    from ztco_aala_parts
     for all entries in it_fsc
   where kokrs eq   p_kokrs
     and matnr eq   it_fsc-matnr
     and gjahr eq   p_gjahr.
  if sy-subrc ne 0.
    message e000(zz) with text-m05.
  endif.
endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alv_object output.
  perform create_alv_object using sy-dynnr.
endmodule.                 " create_alv_object  OUTPUT
