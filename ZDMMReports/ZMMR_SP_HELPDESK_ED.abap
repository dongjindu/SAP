************************************************************************
* Program Name      : ZDMM_SP_HELPDESK_ED
* Creation Date     : 01/2012
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zdmm_sp_helpdesk_ed NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: lfa1, ztmm_sp_help_it.

*DATA: it_itab LIKE table of ztmm_sp_help_it WITH HEADER LINE.

DATA: BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_sp_help_it.
DATA  celltab TYPE lvc_t_styl.
DATA  END OF  it_itab.

DATA: fcode TYPE TABLE OF sy-ucomm.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt TYPE i.

DATA:  wa_stbl  TYPE lvc_s_stbl.
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_exclude      TYPE ui_functions.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname  LIKE LINE OF it_fieldname. "it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS : <fs01>, <fs02>, <fs03>.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

*DATA: w_frm_lab(1),
*      w_frm_bin(1),
*      w_barc_kb(1),
*      w_barc_hd(1),
*      w_barc_no(1),
*      w_icon_stl(1),
*      w_icon_hd(1),
*      w_icon_no(1).

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    DATA: error_in_data TYPE c.

ENDCLASS.                    "lcl_event_receiver DEFINITION
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          w_qty(13),
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
* check if column Name1 of this row was changed
        WHEN 'ITEM_CALLTYPE' OR 'ITEM_PRIORITY' OR
              'ITEM_SEVERITY' OR 'ITEM_STATUS' OR
             'ITEM_REQUESTOR_E' OR 'ITEM_REQUESTOR_P' OR
             'ITEM_SUMMARY'  OR 'ITEM_DEPT'.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.

      ENDCASE.
    ENDLOOP.
*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS: p_lifnr LIKE lfa1-lifnr OBLIGATORY.
SELECT-OPTIONS: s_reqtor FOR ztmm_sp_help_it-item_requestor,
            s_type FOR ztmm_sp_help_it-item_calltype,
            s_status FOR ztmm_sp_help_it-item_status,
            s_crdate FOR ztmm_sp_help_it-item_created_d.

SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
PARAMETERS: p_mode(1).
SELECTION-SCREEN END OF BLOCK block2.

INITIALIZATION.

  IF sy-tcode+0(9) = 'ZMMR022_E'.
    p_mode = 'E'.
  ELSE.
    p_mode = 'D'.
  ENDIF.
  p_lifnr = sy-uname.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'P_MODE'.
      screen-input = '0'.
*            SCREEN-INVISIBLE  = '1'.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_LIFNR' AND
       sy-tcode+9(1) = 'S'.
      screen-input = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM get_data.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    CALL SCREEN 200.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
  DATA: lt_celltab TYPE lvc_t_styl,
        w_celltab TYPE lvc_s_styl,
        l_index TYPE i,
        l_mode TYPE raw4.
  REFRESH: lt_celltab.
  LOOP AT it_itab.
    l_index = sy-tabix.
    REFRESH lt_celltab.

    w_celltab-fieldname = 'ITEM_SUPPLIER'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'ITEM_REQUESTOR'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    l_mode = cl_gui_alv_grid=>mc_style_enabled.
    w_celltab-fieldname = 'ITEM_CALLTYPE'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'ITEM_PRIORITY'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'ITEM_SEVERITY'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'ITEM_DEPT'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'ITEM_SUMMARY'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    IF sy-tcode = 'ZMMR022_EDIT'.
      w_celltab-fieldname = 'ITEM_STATUS'.
      w_celltab-style = cl_gui_alv_grid=>mc_style_enabled..
      INSERT w_celltab INTO TABLE lt_celltab.

    ELSE.
      w_celltab-fieldname = 'ITEM_STATUS'.
      w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT w_celltab INTO TABLE lt_celltab.
    ENDIF.


    INSERT LINES OF lt_celltab INTO TABLE it_itab-celltab.
    MODIFY it_itab INDEX l_index.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  IF p_mode = 'D'.
    REFRESH: fcode.
    APPEND 'SAVE'  TO fcode.
    APPEND 'DELETE'  TO fcode.
    SET PF-STATUS 'ST200'  EXCLUDING fcode.
    SET TITLEBAR 'ST200'.
  ELSE.
    SET PF-STATUS 'ST200'.
    SET TITLEBAR 'ST200'.
  ENDIF.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  REFRESH: it_itab.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
     FROM ztmm_sp_help_it
     WHERE item_supplier = p_lifnr
       AND item_requestor IN s_reqtor
        AND item_calltype IN s_type
        AND item_status IN s_status
        AND item_created_d IN s_crdate.


ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM select_edit_line.
    PERFORM exclude_tb_functions.
    PERFORM build_field_catalog USING 'IT_ITAB'.
*     PERFORM dropdown_table.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
*     PERFORM BUILD_FIELD_CATALOG USING 'IT_TAB_DAY'.
*    PERFORM ASSIGN_ITAB_TO_ALV.
*
    wa_stbl-row = 'X'.
    wa_stbl-col = 'X'.
    CALL METHOD alv_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stbl.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  IF p_mode = 'D'.
    wa_is_layout-edit       =  ' '.
  ELSE.
    wa_is_layout-edit       = 'X'.      "/Edit Mode Enable
  ENDIF.
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = ' '.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-stylefname = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.
*
*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).
  FIELD-SYMBOLS <fs>.

  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.
    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.
    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
*               i_default        = space
               it_toolbar_excluding = it_exclude
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_itab[].
*               it_sort          = it_sort[].

** ENTER
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT g_event_receiver.
  SET HANDLER g_event_receiver->handle_data_changed FOR alv_grid.


  CALL METHOD cl_gui_control=>set_focus
    EXPORTING
      control = alv_grid.


ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
  DATA ls_exclude TYPE ui_func.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.
  IF p_mode = 'D'.
    ls_exclude = 'SAVE'.
    APPEND ls_exclude TO it_exclude.
  ENDIF.
*  Sort buttons
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8).
  DATA ls_fcat TYPE lvc_s_fcat.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.


  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'ITEM_SUPPLIER'  ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Supplier',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'ITEM_REQUESTOR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Requestor',
                                   'E' 'OUTPUTLEN'   '20',

                                  'S' 'ITEM_CALLTYPE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Type',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'ITEM_PRIORITY'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Priority',
                                   'E' 'OUTPUTLEN'   '15',

                                  'S' 'ITEM_SEVERITY'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Severity',
                                   'E' 'OUTPUTLEN'   '20',

                                  'S' 'ITEM_STATUS'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Status',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'ITEM_CREATED_D'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Created Date',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'ITEM_DEPT'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Job Function',
                                   'E' 'OUTPUTLEN'   '10',

                                  'S' 'ITEM_SUMMARY'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Remarks',
                                   'E' 'OUTPUTLEN'   '100',

                                  'S' 'ITEM_SUMMARY1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Remarks',
                                   'E' 'OUTPUTLEN'   '100',

                                  'S' 'ITEM_REQUESTOR_E'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Email',
                                   'E' 'OUTPUTLEN'   '80'.


** To assign dropdown in the fieldcataogue
*  LOOP AT it_fieldcat INTO ls_fcat.
*    CASE ls_fcat-fieldname.
*      WHEN 'ITEM_CALLTYPE'.
**drdn-hndl = '1' is the first list box
*        ls_fcat-drdn_hndl = '1'.
*        ls_fcat-outputlen = 15.
*        MODIFY it_fieldcat FROM ls_fcat.
**drdn-hndl = '2' is the second list box
*      WHEN 'ITEM_PRIORITY'.
*        ls_fcat-drdn_hndl = '2'.
*        ls_fcat-outputlen = 15.
*        MODIFY it_fieldcat FROM ls_fcat.
*    ENDCASE.
*  ENDLOOP.
  LOOP AT it_fieldcat.
    IF it_fieldcat-fieldname = 'ITEM_CALLTYPE'.
      it_fieldcat-f4availabl = 'X'.
      it_fieldcat-ref_field = 'ITEM_CALLTYPE'.
      it_fieldcat-ref_table = 'ZTMM_SP_HELP_TYP'.
      MODIFY it_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
                                          ref_field ref_table.
    ENDIF.
    IF it_fieldcat-fieldname = 'ITEM_PRIORITY'.
      it_fieldcat-f4availabl = 'X'.
      it_fieldcat-ref_field = 'ITEM_PRIORITY'.
      it_fieldcat-ref_table = 'ZTMM_SP_HELP_PRI'.
      MODIFY it_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
                                          ref_field ref_table.
    ENDIF.
    IF it_fieldcat-fieldname = 'ITEM_SEVERITY'.
      it_fieldcat-f4availabl = 'X'.
      it_fieldcat-ref_field = 'ITEM_SEVERITY'.
      it_fieldcat-ref_table = 'ZTMM_SP_HELP_SEV'.
      MODIFY it_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
                                          ref_field ref_table.
    ENDIF.
    IF it_fieldcat-fieldname = 'ITEM_STATUS'.
      it_fieldcat-f4availabl = 'X'.
      it_fieldcat-ref_field = 'ITEM_STATUS'.
      it_fieldcat-ref_table = 'ZTMM_SP_HELP_ST'.
      MODIFY it_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
                                          ref_field ref_table.
    ENDIF.

    IF it_fieldcat-fieldname = 'ITEM_DEPT'.
      it_fieldcat-f4availabl = 'X'.
      it_fieldcat-ref_field = 'DEPT'.
      it_fieldcat-ref_table = 'ZTMM_SP_HELP_DEP'.
      MODIFY it_fieldcat INDEX sy-tabix TRANSPORTING f4availabl
                                          ref_field ref_table.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save_data.
      PERFORM refresh_data.
    WHEN 'DELETE'.
      PERFORM delete_data.
      PERFORM refresh_data.
*    WHEN 'COPY'.
*      PERFORM copy_data.
    WHEN 'REFRESH'.
      PERFORM refresh_data.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  DATA: lt_temp LIKE TABLE OF ztmm_sp_help_it WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

*  READ TABLE lt_rows INDEX 1.
  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_itab TO lt_temp.
      lt_temp-item_modified_d = sy-datum.
      lt_temp-item_modified_t = sy-uzeit.
*      lt_bdp-aenam = sy-uname.
      APPEND lt_temp.
    ENDIF.
    CLEAR: lt_temp.
  ENDLOOP.
  IF  lt_temp[] IS INITIAL.
    MESSAGE i000 WITH 'No Data was selected'.
  ELSE.
    UPDATE ztmm_sp_help_it FROM TABLE lt_temp.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Data was updated successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Data update error'.
    ENDIF.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.
  DATA: lt_temp LIKE TABLE OF ztmm_sp_help_it WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  LOOP AT lt_rows.
    READ TABLE it_itab INDEX lt_rows-index.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_itab TO lt_temp.
*      lt_bdp-aedat = sy-datum.
*      lt_bdp-aezet = sy-uzeit.
*      lt_bdp-aenam = sy-uname.
      APPEND lt_temp.
    ENDIF.
    CLEAR: lt_temp.
  ENDLOOP.

  IF  lt_temp[] IS INITIAL.
    MESSAGE i000 WITH 'No Data was selected'.
  ELSE.
    DELETE ztmm_sp_help_it FROM TABLE lt_temp.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Data was deleted successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Data deletion error'.
    ENDIF.
  ENDIF.
  PERFORM refresh_data.
ENDFORM.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  refresh_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_data.
  PERFORM get_data.
  wa_stbl-row = 'X'.
  wa_stbl-col = 'X'.
  PERFORM select_edit_line.
*  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*    EXPORTING IS_STABLE = WA_STBL.

ENDFORM.                    " refresh_data
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_data.
* DATA: LT_BDP LIKE TABLE OF ZTMM_BDP WITH HEADER LINE.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  READ TABLE it_itab INDEX lt_rows-index.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING it_itab TO ztmm_sp_help_it.
*    WA_BDP-AEDAT = SY-DATUM.
*    WA_BDP-AEZET = SY-UZEIT.
*    WA_BDP-AENAM = SY-UNAME.

    CALL SCREEN '0210'.
    PERFORM refresh_data.
  ELSE.
    MESSAGE i000 WITH 'No Data was selected'.
  ENDIF.
*    INSERT ZTMM_BDP.
*    IF SY-SUBRC = 0.
*      COMMIT WORK.
*      MESSAGE S000 WITH 'Data was deleted successfully'.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE I000 WITH 'Data deletion error'.
*    ENDIF.

ENDFORM.                    " COPY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0210  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0210 OUTPUT.
  SET PF-STATUS 'ST0210'.
  SET TITLEBAR 'ST0210'.

ENDMODULE.                 " STATUS_0210  OUTPUT
**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0210  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE user_command_0210 INPUT.
*  ok_code = sy-ucomm.
*  CASE ok_code.
*    WHEN 'SAVE'.
*      PERFORM insert_data.
*      LEAVE TO SCREEN 0.
*    WHEN 'BACK' OR 'EXIT'.
*      CLEAR ok_code.
*      LEAVE TO SCREEN 0.
*  ENDCASE.
*
*ENDMODULE.                 " USER_COMMAND_0210  INPUT
**&---------------------------------------------------------------------
**
**&      Form  INSERT_DATA
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM insert_data.
*
*  DATA: l_matnr LIKE mard-matnr,
*        l_lgpla LIKE ztmm_bdp-lgpla.
*
*  SELECT SINGLE matnr INTO l_matnr
*  FROM mard
*  WHERE matnr = ztmm_bdp-matnr
*    AND werks = ztmm_bdp-werks
*    AND lgort = ztmm_bdp-lgort.
*  IF sy-subrc <> 0.
*    MESSAGE w009(zmmm) WITH 'No Data in Material Master(MARD)'.
*    EXIT.
*  ENDIF.
*
*  SELECT SINGLE lgpla INTO l_lgpla
*  FROM lagp
*  WHERE lgnum = 'P01'
*    AND lgpla = ztmm_bdp-lgpla.
*  IF sy-subrc <> 0.
*    MESSAGE w009(zmmm) WITH 'BIN not found'.
*    EXIT.
*  ENDIF.
*  SELECT SINGLE matnr INTO l_matnr
*  FROM ztmm_bdp
*  WHERE matnr = ztmm_bdp-matnr
*    AND werks = ztmm_bdp-werks
*    AND lgort = ztmm_bdp-lgort
*    AND lgpla = ztmm_bdp-lgpla.
*  IF sy-subrc = 0.
*    MESSAGE w009(zmmm) WITH 'Data has existed already'.
*  ELSE.
*    ztmm_bdp-aedat = sy-datum.
*    ztmm_bdp-aezet = sy-uzeit.
*    ztmm_bdp-aenam = sy-uname.
*
*    INSERT ztmm_bdp.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*      MESSAGE s009(zmmm) WITH 'Data was saved'.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  DROPDOWN_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dropdown_table .
*Declarations for drop down lists in ALV.
  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.
** First SLART listbox (handle '1').

  ls_dropdown-handle = '1'.
  ls_dropdown-value = '01 Primary school'.
  APPEND ls_dropdown TO lt_dropdown.  ls_dropdown-handle = '1'.
  ls_dropdown-value = '02 Lower Secondary'.
  APPEND ls_dropdown TO lt_dropdown.  ls_dropdown-handle = '1'.
  ls_dropdown-value = '03 Upper Secondary'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '1'.
  ls_dropdown-value = '04 Professional School'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '1'.
  ls_dropdown-value = '05 College'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '1'.
  ls_dropdown-value = '06 University'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '1'.
  ls_dropdown-value = '09 Other Establishment'.
  APPEND ls_dropdown TO lt_dropdown.

* Second ABART listbox (handle '2').
  ls_dropdown-handle = '2'.
  ls_dropdown-value = '001. Not Specified'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value = '002. Low'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value = '003. Medium'.
  APPEND ls_dropdown TO lt_dropdown.
  ls_dropdown-handle = '2'.
  ls_dropdown-value = '004. High'.
  APPEND ls_dropdown TO lt_dropdown.

*method to display the dropdown in ALV
  CALL METHOD alv_grid->set_drop_down_table
    EXPORTING
      it_drop_down = lt_dropdown.
ENDFORM.                    " DROPDOWN_TABLE
