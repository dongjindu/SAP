REPORT zpp_check_planning_profile.
TYPE-POOLS: m60vt.

INCLUDE: <icon>.

TABLES: mara, marc, cuvtab_tx, cuvtab,
        zsbm_planning_profile_9100,
        zsbm_planning_profile_config.

DATA: zsbm_planning_profile_9000 LIKE zsbm_planning_profile_9000.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsbm_planning_profile_9000
                                     WITH HEADER LINE.
DATA: it_9100 LIKE zsbm_planning_profile_9100 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_fsc OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   mast-werks,
        maktx   LIKE   makt-maktx,
        visud   LIKE   tphvp-visud,
      END   OF it_fsc.

DATA: BEGIN OF it_profile OCCURS 0,
        clint   LIKE   tpsvp-clint,
        atbez   LIKE   cabnt-atbez,
        lnpos   LIKE   tpsvp-lnpos,
        plrel   LIKE   tpsvp-pl_rel,
      END   OF it_profile.

DATA: BEGIN OF it_config OCCURS 0,
        atwrt TYPE v_ibin_syval-atwrt,
        atnam TYPE cabn-atnam,
        atbez LIKE cabnt-atbez,
      END   OF it_config.

*---// Contants
CONSTANTS: c_check TYPE c           VALUE 'x',
           c_werks LIKE t001w-werks VALUE 'P001'.

*-----      Table Controls
CONTROLS: tc_9100   TYPE TABLEVIEW USING SCREEN 9100.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : w_is_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSBM_PLANNING_PROFILE_'.

*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click

ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT a~matnr b~werks e~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_fsc
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr EQ b~matnr
                     AND b~lvorm EQ space
                     AND b~werks EQ c_werks
                   INNER JOIN makt AS e
                      ON a~matnr = e~matnr
                     AND e~spras = sy-langu
   WHERE a~mtart EQ 'FERT'
     AND a~matnr IN s_matnr
   GROUP BY A~MATNR b~werks E~MAKTX.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  LOOP AT it_fsc.
    CLEAR: it_9000.
    PERFORM read_planning_profile TABLES it_profile
                                  USING  it_fsc-matnr.
    PERFORM read_configuration_profile TABLES it_config
                                       USING  it_fsc-matnr it_fsc-werks.
    PERFORM set_it_9000.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  read_planning_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_planning_profile TABLES pt_profile STRUCTURE it_profile
                           USING  pw_matnr.
  DATA: lt_values TYPE m60vt_profil.


  DATA: lw_header LIKE tphvp,
        lw_group  LIKE tplvp,
        lw_lines  LIKE tpsvp.

  DATA: lw_objekt LIKE inob-objek,
        lw_atwtb  LIKE rm60rel-atwtb.

  CLEAR: pt_profile, pt_profile[].

  MOVE: pw_matnr TO lw_objekt.

  CALL FUNCTION 'M60V_PROFIL_FOR_PLAN'
       EXPORTING
            objekt      = lw_objekt
            buffer_free = 'X'
            key_date    = sy-datum
            i_pl_rel    = ' '
       IMPORTING
            exp_value   = lt_values
       EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.

  CHECK sy-subrc EQ 0.

  READ TABLE lt_values-headr INTO lw_header INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE: lw_header-visud TO it_fsc-visud.

  LOOP AT lt_values-group INTO lw_group.
    SELECT SINGLE * FROM cuvtab_tx WHERE vtint = lw_group-clint
                                     AND spras = sy-langu.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    SELECT SINGLE * FROM  cuvtab WHERE  vtint  = lw_group-clint.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    CLEAR: pt_profile.
    MOVE: lw_group-clint  TO pt_profile-clint,
          cuvtab_tx-vttxt TO pt_profile-atbez,
          0               TO pt_profile-lnpos.

    APPEND pt_profile.

    LOOP AT lt_values-lines INTO lw_lines
                           WHERE profilid  = lw_group-profilid
                             AND phcounter = lw_group-phcounter
                             AND clint     = lw_group-clint.

      CALL FUNCTION 'M60V_COMBINATION_DISPLAY'
           EXPORTING
                table_name   = cuvtab-vtnam
                table_line   = lw_lines-lnpos
                table_number = lw_lines-clint
           IMPORTING
                string       = lw_atwtb.

      CLEAR: pt_profile.
      MOVE: lw_lines-clint  TO pt_profile-clint,
            lw_lines-lnpos  TO pt_profile-lnpos,
            lw_lines-pl_rel TO pt_profile-plrel,
            lw_atwtb        TO pt_profile-atbez.

      APPEND pt_profile.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " read_planning_profile
*&---------------------------------------------------------------------*
*&      Form  read_configuration_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CONFIG  text
*      -->P_IT_FSC_MATNR  text
*----------------------------------------------------------------------*
FORM read_configuration_profile TABLES  pt_config STRUCTURE it_config
                                USING   pw_matnr pw_werks.
  DATA: l_in_recno LIKE ibin-in_recno.

  CLEAR: pt_config, pt_config[].

  SELECT SINGLE * FROM marc WHERE matnr = pw_matnr
                              AND werks = pw_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  SELECT SINGLE in_recno INTO l_in_recno
                         FROM ibin
                        WHERE instance EQ marc-cuobj.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  SELECT a~atwrt b~atnam c~atbez
    INTO TABLE pt_config
    FROM v_ibin_syval AS a INNER JOIN cabn AS b
                              ON a~atinn EQ b~atinn
                           INNER JOIN cabnt AS c
                              ON b~atinn EQ c~atinn
                             AND b~adzhl EQ c~adzhl
                             AND c~lkenz EQ space
   WHERE a~in_recno EQ l_in_recno.
ENDFORM.                    " read_configuration_profile
*&---------------------------------------------------------------------*
*&      Form  set_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000.
  MOVE: it_fsc-matnr TO it_9000-matnr,
        it_fsc-maktx TO it_9000-maktx.


  PERFORM check_color_option USING 'COLOR_MI'.
  PERFORM check_color_option USING 'COLOR_OPT1'.
  PERFORM check_color_option USING 'COLOR_OPT2'.
  PERFORM check_color_option USING 'COLOR_OPT3'.
  PERFORM check_color_option USING 'COLOR_OPT4'.
  PERFORM check_others.

  IF it_9000-color_mi   EQ icon_led_yellow OR
     it_9000-color_opt1 EQ icon_led_yellow OR
     it_9000-color_opt2 EQ icon_led_yellow OR
     it_9000-color_opt3 EQ icon_led_yellow OR
     it_9000-color_opt4 EQ icon_led_yellow OR
     it_9000-color_ext  EQ icon_led_yellow OR
     it_9000-color_int  EQ icon_led_yellow OR
     it_9000-update     EQ icon_led_yellow.
    MOVE: icon_led_yellow TO it_9000-icon.
  ELSE.
    MOVE: icon_led_green TO it_9000-icon.
  ENDIF.

  APPEND it_9000.
ENDFORM.                    " set_it_9000
*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_others.
  LOOP AT it_profile WHERE atbez(8) EQ 'COLORINT'
                       AND lnpos    NE '00000'
                       AND plrel    EQ space.
    MOVE: icon_led_yellow TO it_9000-color_int.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    LOOP AT it_profile WHERE atbez(8) EQ 'COLORINT'
                         AND lnpos    NE '00000'.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: icon_led_yellow TO it_9000-color_int.
    ELSE.
      MOVE: icon_led_green  TO it_9000-color_int.
    ENDIF.
  ENDIF.

  LOOP AT it_profile WHERE atbez(8) EQ 'COLOREXT'
                       AND lnpos NE '00000'
                       AND plrel EQ space.
    MOVE: icon_led_yellow TO it_9000-color_ext.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    LOOP AT it_profile WHERE atbez(8) EQ 'COLOREXT'
                         AND lnpos    NE '00000'.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: icon_led_yellow TO it_9000-color_ext.
    ELSE.
      MOVE: icon_led_green TO it_9000-color_ext.
    ENDIF.
  ENDIF.

  IF it_fsc-visud EQ space.
    MOVE: icon_led_yellow TO it_9000-update.
  ELSE.
    MOVE: icon_led_green TO it_9000-update.
  ENDIF.
ENDFORM.                    " CHECK_OTHERS
*&---------------------------------------------------------------------*
*&      Form  check_color_option
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0551   text
*----------------------------------------------------------------------*
FORM check_color_option USING pw_field.
  DATA: lw_field(50),
        lw_atbez LIKE it_profile-atbez,
        lw_index TYPE i.

  FIELD-SYMBOLS: <field>.

  CONCATENATE 'IT_9000-' pw_field INTO lw_field.
  ASSIGN (lw_field) TO <field>.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  READ TABLE it_config WITH KEY atnam = pw_field.
  IF sy-subrc NE 0.
    MOVE: icon_led_yellow TO <field>.
    EXIT.
  ENDIF.

  CONCATENATE pw_field '/' it_config-atwrt INTO lw_atbez.

  LOOP AT it_profile WHERE atbez EQ lw_atbez
                       AND lnpos NE '00000'
                       AND plrel EQ 'X'.
    lw_index = lw_index + 1.

    MOVE: icon_led_green TO <field>.

    IF lw_index > 1.
      MOVE: icon_led_yellow TO <field>.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: icon_led_yellow TO <field>.
    EXIT.
  ENDIF.
ENDFORM.                    " check_color_option
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY icon DESCENDING matnr.
  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
      SET TITLEBAR '9100'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container.
  ASSIGN:      (w_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
  ELSE.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
  ENDIF.
ENDFORM.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container,
               'WC_CONTROL_'   p_dynnr INTO w_control,
               'WC_ALV_'       p_dynnr INTO w_alv.

  ASSIGN: (w_container) TO <container>,
          (w_control)   TO <control>,
          (w_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
         EXPORTING i_parent      = <container>
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  ' ' 'OUTPUTLEN'   '18',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  ' ' 'OUTPUTLEN'   '3',
                                  ' ' 'JUST'        'C',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '28',

                                  'S' 'COLOR_MI'    ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_OPT1'  ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_OPT2'  ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_OPT3'  ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_OPT4'  ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_EXT'   ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'COLOR_INT'   ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'UPDATE'      ' ',
                                  ' ' 'JUST'        'C',
                                  'E' 'OUTPUTLEN'   '6'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv,
               c_structure  p_dynnr      INTO w_structure,
               'IT_'        p_dynnr '[]' INTO w_itab.

  ASSIGN: (w_alv)       TO <alv>,
          (w_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
     EXPORTING i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv.
  ASSIGN: (w_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
        EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
  SET HANDLER event_receiver->handle_double_click  FOR <alv>.
**    SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID
  .
**    SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID
  .
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR ALV_GRID
  .

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD alv_grid->set_toolbar_interactive.
*
*    CALL METHOD cl_gui_control=>set_focus
*                        EXPORTING control = alv_grid.
ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1033   text
*      -->P_1034   text
*      -->P_1035   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
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
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO w_cnt.
      p_fieldcat-col_pos = w_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_9000 USING p_column_name             "Column Name
                          ps_row_no  LIKE sy-tabix. "Numeric Row ID
  DATA : lw_sel_index LIKE sy-tabix.

  MOVE: ps_row_no TO lw_sel_index.

  READ TABLE it_9000 INDEX lw_sel_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  MOVE: it_9000 TO zsbm_planning_profile_9000.

  CASE p_column_name.
    WHEN 'MATNR' OR 'MAKTX'.
      SET PARAMETER ID 'MAT' FIELD it_9000-matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
      PERFORM display_detail_profile.
  ENDCASE.
ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  display_DETAIL_PROFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_profile.
  PERFORM read_planning_profile TABLES it_profile
                                USING  it_9000-matnr.
  PERFORM read_configuration_profile TABLES it_config
                                     USING  it_9000-matnr c_werks.
  PERFORM set_screen_9100_data.

  CALL SCREEN 9100.
ENDFORM.                    " display_DETAIL_PROFILE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  table_control_output_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_output_9100 OUTPUT.
  READ TABLE it_9100 INDEX tc_9100-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_9100 TO zsbm_planning_profile_9100.
  ENDIF.
ENDMODULE.                 " table_control_output_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_ATTRIBUTE_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_attribute_9100 OUTPUT.
  LOOP AT SCREEN.
    CHECK zsbm_planning_profile_9100-lnpos EQ '00000'.

    CASE screen-name.
      WHEN 'ZSBM_PLANNING_PROFILE_9100-ATBEZ'.
        screen-intensified = 1.
      WHEN 'ZSBM_PLANNING_PROFILE_9100-PLREL'.
        screen-invisible = 1.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " SET_ATTRIBUTE_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_9100_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_9100_data.
  DATA: lw_field(50).
  FIELD-SYMBOLS: <field>.

  CLEAR: it_9100[].
  LOOP AT it_profile.
    CLEAR: it_9100.
    MOVE-CORRESPONDING it_profile TO it_9100.

    APPEND it_9100.

    AT END OF clint.
      CLEAR: it_9100.
      APPEND it_9100.
    ENDAT.
  ENDLOOP.

  CLEAR: zsbm_planning_profile_config.
  DO.
    READ TABLE it_config INDEX sy-index.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    CONCATENATE 'ZSBM_PLANNING_PROFILE_CONFIG-' it_config-atnam
           INTO lw_field.
    ASSIGN (lw_field) TO <field>.

    MOVE: it_config-atwrt TO <field>.
  ENDDO.
ENDFORM.                    " SET_SCREEN_9100_DATA
*&---------------------------------------------------------------------*
*&      Module  table_control_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_lines OUTPUT.
  DESCRIBE TABLE it_9100 LINES tc_9100-lines.
ENDMODULE.                 " table_control_lines  OUTPUT
