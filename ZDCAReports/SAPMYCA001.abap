*&---------------------------------------------------------------------*
*& Report  SAPMYCA001                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  sapmyca001                    .
TABLES: ztca_vin_usr_set,
        zsca_vin_condition1,
        zsca_vin_condition2,
        zsca_user_setting_1,
        zsca_user_setting_2.

*----- Internal Tables
DATA: it_9000   LIKE zsca_vin_condition2 OCCURS 0 WITH HEADER LINE,
      it_9100_1 LIKE zsca_user_setting_1 OCCURS 0 WITH HEADER LINE,
      it_9100_2 LIKE zsca_user_setting_2 OCCURS 0 WITH HEADER LINE,
      it_ztca_vin_usr_set LIKE ztca_vin_usr_set OCCURS 0
                                                WITH HEADER LINE,
      it_vehicle LIKE zsca_vehicle_char_value OCCURS 0
                                              WITH HEADER LINE,
      it_value  LIKE zsca_char_value     OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_character OCCURS 0,           "Characteristic
        atinn   LIKE   cabn-atinn,              "Characteristic
        atnam   LIKE   cabn-atnam,              "Characteristic name
        atbez   LIKE   cabnt-atbez,             "Characteristic Desc.
        anzst   LIKE   cabn-anzst,              "Length
      END   OF it_character.

DATA: BEGIN OF it_profile OCCURS 0,             "Profile
        using,                                  "Using Flag
        profile LIKE   ztca_vin_usr_set-profile,
      END   OF it_profile.

DATA: BEGIN OF it_previous OCCURS 0,            "Previous first VIN
        objek   LIKE   ausp-objek,              "Vehicle Master
      END   OF it_previous.

*----- Global variables & Structures
DATA: wa_profile LIKE ztca_vin_usr_set-profile, "Profile
      wa_p_first_vin LIKE ausp-objek,           "Previous first Vin
      wa_first_vin   LIKE ausp-objek,           "First vin
      wa_last_vin    LIKE ausp-objek,           "Last vin
      wa_top_vin     LIKE ausp-objek.           "Top vin

*-----      Table Controls
CONTROLS: tc_9000   TYPE TABLEVIEW USING SCREEN 9000,
          tc_9100_1 TYPE TABLEVIEW USING SCREEN 9100,
          tc_9100_2 TYPE TABLEVIEW USING SCREEN 9100.

*...LIST BOX DATA
TYPE-POOLS vrm.

DATA: list  TYPE vrm_values,
      value LIKE LINE OF list.

*-----     Class
FIELD-SYMBOLS <f1>.

CLASS: lcl_application DEFINITION DEFERRED,
       cl_gui_cfw DEFINITION LOAD,
       cl_gui_cfw      DEFINITION LOAD.

DATA: wa_application       TYPE REF TO lcl_application,
      wa_tree_container    TYPE REF TO cl_gui_custom_container,
      wa_tree              TYPE REF TO cl_gui_column_tree,
      wa_docking           TYPE REF TO cl_gui_docking_container.

*-----     Tree Node
TYPES: item_table_type LIKE STANDARD TABLE OF mtreeitm
                                     WITH DEFAULT KEY.

DATA: it_node_itab TYPE treev_ntab,
      wa_node      TYPE treev_node,
      it_item_itab TYPE item_table_type,
      wa_item      TYPE mtreeitm.

*----- Class Definition and Implementation
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS handle_item_double_click
        FOR EVENT item_double_click OF cl_gui_column_tree
        IMPORTING node_key item_name.
ENDCLASS.

*---------------------------------------------------------------------
*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------
CLASS lcl_application IMPLEMENTATION.
  METHOD  handle_item_double_click.

  set parameter id 'EQN' field node_key.

  call transaction 'IE03' AND SKIP FIRST SCREEN.
*
*    SUBMIT ZPJ0100
*           WITH PA_UNAME = NODE_KEY
*           WITH PA_PJTCD = ZPJ008S-PJTCD
*           WITH PA_MONTH = ITEM_NAME
*           AND RETURN.
*
*    PERFORM ENTER_RTN.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
    WHEN 9100.
      SET PF-STATUS '9100'.
    WHEN 9200.
      SET PF-STATUS '9200'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC' OR 'RW'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'NEW_PF'.
      CLEAR: ztca_vin_usr_set.
      CALL SCREEN 9200 STARTING AT  70 7
                       ENDING   AT 100 7.
      LEAVE TO SCREEN sy-dynnr.
    WHEN 'SETTING'.
      CLEAR: sy-ucomm.
      PERFORM user_setting_rtn.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  table_control_output  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_output OUTPUT.
  READ TABLE it_9000 INDEX tc_9000-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_9000 TO zsca_vin_condition2.
  ENDIF.
ENDMODULE.                 " table_control_output  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_tc_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tc_data INPUT.
  CLEAR: it_9000.

  READ TABLE it_9000 INDEX tc_9000-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING zsca_vin_condition2 TO it_9000.

    MODIFY it_9000 INDEX tc_9000-current_line.
  ELSE.
    MOVE-CORRESPONDING zsca_vin_condition2 TO it_9000.

    APPEND it_9000.
  ENDIF.
ENDMODULE.                 " modify_tc_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  table_control_lines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_lines OUTPUT.
  DESCRIBE TABLE it_9000   LINES tc_9000-lines.
  DESCRIBE TABLE it_9100_1 LINES tc_9100_1-lines.
  DESCRIBE TABLE it_9100_2 LINES tc_9100_2-lines.
ENDMODULE.                 " table_control_lines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_AND_INIT_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_and_init_object OUTPUT.
  CHECK wa_tree IS INITIAL.

  PERFORM get_characteristic.
  PERFORM get_user_setting.
  CREATE OBJECT  wa_application.
  PERFORM create_container.
  PERFORM create_tree.
  PERFORM event_assign.
  PERFORM create_column.
  zsca_vin_condition1-count = 20.
ENDMODULE.                 " CREATE_AND_INIT_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container.
  CREATE OBJECT wa_tree_container
    EXPORTING
      container_name = 'CC_9000'
    EXCEPTIONS
      cntl_error = 1
      cntl_system_error = 2
      create_error = 3
      lifetime_error = 4
      lifetime_dynpro_dynpro_link = 5.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  CREATE_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_tree.
  DATA: hierarchy_header TYPE treev_hhdr.

  hierarchy_header-heading = text-b01.
  hierarchy_header-width = 10.

  CREATE OBJECT wa_tree
    EXPORTING
      parent              = wa_tree_container
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
      item_selection = 'X'
      hierarchy_column_name = 'COLUMN01'
      hierarchy_header = hierarchy_header
    EXCEPTIONS
      cntl_system_error           = 1
      create_error                = 2
      failed                      = 3
      illegal_node_selection_mode = 4
      illegal_column_name         = 5
      lifetime_error              = 6.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

ENDFORM.                    " CREATE_TREE
*&---------------------------------------------------------------------*
*&      Form  get_user_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_user_setting.
*---- Read lasted user setting

  DATA: lw_date   LIKE   sy-datum,
        lw_time   LIKE   sy-uzeit.

  SELECT MAX( erdat ) INTO lw_date
    FROM ztca_vin_usr_set
   WHERE uname   = sy-uname.

  CHECK NOT lw_date IS INITIAL.

  SELECT MAX( erzet ) INTO lw_time
    FROM ztca_vin_usr_set
   WHERE uname   = sy-uname
     AND erdat   = lw_date.

  SELECT * INTO TABLE it_ztca_vin_usr_set
    FROM ztca_vin_usr_set
   WHERE uname   = sy-uname
     AND erdat   = lw_date
     AND erzet   = lw_time.

  CLEAR: it_9100_2, it_9100_2[].
  LOOP AT it_ztca_vin_usr_set.
    READ TABLE it_character WITH KEY atinn = it_ztca_vin_usr_set-atinn.
    IF sy-subrc EQ 0.
      MOVE: it_ztca_vin_usr_set-zzseq TO it_9100_2-zzseq,
            it_character-atnam        TO it_9100_2-atnam.
      APPEND it_9100_2.
    ENDIF.
  ENDLOOP.

  SORT it_9100_2 BY zzseq.

  SELECT profile INTO CORRESPONDING FIELDS OF TABLE it_profile
    FROM ztca_vin_usr_set
   WHERE uname   = sy-uname
   GROUP by profile.

  READ TABLE it_profile WITH KEY profile = it_ztca_vin_usr_set-profile.
  IF sy-subrc EQ 0.
    it_profile-using = 'X'.
    MODIFY it_profile INDEX sy-tabix.
  ENDIF.

  SORT it_profile BY using DESCENDING profile.
ENDFORM.                    " get_user_setting
*&---------------------------------------------------------------------*
*&      Form  get_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristic.
  DATA: lw_clint   LIKE   klah-clint,     "Class
        lw_objek   LIKE   kssk-objek.     "Class

  SELECT SINGLE clint INTO lw_clint
    FROM klah
   WHERE class = 'P_VEHICLE_MASTER'
     AND klart = '002'.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m03.
    LEAVE TO SCREEN 0.
  ENDIF.

  MOVE: lw_clint TO lw_objek.

  SELECT d~atinn d~atnam e~atbez d~anzst
    INTO CORRESPONDING FIELDS OF TABLE it_character
    FROM ( (
           kssk AS b INNER JOIN ksml AS c
                        ON b~clint = c~clint
                       AND b~adzhl = c~adzhl )
                     INNER JOIN cabn AS d
                        ON c~imerk = d~atinn
                       AND c~adzhl = d~adzhl )
                     INNER JOIN cabnt AS e
                        ON d~atinn = e~atinn
                       AND d~adzhl = e~adzhl
   WHERE b~objek = lw_objek
     AND b~klart = '002'
     AND e~spras = sy-langu.

  SORT it_character BY atnam.
ENDFORM.                    " get_characteristic
*&---------------------------------------------------------------------*
*&      Form  EVENT_ASSIGN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM event_assign.
  DATA: lt_event       TYPE cntl_simple_events,      "???
        lw_event       TYPE cntl_simple_event.       "Structure

  lw_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  lw_event-appl_event = 'X'.
  APPEND lw_event TO lt_event.

  CALL METHOD wa_tree->set_ctx_menu_select_event_appl
    EXPORTING appl_event = 'X'.

  CALL METHOD wa_tree->set_registered_events
    EXPORTING
      events = lt_event
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  SET HANDLER wa_application->handle_item_double_click FOR wa_tree.
ENDFORM.                    " EVENT_ASSIGN
*&---------------------------------------------------------------------*
*&      Form  create_column
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_column.
  DATA: lw_column   TYPE tv_itmname.

  DATA: hierarchy_header TYPE treev_hhdr.

*-----     ?? M/D ? Column
  LOOP AT it_ztca_vin_usr_set.
    READ TABLE it_character WITH KEY atinn = it_ztca_vin_usr_set-atinn.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m04.
    ENDIF.

    MOVE: it_ztca_vin_usr_set-atinn TO lw_column,
          it_character-atbez        TO hierarchy_header-heading,
          it_character-anzst        TO hierarchy_header-width.

    CALL METHOD wa_tree->add_column
      EXPORTING
        name        = lw_column
        width       = hierarchy_header-width
        header_text = hierarchy_header-heading
        alignment   = wa_tree->align_left
      EXCEPTIONS
        column_exists                 = 1
        illegal_column_name           = 2
        too_many_columns              = 3
        illegal_alignment             = 4
        different_column_types        = 5
        cntl_system_error             = 6
        failed                        = 7
        predecessor_column_not_found  = 8.
    IF sy-subrc <> 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " create_column
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'DISPLAY'.
      CLEAR: sy-ucomm.
      PERFORM display_rtn.
    WHEN 'P-'.
      CLEAR: sy-ucomm.
      PERFORM page_up_rtn.
    WHEN 'P+'.
      CLEAR: sy-ucomm.
      PERFORM page_down_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  USER_SETTING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_setting_rtn.
  LOOP AT it_character.
    READ TABLE it_9100_2 WITH KEY atnam = it_character-atnam.
    IF sy-subrc NE 0.
      MOVE: it_character-atnam TO it_9100_1-atnam.
      APPEND it_9100_1.
    ENDIF.
  ENDLOOP.

  READ TABLE it_profile WITH KEY using = 'X'.
  MOVE it_profile-profile TO wa_profile.

  CALL SCREEN 9100 STARTING AT  40 2
                   ENDING   AT 130 17.
ENDFORM.                    " USER_SETTING_RTN
*&---------------------------------------------------------------------*
*&      Module  table_control_output_9001_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_output_9001_1 OUTPUT.
  READ TABLE it_9100_1 INDEX tc_9100_1-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_9100_1 TO zsca_user_setting_1.
  ENDIF.
ENDMODULE.                 " table_control_output_9001_1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  table_control_output_9001_2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_output_9001_2 OUTPUT.
  READ TABLE it_9100_2 INDEX tc_9100_2-current_line.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_9100_2 TO zsca_user_setting_2.
  ENDIF.
ENDMODULE.                 " table_control_output_9001_2  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  DATA: lw_lines TYPE i.

  PERFORM read_vehicle_master USING ''.
  PERFORM build_node.       "Assign characteristic value

*----- Get Previous First Vin No & First Vin No & Last Vin No

  CLEAR: it_previous, it_previous[].

  DESCRIBE TABLE it_vehicle LINES lw_lines.

  READ TABLE it_vehicle INDEX lw_lines.

  wa_last_vin    = it_vehicle-objek.
ENDFORM.                    " DISPLAY_RTN
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'ENTE'.
      CLEAR: sy-ucomm.
      PERFORM enter_rtn.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_9100_rtn.
    WHEN 'CHANGE_PF'.
      CLEAR: sy-ucomm.
      PERFORM change_profile_rtn.
    WHEN 'SELECT'.
      CLEAR: sy-ucomm.
      PERFORM select_rtn.
    WHEN 'DESELECT'.
      CLEAR: sy-ucomm.
      PERFORM deselect_rtn.
    WHEN 'TOP'.                  "Field Seq. TOP
      CLEAR: sy-ucomm.
      PERFORM top_rtn.
    WHEN 'UP'.                   "Field Seq. UP
      CLEAR: sy-ucomm.
      PERFORM up_rtn.
    WHEN 'DOWN'.                 "Field Seq. DOWN
      CLEAR: sy-ucomm.
      PERFORM down_rtn.
    WHEN 'BOTTOM'.               "Field Seq. BOTTOM
      CLEAR: sy-ucomm.
      PERFORM botton_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC_9100_1_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tc_9100_1_data INPUT.
  CLEAR: it_9100_1.

  READ TABLE it_9100_1 INDEX tc_9100_1-current_line.
  IF sy-subrc EQ 0.
    MOVE zsca_user_setting_1-check TO it_9100_1-check.

    MODIFY it_9100_1 INDEX tc_9100_1-current_line.
  ENDIF.
ENDMODULE.                 " MODIFY_TC_9100_1_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TC_9100_2_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tc_9100_2_data INPUT.
  CLEAR: it_9100_2.

  READ TABLE it_9100_2 INDEX tc_9100_2-current_line.
  IF sy-subrc EQ 0.
    MOVE zsca_user_setting_2-check TO it_9100_2-check.

    MODIFY it_9100_2 INDEX tc_9100_2-current_line.
  ENDIF.
ENDMODULE.                 " MODIFY_TC_9100_2_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_rtn.
  DATA: lw_lines TYPE i.

*----- Check Selected
  READ TABLE it_9100_1 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

*----- Check count
  DESCRIBE TABLE it_9100_2 LINES lw_lines.
  IF lw_lines > 20.
    MESSAGE e000(zz) WITH text-m05.
  ENDIF.

  DELETE it_9100_1 INDEX sy-tabix.
  MOVE: it_9100_1-atnam TO it_9100_2-atnam.
  APPEND it_9100_2.
ENDFORM.                    " SELECT_RTN
*&---------------------------------------------------------------------*
*&      Module  set_list_box_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_list_box_9100 OUTPUT.
  CLEAR: list.

  LOOP AT it_profile.
    MOVE: it_profile-profile TO value-key,
          it_profile-profile TO value-text.

    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'WA_PROFILE'
            values = list.
ENDMODULE.                 " set_list_box_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  deselect_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselect_rtn.
*----- Check Selected
  READ TABLE it_9100_2 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  DELETE it_9100_2 INDEX sy-tabix.

  CLEAR: it_9100_1.
  MOVE: it_9100_2-atnam TO it_9100_1-atnam.
  APPEND it_9100_1.

  SORT it_9100_1 BY atnam.

  PERFORM generate_field_sequence.
ENDFORM.                    " deselect_rtn
*&---------------------------------------------------------------------*
*&      Form  generate_field_sequence
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_field_sequence.
*---- Generate field sequence for selected characteristic

  LOOP AT it_9100_2.
    it_9100_2-zzseq = sy-tabix.
    MODIFY it_9100_2.
  ENDLOOP.
ENDFORM.                    " generate_field_sequence
*&---------------------------------------------------------------------*
*&      Form  top_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_rtn.
*----- Check Selected
  READ TABLE it_9100_2 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  DELETE it_9100_2 INDEX sy-tabix.
  INSERT it_9100_2 INDEX 1.

  PERFORM generate_field_sequence.
ENDFORM.                    " top_rtn
*&---------------------------------------------------------------------*
*&      Form  UP_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM up_rtn.
  DATA: lw_lines TYPE i.

*----- Check Selected
  READ TABLE it_9100_2 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  IF sy-tabix EQ 1.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  lw_lines = sy-tabix - 1.

  DELETE it_9100_2 INDEX sy-tabix.
  INSERT it_9100_2 INDEX lw_lines.

  PERFORM generate_field_sequence.
ENDFORM.                    " UP_RTN
*&---------------------------------------------------------------------*
*&      Form  DOWN_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_rtn.
  DATA: lw_lines TYPE i.

*----- Check Selected
  READ TABLE it_9100_2 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  DESCRIBE TABLE it_9100_2 LINES lw_lines.
  IF sy-tabix EQ lw_lines.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.

  lw_lines = sy-tabix + 1.

  DELETE it_9100_2 INDEX sy-tabix.
  INSERT it_9100_2 INDEX lw_lines.

  PERFORM generate_field_sequence.
ENDFORM.                    " DOWN_RTN
*&---------------------------------------------------------------------*
*&      Form  BOTTON_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM botton_rtn.
*----- Check Selected
  READ TABLE it_9100_2 WITH KEY check = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  DELETE it_9100_2 INDEX sy-tabix.
  APPEND it_9100_2.

  PERFORM generate_field_sequence.
ENDFORM.                    " BOTTON_RTN
*&---------------------------------------------------------------------*
*&      Form  save_9100_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_9100_rtn.
  PERFORM delete_column.

  CLEAR: it_ztca_vin_usr_set, it_ztca_vin_usr_set[].

  PERFORM generate_field_sequence.

  LOOP AT it_9100_2.
    READ TABLE it_character WITH KEY atnam = it_9100_2-atnam.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m09.
    ENDIF.

    MOVE: sy-uname           TO it_ztca_vin_usr_set-uname,
          wa_profile         TO it_ztca_vin_usr_set-profile,
          it_9100_2-zzseq    TO it_ztca_vin_usr_set-zzseq,
          it_character-atinn TO it_ztca_vin_usr_set-atinn,
          sy-datum           TO it_ztca_vin_usr_set-erdat,
          sy-uzeit           TO it_ztca_vin_usr_set-erzet.
    APPEND it_ztca_vin_usr_set.
  ENDLOOP.

  PERFORM create_column.

  PERFORM build_node.

  DELETE FROM ztca_vin_usr_set
   WHERE uname   = sy-uname
     AND profile = wa_profile.

  INSERT ztca_vin_usr_set FROM TABLE it_ztca_vin_usr_set
                          ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.                    " save_9100_rtn
*&---------------------------------------------------------------------*
*&      Form  enter_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enter_rtn.
  PERFORM delete_column.

  CLEAR: it_ztca_vin_usr_set, it_ztca_vin_usr_set[].

  PERFORM generate_field_sequence.

  LOOP AT it_9100_2.
    READ TABLE it_character WITH KEY atnam = it_9100_2-atnam.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m09.
    ENDIF.

    MOVE: sy-uname           TO it_ztca_vin_usr_set-uname,
          wa_profile         TO it_ztca_vin_usr_set-profile,
          it_9100_2-zzseq    TO it_ztca_vin_usr_set-zzseq,
          it_character-atinn TO it_ztca_vin_usr_set-atinn,
          sy-datum           TO it_ztca_vin_usr_set-erdat,
          sy-uzeit           TO it_ztca_vin_usr_set-erzet.
    APPEND it_ztca_vin_usr_set.
  ENDLOOP.

  PERFORM create_column.

  PERFORM build_node.

  LEAVE TO SCREEN 0.
ENDFORM.                    " enter_rtn
*&---------------------------------------------------------------------*
*&      Form  delete_column
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_column.
  DATA: lw_column   TYPE tv_itmname.

  LOOP AT it_ztca_vin_usr_set.
    MOVE: it_ztca_vin_usr_set-atinn TO lw_column.

    CALL METHOD wa_tree->delete_column
      EXPORTING
        column_name       = lw_column
      EXCEPTIONS
        failed            = 1
        column_not_found  = 2
        cntl_system_error = 3
        OTHERS            = 4.
*    IF sy-subrc <> 0.
*      MESSAGE e000(zz) WITH text-m11.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_column
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  CASE sy-ucomm.
    WHEN 'ENTE'.
      PERFORM enter_9200_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*&      Form  ENTER_9200_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enter_9200_rtn.
  READ TABLE it_profile WITH KEY profile = ztca_vin_usr_set-profile.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  CLEAR: it_profile.
  MOVE ztca_vin_usr_set-profile TO it_profile-profile.
  APPEND it_profile.

  LEAVE TO SCREEN 0.
ENDFORM.                    " ENTER_9200_RTN
*&---------------------------------------------------------------------*
*&      Form  change_profile_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_profile_rtn.
  PERFORM delete_column.

*----- Set it_ztca_vin_usr_set
  SELECT * INTO TABLE it_ztca_vin_usr_set
  FROM ztca_vin_usr_set
 WHERE uname   = sy-uname
   AND profile = wa_profile.

*----- Set IT_9100_2
  CLEAR: it_9100_2, it_9100_2[].
  LOOP AT it_ztca_vin_usr_set.
    READ TABLE it_character WITH KEY atinn = it_ztca_vin_usr_set-atinn.
    IF sy-subrc EQ 0.
      MOVE: it_ztca_vin_usr_set-zzseq TO it_9100_2-zzseq,
            it_character-atnam        TO it_9100_2-atnam.
      APPEND it_9100_2.
    ENDIF.
  ENDLOOP.

  SORT it_9100_2 BY zzseq.

*----- Set IT_9100_1
  CLEAR: it_9100_1, it_9100_1[].
  LOOP AT it_character.
    READ TABLE it_9100_2 WITH KEY atnam = it_character-atnam.
    IF sy-subrc NE 0.
      MOVE: it_character-atnam TO it_9100_1-atnam.
      APPEND it_9100_1.
    ENDIF.
  ENDLOOP.

*----- Set IT_PROFILE
  READ TABLE it_profile WITH KEY using = 'X'.
  IF sy-subrc EQ 0.
    it_profile-using = ''.
    MODIFY it_profile INDEX sy-tabix.
  ENDIF.

  READ TABLE it_profile WITH KEY profile = wa_profile.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.

  it_profile-using = 'X'.
  MODIFY it_profile INDEX sy-tabix.

  SORT it_profile BY using DESCENDING profile.
ENDFORM.                    " change_profile_rtn
*&---------------------------------------------------------------------*
*&      Form  build_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_node.
  " Delete all nodes
  CALL METHOD wa_tree->delete_all_nodes.

  CLEAR: wa_item, wa_node, it_item_itab, it_node_itab.

  LOOP AT it_vehicle.
    AT NEW objek.
      PERFORM build_key_node.
    ENDAT.

    PERFORM build_value_node.
  ENDLOOP.

  CALL METHOD wa_tree->add_nodes_and_items
    EXPORTING
      node_table = it_node_itab
      item_table = it_item_itab
      item_table_structure_name = 'MTREEITM'
    EXCEPTIONS
      failed = 1
      cntl_system_error = 3
      error_in_tables = 4
      dp_error = 5
      table_structure_name_not_found = 6.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.
ENDFORM.                    " build_node
*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1002   text
*----------------------------------------------------------------------*
FORM read_vehicle_master USING pw_objek.
  CLEAR: it_vehicle, it_vehicle[].

  LOOP AT it_9100_2.
    MOVE: it_9100_2-atnam TO it_value-atnam.
    APPEND it_value.
  ENDLOOP.

  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
       EXPORTING
            i_atnam                       = zsca_vin_condition1-atnam
            i_atwrt_s                     = zsca_vin_condition1-atwrt_s
            i_atwrt_e                     = zsca_vin_condition1-atwrt_e
            i_objek                       = pw_objek
            i_count                       = zsca_vin_condition1-count
       IMPORTING
            e_hit_count                   = zsca_vin_condition1-hitct
       TABLES
            t_condition                   = it_9000
            t_value                       = it_value
            t_vehicle                     = it_vehicle
       EXCEPTIONS
            date_overflow                 = 1
            invalid_date                  = 2
            condition_does_not_exist      = 3
            characteristic_does_not_exist = 4
            OTHERS                        = 5.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

*----- Delete vehicle master that VIN no is blank.
  DELETE it_vehicle WHERE objek = ''.

  READ TABLE it_vehicle INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
ENDFORM.                    " READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*&      Form  build_Key_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_key_node.
  CLEAR: wa_node, wa_item.
  MOVE: it_vehicle-objek TO wa_node-node_key,
        '@AW@'     TO wa_node-n_image.

  APPEND wa_node      TO it_node_itab.

  MOVE: it_vehicle-objek                    TO wa_item-node_key,
        cl_gui_column_tree=>item_class_text TO wa_item-class,
        'COLUMN01'                          TO wa_item-item_name,
        it_vehicle-objek                    TO wa_item-text.
  APPEND wa_item TO it_item_itab.
ENDFORM.                    " build_Key_node
*&---------------------------------------------------------------------*
*&      Form  build_value_node
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_value_node.
  READ TABLE it_9100_2 WITH KEY atnam = it_vehicle-atnam.
  CHECK sy-subrc EQ 0.

  CLEAR: wa_item.
  MOVE: it_vehicle-objek                    TO wa_item-node_key,
        it_vehicle-atinn                    TO wa_item-item_name,
        cl_gui_column_tree=>item_class_text TO wa_item-class,
        it_vehicle-atwrt                    TO wa_item-text.

  APPEND wa_item TO it_item_itab.
ENDFORM.                    " build_value_node
*&---------------------------------------------------------------------*
*&      Module  check_n_description_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_n_description_9000 INPUT.
  READ TABLE it_character WITH KEY atnam = zsca_vin_condition1-atnam.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.

  MOVE: it_character-atbez TO zsca_vin_condition1-atbez.

  DELETE it_9000 WHERE atnam EQ ' '.
ENDMODULE.                 " check_n_description_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_list_box_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_list_box_9000 OUTPUT.
  CLEAR: list.

  LOOP AT it_character.
    MOVE: it_character-atnam TO value-key,
          it_character-atbez TO value-text.

    APPEND value TO list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'ZSCA_VIN_CONDITION1-ATNAM'
            values = list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'ZSCA_VIN_CONDITION2-ATNAM'
            values = list.
ENDMODULE.                 " set_list_box_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  p-_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_up_rtn.
  DATA: lw_lines TYPE i.

  READ TABLE it_previous INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m15.
  ENDIF.

  DESCRIBE TABLE it_previous LINES lw_lines.
  READ     TABLE it_previous INDEX lw_lines.

  PERFORM read_vehicle_master USING it_previous-objek.
  PERFORM build_node.       "Assign characteristic value

*----- Get Previous First Vin No & First Vin No & Last Vin No
  DELETE it_previous INDEX lw_lines.

  READ TABLE it_vehicle INDEX 1.
  wa_first_vin   = it_vehicle-objek.

  DESCRIBE TABLE it_vehicle LINES lw_lines.
  READ     TABLE it_vehicle INDEX lw_lines.
  wa_last_vin    = it_vehicle-objek.
ENDFORM.                    " p-_rtn
*&---------------------------------------------------------------------*
*&      Form  _rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM page_down_rtn.
  DATA: lw_lines TYPE i.

  DESCRIBE TABLE it_previous LINES lw_lines.
  READ     TABLE it_previous INDEX lw_lines.
  IF wa_last_vin   = it_previous-objek.
    MESSAGE e000(zz) WITH text-m16.
  ENDIF.

  PERFORM read_vehicle_master USING wa_last_vin.
  PERFORM build_node.       "Assign characteristic value


*----- Get Previous First Vin No & First Vin No & Last Vin No
  MOVE: wa_first_vin TO it_previous-objek.
  APPEND it_previous.

  READ TABLE it_vehicle INDEX 1.
  wa_first_vin   = it_vehicle-objek.

  DESCRIBE TABLE it_vehicle LINES lw_lines.
  READ TABLE it_vehicle INDEX lw_lines.
  wa_last_vin    = it_vehicle-objek.
ENDFORM.                    " _rtn
