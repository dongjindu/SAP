************************************************************************
* Program Name      : ZEPP302E_SAP_BOM_CHECK_MST
* Author            : Bae, Byung sung
* Creation Date     : 2004.11.02.
* Specifications By : Bae, Byung sung
* Development Request No : UD1K912804
* Addl Documentation:
* Description       : Magage effective BOM
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zepp302e_sap_bom_check_mst    .
INCLUDE: <icon>.
TABLES: mara, mast, marc,t001w, t416t,
        ztbm_check_bom,
        zsbm_epp302.

*---// Internal Tables
DATA: BEGIN OF it_fsc OCCURS 0.
        INCLUDE STRUCTURE ztbm_check_bom.
DATA:   lvorm   LIKE   mara-lvorm,
        mtart   LIKE   mara-mtart,
      END   OF it_fsc.

DATA: it_module LIKE zsbm_epp302 OCCURS 0 WITH HEADER LINE.

DATA: it_9000 LIKE zsbm_epp302 OCCURS 0 WITH HEADER LINE.

DATA: it_module_fsc LIKE ztbm_check_bom OCCURS 0 WITH HEADER LINE.

DATA: it_ztbm_check_bom LIKE ztbm_check_bom OCCURS 0 WITH HEADER LINE.

*----- Global variables & Structures
DATA: w_fsc_cnt(4)    TYPE n,
      w_module_cnt(4) TYPE n.

*---// Constants
CONSTANTS: c_check                        VALUE 'X',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_module_stlan LIKE mast-stlan VALUE '2'.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

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
       w_save      TYPE c   VALUE 'A',   "for Parameter I_SAVE
       w_it9000_cnt type i.
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
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_stlan FOR mast-stlan,
                s_matnr FOR mara-matnr,
                s_datum FOR sy-datum.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t03.
PARAMETERS: p_update DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

PARAMETERS: p_datuv  LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
PARAMETERS: P_batch DEFAULT ' ' AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK bl2.

*---// Check input fields & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  IF p_update EQ c_check.
    PERFORM table_update_processing.
  ENDIF.

  PERFORM get_data.

START-OF-SELECTION.
describe table it_9000 lines w_it9000_cnt.
if w_it9000_cnt > 0 and P_batch = ' '.
  PERFORM display_data.
endif.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT a~matnr b~maktx c~mtart a~werks a~stlan
         a~stlal a~flag  a~flag  a~type  a~ernam
         a~erdat a~erzet a~aenam a~aedat a~aezet
    INTO CORRESPONDING FIELDS OF TABLE it_9000
    FROM ztbm_check_bom AS a INNER JOIN makt AS b
                                ON a~matnr = b~matnr
                               AND b~spras = sy-langu
                             INNER JOIN mara AS c
                                ON a~matnr = c~matnr
   WHERE a~matnr IN s_matnr
     AND a~stlan IN s_stlan
     AND a~ersda IN s_datum.
  IF sy-subrc NE 0.
    MESSAGE I000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_data_for_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_for_fsc.
  SELECT a~matnr c~maktx b~werks b~stlan b~stlal a~lvorm a~mtart
    INTO CORRESPONDING FIELDS OF TABLE it_fsc
    FROM mara AS a INNER JOIN mast AS b
                      ON a~matnr EQ b~matnr
                   INNER JOIN makt AS c
                      ON a~matnr EQ c~matnr
                     AND c~spras EQ sy-langu
   WHERE a~matnr IN s_matnr
     AND a~mtart EQ 'FERT'
     AND a~ersda IN s_datum
     AND b~stlan IN s_stlan.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " get_data_for_fsc
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE_FOR_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_for_fsc.
  LOOP AT it_fsc.
    CLEAR: ztbm_check_bom.

    SELECT SINGLE * FROM ztbm_check_bom
                   WHERE matnr = it_fsc-matnr
                     AND werks = it_fsc-werks
                     AND stlan = it_fsc-stlan
                     AND stlal = it_fsc-stlal.
    IF sy-subrc EQ 0.
      IF it_fsc-lvorm        EQ c_check AND
         ztbm_check_bom-flag EQ 'Y'.
        MOVE: 'N'      TO ztbm_check_bom-flag,
              sy-uname TO ztbm_check_bom-aenam,
              sy-datum TO ztbm_check_bom-aedat,
              sy-uzeit TO ztbm_check_bom-aezet.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      MOVE: it_fsc-matnr TO ztbm_check_bom-matnr,
            it_fsc-werks TO ztbm_check_bom-werks,
            it_fsc-stlan TO ztbm_check_bom-stlan,
            it_fsc-stlal TO ztbm_check_bom-stlal,
            it_fsc-ersda TO ztbm_check_bom-ersda,
            'Y'          TO ztbm_check_bom-flag,
            'F'          TO ztbm_check_bom-type,
            sy-uname     TO ztbm_check_bom-ernam,
            sy-datum     TO ztbm_check_bom-erdat,
            sy-uzeit     TO ztbm_check_bom-erzet,
            sy-uname     TO ztbm_check_bom-aenam,
            sy-datum     TO ztbm_check_bom-aedat,
            sy-uzeit     TO ztbm_check_bom-aezet.
    ENDIF.

    w_fsc_cnt = w_fsc_cnt + 1.

    MODIFY ztbm_check_bom.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE e000(zz) WITH text-m04.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPDATE_TABLE_FOR_FSC
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MODULE_FSC_MATNR  text
*----------------------------------------------------------------------*
FORM bom_explosion USING pw_matnr pw_stlan pw_stlal pw_werks.
  DATA: lt_stb    TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = p_datuv
            mktls                 = 'X'
            cuobj                 = '999999999999'
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = pw_stlal
            svwvo                 = 'X'
            werks                 = pw_werks
            vrsvo                 = 'X'
       IMPORTING
            topmat                = lw_topmat
       TABLES
            stb                   = lt_stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.
  IF sy-subrc <> 0 OR lw_topmat-stlal <> pw_stlal.
    EXIT.
  ENDIF.

  LOOP AT lt_stb.
    CLEAR: it_module.

    MOVE-CORRESPONDING lt_stb TO it_module.
    MOVE: lt_stb-idnrk TO it_module-matnr.

    " If the component is exist in IT_MODULE alreay,
    " skip BOM explosion.
    READ TABLE it_module WITH KEY matnr = it_module-matnr
                                  werks = it_module-werks.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    " If the component is module, stop BOM explosion
    IF pw_matnr+6(2) EQ lt_stb-idnrk(2).    "Module
      MOVE: 'Y'  TO it_module-flag,
            'M'  TO it_module-type,
            '2'  TO it_module-stlan,
            '01' TO it_module-stlal.
      APPEND it_module.
    ENDIF.

*    PERFORM bom_explosion USING lt_stb-idnrk
*                                lt_stb-stlan
*                                lt_stb-stlal
*                                lt_stb-werks.
  ENDLOOP.
ENDFORM.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Form  table_update_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM table_update_processing.
  PERFORM processing_fsc.
  PERFORM processing_module.
ENDFORM.                    " table_update_processing
*&---------------------------------------------------------------------*
*&      Form  processing_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing_fsc.
  PERFORM get_data_for_fsc.
  PERFORM update_table_for_fsc.
ENDFORM.                    " processing_fsc
*&---------------------------------------------------------------------*
*&      Form  processing_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing_module.
  PERFORM get_data_for_module.
  PERFORM update_table_for_module.
ENDFORM.                    " processing_module
*&---------------------------------------------------------------------*
*&      Form  get_data_for_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_for_module.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_module_fsc
    FROM ztbm_check_bom
   WHERE flag = 'Y'
     AND type = 'F'.

  CLEAR: it_module, it_module[].

  LOOP AT it_module_fsc.
    PERFORM bom_explosion USING it_module_fsc-matnr
                                it_module_fsc-stlan
                                it_module_fsc-stlal
                                it_module_fsc-werks.
  ENDLOOP.
ENDFORM.                    " get_data_for_module
*&---------------------------------------------------------------------*
*&      Form  update_table_for_module
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_for_module.
  DATA: l_msg(50).

  CLEAR: it_ztbm_check_bom, it_ztbm_check_bom[].

  LOOP AT it_module.
    MOVE-CORRESPONDING it_module TO it_ztbm_check_bom.
    MOVE: sy-uname TO it_ztbm_check_bom-ernam,
          sy-datum TO it_ztbm_check_bom-erdat,
          sy-uzeit TO it_ztbm_check_bom-erzet,
          sy-uname TO it_ztbm_check_bom-aenam,
          sy-datum TO it_ztbm_check_bom-aedat,
          sy-uzeit TO it_ztbm_check_bom-aezet.

    APPEND it_ztbm_check_bom.
  ENDLOOP.

  DELETE FROM ztbm_check_bom WHERE type = 'M'.

  DESCRIBE TABLE it_ztbm_check_bom LINES w_module_cnt.

  SORT it_ztbm_check_bom BY matnr stlan stlal.
  INSERT ztbm_check_bom FROM TABLE it_ztbm_check_bom
                        ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.

  CONCATENATE text-m16 w_fsc_cnt text-m17 w_module_cnt
         INTO l_msg.

  MESSAGE s000(zz) WITH text-m15 l_msg.
  COMMIT WORK AND WAIT.
ENDFORM.                    " update_table_for_module
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY matnr stlan stlal.

  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN '9100'.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
    WHEN '9200'.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9200'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  IF wc_container_9000 IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_9000.
    PERFORM set_attributes_alv_grid_9000.
    PERFORM build_field_catalog USING 'IT_9000'.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv_9000.
    PERFORM sssign_event_9000.
  ENDIF.
ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT wc_container_9000
         EXPORTING container_name = wc_control_9000
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
  CREATE OBJECT wc_alv_9000
         EXPORTING i_parent      = wc_container_9000
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_9000.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0761   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  w_repid = sy-repid.
  lw_itab = p_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ERNAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERZET'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEZET'       ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0901   text
*      -->P_0902   text
*      -->P_0903   text
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
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_9000.
  SORT it_9000 BY matnr werks stlan stlal.

  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSBM_EPP302'
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_9000[].
ENDFORM.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event_9000.
*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_double_click  FOR wc_alv_9000.
ENDFORM.                    " sssign_event_9000
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_9000 USING    p_e_column_fieldname
                             p_es_row_no_row_id.

ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'CREATE'.
      CLEAR sy-ucomm.
      PERFORM create_rtn.
    WHEN 'CHANGE'.
      CLEAR sy-ucomm.
      PERFORM change_rtn.
    WHEN 'DELETE'.
      CLEAR sy-ucomm.
      PERFORM delete_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHANGE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_rtn.
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  READ TABLE it_9000 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  CLEAR: zsbm_epp302.
  MOVE-CORRESPONDING it_9000 TO zsbm_epp302.

  SELECT SINGLE * FROM ztbm_check_bom WHERE matnr = it_9000-matnr
                                        AND werks = it_9000-werks
                                        AND stlan = it_9000-stlan
                                        AND stlal = it_9000-stlal.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

  CALL SCREEN 9200.

  PERFORM assign_itab_to_alv_9000.
ENDFORM.                    " CHANGE_RTN
*&---------------------------------------------------------------------*
*&      Form  CREATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_rtn.
  CALL SCREEN 9100.
  PERFORM assign_itab_to_alv_9000.
ENDFORM.                    " CREATE_RTN
*&---------------------------------------------------------------------*
*&      Module  chekc_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chekc_9100 INPUT.
  PERFORM check_matnr.
  PERFORM check_werks.
  PERFORM check_stlan.
  PERFORM check_stlal.
  PERFORM check_others.
ENDMODULE.                 " chekc_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr = zsbm_epp302-matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  MOVE: mara-mtart TO zsbm_epp302-mtart.

  SELECT SINGLE maktx INTO zsbm_epp302-maktx
    FROM makt
   WHERE matnr = zsbm_epp302-matnr
     AND spras = sy-langu.

  SELECT SINGLE * FROM marc WHERE matnr = zsbm_epp302-matnr
                              AND werks = zsbm_epp302-werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06 text-m18 zsbm_epp302-werks.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_werks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.
  SELECT SINGLE * FROM t001w WHERE werks = zsbm_epp302-werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.
ENDFORM.                    " check_werks
*&---------------------------------------------------------------------*
*&      Form  check_stlan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_stlan.
  SELECT SINGLE * FROM t416t WHERE stlan = zsbm_epp302-stlan
                               AND spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zzz) WITH text-m08.
  ENDIF.
ENDFORM.                    " check_stlan
*&---------------------------------------------------------------------*
*&      Form  check_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_others.
  SELECT SINGLE * FROM ztbm_check_bom WHERE matnr = zsbm_epp302-matnr
                                        AND werks = zsbm_epp302-werks
                                        AND stlan = zsbm_epp302-stlan
                                        AND stlal = zsbm_epp302-stlal.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m05.
  ENDIF.

  MOVE: mara-mtart TO zsbm_epp302-mtart,
        'Y'        TO zsbm_epp302-flag,
        sy-uname   TO zsbm_epp302-ernam,
        sy-datum   TO zsbm_epp302-erdat,
        sy-uzeit   TO zsbm_epp302-erzet,
        sy-uname   TO zsbm_epp302-aenam,
        sy-datum   TO zsbm_epp302-aedat,
        sy-uzeit   TO zsbm_epp302-aezet.

  IF mara-mtart EQ 'FERT'.
    MOVE: 'F' TO zsbm_epp302-type.
  ELSE.
    IF marc-dispo EQ 'M01'.
      MOVE: 'M' TO zsbm_epp302-type.
    ELSE.
      MOVE: ' ' TO zsbm_epp302-type.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_others
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_9100_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_9100_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_9100_rtn.
  CLEAR: ztbm_check_bom.
  MOVE: zsbm_epp302-matnr TO ztbm_check_bom-matnr,
        zsbm_epp302-werks TO ztbm_check_bom-werks,
        zsbm_epp302-stlan TO ztbm_check_bom-stlan,
        zsbm_epp302-stlal TO ztbm_check_bom-stlal,
        mara-ersda        TO ztbm_check_bom-ersda,
        zsbm_epp302-flag  TO ztbm_check_bom-flag,
        zsbm_epp302-ernam TO ztbm_check_bom-ernam,
        zsbm_epp302-erdat TO ztbm_check_bom-erdat,
        zsbm_epp302-erzet TO ztbm_check_bom-erzet,
        zsbm_epp302-aenam TO ztbm_check_bom-aenam,
        zsbm_epp302-aedat TO ztbm_check_bom-aedat,
        zsbm_epp302-aezet TO ztbm_check_bom-aezet.

  INSERT ztbm_check_bom.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ELSE.
    MESSAGE s000(zz) WITH text-m11.
    COMMIT WORK AND WAIT.
    CLEAR:it_9000.
    MOVE-CORRESPONDING zsbm_epp302 TO it_9000.
    APPEND it_9000.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " SAVE_9100_RTN
*&---------------------------------------------------------------------*
*&      Form  DELETE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_rtn.
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  READ TABLE it_9000 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  SELECT SINGLE * FROM ztbm_check_bom WHERE matnr = it_9000-matnr
                                        AND werks = it_9000-werks
                                        AND stlan = it_9000-stlan
                                        AND stlal = it_9000-stlal.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

  DELETE ztbm_check_bom.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ELSE.
    DELETE it_9000 INDEX sy-tabix.
    PERFORM assign_itab_to_alv_9000.
    MESSAGE s000(zz) WITH text-m14.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " DELETE_RTN
*&---------------------------------------------------------------------*
*&      Form  check_stlal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_stlal.
  IF zsbm_epp302-stlal IS INITIAL.
    zsbm_epp302-stlal = '0'.
  ENDIF.
ENDFORM.                    " check_stlal
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      PERFORM save_9200_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_9200_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_9200_rtn.
  MOVE: zsbm_epp302-flag  TO ztbm_check_bom-flag,
        zsbm_epp302-aenam TO ztbm_check_bom-aenam,
        zsbm_epp302-aedat TO ztbm_check_bom-aedat,
        zsbm_epp302-aezet TO ztbm_check_bom-aezet.

  UPDATE ztbm_check_bom.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ELSE.
    MESSAGE s000(zz) WITH text-m15.
    COMMIT WORK AND WAIT.

    MOVE: zsbm_epp302-flag  TO it_9000-flag,
          zsbm_epp302-aenam TO it_9000-aenam,
          zsbm_epp302-aedat TO it_9000-aedat,
          zsbm_epp302-aezet TO it_9000-aezet.

    MODIFY it_9000 TRANSPORTING flag aenam aedat aezet
     WHERE matnr = zsbm_epp302-matnr
       AND werks = zsbm_epp302-werks
       AND stlan = zsbm_epp302-stlan
       AND stlal = zsbm_epp302-stlal.

    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " save_9200_rtn
