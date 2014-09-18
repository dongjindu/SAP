************************************************************************
* Program Name      : ZEPP302E_SAP_BOM_CHECK_BK
* Author            : Bae, Byung sung
* Creation Date     : 2004.11.02.
* Specifications By : Bae, Byung sung
* Development Request No : UD1K912804
* Addl Documentation:
* Description       : Read available BOM in present visual point
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zrpp301r_cs13 NO STANDARD PAGE HEADING LINE-SIZE 255.
TABLES: mara, t416, t001w, mast, marc, cabn.

*---// Internal Tables
DATA: BEGIN OF it_mast OCCURS 0,
        matnr   LIKE   mast-matnr,
        werks   LIKE   mast-werks,
        stlan   LIKE   mast-stlan,
        stlnr   LIKE   mast-stlnr,
        stlal   LIKE   mast-stlal,
      END   OF it_mast.

DATA: it_9000 LIKE zspp_zepp302 OCCURS 0 WITH HEADER LINE.

DATA: it_send LIKE zsbm_abxbmodt_rfc OCCURS 0 WITH HEADER LINE.
DATA: it_ftp  LIKE zsbm_epp302_ftp OCCURS 0 WITH HEADER LINE.

DATA: it_ztbm_abxbmodt LIKE ztbm_abxbmodt OCCURS 0 WITH HEADER LINE.

DATA dynpread LIKE dynpread OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF valuetab OCCURS 0,
          value(80).
DATA: END OF valuetab.

DATA: BEGIN OF fields OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF fields.

DATA: BEGIN OF dynpfields  OCCURS 0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: BEGIN OF select_values OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF select_values.

DATA: it_old_file LIKE epsfili OCCURS 0 WITH HEADER LINE.

*---// Global variables and structures
DATA: w_subrc LIKE sy-subrc.             "Return value

DATA  select_index LIKE sy-tabix.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_roh     TYPE i           VALUE 1,
           c_module  TYPE i           VALUE 2,
           c_subpart TYPE i           VALUE 3,
           c_submatl LIKE stpo-zinfo  VALUE 'BULK',
           c_module_stlan LIKE mast-stlan VALUE '2',
           c_count   TYPE i           VALUE 500,
           c_dest(10)                 VALUE 'WMBOM01',
           c_datuv    LIKE sy-datum   VALUE '19000101',
           c_empty_od LIKE cukb-knnam VALUE 'Z_EMPTY_OD',
           c_dir_name LIKE epsf-epsdirnam
                      VALUE '/usr/sap/EDI_SAP/BOM_HMC/',
           c_filename LIKE rlgrap-filename VALUE
                      '/usr/sap/EDI_SAP/BOM_HMC/ABXBMODT_'.

*---// For FTP file creation
DATA: w_filename LIKE rlgrap-filename.

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
       w_it9000_cnt TYPE i.
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
SELECT-OPTIONS: s_werks FOR t001w-werks  NO-EXTENSION NO INTERVALS,
                s_stlan FOR mast-stlan   NO-EXTENSION NO INTERVALS,
                s_stlal FOR mast-stlal   NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_datuv LIKE rc29n-datuv DEFAULT sy-datum,
                p_aedat LIKE stpo-aedat  DEFAULT sy-datum.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t07.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t10.
PARAMETERS: p_chg    DEFAULT c_check AS CHECKBOX USER-COMMAND zss.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t08.
PARAMETERS: p_od    DEFAULT c_check AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t09.
PARAMETERS: p_send  DEFAULT ' '     AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t11.
PARAMETERS: p_batch DEFAULT ' '     AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN END   OF BLOCK bl2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

*---// Check input fields & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM get_bom_data.

  IF p_send EQ c_check.
    PERFORM send_rtn.
  ENDIF.

*---// Display data
START-OF-SELECTION.
  DESCRIBE TABLE it_9000 LINES w_it9000_cnt.
  IF w_it9000_cnt > 0 and P_batch = ' '.
    PERFORM display_data.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
  PERFORM check_werks.
  PERFORM check_stlan.
  PERFORM check_matnr.
  PERFORM check_date.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.
  SELECT SINGLE * FROM t001w WHERE werks IN s_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " CHECK_WERKS
*&---------------------------------------------------------------------*
*&      Form  CHECK_STLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_stlan.
  SELECT SINGLE * FROM t416 WHERE stlan IN s_stlan.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " CHECK_STLAN
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " CHECK_MATNR
*&---------------------------------------------------------------------*
*&      Form  get_bom_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_data.
  PERFORM set_target_material.
  PERFORM get_single_level_bom.
  PERFORM get_sub_item.
  PERFORM get_object_dependency.

  IF p_chg EQ c_check.
    DELETE it_9000 WHERE aedat NE p_aedat.
  ENDIF.
ENDFORM.                    " get_bom_data
*&---------------------------------------------------------------------*
*&      Form  set_target_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_target_material.
  SELECT a~matnr b~werks b~stlan b~stlnr b~stlal
    INTO TABLE it_mast
    FROM mara AS a INNER JOIN mast AS b
                      ON a~matnr = b~matnr
                   INNER JOIN ztbm_check_bom AS c
                      ON b~matnr = c~matnr
                     AND b~werks = c~werks
                     AND b~stlan = c~stlan
                     AND b~stlal = c~stlal
                     AND c~flag  = 'Y'
                   INNER JOIN stko AS d
                      ON b~stlnr = d~stlnr
                     AND b~stlal = d~stlal
                     AND d~stlty = 'M'
                     AND d~stlst = '01'
   WHERE a~mtart = 'FERT'
     AND a~matnr IN s_matnr
     AND b~werks IN s_werks
     AND b~stlan IN s_stlan
     AND b~stlal IN s_stlal
   GROUP BY A~MATNR b~werks B~STLAN b~stlnr b~stlal.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.
ENDFORM.                    " set_target_material
*&---------------------------------------------------------------------*
*&      Form  get_single_level_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_single_level_bom.
  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.

  LOOP AT it_mast.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              aumng                 = 0
              capid                 = c_capid
              cuovs                 = '0'
              datuv                 = c_datuv
*              emeng                 = p_emeng
              mktls                 = 'X'
              cuobj                 = '999999999999'
              mtnrv                 = it_mast-matnr
              stpst                 = 0
              stlan                 = it_mast-stlan
              stlal                 = it_mast-stlal
              svwvo                 = 'X'
              werks                 = it_mast-werks
              vrsvo                 = 'X'
         TABLES
              stb                   = lt_stb
*              matcat                = selpool
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
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_stb.
      IF p_chg EQ space AND lt_stb-datub <= p_datuv.
        CONTINUE.
      ENDIF.

      CHECK lt_stb-zinfo NE c_submatl.

      CLEAR: it_9000.

      MOVE: it_mast-matnr TO it_9000-matnr,
            lt_stb-werks  TO it_9000-werks,
            lt_stb-stlan  TO it_9000-stlan,
            lt_stb-stlal  TO it_9000-stlal,
            lt_stb-posnr  TO it_9000-posnr,
            lt_stb-idnrk  TO it_9000-idnrk,
            lt_stb-suff   TO it_9000-suff,
            lt_stb-aennr  TO it_9000-aennr,
            1             TO it_9000-bmeng,
            'EA'          TO it_9000-bmein,
            '01'          TO it_9000-stlst,
            lt_stb-postp  TO it_9000-postp,
            lt_stb-menge  TO it_9000-menge,
            lt_stb-stgb   TO it_9000-stgb,
            lt_stb-meins  TO it_9000-meins,
            lt_stb-sobsl  TO it_9000-sobsl,
            lt_stb-eitm   TO it_9000-eitm,
            lt_stb-datuv  TO it_9000-datuv,
            lt_stb-datub  TO it_9000-datub,
            lt_stb-sortf  TO it_9000-sortf,
            lt_stb-knobj  TO it_9000-knobj,
            lt_stb-mtart  TO it_9000-mtart,
            lt_stb-aedat  TO it_9000-aedat.

      PERFORM check_od_value USING lt_stb-knobj w_subrc.

      CHECK w_subrc EQ 0.

      APPEND it_9000.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_single_level_bom
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY matnr idnrk seqc.
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
*      -->P_0625   text
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
                                  'E' 'KEY'         'X'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_9000.
*  SORT it_9000 BY flag matnr.

  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSPP_ZEPP302'
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0771   text
*      -->P_0772   text
*      -->P_0773   text
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
*&      Form  get_sub_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_item.
  DATA: lt_one_level LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  lt_one_level[] = it_9000[].

  LOOP AT lt_one_level.

    IF lt_one_level-mtart EQ 'ROH' OR
       lt_one_level-mtart EQ 'ROH1'.
      SELECT SINGLE * FROM marc WHERE matnr = lt_one_level-idnrk
                                  AND werks = lt_one_level-werks.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      IF marc-dispo = 'M01'.         "Module
        PERFORM bom_explosion USING lt_one_level-idnrk c_module_stlan
                                    lt_one_level-werks.
      ENDIF.
    ELSE.
      PERFORM bom_explosion USING lt_one_level-idnrk lt_one_level-stlan
                                  lt_one_level-werks.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_sub_item
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ONE_LEVEL_MATNR  text
*----------------------------------------------------------------------*
FORM bom_explosion USING pw_matnr pw_stlan pw_werks.
  DATA: lt_stb    TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.

  READ TABLE it_9000 WITH KEY matnr = pw_matnr
                              werks = pw_werks
                              stlan = pw_stlan
                              stlal = '01'.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = c_datuv
*              emeng                 = p_emeng
            mktls                 = 'X'
            cuobj                 = '999999999999'
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = '01'
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
  IF sy-subrc <> 0 OR lw_topmat-stlal <> '01'.
    EXIT.
  ENDIF.

  LOOP AT lt_stb.

    IF p_chg EQ space AND lt_stb-datub <= p_datuv.
      CONTINUE.
    ENDIF.

    READ TABLE it_9000 WITH KEY matnr = pw_matnr
                                idnrk = lt_stb-idnrk.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    CLEAR: it_9000.

    MOVE: pw_matnr      TO it_9000-matnr,
          lt_stb-werks  TO it_9000-werks,
          lt_stb-stlan  TO it_9000-stlan,
          lt_stb-stlal  TO it_9000-stlal,
          lt_stb-posnr  TO it_9000-posnr,
          lt_stb-idnrk  TO it_9000-idnrk,
          lt_stb-suff   TO it_9000-suff,
          lt_stb-aennr  TO it_9000-aennr,
          1             TO it_9000-bmeng,
          'EA'          TO it_9000-bmein,
          '01'          TO it_9000-stlst,
          lt_stb-postp  TO it_9000-postp,
          lt_stb-menge  TO it_9000-menge,
          lt_stb-stgb   TO it_9000-stgb,
          lt_stb-meins  TO it_9000-meins,
          lt_stb-sobsl  TO it_9000-sobsl,
          lt_stb-eitm   TO it_9000-eitm,
          lt_stb-datuv  TO it_9000-datuv,
          lt_stb-datub  TO it_9000-datub,
          lt_stb-sortf  TO it_9000-sortf,
          lt_stb-knobj  TO it_9000-knobj,
          lt_stb-mtart  TO it_9000-mtart,
          lt_stb-aedat  TO it_9000-aedat.

    PERFORM check_od_value USING lt_stb-knobj w_subrc.

    CHECK w_subrc EQ 0.

    APPEND it_9000.

    SELECT SINGLE * FROM marc WHERE matnr = lt_stb-idnrk
                                AND werks = lt_stb-werks.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    IF marc-dispo = 'M01'.         "Module
      CLEAR: mast.
      SELECT SINGLE *
        FROM mast
       WHERE matnr EQ lt_stb-idnrk
         AND werks EQ lt_stb-werks
         AND stlan EQ lt_stb-stlan
         AND stlal EQ '01'.
      IF sy-subrc EQ 0 AND NOT mast-stlnr IS INITIAL.
        PERFORM bom_explosion USING lt_stb-idnrk c_module_stlan
                                    lt_stb-werks.
      ENDIF.
    ELSE.
      CLEAR: mast.
      SELECT SINGLE *
        FROM mast
       WHERE matnr EQ lt_stb-idnrk
         AND werks EQ lt_stb-werks
         AND stlan EQ lt_stb-stlan
         AND stlal EQ '01'.
      IF sy-subrc EQ 0 AND NOT mast-stlnr IS INITIAL.
        PERFORM bom_explosion USING lt_stb-idnrk pw_stlan lt_stb-werks.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_object_dependency.
  CHECK p_od EQ c_check.

  PERFORM get_all_dependency.
ENDFORM.                    " GET_OBJECT_DEPENDENCY


*&---------------------------------------------------------------------*
*&      Form  GET_ALL_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_dependency.
  DATA: lw_seqc LIKE sy-tabix.

  DATA: lt_9000_tmp LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  DATA: lt_cukb LIKE cukb OCCURS 0 WITH HEADER LINE.

  lt_9000_tmp[] = it_9000[].

  SORT it_9000 BY matnr idnrk.

  LOOP AT lt_9000_tmp WHERE knobj > 0.

    SELECT * INTO TABLE lt_cuob
      FROM cuob
     WHERE kntab =  'STPO'
       AND knobj =  lt_9000_tmp-knobj
       AND datuv <= p_datuv.
    IF sy-subrc NE 0.
      MOVE: '9999999999' TO lt_cuob-knnum.
      APPEND lt_cuob.
    ENDIF.

    SELECT * INTO TABLE lt_cukb
      FROM cukb
       FOR ALL ENTRIES IN lt_cuob
     WHERE knnum =  lt_cuob-knnum
       AND adzhl =  lt_cuob-adzhl
       AND datuv <= p_datuv.

    LOOP AT lt_cukb WHERE NOT knnam EQ c_empty_od.
      CLEAR: it_9000.

      MOVE: sy-tabix TO lw_seqc.

      IF sy-tabix EQ 1.
        READ TABLE it_9000 WITH KEY lt_9000_tmp.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        MOVE: lt_cukb-knnam TO it_9000-knnam,
              lw_seqc       TO it_9000-seqc.

        MODIFY it_9000 INDEX sy-tabix.
      ELSE.
        MOVE: lt_9000_tmp   TO it_9000,
              lt_cukb-knnam TO it_9000-knnam,
              lw_seqc       TO it_9000-seqc.
        APPEND it_9000.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_ALL_DEPENDENCY
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
    WHEN 'SEND'.
      CLEAR sy-ucomm.
      PERFORM send_rtn.
      PERFORM assign_itab_to_alv_9000.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_P_ATWRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_p_atwre.
  DATA it_ksml   LIKE TABLE OF ksml  WITH HEADER LINE.
  DATA : BEGIN OF it_cawn OCCURS 0,
          atwrt   LIKE  cawn-atwrt,
          atwtb   LIKE  cawnt-atwtb.
  DATA : END OF it_cawn.

  DATA :  l_cuobj   LIKE  inob-cuobj,
          l_clint   LIKE  klah-clint.

  READ TABLE s_matnr INDEX 2.
  CHECK sy-subrc NE 0.

  CHECK NOT ( s_matnr-low NE ' ' AND s_matnr-high NE ' ' ).

  CLEAR: mara.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  CHECK mara-mtart EQ 'FERT'.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: 'S_MATNR-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_matnr-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE cuobj
         INTO l_cuobj
         FROM inob
         WHERE klart EQ '300'
           AND obtab EQ 'MARA'
           AND objek EQ s_matnr-low.

  SELECT SINGLE clint
         INTO l_clint
         FROM kssk
         WHERE objek EQ l_cuobj
           AND mafid EQ 'O'
           AND klart EQ '300'.

  SELECT *
         INTO TABLE it_ksml
         FROM ksml
         WHERE clint EQ l_clint.

  DATA l_tabix   LIKE sy-tabix.
  LOOP AT it_ksml.
    l_tabix = sy-tabix.
    SELECT SINGLE *
              FROM cabn
              WHERE atinn EQ it_ksml-imerk
                AND atnam EQ 'COLOREXT'.
    IF sy-subrc NE 0.
      DELETE it_ksml INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE it_ksml INDEX 1.
  SELECT a~atwrt
         b~atwtb
         INTO TABLE it_cawn
         FROM cawn AS a INNER JOIN cawnt AS b
                        ON  a~atinn EQ b~atinn
                        AND a~atzhl EQ b~atzhl
         WHERE a~atinn EQ it_ksml-omerk.
  SORT it_cawn.
  it_cawn-atwrt = 'No entry'.
  INSERT it_cawn INDEX 1.
  CLEAR: it_cawn.
  LOOP AT it_cawn.
    valuetab-value = it_cawn-atwrt.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = it_cawn-atwtb.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'CAWN'  'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM help_values_get.

  IF select_index > 0.
    READ TABLE it_cawn   INDEX select_index.
    PERFORM value_update USING:
            'X'   'P_ATWRE' it_cawn-atwrt 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_P_ATWRE
*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1975   text
*----------------------------------------------------------------------*
FORM value_read USING  p_name.
  dynpread-fieldname = p_name.
  APPEND dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpread
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            invalid_parameter    = 7
            undefind_error       = 8
            double_conversion    = 9
            OTHERS               = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2222   text
*      -->P_2223   text
*      -->P_2224   text
*----------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  APPEND fields.      CLEAR fields.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_values_get.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display                   = ' '
       IMPORTING
            index                     = select_index
       TABLES
            fields                    = fields
            select_values             = select_values
            valuetab                  = valuetab
       EXCEPTIONS
            field_not_in_ddic         = 1
            more_then_one_selectfield = 2
            no_selectfield            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2256   text
*      -->P_2257   text
*      -->P_IT_CAWN_ATWRT  text
*      -->P_0      text
*----------------------------------------------------------------------*
FORM value_update USING  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
  CLEAR dynpfields.
  dynpfields-fieldname  = p_fieldname.
  dynpfields-fieldvalue = p_fieldvalue.
  IF p_stepl > 0.
    dynpfields-stepl = p_stepl.
  ENDIF.
  APPEND dynpfields.      CLEAR dynpfields.

  IF p_process EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              dyname               = sy-cprog
              dynumb               = sy-dynnr
         TABLES
              dynpfields           = dynpfields
         EXCEPTIONS
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              OTHERS               = 8.
    REFRESH dynpfields.
  ENDIF.
ENDFORM.                    " VALUE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_P_ATWRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_p_atwri.
  DATA it_ksml   LIKE TABLE OF ksml  WITH HEADER LINE.
  DATA : BEGIN OF it_cawn OCCURS 0,
          atwrt   LIKE  cawn-atwrt,
          atwtb   LIKE  cawnt-atwtb.
  DATA : END OF it_cawn.

  DATA :  l_cuobj   LIKE  inob-cuobj,
          l_clint   LIKE  klah-clint.

  READ TABLE s_matnr INDEX 2.
  CHECK sy-subrc NE 0.

  CHECK NOT ( s_matnr-low NE ' ' AND s_matnr-high NE ' ' ).

  CLEAR: mara.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  CHECK mara-mtart EQ 'FERT'.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: 'S_MATNR-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_matnr-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE cuobj
         INTO l_cuobj
         FROM inob
         WHERE klart EQ '300'
           AND obtab EQ 'MARA'
           AND objek EQ s_matnr-low.

  SELECT SINGLE clint
         INTO l_clint
         FROM kssk
         WHERE objek EQ l_cuobj
           AND mafid EQ 'O'
           AND klart EQ '300'.

  SELECT *
         INTO TABLE it_ksml
         FROM ksml
         WHERE clint EQ l_clint.

  DATA l_tabix   LIKE sy-tabix.
  LOOP AT it_ksml.
    l_tabix = sy-tabix.
    SELECT SINGLE *
              FROM cabn
              WHERE atinn EQ it_ksml-imerk
                AND atnam EQ 'COLORINT'.
    IF sy-subrc NE 0.
      DELETE it_ksml INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE it_ksml INDEX 1.
  SELECT a~atwrt
         b~atwtb
         INTO TABLE it_cawn
         FROM cawn AS a INNER JOIN cawnt AS b
                        ON  a~atinn EQ b~atinn
                        AND a~atzhl EQ b~atzhl
         WHERE a~atinn EQ it_ksml-omerk.
  SORT it_cawn.
  it_cawn-atwrt = 'No entry'.
  INSERT it_cawn INDEX 1.
  CLEAR: it_cawn.
  LOOP AT it_cawn.
    valuetab-value = it_cawn-atwrt.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = it_cawn-atwtb.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'CAWN' 'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM help_values_get.

  IF select_index > 0.
    READ TABLE it_cawn   INDEX select_index.
    PERFORM value_update USING:
            'X'   'P_ATWRI' it_cawn-atwrt 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_P_ATWRI
*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_rtn.
  PERFORM set_sending_data.
*  PERFORM send_to_eai.
  PERFORM send_by_ftp.
  PERFORM update_table.
ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_SENDING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sending_data.
  CLEAR: it_send, it_send[].
  LOOP AT it_9000.
    CLEAR: it_send.
    MOVE: it_9000-matnr TO it_send-mtno,
          it_9000-werks TO it_send-plnt,
          it_9000-stlan TO it_send-usag,
          it_9000-stlal TO it_send-altn,
          it_9000-posnr TO it_send-pref,
          it_9000-idnrk TO it_send-comp,
          it_9000-suff  TO it_send-suff,
          it_9000-seqc  TO it_send-seqc,
          it_9000-aennr TO it_send-eono,
          it_9000-bmeng TO it_send-bqty,
          it_9000-bmein TO it_send-hunt,
          it_9000-stlst TO it_send-stat,
          it_9000-postp TO it_send-itca,
          it_9000-menge TO it_send-qnty,
          it_9000-stgb  TO it_send-stgb,
          it_9000-meins TO it_send-unit,
          it_9000-sobsl TO it_send-sppr,
          it_9000-eitm  TO it_send-eitm,
          it_9000-knnam TO it_send-dpid,
          it_9000-datuv TO it_send-datuv,
          it_9000-datub TO it_send-datub,
          sy-uname      TO it_send-zuser,
          sy-datum      TO it_send-zsdat,
          sy-uzeit      TO it_send-zstim,
          sy-datum      TO it_send-zedat,
          sy-uzeit      TO it_send-zetim,
          'C'           TO it_send-zmode,
          'S'           TO it_send-zzret.
    APPEND it_send.
  ENDLOOP.
ENDFORM.                    " SET_SENDING_DATA
*&---------------------------------------------------------------------*
*&      Form  send_to_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_to_eai.
  DATA: lw_from     LIKE sy-index,
        lw_to       LIKE sy-index,
        lw_time(9)  TYPE n,
        lw_curr(9)  TYPE n,
        lw_remain   TYPE p DECIMALS 5,
        lw_lines    TYPE i,
        lw_msg(200).

  DATA: lt_packet LIKE it_send OCCURS 0 WITH HEADER LINE.

  DESCRIBE TABLE it_send LINES lw_lines.

  lw_time   = trunc( lw_lines / c_count ).
  lw_remain = lw_lines MOD c_count.
  IF lw_remain > 0.
    lw_time = lw_time + 1.
  ENDIF.

  DO lw_time TIMES.
    lw_from = ( sy-index - 1 ) * c_count + 1.
    lw_to   = lw_from + c_count - 1.
    IF lw_to > lw_lines.
      lw_to = lw_lines.
    ENDIF.

    MOVE: sy-index TO lw_curr.

    CLEAR: lt_packet, lt_packet[].
    LOOP AT it_send FROM lw_from TO lw_to.
      MOVE: it_send TO lt_packet.
      APPEND lt_packet.
    ENDLOOP.

    CALL FUNCTION 'Z_FBM_ABXBMODT'
      DESTINATION  c_dest
      IMPORTING
        total_c  = lw_time
        curr_c   = lw_curr
      TABLES
        t_abxbmodt       = lt_packet
   EXCEPTIONS
    communication_failure = 1  MESSAGE lw_msg
    system_failure        = 2  MESSAGE lw_msg.

    IF sy-subrc NE 0.
      LOOP AT lt_packet.
        CLEAR: it_send.
        READ TABLE it_send WITH KEY mtno = lt_packet-mtno
                                    plnt = lt_packet-plnt
                                    usag = lt_packet-usag
                                    altn = lt_packet-altn
                                    pref = lt_packet-pref
                                    comp = lt_packet-comp
                                    suff = lt_packet-suff
                                    seqc = lt_packet-seqc.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        MOVE: lw_msg TO it_send-zmsg,
              'E'    TO it_send-zzret.

        MODIFY it_send INDEX sy-tabix.
      ENDLOOP.
    ELSE.
      LOOP AT lt_packet.
        CLEAR: it_send.
        READ TABLE it_send WITH KEY mtno = lt_packet-mtno
                                    plnt = lt_packet-plnt
                                    usag = lt_packet-usag
                                    altn = lt_packet-altn
                                    pref = lt_packet-pref
                                    comp = lt_packet-comp
                                    suff = lt_packet-suff
                                    seqc = lt_packet-seqc.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        MOVE: lt_packet-zmsg  TO it_send-zmsg,
              lt_packet-zzret TO it_send-zzret.

        MODIFY it_send INDEX sy-tabix.
      ENDLOOP.
    ENDIF.
  ENDDO.
ENDFORM.                    " send_to_eai
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  CLEAR: it_ztbm_abxbmodt, it_ztbm_abxbmodt[].

  LOOP AT it_send.
    CLEAR: it_ztbm_abxbmodt.
    MOVE-CORRESPONDING it_send TO it_ztbm_abxbmodt.
    MOVE: it_send-zzret TO it_ztbm_abxbmodt-zresult.
    APPEND it_ztbm_abxbmodt.
  ENDLOOP.

  DELETE FROM ztbm_abxbmodt WHERE mtno >= ''.

  INSERT ztbm_abxbmodt FROM TABLE it_ztbm_abxbmodt
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc EQ 0.
    PERFORM update_screen.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  READ TABLE it_9000 WITH KEY zzret = 'E'.
  IF sy-subrc EQ 0.
    MESSAGE s000(zz) WITH text-m10.
  ELSE.
    MESSAGE s000(zz) WITH text-m11.
  ENDIF.

ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  update_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_screen.
  LOOP AT it_send.
    READ TABLE it_9000 WITH KEY matnr = it_send-mtno
                                werks = it_send-plnt
                                stlan = it_send-usag
                                stlal = it_send-altn
                                posnr = it_send-pref
                                idnrk = it_send-comp
                                suff  = it_send-suff
                                seqc  = it_send-seqc.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.
    MOVE: it_send-zzret TO it_9000-zzret,
          it_send-zmsg  TO it_9000-msg.
    MODIFY it_9000 TRANSPORTING zzret msg WHERE matnr = it_send-mtno
                                            AND werks = it_send-plnt
                                            AND stlan = it_send-usag
                                            AND stlal = it_send-altn
                                            AND posnr = it_send-pref
                                            AND idnrk = it_send-comp
                                            AND suff  = it_send-suff
                                            AND seqc  = it_send-seqc.

  ENDLOOP.
ENDFORM.                    " update_screen
*&---------------------------------------------------------------------*
*&      Form  send_by_FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_by_ftp.
  PERFORM delete_old_file.
  PERFORM write_file.
ENDFORM.                    " send_by_FTP
*&---------------------------------------------------------------------*
*&      Form  check_od_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_KNOBJ  text
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM check_od_value USING pw_knobj pw_subrc.
  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  DATA: lt_cukb LIKE cukb OCCURS 0 WITH HEADER LINE.

  CLEAR: pw_subrc.

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  pw_knobj
     AND datuv <= p_datuv.
  IF sy-subrc NE 0.
    MOVE: '9999999999' TO lt_cuob-knnum.
    APPEND lt_cuob.
  ENDIF.

  SELECT * INTO TABLE lt_cukb
    FROM cukb
     FOR ALL ENTRIES IN lt_cuob
   WHERE knnum =  lt_cuob-knnum
     AND adzhl =  lt_cuob-adzhl
     AND datuv <= p_datuv.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  LOOP AT lt_cukb WHERE NOT knnam EQ c_empty_od.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: 4 TO pw_subrc.
  ENDIF.
ENDFORM.                    " check_od_value
*&---------------------------------------------------------------------*
*&      Form  delete_old_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_old_file.
  DATA: lw_filename LIKE epsf-epsfilnam.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
       EXPORTING
            dir_name               = c_dir_name
       TABLES
            dir_list               = it_old_file
       EXCEPTIONS
            invalid_eps_subdir     = 1
            sapgparam_failed       = 2
            build_directory_failed = 3
            no_authorization       = 4
            read_directory_failed  = 5
            too_many_read_errors   = 6
            empty_directory_list   = 7
            OTHERS                 = 8.
  CASE sy-subrc.
    WHEN 0 OR 7.
      LOOP AT it_old_file WHERE name CP 'ABXBMODT_*'.
        CLEAR: lw_filename.

        CALL FUNCTION 'EPS_DELETE_FILE'
             EXPORTING
                  file_name              = it_old_file-name
                  dir_name               = c_dir_name
             EXCEPTIONS
                  invalid_eps_subdir     = 1
                  sapgparam_failed       = 2
                  build_directory_failed = 3
                  no_authorization       = 4
                  build_path_failed      = 5
                  delete_failed          = 6
                  OTHERS                 = 7.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      ENDLOOP.
    WHEN OTHERS.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDCASE.
ENDFORM.                    " delete_old_file
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_file.
  DATA: lw_mod      TYPE i,
        lw_index(2) TYPE n.

  CLEAR: it_ftp, it_ftp[].

  LOOP AT it_send.
    CLEAR: it_ftp.

    lw_mod = sy-tabix MOD 5000.

    IF lw_mod EQ 0.
      CLEAR: w_filename.

      lw_index = lw_index + 1.

      CONCATENATE c_filename lw_index '.txt'
             INTO w_filename.

      OPEN DATASET w_filename IN TEXT MODE FOR OUTPUT.
      IF sy-subrc <> 0.
        MESSAGE e000(zz) WITH text-m12.
      ENDIF.

      LOOP AT it_ftp.
        OPEN DATASET w_filename IN TEXT MODE FOR APPENDING.
        TRANSFER it_ftp TO w_filename.
      ENDLOOP.

      CLOSE DATASET w_filename.

      CLEAR: it_ftp, it_ftp[].
    ENDIF.

    MOVE-CORRESPONDING it_send TO it_ftp.

    APPEND it_ftp.
  ENDLOOP.

  lw_index = lw_index + 1.

  CLEAR: w_filename.

  CONCATENATE c_filename lw_index '.txt'
         INTO w_filename.

  OPEN DATASET w_filename IN TEXT MODE FOR OUTPUT.
  IF sy-subrc <> 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  LOOP AT it_ftp.
    OPEN DATASET w_filename IN TEXT MODE FOR APPENDING.
    TRANSFER it_ftp TO w_filename.
  ENDLOOP.

  CLOSE DATASET w_filename.

  CLEAR: it_ftp, it_ftp[].
ENDFORM.                    " WRITE_FILE
*&---------------------------------------------------------------------*
*&      Form  check_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_date.
  IF p_chg EQ space.
    IF p_datuv IS INITIAL.
      MESSAGE e000(zz) WITH text-m14.
    ENDIF.

    CLEAR: p_aedat.
  ELSE.
    IF p_aedat IS INITIAL.
      MESSAGE e000(zz) WITH text-m13.
    ENDIF.

    CLEAR: p_datuv.
  ENDIF.
ENDFORM.                    " check_date
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  CASE p_chg.
    WHEN c_check.
      LOOP AT SCREEN.
        IF screen-name = 'P_DATUV'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      CLEAR: p_datuv.
      IF p_aedat IS INITIAL.
        MOVE: sy-datum TO p_aedat.
      ENDIF.
    WHEN space.
      LOOP AT SCREEN.
        IF screen-name = 'P_AEDAT'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      CLEAR: p_aedat.
      IF p_datuv IS INITIAL.
        MOVE: sy-datum TO p_datuv.
      ENDIF.
  ENDCASE.
ENDFORM.                    " screen_modify
