************************************************************************
* Program Name      : ZEMM_UPDATE_FSC_COLOR
* Author            : Byung Sung Bae
* Creation Date     : 2005.04.07.
* Specifications By : Byung Sung Bae12
* Development Request No : UD1K915443
* Addl Documentation:
* Description       : Update Material Master Color
* Modification Logs
* Date            Developer        RequestNo      Description
*
*
************************************************************************
REPORT zemm_update_fsc_color .
INCLUDE <icon>.

TABLES: mara, plaf.
DATA: zsmm_fsc_color_9000 LIKE zsmm_fsc_color_9000.

*---// Internal tables
DATA: it_9000   LIKE zsmm_fsc_color_9000 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

*---// Global variables & Structures
DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

DATA: w_success(4)   TYPE   n,
      w_error(4)     TYPE   n,
      w_ready(4)     TYPE   n,
      w_finish(4)    TYPE   n,
      w_total(4)     TYPE   n.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_success(4)               VALUE icon_green_light,
           c_finish(4)                VALUE icon_light_out,
           c_ready(4)                 VALUE icon_yellow_light,
           c_error(4)                 VALUE icon_red_light,
           c_flag_err                 VALUE 1,
           c_flag_rea                 VALUE 2,
           c_flag_suc                 VALUE 3,
           c_flag_fin                 VALUE 4.

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

CONSTANTS: c_structure(100) VALUE 'ZSMM_FSC_COLOR_'.

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
*  PUBLIC SECTION.
*    METHODS:
*
*    handle_double_click
*        FOR EVENT double_click OF cl_gui_alv_grid
*            IMPORTING e_row
*                      e_column
*                      es_row_no.
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

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
                p_datum LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS: s_matnr FOR  mara-matnr.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_update  DEFAULT ' '     AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END OF BLOCK bl1.

*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM check_color.
  IF p_update EQ 'X'.
    PERFORM update_rtn.
  ENDIF.
  PERFORM count_rtn.
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
  PERFORM get_fsc_color_code.
  PERFORM get_planned_order_color.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  GET_FSC_COLOR_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_color_code.
  DATA: BEGIN OF lt_fsc_color OCCURS 0,
          matnr   LIKE   mara-matnr,
          maktx   LIKE   makt-maktx,
          atnam   LIKE   cabn-atnam,
          atwrt   LIKE   ausp-atwrt,
        END   OF lt_fsc_color.

  SELECT a~matnr b~maktx e~atnam d~atwrt
    INTO CORRESPONDING FIELDS OF TABLE lt_fsc_color
    FROM marc AS a      INNER JOIN makt AS b
                           ON a~matnr = b~matnr
                          AND b~spras = sy-langu
                   LEFT OUTER JOIN ibin AS c
                           ON a~cuobj    = c~instance
                        INNER JOIN v_ibin_syval AS d
                           ON c~in_recno = d~in_recno
                        INNER JOIN cabn AS e
                           ON d~atinn    = e~atinn
                          AND e~adzhl    = '0000'
   WHERE a~matnr IN s_matnr
     AND a~werks EQ p_werks
     AND e~atnam IN ('COLOREXT','COLORINT').

  LOOP AT lt_fsc_color.
    CLEAR: it_9000.

    READ TABLE it_9000 WITH KEY matnr = lt_fsc_color-matnr.
    IF sy-subrc EQ 0.
      CASE lt_fsc_color-atnam.
        WHEN 'COLOREXT'.
          MOVE: lt_fsc_color-atwrt TO it_9000-maext.
        WHEN 'COLORINT'.
          MOVE: lt_fsc_color-atwrt TO it_9000-maint.
      ENDCASE.

      MODIFY it_9000 INDEX sy-tabix.
    ELSE.
      MOVE: lt_fsc_color-matnr TO it_9000-matnr,
            lt_fsc_color-maktx TO it_9000-maktx.

      CASE lt_fsc_color-atnam.
        WHEN 'COLOREXT'.
          MOVE: lt_fsc_color-atwrt TO it_9000-maext.
        WHEN 'COLORINT'.
          MOVE: lt_fsc_color-atwrt TO it_9000-maint.
      ENDCASE.

      APPEND it_9000.
    ENDIF.
  ENDLOOP.

  SORT it_9000 BY matnr.
ENDFORM.                    " GET_FSC_COLOR_CODE
*&---------------------------------------------------------------------*
*&      Form  get_planned_order_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_planned_order_color.
  DATA: lw_atnam   LIKE   cabn-atnam,
        lw_atwrt   LIKE   ausp-atwrt.

  LOOP AT it_9000.
    CLEAR: plaf.

    SELECT SINGLE * FROM plaf WHERE matnr = it_9000-matnr
                                AND plwrk = p_werks
                                AND cuobj > 0.
    IF sy-subrc EQ 0.
      MOVE: plaf-plnum TO it_9000-plnum.

      SELECT c~atnam b~atwrt
        INTO (lw_atnam, lw_atwrt)
        FROM ibin AS a INNER JOIN v_ibin_syval AS b
                          ON a~in_recno = b~in_recno
                       INNER JOIN cabn AS c
                          ON b~atinn    = c~atinn
                         AND c~adzhl    = '0000'
       WHERE a~instance EQ plaf-cuobj
         AND c~atnam    IN ('COLOREXT','COLORINT').

        CASE lw_atnam.
          WHEN 'COLOREXT'.
            MOVE: lw_atwrt TO it_9000-poext.
          WHEN 'COLORINT'.
            MOVE: lw_atwrt TO it_9000-point.
        ENDCASE.
      ENDSELECT.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " get_planned_order_color
*&---------------------------------------------------------------------*
*&      Form  CHECK_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_color.
  LOOP AT it_9000.
    IF IT_9000-PLNUM IS INITIAL.
      MOVE: c_error  TO it_9000-icon,
            text-m04 TO it_9000-msg.
      MODIFY it_9000. CONTINUE.
    ENDIF.

    IF it_9000-poext IS INITIAL.
      MOVE: c_error  TO it_9000-icon,
            text-m02 TO it_9000-msg.
      MODIFY it_9000. CONTINUE.
    ENDIF.

    IF it_9000-point IS INITIAL.
      MOVE: c_error  TO it_9000-icon,
            text-m03 TO it_9000-msg.
      MODIFY it_9000. CONTINUE.
    ENDIF.

    IF it_9000-maext NE it_9000-poext OR
       it_9000-maint NE it_9000-point.
      MOVE: c_ready  TO it_9000-icon,
            text-m03 TO it_9000-msg.
      MODIFY it_9000. CONTINUE.
    ENDIF.

    MOVE: c_finish TO it_9000-icon.
    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " CHECK_COLOR
*&---------------------------------------------------------------------*
*&      Form  count_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn.
  CLEAR: w_total, w_success, w_ready, w_finish, w_error.

  LOOP AT it_9000.
    w_total = w_total + 1.

    CASE it_9000-icon.
      WHEN c_success.
        w_success = w_success + 1.
        MOVE: c_flag_suc TO it_9000-flag.
      WHEN c_ready.
        w_ready = w_ready + 1.
        MOVE: c_flag_rea TO it_9000-flag.
      WHEN c_finish.
        w_finish = w_finish + 1.
        MOVE: c_flag_fin TO it_9000-flag.
      WHEN c_error.
        w_error = w_error + 1.
        MOVE: c_flag_err TO it_9000-flag.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " count_rtn
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY icon matnr.
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
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " create_alv_object  OUTPUT
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
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'UPDATE'.
      CLEAR: sy-ucomm.
      PERFORM update_rtn.
      PERFORM count_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_rtn.
  LOOP AT it_9000 WHERE icon EQ c_ready
                     OR icon EQ c_error.
    CHECK it_9000-poext NE space AND it_9000-point NE space.

    PERFORM generate_bdc.

    CALL TRANSACTION 'MM02'  USING it_bdc
                             OPTIONS FROM wa_opt.
    IF sy-subrc NE 0 OR sy-msgno NE '801'.
      MOVE: c_error             TO it_9000-icon.
      PERFORM get_err_msg USING it_9000-msg.
    ELSE.
      MOVE: c_success           TO it_9000-icon,
            space               TO it_9000-msg,
            it_9000-poext       TO it_9000-maext,
            it_9000-point       TO it_9000-maint.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.

  SORT it_9000 BY flag matnr.
ENDFORM.                    " UPDATE_RTN
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
    PERFORM sssign_event.
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
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
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
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0954   text
*      -->P_0955   text
*      -->P_0956   text
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
FORM sssign_event.

ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  generate_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc.
  REFRESH: it_bdc.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'             '0060',
     ' ' 'RMMG1-MATNR'          it_9000-matnr,
     ' ' 'BDC_OKCODE'           '=AUSW',

     'X' 'SAPLMGMM'             '0070',
     ' ' 'MSICHTAUSW-KZSEL(11)' 'X',
     ' ' 'BDC_OKCODE'           '/00',

     'X' 'SAPLMGMM'             '0080',
     ' ' 'RMMG1-WERKS'          p_werks,
     ' ' 'BDC_OKCODE'           '/00',

     'X' 'SAPLMGMM'             '5000',
     ' ' 'BDC_OKCODE'           '=PB19',

     'X' 'SAPLCEI0'             '0109',
     ' ' 'RCTMS-MNAME(01)'      'COLOREXT',
     ' ' 'RCTMS-MWERT(01)'      it_9000-poext,
     ' ' 'RCTMS-MNAME(02)'      'COLORINT',
     ' ' 'RCTMS-MWERT(02)'      it_9000-point,
     ' ' 'BDC_OKCODE'           '=BACK',

     'X' 'SAPLMGMM'             '5000',
     ' ' 'BDC_OKCODE'           '=BU'.
ENDFORM.                    " generate_bdc
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1155   text
*      -->P_1156   text
*      -->P_1157   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name     TO it_bdc-program,
          value    TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name  TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000_MSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " initialization
