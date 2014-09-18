************************************************************************
* Program Name      : ZAPP722_DELETE_BOM
* Author            : Bae, Byung sung
* Creation Date     : 2004.11.01.
* Specifications By : Bae, Byung sung
* Development Request No : UD1K912770
* Addl Documentation:
* Description       : Delete BOM Structure for 1 level
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT  zapp722_delete_bom.
TABLES: mast, mara, stpo, T001W, T416T.

DATA: it_9000 LIKE zspp_zapp702 OCCURS 0 WITH HEADER LINE,
      it_log  LIKE ztpp_bom_del_log OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.

*---// Global variables and structures
DATA: w_total(4)   TYPE N,
      w_success(4) TYPE N,
      w_fail(4)    TYPE N.

*---// Constants
CONSTANTS: c_check                    VALUE 'X'.

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
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
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
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY MEMORY ID wrk,
            p_stlan LIKE mast-stlan  OBLIGATORY MEMORY ID csv.

SELECT-OPTIONS: s_matnr FOR mara-matnr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Check input fields & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM get_bom_data.

START-OF-SELECTION.
  PERFORM delete_bom.
  PERFORM create_log.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  check_input_value
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
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  get_bom_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_9000
    FROM mast AS a INNER JOIN stko AS b
                      ON a~stlnr = b~stlnr
                     AND a~stlal = b~stlal
                     AND b~stlty = 'M'
                   INNER JOIN makt AS c
                      ON a~matnr = c~matnr
                     AND c~spras = sy-langu
   WHERE a~werks EQ p_werks
     AND a~stlan EQ p_stlan
     AND a~matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  DESCRIBE TABLE it_9000 LINES w_total.
ENDFORM.                    " get_bom_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_bom.
  LOOP AT it_9000.

    PERFORM generate_bdc_data.

    CALL TRANSACTION 'CS02' USING  bdc_tab
                             MODE  'N'
                           UPDATE  'S'.
    IF sy-subrc NE 0 OR NOT ( sy-msgno EQ '032' OR sy-msgno EQ '034' ).
      PERFORM get_error_msg.
      w_fail = w_fail + 1.
    ELSE.
      MOVE: text-m03 TO it_9000-msg.
      w_success = w_success + 1.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " DELETE_BOM
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_data.
  DATA: lw_screen(4).

  REFRESH bdc_tab.

  SELECT SINGLE * FROM stpo WHERE stlty = it_9000-stlty
                              AND stlnr = it_9000-stlnr.
  IF sy-subrc EQ 0.
    lw_screen = '0150'.
  ELSE.
    lw_screen = '0150'.
  ENDIF.

  PERFORM dynpro USING:
        'X' 'SAPLCSDI'              '0100',
        ' ' 'RC29N-MATNR'           it_9000-matnr,
        ' ' 'RC29N-WERKS'           it_9000-werks,
        ' ' 'RC29N-STLAN'           it_9000-stlan,
        ' ' 'RC29N-STLAL'           it_9000-stlal,
        ' ' 'BDC_OKCODE'            '=FCPU',

        'X' 'SAPLCSDI'              lw_screen,
        ' ' 'BDC_OKCODE'            '=FCLO',

        'X' 'SAPLSPO1'              '0100',
        ' ' 'BDC_OKCODE'            '=YES'.
ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0148   text
*      -->P_0149   text
*      -->P_0150   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-program,
          value TO bdc_tab-dynpro,
          'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-fnam,
          value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  GET_ERROR_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_error_msg.
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
            msg_lin = it_9000-msg.
ENDFORM.                    " GET_ERROR_MSG
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY msg matnr stlan stlal.

  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_log.
  DATA: lw_msg01(50),
        lw_msg02(50).

  CONCATENATE text-m04 w_total TEXT-M11 text-m05 w_success
              text-m06 w_fail
         INTO lw_msg01
    SEPARATED BY space.

  LOOP AT it_9000.
    MOVE-CORRESPONDING it_9000 TO it_log.
    MOVE: sy-datum TO it_log-erdat,
          sy-uzeit TO it_log-erzet,
          sy-uname TO it_log-ernam.
    APPEND it_log.
  ENDLOOP.

  INSERT ztpp_bom_del_log FROM TABLE it_log ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH lw_msg01 lw_msg02.
  ELSE.
    MESSAGE s000(zz) WITH lw_msg01.
  ENDIF.
ENDFORM.                    " create_log
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
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
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
*      -->P_0458   text
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

                                  'S' 'MAKTX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLNR'       ' ',
                                  'E' 'NO_OUT'       'X'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0595   text
*      -->P_0596   text
*      -->P_0597   text
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
  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSPP_ZAPP702'
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_werks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_werks.
  SELECT SINGLE * FROM t001w WHERE werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.
endform.                    " check_werks
*&---------------------------------------------------------------------*
*&      Form  check_stlan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_stlan.
  SELECT SINGLE * FROM t416T WHERE stlan = p_stlan
                               AND SPRAS = SY-LANGU.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.
endform.                    " check_stlan
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.
endform.                    " check_matnr
