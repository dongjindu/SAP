*&----------------------------------------------------------------------
*& Development ID : PP-049
*& Program ID     : ZBMC01300T
*& Program Name   : Check configurable indicator
*& Created by     : Byung Sung Bae
*& Created on     : 2005.10.28
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*& 07/24/2009  BS Bae  Add the functionalify with checking the data
*&                     mismatch according to SAP note no. 190363
*& 04.24.2012  Victor  Exclude deletion logic(don't delete Config.Ind.)
*&                     in case of 'PART'
*& 02.08.2013  Han     Performance Tune
*&======================================================================
*&
*&----------------------------------------------------------------------
REPORT  zbmc01300t.

INCLUDE <icon>.
TABLES: mara.

TYPES: BEGIN OF t_upgv,
matnr   LIKE   marc-matnr,
werks   LIKE   marc-werks.
TYPES: END OF t_upgv.

*DATA: zspp0024_9000 LIKE zspp0024_9000.

CONSTANTS: c_capid   LIKE rc29l-capid VALUE 'PP01',
           c_stlan   LIKE mast-stlan  VALUE '1'.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zspp0024_9000
                                     WITH HEADER LINE.

DATA: it_colorless TYPE TABLE OF mast   WITH HEADER LINE,
      xmast TYPE TABLE OF mast  WITH HEADER LINE,
      xmava TYPE TABLE OF mava1 WITH HEADER LINE,
      xcnfg TYPE TABLE OF zspp0024_9000 WITH HEADER LINE.

*DATA: BEGIN OF it_colorless OCCURS 0,
*        matnr   LIKE   marc-matnr,
*        werks   LIKE   marc-werks,
*        stlan   LIKE   mast-stlan,
*      END   OF it_colorless.

DATA: it_upgv TYPE STANDARD TABLE OF t_upgv.

TYPES:
  BEGIN OF st_bom,
    matnr   LIKE   marc-matnr,
    werks   LIKE   marc-werks,
    stlan   LIKE   mast-stlan,
  END   OF st_bom.
DATA: it_bom TYPE HASHED TABLE OF st_bom
      WITH UNIQUE KEY matnr werks stlan
      WITH HEADER LINE.

*DATA: BEGIN OF it_bom OCCURS 0,
*        matnr   LIKE   marc-matnr,
*        werks   LIKE   marc-werks,
*        stlan   LIKE   mast-stlan,
*      END   OF it_bom.

TYPES:
  BEGIN OF st_mara,
    matnr   LIKE   mara-matnr,
    mtart   LIKE   mara-mtart,
    kzkfg   LIKE   mara-kzkfg,
  END   OF st_mara.
DATA: it_mara TYPE HASHED TABLE OF st_mara
      WITH UNIQUE KEY matnr
      WITH HEADER LINE.
* Module ID
RANGES: r_modid FOR ztpp0001-key1.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: gs_opt LIKE ctu_params.

*---// Global variable
DATA: v_stlan        LIKE   mast-stlan,
*      v_total(7)     TYPE   n,
      v_fail(7)      TYPE   n,
      v_ready(7)     TYPE   n,
      v_error(7)     TYPE   n,
      v_finish(7)    TYPE   n,
      v_ernam        LIKE   sy-uname,
      v_erdat        LIKE   sy-datum,
      v_erzet        LIKE   sy-uzeit,
      v_datub        LIKE   sy-datum,
      v_datuv        LIKE   sy-datum.

*---// for Parrell processing
DATA: v_jobname  LIKE btch1140-jobname,
      v_info     LIKE rfcsi,            "Message text
      v_snd      TYPE i,                "Work packet sent for processing
      v_rcv      TYPE i,                "Work packet replies received
      v_err      TYPE i,                "Work packet error
      v_suc      TYPE i,                "Work packet success
      v_msg(80)  VALUE space,           "Container for error message in
      v_excp_flag,                      "Number of RESOURCE_FAILUREs
      v_group    LIKE rzllitab-classname VALUE 'parallel_generators',
      v_total     TYPE i,
      v_available TYPE i,

      v_lines        TYPE   i,
      v_max_item     TYPE   i,
      v_matnr_cnt(4) TYPE   n,
      v_batch_cnt(3) TYPE   n,
      v_matnr_f      TYPE   matnr,
      v_matnr_t      TYPE   matnr.

DATA : BEGIN OF it_tasklist OCCURS 10,
         taskname    LIKE btch1140-jobname,
         rfcdest     LIKE rfcsi-rfcdest,
         rfchost     LIKE rfcsi-rfchost,
         param(50),
         result(100),
       END OF it_tasklist.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9100   TYPE        scrfname VALUE 'CC_9100_ALV',
      wc_alv_9100       TYPE REF TO cl_gui_alv_grid,
      wc_container_9100 TYPE REF TO cl_gui_custom_container.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
      v_structure LIKE dd02l-tabname.

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
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSPP0024_'.

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
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_datuv FOR sy-datum DEFAULT sy-datum OBLIGATORY
                        NO-EXTENSION,
                s_datum FOR sy-datum DEFAULT sy-datum NO-EXTENSION,
                s_matnr FOR mara-matnr.
**selection-screen skip.
*SELECTION-SCREEN ULINE.
**selection-screen skip.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT  1(31) text-t02.
*PARAMETERS: p_update AS CHECKBOX.
*SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl1.
SELECTION-SCREEN BEGIN OF BLOCK bl2  WITH FRAME TITLE text-t02.
*PARAMETERS: p_upgv   AS CHECKBOX,
PARAMETERS: p_parall AS CHECKBOX,
            p_max   TYPE i OBLIGATORY DEFAULT 20.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: p_rver LIKE somlreci1-receiver OBLIGATORY.

SELECTION-SCREEN END OF BLOCK bl2.


*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

*---// At line selection
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM set_date.
  PERFORM read_data.
*  PERFORM read_data_upgv.

START-OF-SELECTION.
*  READ TABLE it_colorless INDEX 1.
*  CHECK sy-subrc EQ 0.
*  IF sy-batch EQ 'X'.
    PERFORM check_data_mismatch.
*  ENDIF.
  PERFORM check_rtn.
*  PERFORM check_upgv.

*  IF sy-batch EQ 'X'.
*    PERFORM change_material_master.
*  ENDIF.

  IF sy-batch EQ 'X' AND NOT it_9000[] IS INITIAL.
    PERFORM send_email.
  ENDIF.

  PERFORM count_rtn.
*
  CALL SCREEN 9000.

*----------------------------------------------------------------------*
FORM initialization .

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  gs_opt-defsize = 'X'.
  gs_opt-dismode = 'N'.
  gs_opt-updmode = 'S'.

  MOVE: sy-uname TO v_ernam,
        sy-datum TO v_erdat,
        sy-uzeit TO v_erzet.

ENDFORM.                    "initialization
*----------------------------------------------------------------------*
FORM read_data.

* --- get colorless material master
* --- material type = 'SEMI'
* --- configurable indicator = 'X'
* --- CCN(document type) > 0 (not blank)
* --- color mark = 'C'
  SELECT b~werks a~matnr s~stlan
    INTO CORRESPONDING FIELDS OF TABLE it_colorless
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr = b~matnr
                   INNER JOIN mast AS s
                      ON b~matnr = s~matnr
                     AND b~werks = s~werks
   WHERE a~mtart = 'HALB'
     AND a~kzkfg = 'X'
*     AND a~zeiar > space
*     AND a~zeifo = 'C'
     AND ( a~ersda IN s_datum OR
           a~laeda IN s_datum    )
     AND a~lvorm = space
     AND a~matnr IN s_matnr
     AND b~lvorm = space
     AND s~stlan IN ('1').
  SORT it_colorless BY matnr werks.
  IF sy-subrc NE 0.
    IF sy-batch = 'X'.
      MESSAGE i000(zz) WITH text-m02.
    ELSE.
      CALL FUNCTION 'WS_MSG'
        EXPORTING
          msg_type = 'E'
          text     = text-m02
          titl     = 'Data Search'.
      STOP.
    ENDIF.
  ENDIF.

ENDFORM.                    "read_data
*----------------------------------------------------------------------*
FORM check_rtn.
  IF p_parall EQ space.
    REFRESH xcnfg.
    CALL FUNCTION 'Z_BM_CHECK_CONFIGURABLE'
      EXPORTING
        datuv   = v_datuv
        datub   = v_datub
      TABLES
        it_mast = it_colorless
        it_cnfg = xcnfg.
    LOOP AT xcnfg.
      CLEAR it_9000.
      MOVE-CORRESPONDING xcnfg TO it_9000.
      APPEND it_9000.
    ENDLOOP.
  ELSE.

    DESCRIBE TABLE it_colorless LINES v_lines.

    v_max_item = v_lines / p_max.
    IF v_max_item < 0.
      v_max_item = v_lines.
    ENDIF.

    SORT it_colorless BY matnr.

    LOOP AT it_colorless.
      v_matnr_cnt = v_matnr_cnt + 1.

      IF v_matnr_cnt = 1.
        MOVE: it_colorless-matnr TO v_matnr_f.
        v_batch_cnt = v_batch_cnt + 1.
        REFRESH xmast.
      ENDIF.

      CLEAR: xmast.
      MOVE: it_colorless TO xmast.
      APPEND xmast.

      IF v_matnr_cnt = v_max_item.
        PERFORM create_parallel_process.

        CLEAR: v_matnr_cnt.
      ENDIF.
    ENDLOOP.

    IF v_matnr_cnt > 0 AND v_matnr_cnt NE v_max_item.
      PERFORM create_parallel_process.
    ENDIF.

    WAIT UNTIL v_rcv >= v_snd UP TO 3600 SECONDS.
*    ULINE.
*
*    WRITE:/ '# Target  process: ', v_snd,
*          / '# Ended   process: ', v_rcv,
*          / '# Success process: ', v_suc,
*          / '# Error   process: ', v_err.
*
*    ULINE.
*
*    WRITE:/1(32) 'Received task' CENTERED,
*            (32) 'Destination' CENTERED,
*            (50) 'Parameter' CENTERED,
*            (100) 'Result'.
*    ULINE.
*    LOOP AT it_tasklist.
*      WRITE:/   it_tasklist-taskname COLOR 1,
*                it_tasklist-rfcdest COLOR 1,
*                it_tasklist-param,
*                it_tasklist-result.
*    ENDLOOP.
  ENDIF.
  SORT it_9000 BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_9000
             COMPARING matnr.
ENDFORM.                    "check_rtn
*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_date .

  IF     s_datuv-low EQ '00000000' AND s_datuv-high EQ '00000000'.
    v_datuv = '19000101'. v_datub = '99991231'.
  ELSEIF s_datuv-low NE '00000000' AND s_datuv-high EQ '00000000'.
    v_datuv = v_datub = s_datuv-low.
  ELSEIF s_datuv-low EQ '00000000' AND s_datuv-high NE '00000000'.
    v_datuv = '19000101'. v_datub = s_datuv-high.
  ELSEIF s_datuv-low NE '00000000' AND s_datuv-high NE '00000000'.
    v_datuv = s_datuv-low. v_datub = s_datuv-high.
  ENDIF.

ENDFORM.                    "set_date
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.

ENDMODULE.                    "status OUTPUT
*----------------------------------------------------------------------*
MODULE exit INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                    "exit INPUT
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXECUTE'.
      CLEAR: sy-ucomm.
      PERFORM execute_rtn.
  ENDCASE.

ENDMODULE.                    "user_command_9000 INPUT
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.

  PERFORM create_alv_object USING sy-dynnr.

ENDMODULE.                    "create_alv_object OUTPUT
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container.
  ASSIGN:      (v_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
  ENDIF.

ENDFORM.                    "create_alv_object
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.

*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container,
               'WC_CONTROL_'   p_dynnr INTO v_control,
               'WC_ALV_'       p_dynnr INTO v_alv.

  ASSIGN: (v_container) TO <container>,
          (v_control)   TO <control>,
          (v_alv)       TO <alv>.

  CREATE OBJECT <container>
    EXPORTING
      container_name              = <control>
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
    EXPORTING
      i_parent      = <container>
      i_appl_events = 'X'.

ENDFORM.                    "create_container_n_object
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.

  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.

ENDFORM.                    "set_attributes_alv_grid
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.

  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
*  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.

ENDFORM.                    "set_attributes_alv_9000
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.

ENDFORM.                    "build_field_catalog
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.

  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO v_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = v_repid
      i_internal_tabname = lw_itab
      i_inclname         = v_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

ENDFORM.                    "set_fieldname
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.

  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.

ENDFORM.                    "set_screen_fields
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.

*  PERFORM setting_fieldcat TABLES it_fieldcat USING :
*                                  'S' 'FSC_TAR'     ' ',
*                                  'E' 'KEY'         'X',
*
*                                  'S' 'KMAT_TAR'    ' ',
*                                  'E' 'KEY'         'X',
*
*                                  'S' 'ICON'        ' ',
*                                  'E' 'KEY'         'X',
*
*                                  'S' 'FSC_SRC'     ' ',
*                                  'E' 'EMPHASIZE'   'C400',
*
*                                  'S' 'KMAT_SRC'    ' ',
*                                  'E' 'EMPHASIZE'   'C400',
*
*                                  'S' 'CHK_EXIST'   ' ',
*                                  'E' 'NO_OUT'      'X'.

ENDFORM.                    "set_screen_fields_9000
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.

  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv,
               c_structure  p_dynnr      INTO v_structure,
               'IT_'        p_dynnr '[]' INTO v_itab.

  ASSIGN: (v_alv)       TO <alv>,
          (v_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
      i_structure_name = v_structure
      is_layout        = v_is_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_outtab        = <itab>.

ENDFORM.                    "assign_itab_to_alv
*----------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.

  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

ENDFORM.                    "sssign_event
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.

  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO v_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: v_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO lv_col.
  ASSIGN (lv_col) TO <fs>.
  MOVE   p_value  TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO v_cnt.
      p_fieldcat-col_pos = v_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.

ENDFORM.                    "setting_fieldcat
*----------------------------------------------------------------------*
FORM execute_rtn .

  PERFORM change_material_master.
  PERFORM check_data_mismatch.
  PERFORM count_rtn.
  PERFORM assign_itab_to_alv USING '9000'.

ENDFORM.                    "execute_rtn
*----------------------------------------------------------------------*
FORM get_err_msg  USING pv_msg.

  DATA: lv_msg LIKE cfgnl-msglin.

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
      msg_lin = lv_msg.

  MOVE: lv_msg TO pv_msg.
ENDFORM.                    "get_err_msg
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.

  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.

ENDFORM.                    "dynpro
*----------------------------------------------------------------------*
FORM change_material_master .
*  LOOP AT it_9000 WHERE icon = icon_yellow_light
*                     OR icon = icon_red_light.
*    REFRESH: it_bdc.
*
*    PERFORM dynpro USING:
*       'X' 'SAPLMGMM'            '0060',
*       ' ' 'RMMG1-MATNR'         it_9000-matnr,
*       ' ' 'BDC_OKCODE'          '=AUSW',
*
*       'X' 'SAPLMGMM'            '0070',
*       ' ' 'MSICHTAUSW-KZSEL(2)' 'X',
*       ' ' 'BDC_OKCODE'          '/00',
*
*       'X' 'SAPLMGMM'            '5004',  " <- befor DI '4004',
*       ' ' 'MARA-KZKFG'          'X',
*       ' ' 'BDC_OKCODE'          '=BU'.
*
*    CALL TRANSACTION 'MM02'  USING it_bdc
*                             OPTIONS FROM gs_opt.
*    IF sy-subrc NE 0 OR sy-msgno NE '801'.
*      MOVE: icon_red_light      TO it_9000-icon.
*      PERFORM get_err_msg USING it_9000-zmsg.
*    ELSE.
*      MOVE: space               TO it_9000-zmsg,
*            icon_green_light    TO it_9000-icon.
*    ENDIF.
*
*    MODIFY it_9000.
*  ENDLOOP.

ENDFORM.                    "change_material_master
*----------------------------------------------------------------------*
FORM count_rtn .

  CLEAR: v_total, v_fail, v_ready, v_error, v_finish.
  LOOP AT it_9000.
    v_total = v_total + 1.

    CASE it_9000-icon.
      WHEN icon_light_out.
        v_fail   = v_fail + 1.
      WHEN icon_yellow_light.
        v_ready  = v_ready + 1.
      WHEN icon_red_light.
        v_error  = v_error + 1.
      WHEN icon_green_light.
        v_finish = v_finish + 1.
    ENDCASE.
  ENDLOOP.

  DATA : l_finish  TYPE i,
         l_ready   TYPE i,
         l_error   TYPE i.
  CLEAR : l_finish, l_ready, l_error.

  l_finish = v_finish.
  l_ready  = v_ready.
  l_error  = v_error.

ENDFORM.                    "count_rtn
*----------------------------------------------------------------------*
FORM read_data_upgv .
*  CHECK p_upgv EQ 'X'.
** --- get UPG-VC material master
** --- material type = 'UPGV'
** --- configurable indicator = space
*  SELECT b~werks
*         a~matnr
*   INTO CORRESPONDING FIELDS OF TABLE it_upgv
*   FROM mara AS a INNER JOIN marc AS b
*                     ON a~matnr = b~matnr
*  WHERE a~mtart = 'UPGV'
*    AND a~kzkfg = space
*    AND ( a~ersda IN s_datum OR
*          a~laeda IN s_datum    )
*    AND a~lvorm = space
*    AND a~matnr IN s_matnr
*    AND b~lvorm = space.
*
**  IF SY-SUBRC NE 0.
**    MESSAGE I000(ZZ) WITH TEXT-M03.
**  IF sy-dbcnt = 0.
**    CALL FUNCTION 'WS_MSG'
**      EXPORTING
**        msg_type = 'E'
**        text     = text-m03
**        titl     = 'Data Search'.
**    STOP.
**  ENDIF.

ENDFORM.                    "read_data_upgv
*----------------------------------------------------------------------*
FORM check_upgv.

*  DATA: it_stb TYPE STANDARD TABLE OF stpox.
*  DATA: wa_topmat TYPE cstmat.
*
*  FIELD-SYMBOLS: <stb> TYPE stpox.
*  FIELD-SYMBOLS: <upgv> TYPE t_upgv.
*
*  CHECK it_upgv[] IS NOT INITIAL.
*
*  LOOP AT it_upgv ASSIGNING <upgv>.
*    CLEAR:   wa_topmat, it_9000.
*    REFRESH it_stb[].
*
*    READ TABLE it_9000 WITH KEY matnr = <upgv>-matnr
*                                werks = <upgv>-werks
*                                icon = icon_yellow_light
*                       BINARY SEARCH.
*
*    CHECK sy-subrc NE 0.
*
*    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
*    EXPORTING
*      capid                 = c_capid
*      datuv                 = sy-datum
*      mtnrv                 = <upgv>-matnr
*      stlan                 = c_stlan
**      stlal                 = p_stlal
*      werks                 = <upgv>-werks
*      ehndl                 = '1'
*      mktls                 = space   "Added by Han
*      mehrs                 = 'X'
**      mdmps                 = 'X'
**      fbstp                 = 'X'
*      mmory                 = '0'
*    IMPORTING
*      topmat                = wa_topmat
*    TABLES
*      stb                   = it_stb[]
*    EXCEPTIONS
*      alt_not_found         = 1
*      call_invalid          = 2
*      material_not_found    = 3
*      missing_authorization = 4
*      no_bom_found          = 5
*      no_plant_data         = 6
*      no_suitable_bom_found = 7
*      conversion_error      = 8
*      OTHERS                = 9.
*
*    IF sy-subrc = 0.
*      READ TABLE it_stb ASSIGNING <stb> WITH KEY kzkfg  = 'X'
*                                                 zzeitm = 'C'
*                                                 loekz  = space.
*      IF sy-subrc = 0.
*        MOVE: wa_topmat-matnr   TO it_9000-matnr,
*              'UPGV'            TO it_9000-mtart,
*              wa_topmat-werks   TO it_9000-werks,
*              wa_topmat-stlan   TO it_9000-stlan,
*              wa_topmat-maktx   TO it_9000-maktx,
*              <stb>-idnrk       TO it_9000-idnrk,
*              icon_yellow_light TO it_9000-icon.
*        APPEND it_9000.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    "check_upgv
*----------------------------------------------------------------------*
FORM check_data_mismatch_old.

  DATA: lt_stzu TYPE TABLE OF stzu WITH HEADER LINE,
        lt_mast TYPE TABLE OF mast WITH HEADER LINE,
        lt_mara TYPE TABLE OF mara WITH HEADER LINE,
        l_zflag TYPE c.

* --- get BOM information with configurable indicator
  SELECT stlnr stlan kbaus
         INTO  CORRESPONDING FIELDS OF TABLE lt_stzu
         FROM  stzu
         WHERE stlty EQ 'M'
         AND   stlan IN ('1', 'M')
         AND   kbaus EQ 'X'.

  CHECK lt_stzu[] IS NOT INITIAL.

  SORT lt_stzu BY stlan stlnr.

* --- get material/BOM link information
  SELECT matnr werks stlan stlnr
         INTO  CORRESPONDING FIELDS OF TABLE lt_mast
         FROM  mast
         FOR   ALL ENTRIES IN lt_stzu
         WHERE stlan EQ lt_stzu-stlan
         AND   stlnr EQ lt_stzu-stlnr
         AND   cslty EQ ''.

  SORT lt_mast BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_mast COMPARING matnr.

* --- get material master
  SELECT matnr mtart kzkfg
         INTO  CORRESPONDING FIELDS OF TABLE lt_mara
         FROM  mara
         FOR   ALL ENTRIES IN lt_mast
         WHERE matnr EQ lt_mast-matnr.

  SORT lt_mara BY matnr.
  SORT lt_mast BY stlan stlnr.

* --- check data mismatch
  LOOP AT lt_stzu.
    READ TABLE lt_mast WITH KEY stlan = lt_stzu-stlan
                                stlnr = lt_stzu-stlnr
                       BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    READ TABLE lt_mara WITH KEY matnr = lt_mast-matnr
                       BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    CLEAR l_zflag.

    CASE lt_mara-mtart.
      WHEN 'KMAT' OR 'UPGV' OR 'SEMI'.
        IF lt_mara-kzkfg NE 'X'.
          UPDATE mara SET kzkfg = 'X'
                      WHERE matnr = lt_mara-matnr.
          IF sy-subrc EQ 0.
            l_zflag = 'S'.
          ELSE.
            l_zflag = 'E'.
          ENDIF.
        ENDIF.

*-<   04.24.2012 Victor don't delete Config. Ind. in case of 'PART'
*      WHEN 'PART' OR 'SUBP'. "Comment
      WHEN 'SUBP'.
*->
        UPDATE stzu SET   kbaus = ''
                    WHERE stlty = 'M'
                    AND   stlnr = lt_stzu-stlnr.
        IF sy-subrc EQ 0.
          l_zflag = 'S'.
          IF lt_mara-kzkfg EQ 'X'.
            UPDATE mara SET   kzkfg = ''
                        WHERE matnr = lt_mara-matnr.
            IF sy-subrc NE 0.
              l_zflag = 'E'.
            ENDIF.
          ENDIF.
        ELSE.
          l_zflag = 'E'.
        ENDIF.

      WHEN OTHERS.
        " Nothing
    ENDCASE.

    IF l_zflag EQ 'S'.
      COMMIT WORK AND WAIT.
    ELSEIF l_zflag EQ 'E'.
      ROLLBACK WORK.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "check_data_mismatch_old
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_MISMATCH
*&---------------------------------------------------------------------*
*       Check & Update Configurable Material
*----------------------------------------------------------------------*
FORM check_data_mismatch .
  DATA:
    BEGIN OF xbom OCCURS 0,
      matnr TYPE mast-matnr,
      werks TYPE mast-werks,
      stlty TYPE stzu-stlty,
      stlnr TYPE stzu-stlnr,
      stlan TYPE stzu-stlan,
      kbaus TYPE stzu-kbaus,
      mtart TYPE mara-mtart,
      kzkfg TYPE mara-kzkfg,
      icon TYPE zicon,
    END OF xbom.

  DATA l_zflag TYPE c.

*--- Get BOM information with wrong configurable indicator
* For KMAT/UPGV/SEMI, Configurable Material should be marked in MARA
* For SUBP, Configurable Material should be demarked in MARA
  SELECT m~matnr m~werks
         s~stlty s~stlnr s~stlan s~kbaus
         a~mtart a~kzkfg
    INTO CORRESPONDING FIELDS OF TABLE xbom
    FROM stzu AS s INNER JOIN mast AS m
                      ON s~stlnr = m~stlnr
                     AND s~stlan = m~stlan
                   INNER JOIN mara AS a
                      ON m~matnr = a~matnr
   WHERE s~stlty EQ 'M'            "Material BOM
     AND s~stlan IN ('1','M')      "BOM Usage 1(Prod.)/M(Module)
     AND s~kbaus EQ 'X'            "Configurable BOM
     AND m~cslty EQ space          "Indicator: configured material
     AND ( ( a~mtart = 'HALB' AND
             a~kzkfg EQ space ) OR
           ( a~mtart EQ 'ROH' AND
             a~kzkfg EQ 'X' ) ).

  SORT xbom BY matnr werks stlty stlnr stlan.
  CHECK xbom[] IS NOT INITIAL.

  LOOP AT xbom.
    CLEAR l_zflag.

    CASE xbom-mtart.
      WHEN 'HALB'.
        IF xbom-kzkfg NE 'X'.
          MOVE: icon_yellow_light TO xbom-icon.
          MODIFY xbom.
          clear: it_9000.
          MOVE-CORRESPONDING xbom to it_9000.
          append it_9000.
*          UPDATE mara SET kzkfg = 'X'
*                    WHERE matnr = xbom-matnr.
*          IF sy-subrc EQ 0.
*            l_zflag = 'S'.
*          ELSE.
*            l_zflag = 'E'.
*          ENDIF.
        ENDIF.
*-----04.24.2012 Victor don't delete Config. Ind. in case of 'PART'
      WHEN 'ROH'.
*                  WRITE: / xbom-matnr.
*        UPDATE stzu SET kbaus = space
*                  WHERE stlty = xbom-stlty
*                    AND stlnr = xbom-stlnr.
*        IF sy-subrc EQ 0.
*          l_zflag = 'S'.

        IF xbom-kzkfg EQ 'X'.
          MOVE: icon_yellow_light TO xbom-icon.
          MODIFY xbom.
          clear: it_9000.
          MOVE-CORRESPONDING xbom to it_9000.
          append it_9000.
*            UPDATE mara SET kzkfg = space
*                      WHERE matnr = xbom-matnr.
*            IF sy-subrc NE 0.
*              l_zflag = 'E'.
*            ENDIF.
*          ENDIF.
*        ELSE.
*          l_zflag = 'E'.
        ENDIF.

      WHEN OTHERS.
        " Nothing
    ENDCASE.

*    IF l_zflag EQ 'S'.
*      COMMIT WORK AND WAIT.
*    ELSEIF l_zflag EQ 'E'.
*      ROLLBACK WORK.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_DATA_MISMATCH
*&---------------------------------------------------------------------*
*&      Form  CREATE_PARALLEL_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_parallel_process .

  CLEAR: it_tasklist.

  MOVE: xmast-matnr TO v_matnr_t.

  CONCATENATE 'Check Configurable BOM #' v_batch_cnt
              ':' v_matnr_f '~' v_matnr_t
         INTO v_jobname SEPARATED BY space.
  REFRESH xcnfg.
  CALL FUNCTION 'Z_BM_CHECK_CONFIGURABLE'
    STARTING NEW TASK v_jobname
    DESTINATION IN GROUP v_group
    PERFORMING return_info ON END OF TASK
    EXPORTING
      datuv                 = v_datuv
      datub                 = v_datub
    TABLES
      it_mast               = xmast
      it_cnfg               = xcnfg
    EXCEPTIONS
      communication_failure = 1  MESSAGE v_msg
      system_failure        = 2  MESSAGE v_msg
      resource_failure      = 3
      OTHERS                = 4.

  CASE sy-subrc.
    WHEN 0.
      it_tasklist-taskname = v_jobname.
      CONCATENATE v_matnr_f '~' v_matnr_t
                  '(' v_matnr_cnt 'CNT )'
      INTO it_tasklist-param
        SEPARATED BY space.

      CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
        IMPORTING
          rfcdest = it_tasklist-rfcdest
        EXCEPTIONS
          OTHERS  = 1.
      APPEND it_tasklist.

      v_snd = v_snd + 1.
    WHEN 1 OR 2.
      it_tasklist-taskname = v_jobname.
      CONCATENATE v_matnr_f '~' v_matnr_t INTO it_tasklist-param
        SEPARATED BY space.

      CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
        IMPORTING
          rfcdest = it_tasklist-rfcdest
        EXCEPTIONS
          OTHERS  = 1.

      DATA: lv_server LIKE pbtresourc-servername.

      MOVE: it_tasklist-rfcdest TO lv_server.

      CALL FUNCTION 'SPBT_DO_NOT_USE_SERVER'
        EXPORTING
          server_name                 = lv_server
        EXCEPTIONS
          invalid_server_name         = 1
          no_more_resources_left      = 2
          pbt_env_not_initialized_yet = 3 "No servers left in group.
          OTHERS                      = 4.

      it_tasklist-result = v_msg.
      APPEND it_tasklist.
      v_snd = v_snd + 1.
      v_err = v_err + 1.
    WHEN 3.
      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING
          group_name                     = v_group
        IMPORTING
          max_pbt_wps                    = v_total
          free_pbt_wps                   = v_available
        EXCEPTIONS
          invalid_group_name             = 1
          internal_error                 = 2
          pbt_env_already_initialized    = 3
          currently_no_resources_avail   = 4
          no_pbt_resources_found         = 5
          cant_init_different_pbt_groups = 6
          OTHERS                         = 7.

      IF v_available > 0.
        PERFORM create_parallel_process.
      ELSE.
        WAIT UP TO 30 SECONDS.
        PERFORM create_parallel_process.
      ENDIF.
  ENDCASE.

ENDFORM.                    " CREATE_PARALLEL_PROCESS
*&---------------------------------------------------------------------*
*&      Form  RETURN_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM return_info USING taskname.
  DATA :  info_rfcdest LIKE it_tasklist-rfcdest.

  RECEIVE RESULTS FROM FUNCTION 'Z_BM_CHECK_CONFIGURABLE'
    TABLES
      it_cnfg               = xcnfg
    EXCEPTIONS system_failure        = 01
               communication_failure = 02
               resource_failure      = 03.
  LOOP AT xcnfg.
    CLEAR it_9000.
    MOVE-CORRESPONDING xcnfg TO it_9000.
    APPEND it_9000.
  ENDLOOP.
*  RECEIVE RESULTS FROM FUNCTION 'RFC_SYSTEM_INFO'
*    IMPORTING rfcsi_export = v_info
*    EXCEPTIONS
*      communication_failure = 1
*      system_failure        = 2.
*
*  v_rcv = v_rcv + 1.  "Receiving data
*  v_suc = v_suc + 1.
*
*  IF sy-subrc NE 0.
**    handle communication and system failure
*  ELSE.
*    READ TABLE it_tasklist WITH KEY taskname = taskname.
*    IF sy-subrc = 0.  "Register data
*      it_tasklist-rfchost = v_info-rfchost.
*      it_tasklist-result  = 'Successfully finished.'.
*      MODIFY it_tasklist INDEX sy-tabix.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " RETURN_INFO
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_email.
  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.

  DATA: l_subject TYPE p15_text150,
        l_p_rec_type  LIKE  somlreci1-rec_type.

  MOVE 'Check configurable indicator' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.
  MOVE '================================' TO lt_body.
  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: 'Material No' TO lt_body+0(20),
        'Plant' TO lt_body+20(8),
        'BOM Usg' TO lt_body+28(8),
        ' Mat Ty' TO lt_body+36(8),
*                  MAKTX TO lt_body+44(40),
        'MEssage' TO lt_body+44(40).

  APPEND lt_body.
  CLEAR: lt_body.

  MOVE: '--------------------' TO lt_body+0(20),
       '--------' TO lt_body+20(8),
       '--------' TO lt_body+28(8),
       '--------' TO lt_body+36(8),
*                  MAKTX TO lt_body+44(40),
       '--------------------' TO lt_body+44(20),
       '--------------------' TO lt_body+64(20).

  APPEND lt_body.
  CLEAR: lt_body.

  LOOP AT it_9000.
    MOVE: it_9000-matnr TO lt_body+0(20),
          it_9000-werks TO lt_body+20(8),
          it_9000-stlan TO lt_body+28(8),
          it_9000-mtart TO lt_body+36(8),
*          it_9000-MAKTX TO lt_body+44(40),
          it_9000-zmsg TO lt_body+44(40).
    APPEND lt_body.
  ENDLOOP.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = 'Check configurable indicator'
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = lt_body.
ENDFORM.                    "SEND_EMAIL
