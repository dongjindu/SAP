************************************************************************
* Program Name      : ZAPP727_REVISION_ENGINE_BOM
* Author            : Byung Sung Bae
* Creation Date     : 2005.03.09.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : Revision Engine BOM
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zapp727_revision_engine_bom LINE-SIZE 255.

INCLUDE <icon>.

TABLES: mara.

DATA: zsbm_app727_9001 LIKE zsbm_app727_9001,
      zsbm_app727_9002 LIKE zsbm_app727_9002,
      zsbm_app727_9003 LIKE zsbm_app727_9003,
      zsbm_app727_9100 LIKE zsbm_app727_9001.

*---// Internal Tables
DATA: it_9001 TYPE STANDARD TABLE OF zsbm_app727_9001
                                     WITH HEADER LINE.

DATA: it_9002 TYPE STANDARD TABLE OF zsbm_app727_9002
                                     WITH HEADER LINE.

DATA: it_9003 TYPE STANDARD TABLE OF zsbm_app727_9003
                                     WITH HEADER LINE.

DATA: it_9100 TYPE STANDARD TABLE OF zsbm_app727_9001
                                     WITH HEADER LINE.

DATA: BEGIN OF it_bom_header OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_bom_header.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_matnr.

DATA: BEGIN OF it_mip OCCURS 0,
        matnr   LIKE   mara-matnr,
        icon(4),
        msg(100),
      END   OF it_mip.

DATA: BEGIN OF it_assy OCCURS 0,
        matnr   LIKE   mara-matnr,
        icon(4),
        msg(100),
      END   OF it_assy.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

*---// Global variables & Structures
DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

DATA: w_total    TYPE   i,
      w_success  TYPE   i,
      w_warining TYPE   i,
      w_ready    TYPE   i,
      w_warning  TYPE   i,
      w_error    TYPE   i.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_stlan   LIKE mast-stlan  VALUE '1',
           c_stlal   LIKE mast-stlal  VALUE '01',
           c_mip                      VALUE 'M',
           c_assy                     VALUE 'A',
           c_both                     VALUE 'B',
           c_error(4)                 VALUE icon_red_light,
           c_warning(4)               VALUE icon_yellow_light,
           c_ready(4)                 VALUE icon_light_out,
           c_success(4)               VALUE icon_green_light.

* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
CONSTANTS: BEGIN OF c_tab_9000,
             tab1 LIKE sy-ucomm VALUE 'TAB_9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_9000_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_9000_FC3',
           END OF c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
CONTROLS:  tab_9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab_9000,
             subscreen   LIKE sy-dynnr,
prog        LIKE sy-repid VALUE 'ZAPP727_REVISION_ENGINE_BOM',
             pressed_tab LIKE sy-ucomm VALUE c_tab_9000-tab1,
           END OF g_tab_9000.
DATA:      ok_code LIKE sy-ucomm.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9001   TYPE        scrfname VALUE 'CC_9001_ALV',
      wc_alv_9001       TYPE REF TO cl_gui_alv_grid,
      wc_container_9001 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9002   TYPE        scrfname VALUE 'CC_9002_ALV',
      wc_alv_9002       TYPE REF TO cl_gui_alv_grid,
      wc_container_9002 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9003   TYPE        scrfname VALUE 'CC_9003_ALV',
      wc_alv_9003       TYPE REF TO cl_gui_alv_grid,
      wc_container_9003 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9100   TYPE        scrfname VALUE 'CC_9100_ALV',
      wc_alv_9100       TYPE REF TO cl_gui_alv_grid,
      wc_container_9100 TYPE REF TO cl_gui_custom_container.

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

CONSTANTS: c_structure(100) VALUE 'ZSBM_APP727_'.

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
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
*  METHOD  handle_data_changed.
*  ENDMETHOD.
*
*  METHOD  handle_user_command.
*  ENDMETHOD.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_werks LIKE marc-werks DEFAULT 'E001' OBLIGATORY,
            p_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
*selection-screen skip.
SELECTION-SCREEN ULINE.
*selection-screen skip.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_update AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

** for E002
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM update_rtn.
  PERFORM display_rtn.

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
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM get_engine_inf_data.
  PERFORM set_target_material.
  PERFORM set_bom_header.
  PERFORM read_bom_structure.
  PERFORM check_bom.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  get_engine_inf_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_engine_inf_data.
  SELECT item text valu1
    INTO CORRESPONDING FIELDS OF TABLE it_9003
    FROM ztbm_fsc_cre_inf
   WHERE valu1 IN s_matnr
     AND ( item  LIKE 'EN_6%' OR
           item  LIKE 'EN_7%' ).

  LOOP AT it_9003.
    CASE it_9003-item(4).
      WHEN 'EN_6'.
        MOVE: it_9003-valu1 TO it_assy-matnr.
        COLLECT it_assy.
      WHEN 'EN_7'.
        MOVE: it_9003-valu1 TO it_mip-matnr.
        COLLECT it_mip.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_engine_inf_data
*&---------------------------------------------------------------------*
*&      Form  set_target_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_target_material.
*  LOOP AT it_inf.
*    MOVE: it_inf-valu1 TO it_matnr-matnr.
*    COLLECT it_matnr.
*  ENDLOOP.
ENDFORM.                    " set_target_material
*&---------------------------------------------------------------------*
*&      Form  set_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_bom_header.
  SELECT a~matnr
    INTO CORRESPONDING FIELDS OF TABLE it_bom_header
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr EQ b~matnr
                     AND b~lvorm EQ space
                     AND b~werks EQ p_werks
                   INNER JOIN mast AS c
                      ON b~matnr EQ c~matnr
                     AND b~werks EQ c~werks
                     AND c~stlal EQ c_stlal
                     AND c~stlan EQ c_stlan
                   INNER JOIN stko AS d
                      ON c~stlnr EQ d~stlnr
                     AND c~stlal EQ d~stlal
                     AND d~stlty EQ 'M'
                     AND d~lkenz EQ space
                     AND d~loekz EQ space
                     AND d~stlst EQ '01'
                     AND d~datuv <= p_datuv.

  SORT it_bom_header BY matnr.
ENDFORM.                    " set_bom_header
*&---------------------------------------------------------------------*
*&      Form  read_bom_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bom_structure.
*  PERFORM display_progress_bar USING text-b06.

  PERFORM check_assy_bom.
  PERFORM check_mip_bom.
  PERFORM append_1_level.
*  PERFORM check_mixed_component.
ENDFORM.                    " read_bom_structure
*&---------------------------------------------------------------------*
*&      Form  check_engine_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mip_bom.
  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  LOOP AT it_mip.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = c_capid
        datuv                 = p_datuv
        cuobj                 = c_cuobj
        mtnrv                 = it_mip-matnr
        stlan                 = c_stlan
        stlal                 = c_stlal
        werks                 = p_werks
        mmory                 = '0'
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
    IF sy-subrc NE 0 OR lw_topmat-stlal NE c_stlal.
      MOVE: c_error  TO it_mip-icon,
            text-m02 TO it_mip-msg.
      MODIFY it_mip.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.
      READ TABLE it_assy WITH KEY matnr = lt_stb-idnrk.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.

      READ TABLE it_bom_header WITH KEY matnr = lt_stb-idnrk
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        PERFORM append_it_bom USING it_mip-matnr c_mip ' ' lt_stb.
        PERFORM read_sub_bom USING c_mip lt_stb.
      ELSE.
        PERFORM append_it_bom USING it_mip-matnr c_mip 'X' lt_stb.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " check_engine_bom
*&---------------------------------------------------------------------*
*&      Form  append_it_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MIP_MATNR  text
*      -->P_C_MIP  text
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM append_it_bom USING pw_matnr pw_type pw_end_type
                         pw_stb   LIKE   stpox.

  DATA: lw_key(26).

  CONCATENATE pw_matnr pw_stb-stlkn INTO lw_key.

  CLEAR: it_9002.
  MOVE: lw_key       TO it_9002-key,
        pw_matnr     TO it_9002-matnr,
        pw_stb-werks TO it_9002-werks,
        pw_stb-stlan TO it_9002-stlan,
        pw_stb-stlal TO it_9002-stlal,
        pw_stb-posnr TO it_9002-posnr,
        pw_stb-idnrk TO it_9002-idnrk,
        pw_stb-ojtxb TO it_9002-maktx,
        pw_stb-suff  TO it_9002-suff,
        pw_stb-stlkn TO it_9002-stlkn,
        pw_stb-eitm  TO it_9002-eitm,
        pw_stb-datuv TO it_9002-datuv,
        pw_stb-sanko TO it_9002-sanko,
        pw_stb-sanfe TO it_9002-sanfe,
        pw_stb-sanka TO it_9002-sanka,
        pw_type      TO it_9002-type,
        pw_end_type  TO it_9002-end_item.

  APPEND it_9002.
ENDFORM.                    " append_IT_9002
*&---------------------------------------------------------------------*
*&      Form  read_sub_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_MIP  text
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM read_sub_bom USING pw_type
                        pw_stb LIKE stpox.
  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.
  DATA: lw_key(26).


  CASE pw_type.
    WHEN c_mip.
      READ TABLE it_9002 WITH KEY matnr = pw_stb-idnrk
                                  type  = c_assy.
      IF sy-subrc EQ 0.
        PERFORM check_multiple_usage USING pw_stb-idnrk.
      ENDIF.

      READ TABLE it_9002 WITH KEY matnr = pw_stb-idnrk.
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    WHEN c_assy.
      READ TABLE it_9002 WITH KEY key = lw_key.
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
  ENDCASE.


  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
    EXPORTING
      capid                 = c_capid
      datuv                 = p_datuv
      cuobj                 = c_cuobj
      mtnrv                 = pw_stb-idnrk
      stlan                 = c_stlan
      stlal                 = c_stlal
      werks                 = p_werks
      mmory                 = '0'
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
  IF sy-subrc NE 0 OR lw_topmat-stlal NE c_stlal.
    PERFORM append_it_bom USING it_assy-matnr pw_type 'X' pw_stb.
    EXIT.
  ENDIF.

  LOOP AT lt_stb.
    READ TABLE it_bom_header WITH KEY matnr = lt_stb-idnrk
                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      PERFORM append_it_bom USING pw_stb-idnrk pw_type ' ' lt_stb.
      PERFORM read_sub_bom USING pw_type lt_stb.
    ELSE.
      PERFORM append_it_bom USING pw_stb-idnrk pw_type 'X' lt_stb.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_sub_bom
*&---------------------------------------------------------------------*
*&      Form  check_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_bom.
  LOOP AT it_9002.
    CLEAR: it_9001.

    MOVE-CORRESPONDING it_9002 TO it_9001.

    CASE it_9002-end_item.
      WHEN c_check.
        PERFORM set_end_item_fields.
      WHEN space.
        MOVE: space   TO it_9001-sanko_n,
              c_check TO it_9001-sanfe_n,
              c_check TO it_9001-sanka_n.
    ENDCASE.


    IF it_9001-sanko NE it_9001-sanko_n OR
       it_9001-sanfe NE it_9001-sanfe_n OR
       it_9001-sanka NE it_9001-sanka_n OR
       it_9001-icon  EQ c_error         OR
       it_9001-icon  EQ c_warning.
      CASE it_9001-icon.
        WHEN c_error.
        WHEN c_warning.
          MOVE: c_warning TO it_9001-icon.
        WHEN OTHERS.
          MOVE: c_ready TO it_9001-icon.
      ENDCASE.

      APPEND it_9001.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_bom
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  IF sy-batch EQ space.
    CALL SCREEN 9000.
  ELSE.
    it_9100[] = it_9001[].
    CALL SCREEN 9100.
  ENDIF.
ENDFORM.                    " display_rtn
*&---------------------------------------------------------------------*
*&      Form  update_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_rtn.
  CHECK p_update EQ 'X'.

  PERFORM update_bom.
ENDFORM.                    " update_rtn
*&---------------------------------------------------------------------*
*&      Form  update_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_bom.
  LOOP AT it_9001 WHERE icon EQ c_error
                     OR icon EQ c_ready.
    PERFORM generate_bdc.

    CALL TRANSACTION 'CS02'  USING it_bdc
                             OPTIONS FROM wa_opt.
    IF sy-subrc NE 0 OR sy-msgno NE '031'.
      MOVE: c_error TO it_9001-icon.
      PERFORM get_err_msg USING it_9001-msg.
    ELSE.
      IF it_9001-icon NE c_warning.
        MOVE: c_success TO it_9001-icon.
      ENDIF.
    ENDIF.

    MODIFY it_9001.
  ENDLOOP.

  CLEAR: w_total, w_success, w_error, w_warning.

  LOOP AT it_9001.
    CASE it_9001-icon.
      WHEN c_ready.
        w_ready = w_ready + 1.
      WHEN c_error.
        w_error = w_error + 1.
      WHEN c_success.
        w_success = w_success + 1.
      WHEN c_warning.
        w_warning = w_warning + 1.
    ENDCASE.
  ENDLOOP.

  DESCRIBE TABLE it_9001 LINES w_total.
ENDFORM.                    " update_bom
*&---------------------------------------------------------------------*
*&      Form  generate_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc.
  DATA: lw_datuv(10).

  REFRESH: it_bdc.

  WRITE: it_9001-datuv TO lw_datuv.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-werks,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-stlan,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-stlal,    "ALT BOM
     ' ' 'RC29N-AENNR'     space,            "Change number
     ' ' 'RC29N-DATUV'     lw_datuv,         "Valid from
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,    "Item No
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',              "CHECK
     ' ' 'BDC_OKCODE'      '=PALL',

     'X' 'SAPLCSDI'        '2130',
     ' ' 'BDC_OKCODE'      '=PDAT',

     'X' 'SAPLCSDI'        '2130',
     ' ' 'RC29P-SANKO'     it_9001-sanko_n,  "Engineering/design
     ' ' 'RC29P-SANFE'     it_9001-sanfe_n,  "Production Relevant
     ' ' 'RC29P-SANKA'     it_9001-sanka_n,  "Costing Relevncy
     ' ' 'BDC_OKCODE'      '=FCBU'.
ENDFORM.                    " generate_bdc
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1212   text
*      -->P_1213   text
*      -->P_1214   text
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
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9001_ZMSG  text
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

* OUTPUT MODULE FOR TABSTRIP 'TAB_9000': SETS ACTIVE TAB
MODULE tab_9000_active_tab_set OUTPUT.
  tab_9000-activetab = g_tab_9000-pressed_tab.
  CASE g_tab_9000-pressed_tab.
    WHEN c_tab_9000-tab1.
      g_tab_9000-subscreen = '9001'.
    WHEN c_tab_9000-tab2.
      g_tab_9000-subscreen = '9002'.
    WHEN c_tab_9000-tab3.
      g_tab_9000-subscreen = '9003'.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.                    "tab_9000_active_tab_set OUTPUT

* INPUT MODULE FOR TABSTRIP 'TAB_9000': GETS ACTIVE TAB
MODULE tab_9000_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_9000-tab1.
      g_tab_9000-pressed_tab = c_tab_9000-tab1.
    WHEN c_tab_9000-tab2.
      g_tab_9000-pressed_tab = c_tab_9000-tab2.
    WHEN c_tab_9000-tab3.
      g_tab_9000-pressed_tab = c_tab_9000-tab3.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.                    "tab_9000_active_tab_get INPUT
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
      SET TITLEBAR  '9000'.
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
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'EXCUTE'.
      CLEAR: sy-ucomm.
      PERFORM excute_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_rtn.
  PERFORM update_bom.

*  SORT it_9000 BY icon matnr werks stlan stlal idnrk.
*
*  PERFORM assign_itab_to_alv_9000.
*  PERFORM sssign_event_9000.
ENDFORM.                    " EXCUTE_RTN
*&---------------------------------------------------------------------*
*&      Form  check_assy_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_assy_bom.
  DATA: lw_key(26).

  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  LOOP AT it_assy.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = c_capid
        datuv                 = p_datuv
        cuobj                 = c_cuobj
        mtnrv                 = it_assy-matnr
        stlan                 = c_stlan
        stlal                 = c_stlal
        werks                 = p_werks
        mmory                 = '0'
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
    IF sy-subrc NE 0 OR lw_topmat-stlal NE c_stlal.
      MOVE: c_error  TO it_assy-icon,
            text-m02 TO it_assy-msg.
      MODIFY it_assy.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.
      CONCATENATE it_assy-matnr lt_stb-stlkn INTO lw_key.

      CLEAR: it_9002.
      READ TABLE it_9002 WITH KEY key = lw_key.
      CHECK sy-subrc NE 0.

      READ TABLE it_bom_header WITH KEY matnr = lt_stb-idnrk
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        PERFORM append_it_bom USING it_assy-matnr c_assy ' ' lt_stb.
        PERFORM read_sub_bom USING c_assy lt_stb.
      ELSE.
        PERFORM append_it_bom USING it_assy-matnr c_assy 'X' lt_stb.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " check_assy_bom
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
    EXPORTING
      container_name              = <control>
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
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
    EXPORTING
      i_parent      = <container>
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
    WHEN '9001' OR '9100'.
      PERFORM set_attributes_alv_9001.
    WHEN '9002'.
      PERFORM set_attributes_alv_9002.
    WHEN '9003'.
      PERFORM set_attributes_alv_9003.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9001.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9001
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9002.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9002
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9003.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9003
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
    WHEN '9001' OR '9100'.
      PERFORM set_screen_fields_9001.
    WHEN '9002'.
      PERFORM set_screen_fields_9002.
    WHEN '9003'.
      PERFORM set_screen_fields_9003.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9001.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  ' ' 'COLTEXT'     'Component',
                                  'E' 'KEY'         'X',

                                  'S' 'TYPE'        ' ',
                                  ' ' 'COLTEXT'     'PType',
                                  'E' 'KEY'         'X',

                                  'S' 'EITM'        ' ',
                                  ' ' 'COLTEXT'     'EndItem',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  ' ' 'COLTEXT'     'Icon',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'WERKS'       ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLAN'       ' ',
                                  ' ' 'COLTEXT'     'Usage',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLAL'       ' ',
                                  ' ' 'COLTEXT'     'AltBOM',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'POSNR'       ' ',
                                  ' ' 'COLTEXT'     'Item',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SUFF'        ' ',
                                  ' ' 'COLTEXT'     'Suffix',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DATUV'       ' ',
                                  ' ' 'COLTEXT'     'Valid from',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLKN'       ' ',
                                  ' ' 'COLTEXT'     'Node',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SANKO'       ' ',
                                  ' ' 'COLTEXT'     'ENG',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'SANFE'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  ' ' 'COLTEXT'     'Prod',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'SANKA'       ' ',
                                  ' ' 'COLTEXT'     'Costing',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'SANKO_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  ' ' 'COLTEXT'     'ENG(N)',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'SANFE_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  ' ' 'COLTEXT'     'Prod(N)',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'SANKA_N'     ' ',
                                  ' ' 'COLTEXT'     'Costing(N)',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'MSG'         ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'EMPHASIZE'   'C500'.
ENDFORM.                    " set_screen_fields_9001
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9002.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'TYPE'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'EITM'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'WERKS'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLAN'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLAL'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'POSNR'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SUFF'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DATUV'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLKN'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SANKO'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'SANFE'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'SANKA'       ' ',
                                  'E' 'EMPHASIZE'   'C000',

                                  'S' 'KEY'         ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " set_screen_fields_9002
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9003.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'ITEM'        ' ',
                                  'E' 'KEY'         'X'.
ENDFORM.                    " set_screen_fields_9003
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
    EXPORTING
      i_structure_name = w_structure
      is_layout        = w_is_layout
      i_save           = w_save
      is_variant       = w_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_2200   text
*      -->P_2201   text
*      -->P_2202   text
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
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
    IF screen-name = 'P_WERKS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  check_mixed_component
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mixed_component.
  DATA: lt_9002 LIKE it_9002 OCCURS 0 WITH HEADER LINE.

  lt_9002[] = it_9002[].
  SORT lt_9002 BY idnrk type.

  LOOP AT lt_9002 WHERE type = c_both.
    MOVE: c_both TO it_9002-type.
    MODIFY  it_9002 TRANSPORTING type WHERE idnrk = lt_9002-idnrk.
  ENDLOOP.

  LOOP AT it_9002.
    CASE it_9002-type.
      WHEN c_mip.
        READ TABLE lt_9002 WITH KEY idnrk = it_9002-idnrk
                                    type  = c_assy
                           BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE: c_both TO it_9002-type.
          MODIFY  it_9002 TRANSPORTING type WHERE idnrk = it_9002-idnrk.
        ENDIF.
      WHEN c_assy.
        READ TABLE lt_9002 WITH KEY idnrk = it_9002-idnrk
                                    type  = c_mip
                           BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE: c_both TO it_9002-type.
          MODIFY  it_9002 TRANSPORTING type WHERE idnrk = it_9002-idnrk.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " check_mixed_component
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_multiple_usagerc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_multiple_usage USING pw_matnr.
  DATA: lw_idnrk LIKE mara-matnr.

  LOOP AT it_9002 WHERE matnr = pw_matnr
                    AND type  = c_assy.
    MOVE: c_both TO it_9002-type.
    MODIFY it_9002.

    READ TABLE it_9002 WITH KEY matnr = it_9002-idnrk.
    IF sy-subrc EQ 0.
      MOVE: it_9002-idnrk TO lw_idnrk.
      PERFORM check_multiple_usage USING lw_idnrk.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_multiple_usage
*&---------------------------------------------------------------------*
*&      Form  append_assy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_assy.
  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  LOOP AT it_mip.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = c_capid
        datuv                 = p_datuv
        cuobj                 = c_cuobj
        mtnrv                 = it_mip-matnr
        stlan                 = c_stlan
        stlal                 = c_stlal
        werks                 = p_werks
        mmory                 = '0'
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
    IF sy-subrc NE 0 OR lw_topmat-stlal NE c_stlal.
      MOVE: c_error  TO it_mip-icon,
            text-m02 TO it_mip-msg.
      MODIFY it_mip.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.
      READ TABLE it_assy WITH KEY matnr = lt_stb-idnrk.

      CHECK sy-subrc EQ 0.

      PERFORM append_it_bom USING it_mip-matnr c_mip ' ' lt_stb.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " append_assy
*&---------------------------------------------------------------------*
*&      Form  set_end_item_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_end_item_fields.
  CASE it_9002-eitm.
    WHEN 'G'.
      CASE it_9002-type.
        WHEN c_mip.
          MOVE: c_check TO it_9001-sanko_n,
                space   TO it_9001-sanfe_n,
                space   TO it_9001-sanka_n.
        WHEN c_assy.
          MOVE: space   TO it_9001-sanko_n,
                c_check TO it_9001-sanfe_n,
                c_check TO it_9001-sanka_n.
        WHEN c_both.
          MOVE: space   TO it_9001-sanko_n,
                c_check TO it_9001-sanfe_n,
                c_check TO it_9001-sanka_n,
                c_warning TO it_9001-icon,
                text-m03  TO it_9001-msg.
      ENDCASE.
    WHEN 'K'.
      CASE it_9002-type.
        WHEN c_mip.
          MOVE: space   TO it_9001-sanko_n,
                c_check TO it_9001-sanfe_n,
                c_check TO it_9001-sanka_n.
        WHEN c_assy.
          MOVE: c_check TO it_9001-sanko_n,
                space   TO it_9001-sanfe_n,
                space   TO it_9001-sanka_n.
        WHEN c_both.
          MOVE: c_check   TO it_9001-sanko_n,
                space     TO it_9001-sanfe_n,
                space     TO it_9001-sanka_n,
                c_warning TO it_9001-icon,
                text-m03  TO it_9001-msg.
      ENDCASE.
    WHEN OTHERS.
      CASE it_9002-type.
        WHEN c_both.
          MOVE: c_error  TO it_9001-icon,
                text-m03 TO it_9001-msg,
                space    TO it_9001-sanko_n,
                space    TO it_9001-sanfe_n,
                space    TO it_9001-sanka_n.
        WHEN OTHERS.
          MOVE: c_check TO it_9001-sanko_n,
                space   TO it_9001-sanfe_n,
                space   TO it_9001-sanka_n.
      ENDCASE.
  ENDCASE.
ENDFORM.                    " set_end_item_fields
*&---------------------------------------------------------------------*
*&      Form  append_1_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_1_level.
  PERFORM append_assy.
*  perform append_assy_1_level.
ENDFORM.                    " append_1_level
*&---------------------------------------------------------------------*
*&      Form  append_assy_1_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_assy_1_level.
  DATA: lw_key(26).

  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  LOOP AT it_assy.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
      EXPORTING
        capid                 = c_capid
        datuv                 = p_datuv
        cuobj                 = c_cuobj
        mtnrv                 = it_assy-matnr
        stlan                 = c_stlan
        stlal                 = c_stlal
        werks                 = p_werks
        mmory                 = '0'
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
    IF sy-subrc NE 0 OR lw_topmat-stlal NE c_stlal.
      MOVE: c_error  TO it_assy-icon,
            text-m02 TO it_assy-msg.
      MODIFY it_assy.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.
      CONCATENATE it_assy-matnr lt_stb-stlkn INTO lw_key.

      CLEAR: it_9002.
      READ TABLE it_9002 WITH KEY key = lw_key.
      CHECK sy-subrc NE 0.

      READ TABLE it_bom_header WITH KEY matnr = lt_stb-idnrk
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        PERFORM append_it_bom USING it_assy-matnr c_assy ' ' lt_stb.
        PERFORM read_sub_bom USING c_assy lt_stb.
      ELSE.
        PERFORM append_it_bom USING it_assy-matnr c_assy 'X' lt_stb.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " append_assy_1_level
