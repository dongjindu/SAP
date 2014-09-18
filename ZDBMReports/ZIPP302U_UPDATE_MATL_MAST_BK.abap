************************************************************************
* Program Name      : ZIPP302U_UPDATE_MATL_MAST
* Author            : Byung Sung Bae
* Creation Date     : 2005.02.01.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K913905
* Addl Documentation:
* Description       : Material Master Reivision after BOM Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT  zipp302u_update_matl_mast.
INCLUDE <icon>.
TABLES: mara, marc, t001w,
        ztbm_ipp302u_tmp.

DATA: zsbm_zipp302u_upt_mm_9001 LIKE zsbm_zipp302u_upt_mm_9001,
      zsbm_zipp302u_upt_mm_9002 LIKE zsbm_zipp302u_upt_mm_9002,
      zsbm_zipp302u_upt_mm_9100 LIKE zsbm_zipp302u_upt_mm_9001.

DATA: it_9001 TYPE STANDARD TABLE OF zsbm_zipp302u_upt_mm_9001
                                     WITH HEADER LINE.
DATA: it_9002 TYPE STANDARD TABLE OF zsbm_zipp302u_upt_mm_9002
                                     WITH HEADER LINE.
DATA: it_9100 TYPE STANDARD TABLE OF zsbm_zipp302u_upt_mm_9001
                                     WITH HEADER LINE.

DATA: it_9002_tmp LIKE it_9002 OCCURS 0 WITH HEADER LINE.

DATA: it_ipp302u_tmp LIKE ztbm_ipp302u_tmp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_fsc OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   mast-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
      END   OF it_fsc.

DATA: BEGIN OF it_end OCCURS 0,
        idnrk   LIKE   stpo-idnrk,
      END   OF it_end.

DATA: BEGIN OF it_bom OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   like   mast-stlal,
      END   OF it_bom.


DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*---// Global variables & Structures
DATA: BEGIN OF w_master,
        matnr     LIKE mara-matnr,
        maktx     LIKE makt-maktx,
        werks     LIKE marc-werks,
        mtart     LIKE mara-mtart,
        profl     LIKE mara-profl,
        normt     LIKE mara-normt,
        kzkfg     LIKE mara-kzkfg,
        dispo     LIKE marc-dispo,
        beskz     LIKE marc-beskz,
        sobsl     LIKE marc-sobsl,
        ncost     LIKE marc-ncost,
        verkz     LIKE marc-verkz,
      END   OF w_master.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

DATA: w_head  LIKE bapimathead,
      w_mara  LIKE bapi_mara,
      w_marax LIKE bapi_marax,
      w_marc  LIKE bapi_marc,
      w_marcx LIKE bapi_marcx.


DATA: w_stlan        LIKE   mast-stlan,
      w_success(4)   TYPE   n,
      w_error(4)     TYPE   n,
      w_ready(4)     TYPE   n,
      w_finish(4)    TYPE   n,
      w_total(4)     TYPE   n,
      w_ernam        LIKE   sy-uname,
      w_erdat        LIKE   sy-datum,
      w_erzet        LIKE   sy-uzeit.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_stlal   LIKE mast-stlal  VALUE '01',
           c_fsc                      VALUE 'F',
           c_halb                     VALUE 'H',
           c_end                      VALUE 'R',
           c_engine                   VALUE 'E',
           c_tm                       VALUE 'T',
           c_module                   VALUE 'M',
           c_success                  VALUE 'S',
           c_error                    VALUE 'E',
           c_ready                    VALUE 'R',
           c_finish                   VALUE 'F',
           c_warning                  VALUE 'W'.

* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
CONSTANTS: BEGIN OF c_tab_9000,
             tab1 LIKE sy-ucomm VALUE 'TAB_9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_9000_FC2',
           END OF c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
CONTROLS:  tab_9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab_9000,
             subscreen   LIKE sy-dynnr,
prog        LIKE sy-repid VALUE 'ZIPP302U_UPDATE_MATL_MAST',
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

DATA: wc_control_9100   TYPE        scrfname VALUE 'CC_9100_ALV',
      wc_alv_9100       TYPE REF TO cl_gui_alv_grid,
      wc_container_9100 TYPE REF TO cl_gui_custom_container.

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

DATA: w_container(100),
      w_control(100),
      w_alv(100),
      w_itab(100),
      w_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO   cl_gui_custom_container,
               <control>   TYPE          scrfname,
               <alv>       TYPE REF TO   cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

CONSTANTS: c_structure(100) VALUE 'ZSBM_ZIPP302U_UPT_MM_'.

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
PARAMETERS: p_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
*selection-screen skip.
SELECTION-SCREEN ULINE.
*selection-screen skip.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_check AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

*---// Check input fields & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM update_material_master.
  PERFORM count_rtn.
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

ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM set_fsc_list.
  PERFORM set_bom_header.
  PERFORM read_bom_structure.
  PERFORM set_matl_master.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  SET_BOM_STRUCTURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bom_structure.
  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  PERFORM display_progress_bar USING text-b06.

  LOOP AT it_fsc.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              capid                 = c_capid
              datuv                 = p_datuv
              cuobj                 = c_cuobj
              mtnrv                 = it_fsc-matnr
              stlan                 = it_fsc-stlan
              stlal                 = it_fsc-stlal
              werks                 = it_fsc-werks
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
    IF sy-subrc NE 0 OR lw_topmat-stlal NE it_fsc-stlal.
      DELETE it_fsc.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.
      PERFORM check_n_append_bom_for_fsc USING it_fsc-matnr lt_stb.
      PERFORM read_sub_bom USING it_fsc-matnr w_stlan lt_stb it_9002.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " SET_BOM_STRUCTURE
*&---------------------------------------------------------------------*
*&      Form  read_sub_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM read_sub_bom USING pw_matnr pw_stlan
                        pw_stb LIKE stpox
                        pw_bom LIKE it_9002.
  DATA: lt_stb    TYPE stpox OCCURS 0 WITH HEADER LINE.

  DATA: lw_topmat LIKE cstmat.

  CHECK pw_bom-stlkz EQ c_check.

  READ TABLE it_9002 WITH KEY matnr = pw_stb-idnrk
                              werks = pw_stb-werks
                              stlan = pw_stlan
                              stlal = pw_stb-stlal.
  CHECK sy-subrc NE 0.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid                 = c_capid
            datuv                 = p_datuv
            cuobj                 = c_cuobj
            mtnrv                 = pw_stb-idnrk
            stlan                 = pw_stlan
            stlal                 = c_stlal
            werks                 = pw_stb-werks
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
    EXIT.
  ENDIF.

  LOOP AT lt_stb.
    PERFORM check_n_append_it_9002 USING pw_stb-idnrk pw_stlan lt_stb.
    PERFORM read_sub_bom USING pw_stb-idnrk pw_stlan lt_stb it_9002.
  ENDLOOP.
  IF sy-subrc NE 0.
    CLEAR: it_9002-stlkz.
    MODIFY it_9002 TRANSPORTING stlkz WHERE matnr = pw_matnr
                                        AND werks = pw_stb-werks
                                        AND stlan = pw_stb-stlan
                                        AND stlal = pw_stb-stlal
                                        AND idnrk = pw_stb-idnrk.
    EXIT.
  ENDIF.
ENDFORM.                    " read_sub_bom
*&---------------------------------------------------------------------*
*&      Form  check_n_append_IT_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM check_n_append_it_9002 USING pw_matnr pw_stlan
                                  pw_stb   LIKE stpox.
  CLEAR: it_9002.

  MOVE-CORRESPONDING pw_stb TO it_9002.
  MOVE: c_stlal             TO it_9002-stlal,
        pw_matnr            TO it_9002-matnr.

*---// Module check
  IF pw_matnr+6(2) EQ pw_stb-idnrk(2).    "Module
    MOVE: c_module TO it_9002-node_type.
  ENDIF.

*---// Engine Check
  IF pw_stb-zinfo EQ 'ENG' OR
     pw_stb-zinfo EQ 'TM'.
    SELECT SINGLE * FROM marc WHERE matnr = pw_stb-idnrk
                                AND verkz = 'X'.
    IF sy-subrc EQ 0.
      MOVE: marc-werks TO it_9002-werks.
    ENDIF.

    CASE pw_stb-zinfo.
      WHEN 'ENG'.
        MOVE: c_engine TO it_9002-node_type.
      WHEN 'TM'.
        MOVE: c_tm     TO it_9002-node_type.
    ENDCASE.
  ENDIF.

*---// Sub BOM check
  READ TABLE it_bom WITH KEY matnr = pw_stb-idnrk
                             werks = it_9002-werks
                             stlan = w_stlan
                    BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE: c_check TO it_9002-stlkz.
  ENDIF.

*---// Color Check
  IF NOT pw_stb-knobj IS INITIAL.
    MOVE: c_check TO it_9002-kzbez.
  ENDIF.

  APPEND it_9002.
ENDFORM.                    " check_n_append_IT_9002
*&---------------------------------------------------------------------*
*&      Form  check_n_append_bom_for_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_STB  text
*----------------------------------------------------------------------*
FORM check_n_append_bom_for_fsc USING pw_matnr pw_stb LIKE stpox.
  CLEAR: it_9002.

  MOVE-CORRESPONDING pw_stb TO it_9002.
  MOVE: pw_matnr TO it_9002-matnr.

*---// Module check
  IF pw_matnr+6(2) EQ pw_stb-idnrk(2).    "Module
    MOVE: '2'      TO w_stlan,
          c_module TO it_9002-node_type.
  ELSE.
    MOVE: '1' TO w_stlan.
  ENDIF.

*---// Engine Check
  IF pw_stb-zinfo EQ 'ENG' OR
     pw_stb-zinfo EQ 'TM'.
    SELECT SINGLE * FROM marc WHERE matnr = pw_stb-idnrk
                                AND verkz = 'X'.
    IF sy-subrc EQ 0.
      MOVE: marc-werks TO it_9002-werks.
    ENDIF.

    CASE pw_stb-zinfo.
      WHEN 'ENG'.
        MOVE: c_engine TO it_9002-node_type.
      WHEN 'TM'.
        MOVE: c_tm     TO it_9002-node_type.
    ENDCASE.
  ENDIF.

*---// Sub BOM check
  READ TABLE it_bom WITH KEY matnr = pw_stb-idnrk
                             werks = it_9002-werks
                             stlan = it_9002-stlan
                    BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE: c_check TO it_9002-stlkz.
  ENDIF.

*---// Color Check
  IF NOT pw_stb-knobj IS INITIAL.
    MOVE: c_check TO it_9002-kzbez.
  ENDIF.

  APPEND it_9002.
ENDFORM.                    " check_n_append_bom_for_fsc
*&---------------------------------------------------------------------*
*&      Form  check_matl_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_matl_master.
  PERFORM display_progress_bar USING text-b07.
  PERFORM append_fsc_to_it_9001.
  PERFORM append_halb_end_to_it_9001.
  PERFORM check_it_9001.
ENDFORM.                    " check_matl_master
*&---------------------------------------------------------------------*
*&      Form  get_matl_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_END_IDNRK  text
*----------------------------------------------------------------------*
FORM get_matl_master USING pw_matnr pw_werks
                           pw_master LIKE w_master.
  CLEAR: pw_master.

  SELECT SINGLE a~matnr b~maktx c~werks a~mtart a~profl a~normt
                a~kzkfg c~dispo c~beskz c~sobsl c~ncost c~verkz
    INTO CORRESPONDING FIELDS OF pw_master
    FROM mara AS a LEFT OUTER JOIN makt AS b
                      ON a~matnr EQ b~matnr
                     AND b~spras EQ sy-langu
                   LEFT OUTER JOIN marc AS c
                      ON a~matnr EQ c~matnr
                     AND c~werks EQ pw_werks
   WHERE a~matnr EQ pw_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " get_matl_master
*&---------------------------------------------------------------------*
*&      Form  append_fsc_to_IT_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_fsc_to_it_9001.
  SORT it_fsc BY matnr werks stlan stlal.
  LOOP AT it_fsc.
    ON CHANGE OF it_fsc-matnr.
      PERFORM append_it_9001 USING it_fsc-matnr it_fsc-werks
                                   space        it_fsc-stlan
                                   c_fsc         space
                                   'X'.
    ENDON.
  ENDLOOP.
ENDFORM.                    " append_fsc_to_IT_9001
*&---------------------------------------------------------------------*
*&      Form  APPEND_IT_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MATNR  text
*      -->P_WERKS  text
*      -->P_C_END  text
*----------------------------------------------------------------------*
FORM append_it_9001 USING pw_matnr pw_werks     pw_normt
                          pw_stlan pw_node_type pw_color pw_stlkz.
  CLEAR: it_9001.

  PERFORM get_matl_master USING pw_matnr pw_werks
                                w_master.

  MOVE: pw_matnr       TO it_9001-matnr,
        w_master-maktx TO it_9001-maktx,
        pw_werks       TO it_9001-werks,
        w_master-mtart TO it_9001-mtart,
        w_master-profl TO it_9001-profl_m,
        w_master-normt TO it_9001-normt_m,
        w_master-kzkfg TO it_9001-kzkfg_m,
        w_master-dispo TO it_9001-dispo_m,
        w_master-beskz TO it_9001-beskz_m,
        w_master-sobsl TO it_9001-sobsl_m,
        w_master-ncost TO it_9001-ncost_m,
        pw_node_type   TO it_9001-node_type,
        pw_stlkz       TO it_9001-stlkz.

  CASE pw_node_type.
    WHEN c_fsc.
      MOVE: 'M'             TO it_9001-profl_n,
            it_9001-normt_m TO it_9001-normt_n,
            c_check         TO it_9001-kzkfg_n,
            it_9001-dispo_m TO it_9001-dispo_n,
            it_9001-beskz_m TO it_9001-beskz_n,
            it_9001-sobsl_m TO it_9001-sobsl_n,
            it_9001-ncost_m TO it_9001-ncost_n.
    WHEN c_halb.
      MOVE: it_9001-profl_m TO it_9001-profl_n,
            pw_normt        TO it_9001-normt_n,
            it_9001-dispo_m TO it_9001-dispo_n.

      IF pw_color EQ c_check.
        MOVE: 'X'      TO it_9001-kzkfg_n.
      ELSE.
        MOVE: space    TO it_9001-kzkfg_n.
      ENDIF.

      IF w_master-verkz EQ c_check.
        MOVE: 'X'      TO it_9001-beskz_n,
              '50'     TO it_9001-sobsl_n,
              'X'      TO it_9001-ncost_n.
      ELSE.
        MOVE: space    TO it_9001-beskz_n,
              space    TO it_9001-sobsl_n,
              space    TO it_9001-ncost_n.
      ENDIF.
    WHEN c_module.
      MOVE: it_9001-profl_m TO it_9001-profl_n,
            pw_normt        TO it_9001-normt_n,
            space           TO it_9001-kzkfg_n,
            'M01'           TO it_9001-dispo_n,
            it_9001-beskz_m TO it_9001-beskz_n,
            it_9001-sobsl_m TO it_9001-sobsl_n,
            it_9001-ncost_m TO it_9001-ncost_n.
    WHEN c_engine OR c_tm.
      MOVE: it_9001-profl_m TO it_9001-profl_n,
            pw_normt        TO it_9001-normt_n,
            it_9001-dispo_m TO it_9001-dispo_n,
            it_9001-beskz_m TO it_9001-beskz_n,
            it_9001-sobsl_m TO it_9001-sobsl_n,
            it_9001-ncost_m TO it_9001-ncost_n.

      IF pw_color EQ c_check.
        MOVE: 'X'      TO it_9001-kzkfg_n.
      ELSE.
        MOVE: space    TO it_9001-kzkfg_n.
      ENDIF.
    WHEN c_end.
      MOVE: it_9001-profl_m TO it_9001-profl_n,
            pw_normt        TO it_9001-normt_n,
            space           TO it_9001-kzkfg_n,
            it_9001-beskz_m TO it_9001-beskz_n,
            it_9001-sobsl_m TO it_9001-sobsl_n,
            it_9001-ncost_m TO it_9001-ncost_n.

      IF pw_stlan EQ '2'.
        MOVE: 'M02'    TO it_9001-dispo_n.
      ELSE.
        MOVE: it_9001-dispo_m TO it_9001-dispo_n.
      ENDIF.
  ENDCASE.

  APPEND it_9001.
ENDFORM.                    " APPEND_IT_9001
*&---------------------------------------------------------------------*
*&      Form  CHECK_MRP_CONTROLLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mrp_controller USING pw_idnrk pw_werks pw_stlan pw_dispo.
  CHECK pw_stlan EQ '2' AND pw_dispo EQ 'M02'.

  SELECT SINGLE * FROM ztbm_ipp302u_tmp WHERE idnrk = pw_idnrk
                                          AND stlan = '1'.
  IF sy-subrc EQ 0.
    MOVE: 'M03' TO it_9001-dispo_n.

    MODIFY it_9001 TRANSPORTING dispo_n WHERE matnr = pw_idnrk
                                          AND werks = pw_werks.
  ENDIF.
ENDFORM.                    " CHECK_MRP_CONTROLLER
*&---------------------------------------------------------------------*
*&      Form  CHECK_HALB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9002_MATNR  text
*      -->P_IT_9002_KZBEZ  text
*----------------------------------------------------------------------*
FORM append_halb_to_it_9001 USING pw_color
                                  pw_bom LIKE it_9002.
  DATA: lt_9002 LIKE it_9002 OCCURS 0 WITH HEADER LINE.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_9002
    FROM ztbm_ipp302u_tmp
   WHERE idnrk = pw_bom-matnr.
  LOOP AT lt_9002.
    READ TABLE it_9001 WITH KEY matnr = lt_9002-idnrk
                                werks = lt_9002-werks.
    IF sy-subrc NE 0.
      IF lt_9002-node_type IS INITIAL.
        MOVE: c_halb TO lt_9002-node_type.
      ENDIF.

      PERFORM append_it_9001 USING lt_9002-idnrk     lt_9002-werks
                                   lt_9002-stgb      lt_9002-stlan
                                   lt_9002-node_type pw_color
                                   lt_9002-stlkz.
    ENDIF.

    PERFORM check_color_od USING lt_9002-idnrk lt_9002-werks pw_color
                                 lt_9002-kzbez lt_9002-node_type.

    PERFORM append_halb_to_it_9001 USING lt_9002-kzbez lt_9002.
  ENDLOOP.



*  LOOP AT it_9002_tmp INTO lw_bom WHERE idnrk = pw_bom-matnr.
*    READ TABLE it_9001 WITH KEY matnr = lw_bom-idnrk
*                                werks = lw_bom-werks
*                                BINARY SEARCH.
*    IF sy-subrc NE 0.
*      IF lw_bom-node_type IS INITIAL.
*        MOVE: c_halb TO lw_bom-node_type.
*      ENDIF.
*
*      PERFORM append_it_9001 USING lw_bom-idnrk     lw_bom-werks
*                                   lw_bom-stgb      lw_bom-stlan
*                                   lw_bom-node_type pw_color
*                                   lw_bom-stlkz.
*    ENDIF.
*
*    PERFORM check_color_od USING lw_bom-idnrk lw_bom-werks pw_color
*                                 lw_bom-kzbez lw_bom-node_type.
*
*    DELETE it_9002_tmp.
*
*    PERFORM append_halb_to_it_9001 USING lw_bom-kzbez lw_bom.
*  ENDLOOP.
ENDFORM.                    " CHECK_HALB
*&---------------------------------------------------------------------*
*&      Form  check_color_od
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_IDNRK  text
*      -->P_PW_WERKS  text
*----------------------------------------------------------------------*
FORM check_color_od USING pw_matnr pw_werks pw_child_color
                          pw_current_color  pw_node_type.
  CHECK pw_node_type NE c_module.

  IF pw_child_color   EQ c_check AND
     pw_current_color EQ c_check.
    MOVE: c_error        TO it_9001-flag,
          icon_red_light TO it_9001-icon,
          text-b01       TO it_9001-zmsg.
    MODIFY it_9001 TRANSPORTING flag icon zmsg
     WHERE matnr = pw_matnr
       AND werks = pw_werks.
    EXIT.
  ENDIF.

  IF pw_current_color EQ 'X' AND
     pw_child_color   EQ space.
    MOVE: c_error        TO it_9001-flag,
          icon_red_light TO it_9001-icon,
          text-b02       TO it_9001-zmsg.
    MODIFY it_9001 TRANSPORTING flag icon zmsg
     WHERE matnr = pw_matnr
       AND werks = pw_werks.
    EXIT.
  ENDIF.

  CHECK pw_child_color EQ c_check.

  MOVE: 'X' TO it_9001-kzkfg_n.

  MODIFY it_9001 TRANSPORTING kzkfg_n WHERE matnr = pw_matnr
                                        AND werks = pw_werks.
ENDFORM.                    " check_color_od
*&---------------------------------------------------------------------*
*&      Form  append_halb_end_to_IT_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_halb_end_to_it_9001.
  PERFORM set_temp_table.
  PERFORM set_end_item.
  PERFORM set_it_9001.

  DELETE FROM ztbm_ipp302u_tmp WHERE ernam = w_ernam
                                 AND erdat = w_erdat
                                 AND erzet = w_erzet.
ENDFORM.                    " append_halb_end_to_IT_9001
*&---------------------------------------------------------------------*
*&      Form  SET_FSC_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fsc_list.
  PERFORM display_progress_bar USING text-b05.

  SELECT a~matnr b~werks c~stlan c~stlal
    INTO CORRESPONDING FIELDS OF TABLE it_fsc
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr EQ b~matnr
                     AND b~lvorm EQ space
                   INNER JOIN mast AS c
                      ON b~matnr EQ c~matnr
                     AND b~werks EQ c~werks
                   INNER JOIN stko AS d
                      ON c~stlnr EQ d~stlnr
                     AND c~stlal EQ d~stlal
                     AND d~stlty EQ 'M'
                     AND d~lkenz EQ space
                     AND d~loekz EQ space
                     AND d~stlst EQ '01'
   WHERE a~mtart EQ 'FERT'
     AND c~stlan IN ('1','2')
     AND a~matnr IN s_matnr.
ENDFORM.                " SET_FSC_LIST
*&---------------------------------------------------------------------*
*&      Form  check_it_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_it_9001.
  LOOP AT it_9001 WHERE flag EQ space.
    CASE it_9001-node_type.
      WHEN c_fsc.
        IF it_9001-profl_m NE it_9001-profl_n OR
           it_9001-kzkfg_m NE it_9001-kzkfg_n.
          MOVE: c_ready           TO it_9001-flag,
                icon_yellow_light TO it_9001-icon.
        ELSE.
          MOVE: c_finish       TO it_9001-flag,
                icon_light_out TO it_9001-icon.
        ENDIF.
      WHEN c_halb.
        IF it_9001-normt_m NE it_9001-normt_n OR
           it_9001-kzkfg_m NE it_9001-kzkfg_n OR
           it_9001-dispo_m NE it_9001-dispo_n OR
           it_9001-beskz_m NE it_9001-beskz_n OR
           it_9001-sobsl_m NE it_9001-sobsl_n OR
           it_9001-ncost_m NE it_9001-ncost_n.
          MOVE: c_ready           TO it_9001-flag,
                icon_yellow_light TO it_9001-icon.
        ELSE.
          MOVE: c_finish       TO it_9001-flag,
                icon_light_out TO it_9001-icon.
        ENDIF.
      WHEN c_end.
        IF it_9001-normt_m NE it_9001-normt_n OR
           it_9001-dispo_m NE it_9001-dispo_n.
          MOVE: c_ready           TO it_9001-flag,
                icon_yellow_light TO it_9001-icon.
        ELSE.
          MOVE: c_finish       TO it_9001-flag,
                icon_light_out TO it_9001-icon.
        ENDIF.

        IF NOT ( it_9001-mtart EQ 'ROH'   OR
                 it_9001-mtart EQ 'ROH1' ).
          MOVE: c_error        TO it_9001-flag,
                icon_red_light TO it_9001-icon.
          CONCATENATE text-b03 it_9001-mtart INTO it_9001-zmsg
            SEPARATED BY space.
        ENDIF.
      WHEN c_module.
        IF it_9001-normt_m NE it_9001-normt_n OR
           it_9001-kzkfg_m NE it_9001-kzkfg_n OR
           it_9001-dispo_m NE it_9001-dispo_n.
          MOVE: c_ready           TO it_9001-flag,
                icon_yellow_light TO it_9001-icon.
        ELSE.
          MOVE: c_finish       TO it_9001-flag,
                icon_light_out TO it_9001-icon.
        ENDIF.

        IF NOT ( it_9001-mtart EQ 'ROH'   OR
                 it_9001-mtart EQ 'ROH1' ).
          MOVE: c_error        TO it_9001-flag,
                icon_red_light TO it_9001-icon.
          CONCATENATE text-b03 it_9001-mtart INTO it_9001-zmsg
            SEPARATED BY space.
        ENDIF.

        IF it_9001-stlkz EQ space.
          MOVE: c_error        TO it_9001-flag,
                icon_red_light TO it_9001-icon,
                text-b13       TO it_9001-zmsg.
        ENDIF.
      WHEN c_engine OR c_tm.
        IF it_9001-normt_m NE it_9001-normt_n OR
           it_9001-kzkfg_m NE it_9001-kzkfg_n.
          MOVE: c_ready           TO it_9001-flag,
                icon_yellow_light TO it_9001-icon.
        ELSE.
          MOVE: c_finish       TO it_9001-flag,
                icon_light_out TO it_9001-icon.
        ENDIF.

        CASE it_9001-mtart.
          WHEN 'ROH'.
            IF it_9001-stlkz EQ c_check.
              CONCATENATE text-b10 it_9001-mtart text-b11
                     INTO it_9001-zmsg.
            ENDIF.
          WHEN 'HALB'.
            IF it_9001-stlkz EQ space.
              CONCATENATE text-b10 it_9001-mtart text-b12
                     INTO it_9001-zmsg.
            ENDIF.
          WHEN OTHERS.
            CONCATENATE text-b10 it_9001-mtart
                   INTO it_9001-zmsg.
        ENDCASE.
    ENDCASE.

    MODIFY it_9001.
  ENDLOOP.
ENDFORM.                    " check_it_9002
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM display_progress_bar USING text-b09.

  IF sy-batch EQ space.
    SORT it_9001 BY flag matnr werks mtart node_type.
    SORT it_9002 BY matnr werks stlan stlal idnrk.
    CALL SCREEN 9000.
  ELSE.
    it_9100[] = it_9001[].
    SORT it_9100 BY flag matnr werks mtart node_type.
    CALL SCREEN 9100.
  ENDIF.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  process_upt_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_material_master.
  CHECK p_check EQ c_check.

  PERFORM display_progress_bar USING text-b08.

  PERFORM update_master.
ENDFORM.                    " process_upt_material_master

* OUTPUT MODULE FOR TABSTRIP 'TAB_9000': SETS ACTIVE TAB
MODULE tab_9000_active_tab_set OUTPUT.
  tab_9000-activetab = g_tab_9000-pressed_tab.
  CASE g_tab_9000-pressed_tab.
    WHEN c_tab_9000-tab1.
      g_tab_9000-subscreen = '9001'.
    WHEN c_tab_9000-tab2.
      g_tab_9000-subscreen = '9002'.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.

* INPUT MODULE FOR TABSTRIP 'TAB_9000': GETS ACTIVE TAB
MODULE tab_9000_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_9000-tab1.
      g_tab_9000-pressed_tab = c_tab_9000-tab1.
    WHEN c_tab_9000-tab2.
      g_tab_9000-pressed_tab = c_tab_9000-tab2.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.
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
    WHEN '9001'.
      PERFORM set_attributes_alv_9001.
    WHEN '9002'.
      PERFORM set_attributes_alv_9002.
    WHEN '9100'.
      PERFORM set_attributes_alv_9100.
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
    WHEN '9001'.
      PERFORM set_screen_fields_9001.
    WHEN '9002'.
      PERFORM set_screen_fields_9002.
    WHEN '9100'.
      PERFORM set_screen_fields_9001.
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
                                  'E' 'KEY'         'X',

                                  'S' 'WERKS'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MTART'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'NODE_TYPE'   ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLKZ'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'PROFL_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NORMT_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'KZKFG_M'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DISPO_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'BESKZ_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SOBSL_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NCOST_M'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PROFL_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'NORMT_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'KZKFG_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'DISPO_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'BESKZ_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'SOBSL_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'NCOST_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
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

                                  'S' 'WERKS'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLAN'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLAL'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'OJTXP'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MTART'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NODE_TYPE'   ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'STLKZ'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'KZBEZ'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STGB'        ' ',
                                  'E' 'EMPHASIZE'   'C400'.
ENDFORM.                    " set_screen_fields_9002
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_2617   text
*      -->P_2618   text
*      -->P_2619   text
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
    WHEN 'EXECUTE'.
      CLEAR sy-ucomm.
      PERFORM update_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9100.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9100
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9100.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'WERKS'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MTART'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'NODE_TYPE'   ' ',
                                  ' ' 'COLTEXT'     'Node',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'PROFL_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NORMT_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'KZKFG_M'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DISPO_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'BESKZ_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SOBSL_M'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NCOST_M'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PROFL_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'NORMT_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'KZKFG_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'DISPO_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'BESKZ_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'SOBSL_N'     ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'NCOST_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " set_screen_fields_9100
*&---------------------------------------------------------------------*
*&      Form  update_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_rtn.
  PERFORM update_master.
  PERFORM count_rtn.
  PERFORM set_attributes_alv_grid USING '9001'.
  PERFORM build_field_catalog USING '9001'.
  PERFORM assign_itab_to_alv USING '9001'.
ENDFORM.                    " update_rtn
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_master.
  LOOP AT it_9001 WHERE flag EQ c_ready.
    CLEAR: w_head, w_mara, w_marax, w_marc, w_marcx,
    it_bapiret2, it_bapiret2[].

    MOVE: it_9001-matnr TO w_head-material,
          'X'           TO w_head-basic_view,
          'X'           TO w_head-mrp_view,
          'X'           TO w_head-cost_view.

    MOVE: it_9001-profl_n TO w_mara-hazmatprof,
          it_9001-normt_n TO w_mara-std_descr,
*          it_9001-kzkfg_n TO w_mara-cm_relevance_flag,
          it_9001-werks   TO w_marc-plant,
          it_9001-dispo_n TO w_marc-mrp_ctrler,
          it_9001-beskz_n TO w_marc-proc_type,
          it_9001-sobsl_n TO w_marc-spproctype,
          it_9001-ncost_n TO w_marc-no_costing,
          'X'             TO w_marax-hazmatprof,
          'X'             TO w_marax-std_descr,
*          'X'             TO w_marax-cm_relevance_flag,
          it_9001-werks   TO w_marcx-plant,
          'X'             TO w_marcx-mrp_ctrler,
          'X'             TO w_marcx-proc_type,
          'X'             TO w_marcx-spproctype,
          'X'             TO w_marcx-no_costing.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = w_head
              clientdata     = w_mara
              clientdatax    = w_marax
              plantdata      = w_marc
              plantdatax     = w_marcx
         TABLES
              returnmessages = it_bapiret2.
    LOOP AT it_bapiret2 WHERE type = 'E'
                           OR type = 'A'.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      MOVE: c_error             TO it_9001-flag,
            icon_red_light      TO it_9001-icon,
            it_bapiret2-message TO it_9001-zmsg.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
      IF it_9001-kzkfg_m NE it_9001-kzkfg_n.
        PERFORM change_configurable_field.
      ELSE.
        MOVE: c_success           TO it_9001-flag,
              icon_green_light    TO it_9001-icon.
      ENDIF.
    ENDIF.

    MODIFY it_9001.
  ENDLOOP.
ENDFORM.                    " UPDATE_MASTER
*&---------------------------------------------------------------------*
*&      Form  COUNT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn.
  CLEAR: w_total, w_finish, w_success, w_error, w_ready.
  LOOP AT it_9001.
    w_total = w_total + 1.

    CASE it_9001-flag.
      WHEN c_success.
        w_success = w_success + 1.
      WHEN c_finish.
        w_finish = w_finish + 1.
      WHEN c_ready.
        w_ready = w_ready + 1.
      WHEN c_error.
        w_error = w_error + 1.
    ENDCASE.
  ENDLOOP.

  IF w_error > 0.
    MESSAGE s000(zz) WITH w_error text-m02.
  ELSE.
    IF w_ready > 0.
      MESSAGE s000(zz) WITH w_ready text-m04.
    ELSE.
      MESSAGE s000(zz) WITH text-m03.
    ENDIF.
  ENDIF.
ENDFORM.                    " COUNT_RTN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_CONFIGURABLE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_configurable_field.
  REFRESH: it_bdc.

  PERFORM dynpro USING:
     'X' 'SAPLMGMM'            '0060',
     ' ' 'RMMG1-MATNR'         it_9001-matnr,
     ' ' 'BDC_OKCODE'          '=FCPU',

     'X' 'SAPLMGMM'            '0070',
     ' ' 'MSICHTAUSW-DYTXT(2)' 'X',
     ' ' 'BDC_OKCODE'          '/00',


     'X' 'SAPLMGMM'            '5004',
     ' ' 'MARA-KZKFG'          it_9001-kzkfg_n,
     ' ' 'BDC_OKCODE'          '=BU'.

  CALL TRANSACTION 'MM02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '801'.
    MOVE: c_error             TO it_9001-flag,
          icon_red_light      TO it_9001-icon.
    PERFORM get_err_msg USING it_9001-zmsg.
  ELSE.
    MOVE: c_success           TO it_9001-flag,
          icon_green_light    TO it_9001-icon.
  ENDIF.
ENDFORM.                    " CHANGE_CONFIGURABLE_FIELD
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3789   text
*      -->P_3790   text
*      -->P_3791   text
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

  MOVE: sy-uname TO w_ernam,
        sy-datum TO w_erdat,
        sy-uzeit TO w_erzet.
ENDFORM.                    " initialization
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

  MOVE: lw_msg TO lw_msg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_B05  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  SET_BOM_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_bom_header.
  SELECT a~matnr b~werks c~stlan
    INTO CORRESPONDING FIELDS OF TABLE it_bom
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr EQ b~matnr
                     AND b~lvorm EQ space
                   INNER JOIN mast AS c
                      ON b~matnr EQ c~matnr
                     AND b~werks EQ c~werks
                     AND c~stlan EQ '01'
                   INNER JOIN stko AS d
                      ON c~stlnr EQ d~stlnr
                     AND c~stlal EQ d~stlal
                     AND d~stlty EQ 'M'
                     AND d~lkenz EQ space
                     AND d~loekz EQ space
                     AND d~stlst EQ '01'
                     AND d~datuv <= p_datuv.

  SORT it_bom BY matnr werks stlan stlal.
ENDFORM.                    " SET_BOM_HEADER
*&---------------------------------------------------------------------*
*&      Form  set_temp_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_temp_table.
  sort it_9002 by matnr werks stlan stlal idnrk kzbez.
  LOOP AT it_9002 where kzbez eq space.
    if it_9002-kzbez is initial.
      read table it_9002 with key matnr = it_9002-matnr
                                  werks = it_9002-werks
                                  stlan = it_9002-stlan
                                  stlal = it_9002-stlal
                                  idnrk = it_9002-idnrk
                                  kzbez = 'X'
                         transporting no fields
                         binary search.
      if sy-subrc eq 0.
        delete it_9002.
      endif.
    endif.
  endloop.

  DELETE ADJACENT DUPLICATES FROM it_9002
                  comparing matnr werks stlan stlal idnrk.

  LOOP AT it_9002.
    CLEAR: it_ipp302u_tmp.

    MOVE: w_ernam TO it_ipp302u_tmp-ernam,
          w_erdat TO it_ipp302u_tmp-erdat,
          w_erzet TO it_ipp302u_tmp-erzet.

    MOVE-CORRESPONDING it_9002 TO it_ipp302u_tmp.

    APPEND it_ipp302u_tmp.
  ENDLOOP.

  SORT it_ipp302u_tmp BY matnr werks stlan stlal idnrk.
  INSERT ztbm_ipp302u_tmp FROM TABLE it_ipp302u_tmp
                          ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m01.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.                    " set_temp_table
*&---------------------------------------------------------------------*
*&      Form  set_end_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_end_item.
  SELECT idnrk
    INTO TABLE it_end
    FROM ztbm_ipp302u_tmp
   WHERE   node_type EQ c_end
      OR ( node_type IN (c_engine,c_tm) AND
           stlkz     EQ space ).
ENDFORM.                    " set_end_item
*&---------------------------------------------------------------------*
*&      Form  set_it_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9001.
  DATA: lt_9002 LIKE it_9002 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_end.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lt_9002
      FROM ztbm_ipp302u_tmp
     WHERE idnrk = it_end-idnrk.
    LOOP AT lt_9002.
      READ TABLE it_9001 WITH KEY matnr = lt_9002-idnrk
                                  werks = lt_9002-werks.
      IF sy-subrc NE 0.
        IF lt_9002-node_type IS INITIAL.
          MOVE: c_end TO lt_9002-node_type.
        ENDIF.

        PERFORM append_it_9001 USING lt_9002-idnrk lt_9002-werks
                                     lt_9002-stgb  lt_9002-stlan
                                     lt_9002-node_type
                                     lt_9002-kzbez
                                     lt_9002-stlkz.
      ENDIF.

      PERFORM check_mrp_controller USING lt_9002-idnrk
                                         lt_9002-werks
                                         lt_9002-stlan
                                         it_9001-dispo_n.

      PERFORM append_halb_to_it_9001 USING lt_9002-kzbez
                                           lt_9002.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " set_it_9001
