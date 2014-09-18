************************************************************************
* Program Name      : ZAPP723_MAINTAIN_ENGINE_TM_BK
* Author            : Bongsoo, Kim
* Creation Date     : 2004.03.25.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : Maintain Engine T/M
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zapp723_maintain_engine_tm.
INCLUDE <icon>.

TABLES: mara, marc, aenr.

*---// Internal tables
DATA: BEGIN OF it_fsc OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
        maktx   LIKE   makt-maktx,
      END   OF it_fsc.

DATA: it_9000 LIKE zspp_zapp723 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*---// Global variables & Structures
DATA: w_success(4)   TYPE   n,
      w_error(4)     TYPE   n,
      w_ready(4)     TYPE   n,
      w_finish(4)    TYPE   n,
      w_total(4)     TYPE   n.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_engine  LIKE cabn-atnam  VALUE 'P_ALC_U_1', "Engine
           c_tm      LIKE cabn-atnam  VALUE 'P_ALC_U_2', "T/M
           c_engtxt  LIKE stpo-zinfo  VALUE 'ENG',
           c_tmtxt   LIKE stpo-zinfo  VALUE 'TM',
           c_capid   LIKE rc29l-capid VALUE 'PP01',      "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_datuv   LIKE sy-datum VALUE '19000101',  "Initial date
           c_datub   LIKE sy-datum VALUE '99991230',  "End date
           c_object  LIKE inri-object VALUE 'ZBM_CHG_NO',"Number Range
           c_success                  VALUE 'S',         "Success
           c_create                   VALUE 'C',
           c_update                   VALUE 'U',
           c_finish                   VALUE 'F',
           c_error                    VALUE 'E'.         "Error

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
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_ersda FOR mara-ersda.
PARAMETERS: p_datuv LIKE stko-datuv DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_update  DEFAULT ' '     AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_bom_infomation.

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
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
*  PERFORM check_matnr.
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr
                              AND mtart EQ 'FERT'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  read_bom_infomation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bom_infomation.
  PERFORM get_fsc_list.
  PERFORM get_alc_engine_tm_code.
  PERFORM get_sap_engine_tm_code.
  PERFORM set_engine_tm_mode.
  PERFORM set_count.
ENDFORM.                    " read_bom_infomation
*&---------------------------------------------------------------------*
*&      Form  get_fsc_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_list.
  SELECT a~matnr b~werks b~stlan b~stlal d~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_fsc
    FROM mara AS a INNER JOIN mast AS b
                      ON a~matnr = b~matnr
                     AND b~stlan = '1'
                   INNER JOIN stko AS c
                      ON b~stlnr = c~stlnr
                     AND b~stlal = c~stlal
                     AND c~stlty = 'M'
                     AND c~stlst = '01'
                   INNER JOIN makt AS d
                      ON b~matnr = d~matnr
                     AND d~spras = sy-langu
   WHERE a~matnr IN s_matnr
     AND a~ersda IN s_ersda
     AND a~mtart EQ 'FERT'.

  LOOP AT it_fsc.
    MOVE: it_fsc-matnr TO it_9000-matnr,
          it_fsc-werks TO it_9000-werks,
          it_fsc-stlan TO it_9000-stlan,
          it_fsc-stlal TO it_9000-stlal,
          it_fsc-maktx TO it_9000-maktx,
          '8000'       TO it_9000-posnr.

    MOVE: c_engine     TO it_9000-atnam,
          c_engtxt     TO it_9000-engtm.

    COLLECT it_9000.

    MOVE: c_tm         TO it_9000-atnam,
          c_tmtxt      TO it_9000-engtm.

    COLLECT it_9000.
  ENDLOOP.
ENDFORM.                    " get_fsc_list
*&---------------------------------------------------------------------*
*&      Form  GET_ALC_ENGINE_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alc_engine_code.
  DATA: BEGIN OF lt_matnr OCCURS 0,
          matnr   LIKE   mara-matnr,
        END   OF lt_matnr.

  DATA: BEGIN OF lt_atwrt OCCURS 0,
          atwrt   LIKE   ausp-atwrt,
        END   OF lt_atwrt.

  DATA: BEGIN OF lt_engine OCCURS 0,
          engine   LIKE   mara-matnr,
        END   OF lt_engine.

  DATA: lw_worko LIKE makt-maktx,
        lw_stlal(3),
        lw_atinn LIKE cabn-atinn.

  LOOP AT it_9000 WHERE atnam EQ c_engine.
    CLEAR: lt_matnr,  lt_matnr[],
           lt_atwrt,  lt_atwrt[],
           lt_engine, lt_engine[].

    CONCATENATE '0' it_9000-stlal INTO lw_stlal.
    CONCATENATE it_9000-matnr lw_stlal INTO lw_worko SEPARATED BY space.

    SELECT a~matnr
      INTO TABLE lt_matnr
      FROM mara AS a INNER JOIN makt AS b
                             ON a~matnr EQ b~matnr
     WHERE a~mtart EQ 'WOHD'
       AND b~maktx EQ lw_worko
       AND b~spras EQ sy-langu.
    IF sy-subrc NE 0.
      MOVE: c_error        TO it_9000-flag,
            icon_red_light TO it_9000-icon,
            text-b03  TO it_9000-zmsg.
      MODIFY it_9000.
      CONTINUE.
    ENDIF.

    LOOP AT lt_matnr.
      PERFORM read_cabn USING    it_9000-atnam
                        CHANGING lw_atinn.

      SELECT atwrt
        APPENDING CORRESPONDING FIELDS OF TABLE lt_atwrt
        FROM ausp
       WHERE objek EQ lt_matnr-matnr
         AND atinn EQ lw_atinn.
    ENDLOOP.

    LOOP AT lt_atwrt.
      CLEAR: lt_engine.

      MOVE: lt_atwrt-atwrt TO lt_engine-engine.
      COLLECT lt_engine.
    ENDLOOP.

    DELETE lt_engine WHERE engine EQ space.

    LOOP AT lt_engine.
      MOVE: lt_engine-engine TO it_9000-idnrk_a.

      IF sy-tabix EQ 2.
        MOVE: c_error        TO it_9000-flag,
              icon_red_light TO it_9000-icon,
              text-b15       TO it_9000-zmsg.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: c_error        TO it_9000-flag,
            icon_red_light TO it_9000-icon,
            text-b03      TO it_9000-zmsg.
    ENDIF.

    IF it_9000-flag EQ space.
      SELECT SINGLE * FROM marc WHERE matnr = it_9000-idnrk_a
                                  AND werks = it_9000-werks.
      IF sy-subrc NE 0.
        MOVE: c_error        TO it_9000-flag,
              icon_red_light TO it_9000-icon,
              text-b06       TO it_9000-zmsg.
      ENDIF.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " GET_ALC_ENGINE_CODE
*&---------------------------------------------------------------------*
*&      Form  read_cabn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000_ATNAM  text
*      <--P_LW_ATINN  text
*----------------------------------------------------------------------*
FORM read_cabn USING    pw_atnam
               CHANGING pw_atinn.
  SELECT SINGLE atinn INTO pw_atinn
                      FROM cabn
                     WHERE atnam EQ pw_atnam.
ENDFORM.                    " read_cabn
*&---------------------------------------------------------------------*
*&      Form  get_alc_tm_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alc_tm_code.
  DATA: BEGIN OF lt_matnr OCCURS 0,
          matnr   LIKE   mara-matnr,
        END   OF lt_matnr.

  DATA: BEGIN OF lt_atwrt OCCURS 0,
          atwrt   LIKE   ausp-atwrt,
        END   OF lt_atwrt.

  DATA: BEGIN OF lt_tm OCCURS 0,
          tm   LIKE   mara-matnr,
        END   OF lt_tm.

  DATA: lw_worko LIKE makt-maktx,
        lw_stlal(3),
        lw_atinn LIKE cabn-atinn.

  LOOP AT it_9000 WHERE atnam EQ c_tm.
    CLEAR: lt_matnr,  lt_matnr[],
           lt_atwrt,  lt_atwrt[],
           lt_tm,     lt_tm[].

    CONCATENATE '0' it_9000-stlal INTO lw_stlal.
    CONCATENATE it_9000-matnr lw_stlal INTO lw_worko SEPARATED BY space.

    SELECT a~matnr
      INTO TABLE lt_matnr
      FROM mara AS a INNER JOIN makt AS b
                             ON a~matnr EQ b~matnr
     WHERE a~mtart EQ 'WOHD'
       AND b~maktx EQ lw_worko
       AND b~spras EQ sy-langu.
    IF sy-subrc NE 0.
      MOVE: c_error        TO it_9000-flag,
            icon_red_light TO it_9000-icon,
            text-b04       TO it_9000-zmsg.
      MODIFY it_9000.
      CONTINUE.
    ENDIF.

    LOOP AT lt_matnr.
      PERFORM read_cabn USING    it_9000-atnam
                        CHANGING lw_atinn.

      SELECT atwrt
        APPENDING CORRESPONDING FIELDS OF TABLE lt_atwrt
        FROM ausp
       WHERE objek EQ lt_matnr-matnr
         AND atinn EQ lw_atinn.
    ENDLOOP.

    LOOP AT lt_atwrt.
      CLEAR: lt_tm.

      MOVE: lt_atwrt-atwrt TO lt_tm-tm.
      COLLECT lt_tm.
    ENDLOOP.

    DELETE lt_tm WHERE tm EQ space.

    LOOP AT lt_tm.
      MOVE: lt_tm-tm TO it_9000-idnrk_a.

      IF sy-tabix EQ 2.
        MOVE: c_error        TO it_9000-flag,
              icon_red_light TO it_9000-icon,
              text-b16     TO it_9000-zmsg.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: c_error        TO it_9000-flag,
            icon_red_light TO it_9000-icon,
            text-b04       TO it_9000-zmsg.
    ENDIF.

    IF it_9000-flag EQ space.
      SELECT SINGLE * FROM marc WHERE matnr = it_9000-idnrk_a
                                  AND werks = it_9000-werks.
      IF sy-subrc NE 0.
        MOVE: c_error        TO it_9000-flag,
              icon_red_light TO it_9000-icon,
              text-b07       TO it_9000-zmsg.
      ENDIF.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " get_alc_tm_code
*&---------------------------------------------------------------------*
*&      Form  GET_ALC_ENGINE_TM_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_alc_engine_tm_code.
  PERFORM get_alc_engine_code.
  PERFORM get_alc_tm_code.
ENDFORM.                    " GET_ALC_ENGINE_TM_CODE
*&---------------------------------------------------------------------*
*&      Form  GET_SAP_ENGINE_TM_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sap_engine_tm_code.
  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc.

  LOOP AT it_fsc.
    CLEAR: lw_topmat,
           lt_stb,    lt_stb[].

    PERFORM get_sap_bom TABLES lt_stb
                        USING  lw_topmat
                               it_fsc-matnr it_fsc-werks it_fsc-stlan
                               it_fsc-stlal lw_subrc.

    PERFORM check_engine_tm    TABLES lt_stb
                               USING  lw_subrc.
  ENDLOOP.
ENDFORM.                    " GET_SAP_ENGINE_TM_CODE
*&---------------------------------------------------------------------*
*&      Form  get_sap_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LW_TOPMAT  text
*      -->P_IT_FSC_MTNO  text
*      -->P_IT_FSC_PLNT  text
*      -->P_IT_FSC_USAG  text
*      -->P_IT_FSC_ALTN  text
*      -->P_IT_FSC_DATF  text
*----------------------------------------------------------------------*
FORM get_sap_bom TABLES pt_stb    STRUCTURE stpox
                 USING  pw_topmat LIKE      cstmat
                        pw_matnr pw_werks pw_stlan
                        pw_stlal pw_subrc.
  CLEAR: pw_topmat, pt_stb, pt_stb[], pw_subrc.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid                 = c_capid
            datuv                 = c_datuv
            cuobj                 = c_cuobj
            mtnrv                 = pw_matnr
            stlan                 = pw_stlan
            stlal                 = pw_stlal
            werks                 = pw_werks
            mmory                 = '0'
       IMPORTING
            topmat                = pw_topmat
       TABLES
            stb                   = pt_stb
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
  IF sy-subrc NE 0 OR pw_topmat-stlal NE pw_stlal.
    CLEAR: pt_stb, pt_stb[].
    MOVE: c_error TO pw_subrc.
  ENDIF.
ENDFORM.                    " get_sap_bom
*&---------------------------------------------------------------------*
*&      Form  check_engine_tm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM check_engine_tm TABLES pt_stb STRUCTURE stpox
                     USING  pw_subrc.
  DATA: lw_index  LIKE sy-tabix.

  LOOP AT it_9000 WHERE matnr = it_fsc-matnr
                    AND werks = it_fsc-werks
                    AND stlan = it_fsc-stlan
                    AND stlal = it_fsc-stlal.
    IF pw_subrc NE space.
      MOVE: c_error        TO it_9000-flag,
            icon_red_light TO it_9000-icon,
            text-b09       TO it_9000-zmsg.
    ENDIF.

    CASE it_9000-atnam.
      WHEN c_engine.
        CLEAR: lw_index.

        LOOP AT pt_stb WHERE zinfo EQ c_engtxt
                          OR idnrk EQ it_9000-idnrk_a.
          lw_index = lw_index + 1.

          MOVE: pt_stb-idnrk TO it_9000-idnrk_s,
                pt_stb-datuv TO it_9000-datuv,
                pt_stb-aennr TO it_9000-aennr,
                pt_stb-datub TO it_9000-datub,
                pt_stb-aenra TO it_9000-aenra,
                pt_stb-stlkn TO it_9000-stlkn,
                pt_stb-zinfo TO it_9000-zinfo.

          IF lw_index = 2.
            MOVE: c_error        TO it_9000-flag,
                  icon_red_light TO it_9000-icon,
                  text-b19       TO it_9000-zmsg.
            EXIT.
          ENDIF.
        ENDLOOP.
      WHEN c_tm.
        CLEAR: lw_index.

        LOOP AT pt_stb WHERE zinfo EQ c_tmtxt
                          OR idnrk EQ it_9000-idnrk_a.
          lw_index = lw_index + 1.

          MOVE: pt_stb-idnrk TO it_9000-idnrk_s,
                pt_stb-datuv TO it_9000-datuv,
                pt_stb-aennr TO it_9000-aennr,
                pt_stb-datub TO it_9000-datub,
                pt_stb-aenra TO it_9000-aenra,
                pt_stb-stlkn TO it_9000-stlkn,
                pt_stb-zinfo TO it_9000-zinfo.

          IF lw_index = 2.
            MOVE: c_error        TO it_9000-flag,
                  icon_red_light TO it_9000-icon,
                  text-b20       TO it_9000-zmsg.
            EXIT.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " check_engine_tm
*&---------------------------------------------------------------------*
*&      Form  SET_ENGINE_TM_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_engine_tm_mode.
  LOOP AT it_9000 WHERE flag EQ space.
    IF it_9000-idnrk_s EQ space.
      MOVE: c_create          TO it_9000-flag,
            icon_yellow_light TO it_9000-icon.
    ELSE.
      IF it_9000-idnrk_s EQ it_9000-idnrk_a.
        IF NOT ( it_9000-zinfo EQ c_engtxt OR
                 it_9000-zinfo EQ c_tmtxt  ).
          MOVE: c_update       TO it_9000-flag,
                icon_yellow_light TO it_9000-icon.
        ELSE.
          MOVE: c_finish       TO it_9000-flag,
                icon_light_out TO it_9000-icon.
        ENDIF.
      ELSE.
        MOVE: c_update          TO it_9000-flag,
              icon_yellow_light TO it_9000-icon.
      ENDIF.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " SET_ENGINE_TM_MODE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_bom.

  LOOP AT it_9000.
    CASE it_9000-flag.
      WHEN c_create.
        PERFORM create_engine_tm.
      WHEN c_update.
        PERFORM update_engine_tm.
    ENDCASE.

*    IF it_9000-flag EQ space.
*      MOVE: c_success TO it_9000-flag.
*    ENDIF.

    MODIFY it_9000.
  ENDLOOP.

  PERFORM set_count.
ENDFORM.                    " UPDATE_BOM
*&---------------------------------------------------------------------*
*&      Form  create_engine_tm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_engine_tm.
  PERFORM create_change_no.
  PERFORM create_bom_item.
ENDFORM.                    " create_engine_tm
*&---------------------------------------------------------------------*
*&      Form  create_change_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_change_no.
  PERFORM get_change_no USING 'IN'  it_9000-aennr.
  PERFORM get_change_no USING 'OUT' it_9000-aenra.

  PERFORM generate_bdc_cc01 USING it_9000-aennr   it_9000-matnr
                                  it_9000-idnrk_a c_datuv.

  PERFORM generate_bdc_cc01 USING it_9000-aenra   it_9000-matnr
                                  it_9000-idnrk_a c_datub.
ENDFORM.                    " create_change_no
*&---------------------------------------------------------------------*
*&      Form  get_change_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1390   text
*      -->P_IT_9000_AENNR  text
*----------------------------------------------------------------------*
FORM get_change_no USING pw_inout pw_aennr.
  DATA: lw_number(11) TYPE n.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = c_object
       IMPORTING
            number                  = lw_number
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
    MOVE: c_error        TO it_9000-flag,
          icon_red_light TO it_9000-icon,
          text-b10       TO it_9000-zmsg.
    EXIT.
  ENDIF.

  CASE pw_inout.
    WHEN 'IN'.
      CONCATENATE: lw_number 'I' INTO pw_aennr.
    WHEN 'OUT'.
      CONCATENATE: lw_number 'O' INTO pw_aennr.
  ENDCASE.

  SELECT SINGLE * FROM aenr WHERE aennr = pw_aennr.
  IF sy-subrc EQ 0.
    CLEAR: pw_aennr.

    PERFORM get_change_no USING pw_inout pw_aennr.
  ENDIF.
ENDFORM.                    " get_change_no
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_cc01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000_AENNR  text
*      -->P_IT_9000_MATN  text
*      -->P_it_9000_COMP  text
*      -->P_it_9000_DATF  text
*      -->P_it_9000_EONO(10)  text
*----------------------------------------------------------------------*
FORM generate_bdc_cc01 USING pw_aennr pw_matnr pw_idnrk
                             pw_datuv.
  DATA: lw_datum(10),
        lw_aetxt   LIKE   aenr-aetxt,
        lw_aegru   LIKE   rc29a-aegru.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  CASE it_9000-atnam.
    WHEN c_engine.
      MOVE: c_engtxt TO lw_aegru.
    WHEN c_tm.
      MOVE: c_tmtxt  TO lw_aegru.
  ENDCASE.

  REFRESH: it_bdc.

  WRITE: pw_datuv TO lw_datum.
  CONCATENATE pw_matnr '-' pw_idnrk INTO lw_aetxt.


  PERFORM dynpro USING:
   'X' 'SAPMC29C'            '0100',
   ' ' 'RC29A-AENNR'         pw_aennr,          "Change No
   ' ' 'RAD_BUT_ECNTYP-NORM' 'X',               "Change Master
   ' ' 'BDC_OKCODE'          '/00',

   'X' 'SAPMC29C'            '0010',
   ' ' 'RC29A-AETXT'         lw_aetxt,          "Text
   ' ' 'RC29A-DATUV'         lw_datum,          "Valid from
   ' ' 'RC29A-AEGRU'         lw_aegru,          "Reason of Change/EO No
   ' ' 'RC29A-AENST'         '01',              "Change no. status
   ' ' 'BDC_OKCODE'          '=VWND',           "Save

   'X' 'SAPMC29C'            '0020',
   ' ' 'RC29A-AEERL(1)'      'X',               "Actv.
   ' ' 'RC29A-INDFL(1)'      'X',               "Object
   ' ' 'RC29A-OIGEN(1)'      'X',               "MgtRec
   ' ' 'BDC_OKCODE'          '=FCBU'.           "Save

  CALL TRANSACTION 'CC01'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '001'.
    MOVE: c_error        TO it_9000-flag,
          icon_red_light TO it_9000-icon.
    PERFORM get_err_msg USING 'CC01' it_9000-zmsg.
  ENDIF.
ENDFORM.                    " generate_bdc_cc01
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1590   text
*      -->P_1591   text
*      -->P_1592   text
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
*      -->P_1700   text
*      -->P_it_9000_ZMSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_tcode pw_msg.
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

  CONCATENATE pw_tcode ':' lw_msg+5 INTO pw_msg
    SEPARATED BY space.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  create_bom_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bom_item.
  PERFORM bdc_cs02_create_in.
  PERFORM bdc_cs02_create_out.
ENDFORM.                    " create_bom_item
*&---------------------------------------------------------------------*
*&      Form  bdc_cc02_create_in
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_create_in.
  DATA: lw_qnty(10),
        lw_zinfo   LIKE   stpo-zinfo.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  REFRESH: it_bdc.

  MOVE: 1 TO lw_qnty.

  CASE it_9000-atnam.
    WHEN c_engine.
      MOVE: c_engtxt TO lw_zinfo.
    WHEN c_tm.
      MOVE: c_tmtxt  TO lw_zinfo.
  ENDCASE.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9000-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9000-werks,    "PLANT
     ' ' 'RC29N-STLAN'     it_9000-stlan,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9000-stlal,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9000-aennr,    "Change number
     ' ' 'BDC_OKCODE'      '=FCPU',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCNP',

     'X' 'SAPLCSDI'        '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X',             "CHECK
     ' ' 'RC29P-POSNR(02)' it_9000-posnr,   "BOM item number
     ' ' 'RC29P-IDNRK(02)' it_9000-idnrk_a, "BOM compenent
     ' ' 'RC29P-MENGE(02)' lw_qnty,         "Compenent quantity
     ' ' 'RC29P-MEINS(02)' 'EA',            "Compenent Uom
     ' ' 'RC29P-POSTP(02)' 'L',             "Item category
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZINFO'           lw_zinfo,         "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0140',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_error        TO it_9000-flag,
          icon_red_light TO it_9000-icon.
    PERFORM get_err_msg USING 'CS02' it_9000-zmsg.
  ENDIF.
ENDFORM.                    " bdc_cc02_create_in
*&---------------------------------------------------------------------*
*&      Form  bdc_cc02_create_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_create_out.
  DATA: lt_stb    LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc.
  DATA: lw_index TYPE i.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  PERFORM get_sap_bom TABLES lt_stb
                      USING  lw_topmat
                             it_9000-matnr it_9000-werks it_9000-stlan
                             it_9000-stlal lw_subrc.
  IF lw_subrc NE space.
    MOVE: c_error  TO it_9000-flag,
          text-b11 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  PERFORM get_item_id_for_del TABLES lt_stb.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  IF it_9000-aenra IS INITIAL.
    PERFORM get_change_no USING 'OUT' it_9000-aenra.
  ENDIF.

  REFRESH: it_bdc.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9000-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9000-werks,    "PLANT
     ' ' 'RC29N-STLAN'     it_9000-stlan,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9000-stlal,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9000-aenra,    "Change number
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9000-stlkn,   "Node number
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',
     ' ' 'BDC_OKCODE'      '=FCDL',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_error        TO it_9000-flag,
          icon_red_light TO it_9000-icon.
    PERFORM get_err_msg USING 'CS02' it_9000-zmsg.
  ELSE.
    MOVE: it_9000-idnrk_a  TO it_9000-idnrk_s,
          c_success        TO it_9000-flag,
          icon_green_light TO it_9000-icon.
  ENDIF.
ENDFORM.                    " bdc_cc02_create_out
*&---------------------------------------------------------------------*
*&      Form  get_item_id_for_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*----------------------------------------------------------------------*
FORM get_item_id_for_del TABLES pt_stb    STRUCTURE stpox.
  DATA: lw_index TYPE i.

  LOOP AT pt_stb WHERE werks = it_9000-werks
                   AND stlan = it_9000-stlan
                   AND stlal = it_9000-stlal
                   AND posnr = it_9000-posnr
                   AND idnrk = it_9000-idnrk_a.
    lw_index = lw_index + 1.

    MOVE: pt_stb-stlkn TO it_9000-stlkn.

    IF lw_index = 2.
      MOVE: c_error  TO it_9000-flag,
            text-b11 TO it_9000-zmsg.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-flag,
          text-b11 TO it_9000-zmsg.
  ENDIF.
ENDFORM.                    " get_item_id_for_del
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ENGINE_TM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_engine_tm.
  DATA: lw_qnty(10),
        lw_datum(10),
        lw_zinfo LIKE stpo-zinfo.

  CHECK it_9000-flag EQ c_create OR
        it_9000-flag EQ c_update.

  REFRESH: it_bdc.

  WRITE: it_9000-datuv TO lw_datum.
  MOVE: 1 TO lw_qnty.

  CASE it_9000-atnam.
    WHEN c_engine.
      MOVE: c_engtxt TO lw_zinfo.
    WHEN c_tm.
      MOVE: c_tmtxt  TO lw_zinfo.
  ENDCASE.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9000-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9000-werks,    "PLANT
     ' ' 'RC29N-STLAN'     it_9000-stlan,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9000-stlal,    "ALT BOM
     ' ' 'RC29N-AENNR'     space,           "Change number
     ' ' 'RC29N-DATUV'     lw_datum,        "DATE
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9000-stlkn,   "Item No
     ' ' 'BDC_OKCODE'      '=CLWI',



     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',            "CHECK
     ' ' 'RC29P-POSNR(01)' it_9000-posnr,  "BOM item number
     ' ' 'RC29P-IDNRK(01)' it_9000-idnrk_a,"BOM compenent
     ' ' 'RC29P-MENGE(01)' lw_qnty,        "Compenent quantity
     ' ' 'RC29P-MEINS(01)' 'EA',           "Compenent Uom
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZINFO'           lw_zinfo,   "Info
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_error        TO it_9000-flag,
          icon_red_light TO it_9000-icon.
    PERFORM get_err_msg USING 'CS02' it_9000-zmsg.
  ELSE.
    MOVE: it_9000-idnrk_a  TO it_9000-idnrk_s,
          c_success        TO it_9000-flag,
          icon_green_light TO it_9000-icon.
  ENDIF.
ENDFORM.                    " UPDATE_ENGINE_TM
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_rtn.
  CHECK p_update EQ 'X'.

  PERFORM update_bom.
ENDFORM.                    " UPDATE_RTN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  SORT it_9000 BY icon matnr werks stlan stlal idnrk_a.
  CALL SCREEN 9000.
ENDFORM.                    " DISPLAY_RTN
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN '9000'.
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
  IF wc_container_9000 IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_9000.
    PERFORM set_attributes_alv_grid_9000.
    PERFORM build_field_catalog USING 'IT_9000'.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv_9000.
    PERFORM sssign_event_9000.
  ENDIF.
ENDMODULE.                 " create_alv_object  OUTPUT
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
*      -->P_2602   text
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

                                  'S' 'WERKS'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLAN'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STLAL'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ENGTM'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'POSNR'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ATNAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ZINFO'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'STLKN'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_2739   text
*      -->P_2740   text
*      -->P_2741   text
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
     EXPORTING i_structure_name = 'ZSPP_ZAPP723'
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
    WHEN 'EXCUTE'.
      CLEAR sy-ucomm.
      PERFORM excute_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_count.
  CLEAR: w_total, w_finish, w_success, w_error, w_ready.
  LOOP AT it_9000.
    w_total = w_total + 1.

    CASE it_9000-icon.
      WHEN icon_green_light.
        w_success = w_success + 1.
      WHEN icon_light_out.
        w_finish = w_finish + 1.
      WHEN icon_yellow_light.
        w_ready = w_ready + 1.
      WHEN icon_red_light.
        w_error = w_error + 1.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " SET_COUNT
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

  SORT it_9000 BY icon matnr werks stlan stlal idnrk_a.

  PERFORM assign_itab_to_alv_9000.
  PERFORM sssign_event_9000.
ENDFORM.                    " EXCUTE_RTN
