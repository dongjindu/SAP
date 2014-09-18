************************************************************************
* Program Name      : ZAPP725_MODULE_CREATION_TO_WO
* Author            : Bae, Byung sung
* Creation Date     : 2004.12.01.
* Specifications By : Bae, Byung sung
* Development Request No : D1K913286
* Addl Documentation:
* Description       : Module Code Creation by FSC or Work Order Header
*
* Modification Logs
* Date       Developer    RequestNo    Description
*04/13/2006  Manju        UD1K920135   Ignore color code where length
*                                      of material is > than 10
*                                      *Ticket (64DH454728)*
*05/08/2006  Manju        UD1K940496   Select only newly created Work
*                                      orders ( As per CH)
************************************************************************
REPORT zapp725_module_creation_to_wo.
INCLUDE <icon>.
TABLES: mara, t001w, marc,
        zspp_app725_9000.


TYPE-POOLS: vrm.
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.

*---// Internal tables
DATA: BEGIN OF it_wohd OCCURS 0,             "Work Order Information
        wohd    LIKE   mara-matnr,           "Work Order Header
        maktx   LIKE   makt-maktx,           "Description
        atbez   LIKE   cabnt-atbez,          "Characteristic
        module  LIKE   mara-matnr,           "Module code
        mdlfg(2),                            "Which module
      END   OF it_wohd.

DATA: BEGIN OF it_fsc OCCURS 0,              "FSC Infomation
        fsc     LIKE   mara-matnr,           "Full Spec Code
        werks   LIKE   t001w-werks,          "Plant
        stlan   LIKE   mast-stlan,           "Usage
        stlal   LIKE   mast-stlal,           "Alt BOM
        maktx   LIKE   makt-maktx,           "Description
        module  LIKE   mara-matnr,           "Module code
        mdlfg(2),                            "Which module
        usefg,                               "Use or not flag
      END   OF it_fsc.

DATA: BEGIN OF it_fsc_target OCCURS 0,       "FSC List by WO
        fsc     LIKE   mara-matnr,           "Full Spec Code
        stlal   LIKE   mast-stlal,           "Alt BOM
      END   OF it_fsc_target.

DATA: BEGIN OF it_plant OCCURS 0,
        werks   LIKE   t001w-werks,
      END   OF it_plant.

DATA: BEGIN OF it_cabnt OCCURS 0,
        atinn   LIKE   cabn-atinn,
        atnam   LIKE   cabn-atnam,
        atbez   LIKE   cabnt-atbez,
      END   OF it_cabnt.

DATA: it_itab LIKE zspp_app725_9100 OCCURS 0 WITH HEADER LINE,
      it_9000 LIKE zspp_app725_9000 OCCURS 0 WITH HEADER LINE,
      it_9100 LIKE zspp_app725_9100 OCCURS 0 WITH HEADER LINE.

*---// Work area
DATA: w_module_cnt TYPE i,               "Count of module characteristic
      w_subrc      LIKE sy-subrc,        "Return value
      l_msg(100) type c.

*---// Ranges
RANGES: r_wohd_target FOR makt-maktx.

*---// Constants
CONSTANTS: c_check                  VALUE 'X',
           c_module(8)              VALUE 'MODULE_%',
           c_capid LIKE rc29l-capid VALUE 'PP01',         "Application
           c_stlan LIKE mast-stlan  VALUE '1',
           c_icon_equal(4)          VALUE icon_green_light,
           c_icon_diff(4)           VALUE icon_yellow_light,
           c_icon_err(4)            VALUE icon_red_light,
           c_icon_none(4)           VALUE icon_light_out,
           c_empty_od LIKE cukb-knnam VALUE 'Z_EMPTY_OD'.

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
                      es_row_no,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm,

    handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
            IMPORTING er_data_changed
                      e_onf4
                      e_onf4_before
                      e_onf4_after.
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click

  METHOD  handle_data_changed.
  ENDMETHOD.

  METHOD  handle_user_command.
  ENDMETHOD.
ENDCLASS.


*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_werks FOR t001w-werks NO-EXTENSION NO INTERVALS
                                        MEMORY ID wrk,

                s_wohd  FOR mara-matnr MODIF ID gr1,
                s_fsc   FOR mara-matnr MODIF ID gr2,
                s_datum FOR sy-datum NO-EXTENSION NO INTERVALS
                        DEFAULT sy-datum MODIF ID gr1.

PARAMETERS: p_datuv LIKE rc29n-datuv OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN SKIP.
parameters : p_modlen(2) type c default '10' OBLIGATORY.  "UD1K940496
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
*OBLIGATORY.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_check AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP.
PARAMETERS: p_rdo1  RADIOBUTTON GROUP r1 USER-COMMAND zsource
                    DEFAULT 'X',
            p_rdo2  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END   OF BLOCK bl1.

*INITIALIZATION.
** for combo box
*  w_line-key  = '10'.
*  w_line-text = 'Module code Length'.
*  APPEND w_line TO it_val.
*  w_line-key  = '11'.
*  w_line-text = 'Module code Length'.
*  APPEND w_line TO it_val.
*  w_line-key  = '12'.
*  w_line-text = 'Module code Length'.
*  APPEND w_line TO it_val.
*   w_line-key  = '13'.
*  w_line-text = 'Module code Length'.
*  APPEND w_line TO it_val.
*
*P_MODLEN  = '10'.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.



AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM processing_blank_wohd.
  PERFORM display_data.

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

  CHECK sy-dynnr EQ '9000'.

  READ TABLE it_9000 INDEX lw_sel_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CLEAR: it_9100, it_9100[].

  MOVE: it_9000 TO zspp_app725_9000.

  LOOP AT it_itab WHERE wohd  EQ it_9000-wohd
                    AND fsc   EQ it_9000-fsc
                    AND werks EQ it_9000-werks
                    AND stlan EQ it_9000-stlan
                    AND stlal EQ it_9000-stlal.
    MOVE: it_itab TO it_9100.
    APPEND it_9100.
  ENDLOOP.

  SORT it_itab BY wohd fsc werks stlan stlal mdlfg.

  PERFORM create_alv_object_9100.

  CALL SCREEN 9100.
ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  CASE c_check.
    WHEN p_rdo1.
      LOOP AT SCREEN.
        IF screen-group1 = 'GR2'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
*
*      MOVE: sy-datum TO p_datum.
    WHEN p_rdo2.
      LOOP AT SCREEN.
        IF screen-group1 = 'GR1'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      CLEAR: s_datum, s_datum[].
  ENDCASE.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_plant.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lw_continue VALUE 'X'.

  CASE c_check.
    WHEN p_rdo1.                    "W/O Header
      PERFORM get_wohd_info_input_wo USING lw_continue.
      PERFORM get_fsc_info_input_wo.
    WHEN p_rdo2.                    "FSC
      PERFORM get_fsc_info_input_fsc.
      PERFORM get_wohd_info_input_fsc USING lw_continue.
  ENDCASE.

  PERFORM set_it_itab.
  PERFORM set_it_9000.

  IF lw_continue EQ space.
    CLEAR: it_9000, it_9000[].
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_wohd_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wohd_info_input_wo USING pw_continue.
  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr   LIKE   mara-matnr,
          maktx   LIKE   makt-maktx,
        END   OF lt_mara.

  DATA: BEGIN OF lt_ausp OCCURS 0,
          atinn   LIKE   ausp-atinn,
          atwrt   LIKE   ausp-atwrt,
        END   OF lt_ausp.

*---// Read Work order header
  SELECT a~matnr b~maktx
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FROM mara AS a INNER JOIN makt AS b
                      ON a~matnr EQ b~matnr
                     AND b~spras EQ sy-langu
   WHERE a~matnr IN s_wohd
     AND a~mtart EQ 'WOHD'
     AND  a~ersda IN s_datum .                              "UD1K940496
*     AND ( a~ersda IN s_datum OR    "UD1K940496
*           a~laeda IN s_datum    ). "UD1K940496
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m02.
    CLEAR: pw_continue. EXIT.
  ENDIF.

*---// Delete special material & tesing car for body * welding
  LOOP AT lt_mara.
    IF lt_mara-matnr CP '*WOHD*' OR
       lt_mara-matnr CP '*WOCL*'.
      DELETE lt_mara.
    ELSE.
** added by Furong on 12/16/2004
      IF lt_mara-matnr+12(2) = 'XX' OR
         lt_mara-matnr+12(2) = 'XY'.
        DELETE lt_mara.
      ENDIF.
** end of additon
    ENDIF.
  ENDLOOP.

  READ TABLE lt_mara INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m02.
    CLEAR: pw_continue. EXIT.
  ENDIF.

*---// Read Module Characteristic code
  CLEAR: it_cabnt, it_cabnt[].

  SELECT a~atinn atnam atbez
    INTO CORRESPONDING FIELDS OF TABLE it_cabnt
    FROM cabnt AS a INNER JOIN cabn AS b
                       ON a~atinn EQ b~atinn
                      AND a~adzhl EQ b~adzhl
   WHERE a~spras =    sy-langu
     AND a~atbez LIKE c_module
     AND a~lkenz =    ' '
     AND b~lkenz =    ' '.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m03.
    CLEAR: pw_continue. EXIT.
  ENDIF.

  DESCRIBE TABLE it_cabnt LINES w_module_cnt.

*---// Set Work Order Information
  CLEAR: it_wohd, it_wohd[].

  LOOP AT lt_mara.
    CLEAR: lt_ausp, lt_ausp[].

    SELECT atinn atwrt
      INTO CORRESPONDING FIELDS OF TABLE lt_ausp
      FROM ausp
       FOR ALL ENTRIES IN it_cabnt
     WHERE objek EQ lt_mara-matnr
       AND atinn EQ it_cabnt-atinn
       AND klart EQ '001'.

    LOOP AT it_cabnt.
      CLEAR: lt_ausp.

      READ TABLE lt_ausp WITH KEY atinn = it_cabnt-atinn.

      MOVE: lt_mara-matnr       TO it_wohd-wohd,
            lt_mara-maktx       TO it_wohd-maktx,
            it_cabnt-atbez      TO it_wohd-atbez,
            it_cabnt-atbez+7(2) TO it_wohd-mdlfg.

      IF lt_ausp-atwrt EQ '-'.
        CLEAR: lt_ausp-atwrt.
      ENDIF.

      MOVE: lt_ausp-atwrt       TO it_wohd-module.

      APPEND it_wohd.
    ENDLOOP.
  ENDLOOP.

*---// Set Target FSC List
  CLEAR: it_fsc_target, it_fsc_target[].

  LOOP AT lt_mara.
    MOVE: lt_mara-maktx(18)   TO it_fsc_target-fsc,
          lt_mara-maktx+20(2) TO it_fsc_target-stlal.

    COLLECT it_fsc_target.
  ENDLOOP.
ENDFORM.                    " get_wohd_info
*&---------------------------------------------------------------------*
*&      Form  get_fsc_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_info_input_wo.
  DATA: lw_strlen   TYPE   i,
        lw_topmat   LIKE   cstmat.

  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.

  CLEAR: it_fsc, it_fsc[].

  LOOP AT it_fsc_target.
    LOOP AT it_plant.
      CLEAR: lw_topmat, lt_stb, lt_stb[].

      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
           EXPORTING
                aumng                 = 0
                capid                 = c_capid
                cuovs                 = '0'
                datuv                 = p_datuv
                mktls                 = 'X'
                cuobj                 = '999999999999'
                mmory                 = '0'
                mtnrv                 = it_fsc_target-fsc
                stpst                 = 0
                stlan                 = c_stlan
                stlal                 = it_fsc_target-stlal
                svwvo                 = 'X'
                werks                 = it_plant-werks
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
      IF sy-subrc <> 0 OR lw_topmat-stlal NE it_fsc_target-stlal.
        CONTINUE.
      ENDIF.

      LOOP AT lt_stb.
* CHeck for BOM prefix number 7000 ( for Module )


        check lt_stb-posnr eq '7000'.

*        SELECT SINGLE * FROM marc WHERE matnr = lt_stb-idnrk
*                                    AND werks = lt_stb-werks.
*        IF sy-subrc NE 0.
*          MESSAGE e000(zz) WITH text-m01.
*        ENDIF.

*        CHECK marc-dispo = 'M01'.            "Check module or not

        MOVE: it_fsc_target-fsc   TO it_fsc-fsc,
              lt_stb-werks        TO it_fsc-werks,
              lt_stb-stlan        TO it_fsc-stlan,
              lt_stb-stlal        TO it_fsc-stlal,
              lt_stb-ojtxb        TO it_fsc-maktx,
              lt_stb-idnrk+3(2)   TO it_fsc-mdlfg.

        PERFORM check_od_value USING lt_stb-knobj w_subrc.

        CHECK w_subrc EQ 0.
* Begin of changes - UD1K920135
*       IF lt_stb-idnrk+3(2) EQ 'CP'.
*        lw_strlen = strlen( lt_stb-idnrk ).
        if p_modlen >= 10.   "UD1K940496
* End of changes - UD1K920135
*          lw_strlen = lw_strlen - 2.
*          MOVE: lt_stb-idnrk(lw_strlen) TO it_fsc-module.
          MOVE: lt_stb-idnrk(p_modlen) TO it_fsc-module.  "UD1K940496

        ELSE.
          MOVE: lt_stb-idnrk TO it_fsc-module.
        ENDIF.

        COLLECT it_fsc.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_fsc_info
*&---------------------------------------------------------------------*
*&      Form  get_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_plant.
  SELECT werks INTO TABLE it_plant
    FROM t001w
   WHERE werks IN s_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.
ENDFORM.                    " get_plant
*&---------------------------------------------------------------------*
*&      Form  set_it_itab_input_wo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_itab.
  CLEAR: it_itab, it_itab[].

  LOOP AT it_wohd.
    CLEAR: it_fsc.

    LOOP AT it_fsc WHERE fsc   = it_wohd-maktx(18)
                     AND stlal = it_wohd-maktx+20(2)
                     AND mdlfg = it_wohd-mdlfg.
      IF it_wohd-module IS INITIAL.
        PERFORM append_it_itab USING c_icon_none text-m08 it_fsc-mdlfg
                                     it_wohd-module it_fsc-module.
      ELSEIF it_wohd-module NE it_fsc-module.
        PERFORM append_it_itab USING c_icon_diff text-m05 it_wohd-mdlfg
                                     it_wohd-module it_fsc-module.
      ELSE.
        PERFORM append_it_itab USING c_icon_equal '' it_wohd-mdlfg
                                     it_wohd-module it_fsc-module.
      ENDIF.

      MOVE: c_check TO it_fsc-usefg.
      MODIFY it_fsc.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT it_fsc WHERE fsc   = it_wohd-maktx(18)
                       AND stlal = it_wohd-maktx+20(2).
        PERFORM append_it_itab USING c_icon_err text-m04 it_wohd-mdlfg
                                                     it_wohd-module ''.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        PERFORM append_it_itab USING c_icon_err  text-m06 it_wohd-mdlfg
                                     it_wohd-module ''.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA: lw_wotxt LIKE makt-maktx,
        lw_stlal(03).

  LOOP AT it_fsc WHERE usefg NE c_check.
    CLEAR: it_wohd.

    CONCATENATE '0' it_fsc-stlal INTO lw_stlal.
    CONCATENATE it_fsc-fsc lw_stlal INTO lw_wotxt SEPARATED BY space.

    LOOP AT it_wohd WHERE maktx = lw_wotxt
                      AND mdlfg = it_fsc-mdlfg.
      IF     it_wohd-module IS INITIAL.
        PERFORM append_it_itab USING c_icon_none text-m08 it_fsc-mdlfg
                                     it_wohd-module it_fsc-module.
      ELSEIF it_wohd-module NE it_fsc-module.
        PERFORM append_it_itab USING c_icon_diff text-m05 it_fsc-mdlfg
                                     it_wohd-module it_fsc-module.
      ELSE.
        PERFORM append_it_itab USING c_icon_equal '' it_fsc-mdlfg
                                     it_wohd-module it_fsc-module.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      LOOP AT it_wohd WHERE maktx = lw_wotxt.
        PERFORM append_it_itab USING c_icon_err text-m03 it_fsc-mdlfg
                                     '' it_fsc-module.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        PERFORM append_it_itab USING c_icon_err text-m07 it_fsc-mdlfg
                                     '' it_fsc-module.
      ENDIF.
    ENDIF.
  ENDLOOP.

*---// Set ATNAM
  LOOP AT it_cabnt.
    MOVE: it_cabnt-atnam TO it_itab-atnam.

    MODIFY it_itab TRANSPORTING atnam
     WHERE mdlfg EQ it_cabnt-atbez+7(2).
  ENDLOOP.
ENDFORM.                    " set_it_itab_input_wo
*&---------------------------------------------------------------------*
*&      Form  append_it_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_ICON_DIFF  text
*      -->P_TEXT_M04  text
*----------------------------------------------------------------------*
FORM append_it_itab USING pw_icon pw_msg pw_mdlfg pw_mdlwo pw_mdlfsc.
  CLEAR: it_itab.

  MOVE: pw_icon        TO it_itab-icon,
        pw_msg         TO it_itab-msg,
        it_wohd-wohd   TO it_itab-wohd,
        it_fsc-fsc     TO it_itab-fsc,
        it_fsc-maktx   TO it_itab-maktx,
        it_fsc-werks   TO it_itab-werks,
        it_fsc-stlan   TO it_itab-stlan,
        it_fsc-stlal   TO it_itab-stlal,
        pw_mdlfg       TO it_itab-mdlfg,
        pw_mdlwo       TO it_itab-mdlwo,
        pw_mdlfsc      TO it_itab-mdlfsc.

  APPEND it_itab.
ENDFORM.                    " append_it_itab
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 9000.
ENDFORM.                    " display_data
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
    WHEN '9100'.
      SET PF-STATUS '9100'.
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
*      -->P_1088   text
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
*                                  'S' 'CHECK'       ' ',
*                                  ' ' 'EDIT'        'X',
*                                  ' ' 'CHECKBOX'    'X',
*                                  'E' 'KEY'         'X',

                                  'S' 'WOHD'       ' ',
                                  'E' 'KEY'        'X',

                                  'S' 'FSC'        ' ',
                                  'E' 'KEY'        'X',

                                  'S' 'STLAL'        ' ',
                                  'E' 'KEY'        'X',

                                  'S' 'ICON'       ' ',
                                  'E' 'KEY'        'X',

                                  'S' 'CHECK'      ' ',
                                  'E' 'NO_OUT'     'X'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1225   text
*      -->P_1226   text
*      -->P_1227   text
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
     EXPORTING i_structure_name = 'ZSPP_APP725_9000'
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
  SET HANDLER event_receiver->handle_data_changed  FOR wc_alv_9000.
  SET HANDLER event_receiver->handle_user_command  FOR wc_alv_9000.
ENDFORM.                    " sssign_event_9000
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
    WHEN 'UPDATE'.
      CLEAR sy-ucomm.
      PERFORM update_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000.
  DATA: lw_appended.


  SORT it_itab BY wohd fsc werks stlan stlal mdlfg.

  LOOP AT it_itab.
    l_msg = it_itab-msg.
    AT NEW stlal.
      CLEAR: lw_appended.

      MOVE: it_itab-wohd  TO it_9000-wohd,
            it_itab-fsc   TO it_9000-fsc,
            it_itab-maktx TO it_9000-maktx,
            it_itab-werks TO it_9000-werks,
            it_itab-stlan TO it_9000-stlan,
            it_itab-stlal TO it_9000-stlal.

      PERFORM check_error       USING lw_appended.
      PERFORM check_diffrent_01 USING lw_appended.
      PERFORM check_diffrent_02 USING lw_appended.
      PERFORM check_equal       USING lw_appended.
      PERFORM check_blank       USING lw_appended.
    ENDAT.
  ENDLOOP.

*---// Module count check
  DATA: lw_count TYPE i.

  LOOP AT it_9000 WHERE icon NE c_icon_err.
    CLEAR: lw_count.

    LOOP AT it_itab WHERE wohd  = it_9000-wohd
                      AND fsc   = it_9000-fsc
                      AND werks = it_9000-werks
                      AND stlan = it_9000-stlan
                      AND stlal = it_9000-stlal.
      lw_count = lw_count + 1.
    ENDLOOP.

    IF w_module_cnt NE lw_count.
      MOVE: c_icon_err TO it_9000-icon,
            text-m11   TO it_9000-msg.

      MODIFY it_9000.
    ENDIF.
  ENDLOOP.

*---// Set checkbox attribute
  LOOP AT it_9000.
    IF it_9000-icon EQ c_icon_equal OR
       it_9000-icon EQ c_icon_err.
      MOVE: '-' TO it_9000-check.
    ENDIF.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " set_it_9000
*&---------------------------------------------------------------------*
*&      Form  check_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM check_error USING pw_appended.
  READ TABLE it_itab WITH KEY wohd  = it_itab-wohd
                              fsc   = it_itab-fsc
                              werks = it_itab-werks
                              stlan = it_itab-stlan
                              stlal = it_itab-stlal
                              icon  = c_icon_err
                     TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    MOVE: c_icon_err TO it_9000-icon.
*          text-m09   TO it_9000-msg.  "UD1K940496
    it_9000-msg = l_msg.                                    "UD1K940496
    APPEND it_9000.

    MOVE: c_check TO pw_appended.
  ENDIF.
ENDFORM.                    " check_error
*&---------------------------------------------------------------------*
*&      Form  check_diffrent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM check_diffrent_01 USING pw_appended.
  CHECK pw_appended IS INITIAL.

  READ TABLE it_itab WITH KEY wohd  = it_itab-wohd
                              fsc   = it_itab-fsc
                              werks = it_itab-werks
                              stlan = it_itab-stlan
                              stlal = it_itab-stlal
                              icon  = c_icon_diff
                     TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    MOVE: c_icon_diff TO it_9000-icon,
          text-m10    TO it_9000-msg.
    APPEND it_9000.

    MOVE: c_check TO pw_appended.
  ENDIF.
ENDFORM.                    " check_diffrent
*&---------------------------------------------------------------------*
*&      Form  check_diffrent_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_diffrent_02 USING pw_appended.
  CHECK pw_appended IS INITIAL.

  READ TABLE it_itab WITH KEY wohd  = it_itab-wohd
                              fsc   = it_itab-fsc
                              werks = it_itab-werks
                              stlan = it_itab-stlan
                              stlal = it_itab-stlal
                              icon  = c_icon_equal
                     TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    READ TABLE it_itab WITH KEY wohd  = it_itab-wohd
                                fsc   = it_itab-fsc
                                werks = it_itab-werks
                                stlan = it_itab-stlan
                                stlal = it_itab-stlal
                                icon  = c_icon_none
                       TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      MOVE: c_icon_diff TO it_9000-icon,
            text-m10    TO it_9000-msg.
      APPEND it_9000.

      MOVE: c_check TO pw_appended.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_diffrent_02
*&---------------------------------------------------------------------*
*&      Form  check_equal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM check_equal USING pw_appended.
  CHECK pw_appended IS INITIAL.

  READ TABLE it_itab WITH KEY wohd  = it_itab-wohd
                              fsc   = it_itab-fsc
                              werks = it_itab-werks
                              stlan = it_itab-stlan
                              stlal = it_itab-stlal
                              icon  = c_icon_equal
                     TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    MOVE: c_icon_equal TO it_9000-icon,
          ' '          TO it_9000-msg.
    APPEND it_9000.

    MOVE: c_check TO pw_appended.
  ENDIF.
ENDFORM.                    " check_equal
*&---------------------------------------------------------------------*
*&      Form  check_blank
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM check_blank USING pw_appended.
  CHECK pw_appended IS INITIAL.

  MOVE: c_icon_none  TO it_9000-icon,
        text-m08     TO it_9000-msg.
  APPEND it_9000.
ENDFORM.                    " check_blank
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_object_9100.
  IF wc_container_9100 IS INITIAL.    "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_9100.
    PERFORM set_attributes_alv_grid_9100.
    PERFORM build_field_catalog_9100 USING 'IT_9100'.
    PERFORM assign_itab_to_alv_9100.
    PERFORM sssign_event_9100.
  ELSE.
    PERFORM build_field_catalog_9100 USING 'IT_9100'.
    PERFORM assign_itab_to_alv_9100.
  ENDIF.
ENDFORM.                    " create_alv_object_9100
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_9100.
  CALL METHOD wc_alv_9100->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSPP_APP725_9100'
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_9100[].
ENDFORM.                    " assign_itab_to_alv_9100
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event_9100.
*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_double_click  FOR wc_alv_9100.
ENDFORM.                    " sssign_event_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1945   text
*----------------------------------------------------------------------*
FORM build_field_catalog_9100 USING p_itab.
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
                                  'S' 'WOHD'       ' ',
                                  ' ' 'COL_POS'    '1',
                                  'E' 'KEY'        'X',

                                  'S' 'FSC'        ' ',
                                  ' ' 'COL_POS'    '2',
                                  'E' 'KEY'        'X',

                                  'S' 'STLAL'      ' ',
                                  ' ' 'COL_POS'    '3',
                                  'E' 'KEY'        'X',

                                  'S' 'ICON'       ' ',
                                  ' ' 'COL_POS'    '4',
                                  'E' 'KEY'        'X',

                                  'S' 'MAKTX'      ' ',
                                  'E' 'COL_POS'    '5',

                                  'S' 'WERKS'      ' ',
                                  'E' 'COL_POS'    '6',

                                  'S' 'STLAN'      ' ',
                                  'E' 'COL_POS'    '7',

                                  'S' 'MDLFG'      ' ',
                                  'E' 'COL_POS'    '8',

                                  'S' 'MDLWO'      ' ',
                                  'E' 'COL_POS'    '9',

                                  'S' 'MDLFSC'     ' ',
                                  'E' 'COL_POS'    '10',

                                  'S' 'MSG'        ' ',
                                  'E' 'COL_POS'    '11',

                                  'S' 'ATNAM'      ' ',
                                  'E' 'NO_OUT'     'X'.
ENDFORM.                    " build_field_catalog_9100
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object_9100.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT wc_container_9100
         EXPORTING container_name = wc_control_9100
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
  CREATE OBJECT wc_alv_9100
         EXPORTING i_parent      = wc_container_9100
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object_9100
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_9100.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid_9100
*&---------------------------------------------------------------------*
*&      Form  processing_blank_wohd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing_blank_wohd.
  CHECK p_check EQ c_check.

  LOOP AT it_9000 WHERE icon EQ c_icon_none.
    PERFORM update_material_master USING it_9000.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " processing_blank_wohd
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_rtn.
*"/Indexes of Selected Rows
  DATA: lt_rows   TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD wc_alv_9000->set_user_command
    EXPORTING
      i_ucomm = 'UPDATE'.


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
  IF sy-subrc NE 0 OR lt_rows-index EQ 0.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.

  DATA: lw_index LIKE sy-tabix.
  LOOP AT lt_rows.
    READ TABLE it_9000 INDEX lt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    CHECK it_9000-icon EQ c_icon_none OR
          it_9000-icon EQ c_icon_diff.

    MOVE: sy-tabix TO lw_index.
    PERFORM update_material_master USING it_9000.

    MODIFY it_9000 INDEX lw_index.
  ENDLOOP.

  PERFORM build_field_catalog USING 'IT_9000'.
  PERFORM assign_itab_to_alv_9000.
ENDFORM.                    " UPDATE_RTN
*&---------------------------------------------------------------------*
*&      Form  update_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000  text
*----------------------------------------------------------------------*
FORM update_material_master USING pw_9000 LIKE zspp_app725_9000.
  DATA: lt_val_table LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  LOOP AT it_itab WHERE wohd  = pw_9000-wohd
                    AND fsc   = pw_9000-fsc
                    AND werks = pw_9000-werks
                    AND stlan = pw_9000-stlan
                    AND stlal = pw_9000-stlal.
    MOVE: it_itab-atnam  TO lt_val_table-atnam,
          it_itab-mdlfsc TO lt_val_table-atwrt.

    APPEND lt_val_table.
  ENDLOOP.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            object       = pw_9000-wohd
            mode         = 'W'
            ctype        = '001'
       TABLES
            val_table    = lt_val_table
       EXCEPTIONS
            no_data      = 1
            error_mode   = 2
            error_object = 3
            error_value  = 4
            OTHERS       = 5.
  IF sy-subrc <> 0.
    MOVE: text-m12   TO pw_9000-msg.

  ELSE.
    MOVE: c_icon_equal TO pw_9000-icon,
          text-m13     TO pw_9000-msg,
          '-'          TO pw_9000-check.


    LOOP AT it_itab WHERE wohd  = pw_9000-wohd
                      AND fsc   = pw_9000-fsc
                      AND werks = pw_9000-werks
                      AND stlan = pw_9000-stlan
                      AND stlal = pw_9000-stlal.
      MOVE: c_icon_equal   TO it_itab-icon,
            text-m13       TO it_itab-msg,
            it_itab-mdlfsc TO it_itab-mdlwo.

      MODIFY it_itab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " update_material_master
*&---------------------------------------------------------------------*
*&      Form  get_fsc_info_input_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_info_input_fsc.
  DATA: lw_strlen   TYPE   i,
        lw_topmat   LIKE   cstmat.

  DATA: BEGIN OF lt_mast OCCURS 0,
          matnr   LIKE   mast-matnr,
          werks   LIKE   mast-werks,
          stlan   LIKE   mast-stlan,
          stlal   LIKE   mast-stlal,
        END   OF lt_mast.

  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.

  CLEAR: it_fsc, it_fsc[].

  SELECT a~matnr b~werks b~stlan b~stlal
    INTO CORRESPONDING FIELDS OF TABLE lt_mast
    FROM mara AS a INNER JOIN mast AS b
                      ON a~matnr = b~matnr
                   INNER JOIN stko AS c
                      ON b~stlnr = c~stlnr
                     AND b~stlal = c~stlal
                     AND c~stlty = 'M'
                     AND c~stlst = '01'
   WHERE a~mtart EQ 'FERT'
     AND a~matnr IN s_fsc
     AND b~werks IN s_werks
     AND b~stlan EQ c_stlan.

  LOOP AT lt_mast.
    CLEAR: lw_topmat, lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              aumng                 = 0
              capid                 = c_capid
              cuovs                 = '0'
              datuv                 = p_datuv
              mktls                 = 'X'
              cuobj                 = '999999999999'
              mmory                 = '0'
              mtnrv                 = lt_mast-matnr
              stpst                 = 0
              stlan                 = lt_mast-stlan
              stlal                 = lt_mast-stlal
              svwvo                 = 'X'
              werks                 = lt_mast-werks
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
    IF sy-subrc <> 0 OR lw_topmat-stlal NE lt_mast-stlal.
      CONTINUE.
    ENDIF.

    LOOP AT lt_stb.

      check lt_stb-posnr eq '7000'.
*      SELECT SINGLE * FROM marc WHERE matnr = lt_stb-idnrk
*                                  AND werks = lt_stb-werks.
*      IF sy-subrc NE 0.
*        MESSAGE e000(zz) WITH text-m01.
*      ENDIF.
*
*      CHECK marc-dispo = 'M01'.            "Check module or not

      PERFORM check_od_value USING lt_stb-knobj w_subrc.

      CHECK w_subrc EQ 0.

      MOVE: lt_mast-matnr       TO it_fsc-fsc,
            lt_stb-werks        TO it_fsc-werks,
            lt_stb-stlan        TO it_fsc-stlan,
            lt_stb-stlal        TO it_fsc-stlal,
            lt_stb-ojtxb        TO it_fsc-maktx,
            lt_stb-idnrk+3(2)   TO it_fsc-mdlfg.
*      IF lt_stb-idnrk+3(2) EQ 'CP'.
*        lw_strlen = strlen( lt_stb-idnrk ).
*        lw_strlen = lw_strlen - 2.
*
*        MOVE: lt_stb-idnrk(lw_strlen) TO it_fsc-module.
      if  p_modlen >= 10.    "UD1K940496
        MOVE: lt_stb-idnrk(p_modlen) TO it_fsc-module.  "UD1K940496
      ELSE.
        MOVE: lt_stb-idnrk TO it_fsc-module.
      ENDIF.

      COLLECT it_fsc.
    ENDLOOP.
  ENDLOOP.

*---// Set target WOHD list
  DATA: lw_surfix(3).

  CLEAR: r_wohd_target, r_wohd_target[].

  LOOP AT lt_mast.
    CONCATENATE '0' lt_mast-stlal INTO lw_surfix.
    CONCATENATE lt_mast-matnr lw_surfix INTO r_wohd_target-low
      SEPARATED BY space.

    MOVE: 'I'  TO r_wohd_target-sign,
          'EQ' TO r_wohd_target-option.

    COLLECT r_wohd_target.
  ENDLOOP.

  READ TABLE r_wohd_target INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'I'                  TO r_wohd_target-sign,
          'EQ'                 TO r_wohd_target-option,
          'Z18Z18Z18Z18Z18Z18' TO r_wohd_target-low.

    APPEND r_wohd_target.
  ENDIF.
ENDFORM.                    " get_fsc_info_input_fsc
*&---------------------------------------------------------------------*
*&      Form  GET_WOHD_INFO_INPUT_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wohd_info_input_fsc USING pw_continue.
  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr   LIKE   mara-matnr,
          maktx   LIKE   makt-maktx,
        END   OF lt_mara.

  DATA: BEGIN OF lt_ausp OCCURS 0,
          atinn   LIKE   ausp-atinn,
          atwrt   LIKE   ausp-atwrt,
        END   OF lt_ausp.

*---// Read Work order header
  SELECT a~matnr a~maktx
    INTO CORRESPONDING FIELDS OF TABLE lt_mara
    FROM makt AS a INNER JOIN mara AS b
                      ON a~matnr EQ b~matnr
   WHERE a~maktx IN r_wohd_target
     AND a~spras EQ sy-langu
     AND b~mtart EQ 'WOHD'.

  CHECK sy-subrc EQ 0.

  LOOP AT lt_mara.
    IF lt_mara-matnr+12(2) = 'XX' OR
       lt_mara-matnr+12(2) = 'XY'.
      DELETE lt_mara.
    ENDIF.
  ENDLOOP.

*---// Read Module Characteristic code
  CLEAR: it_cabnt, it_cabnt[].

  SELECT a~atinn atnam atbez
    INTO CORRESPONDING FIELDS OF TABLE it_cabnt
    FROM cabnt AS a INNER JOIN cabn AS b
                       ON a~atinn EQ b~atinn
                      AND a~adzhl EQ b~adzhl
   WHERE a~spras =    sy-langu
     AND a~atbez LIKE c_module
     AND a~lkenz =    ' '
     AND b~lkenz =    ' '.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m03.
    CLEAR: pw_continue. EXIT.
  ENDIF.

  DESCRIBE TABLE it_cabnt LINES w_module_cnt.

*---// Set Work Order Information
  CLEAR: it_wohd, it_wohd[].

  LOOP AT lt_mara.
    CLEAR: lt_ausp, lt_ausp[].

    SELECT atinn atwrt
      INTO CORRESPONDING FIELDS OF TABLE lt_ausp
      FROM ausp
       FOR ALL ENTRIES IN it_cabnt
     WHERE objek EQ lt_mara-matnr
       AND atinn EQ it_cabnt-atinn
       AND klart EQ '001'.

    LOOP AT it_cabnt.
      CLEAR: lt_ausp.

      READ TABLE lt_ausp WITH KEY atinn = it_cabnt-atinn.

      MOVE: lt_mara-matnr       TO it_wohd-wohd,
            lt_mara-maktx       TO it_wohd-maktx,
            it_cabnt-atbez      TO it_wohd-atbez,
            it_cabnt-atbez+7(2) TO it_wohd-mdlfg.

      IF lt_ausp-atwrt EQ '-'.
        CLEAR: lt_ausp-atwrt.
      ENDIF.

      MOVE: lt_ausp-atwrt       TO it_wohd-module.


      APPEND it_wohd.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_WOHD_INFO_INPUT_FSC
*&---------------------------------------------------------------------*
*&      Form  check_od_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
