************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_03
* Author            : Bongsoo, Kim
* Creation Date     : 2004.03.25.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K907658
* Addl Documentation:
* Description       : BOM Structure VERSION 3
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.04.01  ZDBM         UD1K908978
* 12/04/06   Manju        UD1K923301   Program changes for
*                                      e-BOM
************************************************************************
REPORT zipp302u_bom_bdc_03
                NO STANDARD PAGE HEADING
                LINE-SIZE  255
                LINE-COUNT 65
                MESSAGE-ID zmbm.
INCLUDE: <icon>.
TABLES: mara, marc, aenr,
        bapimathead, bapi_mara, bapi_marax,
        ztbm_bom_ecm.

DATA: zsbm_ipp302_9001 LIKE zsbm_ipp302_9001,
      zsbm_ipp302_9002 LIKE zsbm_ipp302_9002.

*---// Internal Tables
DATA: BEGIN OF it_bom_ecm OCCURS 0.
        INCLUDE STRUCTURE ztbm_bom_ecm.
DATA:   create_seq(14),
      END   OF it_bom_ecm.

DATA: it_9001 TYPE STANDARD TABLE OF zsbm_ipp302_9001
                                     WITH HEADER LINE.
DATA: it_9002 TYPE STANDARD TABLE OF zsbm_ipp302_9002
                                     WITH HEADER LINE.

DATA: BEGIN OF it_od OCCURS 0,
*        mtno    LIKE   zsbm_ipp302_9001-mtno,
*        plnt    LIKE   zsbm_ipp302_9001-plnt,
*        usag    LIKE   zsbm_ipp302_9001-usag,
*        altn    LIKE   zsbm_ipp302_9001-altn,
*        pref    LIKE   zsbm_ipp302_9001-pref,
*        comp    LIKE   zsbm_ipp302_9001-comp,
*        suff    LIKE   zsbm_ipp302_9001-suff,
*        knobj   LIKE   stpo-knobj,
*        stlkn   LIKE   stpo-stlkn,
        knnam   LIKE   zsbm_ipp302_9001-dpid,
      END   OF it_od.

DATA: BEGIN OF it_fsc OCCURS 0,
        mtno   LIKE   it_bom_ecm-mtno,
        plnt   LIKE   it_bom_ecm-plnt,
        usag   LIKE   it_bom_ecm-usag,
        altn   LIKE   it_bom_ecm-altn,
        datf   LIKE   it_bom_ecm-datf,
      END   OF it_fsc.

DATA: BEGIN OF it_engtm OCCURS 0,
        mtno   LIKE   it_bom_ecm-mtno,
        plnt   LIKE   it_bom_ecm-plnt,
        usag   LIKE   it_bom_ecm-usag,
        altn   LIKE   it_bom_ecm-altn,
        datf   LIKE   it_bom_ecm-datf,
        comp_b LIKE   it_bom_ecm-comp, "Engine, T/M in BOM
        comp_w LIKE   it_bom_ecm-comp, "Engine, T/M in WOHD
        atnam  LIKE   cabn-atnam,      "Engine, T/M
        zinfo  LIKE   stpo-zinfo,
        flag,
        zmsg   LIKE   it_bom_ecm-zmsg,
      END   OF it_engtm.

DATA: BEGIN OF it_sub_mat_alc OCCURS 0,
        mtno   LIKE   it_bom_ecm-mtno,
        plnt   LIKE   it_bom_ecm-plnt,
        usag   LIKE   it_bom_ecm-usag,
        altn   LIKE   it_bom_ecm-altn,
        comp   LIKE   it_bom_ecm-comp,
        flag,
        zmsg   LIKE   it_bom_ecm-zmsg,
      END   OF it_sub_mat_alc.

DATA: BEGIN OF it_inf OCCURS 0,
        valu1   LIKE   ztbm_fsc_cre_inf-valu1,
        item    LIKE   ztbm_fsc_cre_inf-item,
      END   OF it_inf.

DATA: BEGIN OF it_inf_compare OCCURS 0,
        item(4),
      END   OF it_inf_compare.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_matnr.

DATA: BEGIN OF it_idnrk OCCURS 0,
        idnrk   LIKE   stpo-idnrk,
      END   OF it_idnrk.

DATA: it_sub_mat_sap LIKE it_sub_mat_alc OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_sub_val OCCURS 0.
        INCLUDE STRUCTURE ztbm_sub_bom_vel.
DATA:   check   LIKE   mara-matnr,
      END   OF it_sub_val.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

*---// Work area
DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

DATA: wa_od    LIKE it_od.

DATA: BEGIN OF wa_diff_chk,
        from_date_change(1),
        to_date_change(1),
        od_change(1),
        others_change(1),
      END   OF wa_diff_chk.

DATA: w_datuv      LIKE   sy-datum,
      w_datub      LIKE   sy-datum,
      w_header_flg(4),
      w_tot_cnt    TYPE   i,
      w_tot_suc    TYPE   i,
      w_tot_leg    TYPE   i,
      w_tot_sap    TYPE   i,
      w_other_cnt  TYPE   i,
      w_other_suc  TYPE   i,
      w_other_sap  TYPE   i,
      w_ecm_cnt    TYPE   i,
      w_ecm_suc    TYPE   i,
      w_ecm_leg    TYPE   i,
      w_ecm_sap    TYPE   i,
      w_matl_cnt   TYPE   i,
      w_matl_suc   TYPE   i,
      w_matl_err   TYPE   i,
      w_read_bom.             "Read BOM Flag

DATA: g_first(1).

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_legacy_err               VALUE 'L',      "Legacy data error
           c_sap_err                  VALUE 'E',         "SAP error
           c_success                  VALUE 'S',         "Success
           c_error                    VALUE 'E',         "Warning
           c_engine  LIKE cabn-atnam  VALUE 'P_ALC_U_1', "Engine
           c_tm      LIKE cabn-atnam  VALUE 'P_ALC_U_2', "T/M
           c_engtxt  LIKE stpo-zinfo  VALUE 'ENG',
           c_tmtxt   LIKE stpo-zinfo  VALUE 'TM',
           c_subtxt  LIKE stpo-zinfo  VALUE 'BULK',
           c_create                   VALUE 'C',         "Create
           c_update                   VALUE 'U',         "Update
           c_delete                   VALUE 'D',         "Delete
           c_processed                VALUE 'P',         "Processed
           c_stlst   LIKE stko-stlst  VALUE '01',        "BOM Status
           c_object  LIKE inri-object VALUE 'ZBM_CHG_NO',"Number Range
           c_capid   LIKE rc29l-capid VALUE 'PP01',      "Application
           c_init_date  LIKE sy-datum VALUE '19000101',  "Initial date
           c_end_date   LIKE sy-datum VALUE '99991230',  "End date
           c_engine_plant LIKE marc-werks VALUE 'E001',
           c_plant        LIKE marc-werks VALUE 'P001',
           c_init_chgno LIKE aenr-aennr VALUE '19000101-001'.

*---// Tabstrip
* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
CONSTANTS: BEGIN OF c_tab_9000,
             tab1 LIKE sy-ucomm VALUE 'TAB_9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_9000_FC2',
           END OF c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
CONTROLS:  tab_9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab_9000,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZIPP302U_BOM_BDC_03_BK',
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

CONSTANTS: c_structure(100) VALUE 'ZSBM_IPP302_'.

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
SELECT-OPTIONS: s_plnt FOR ztbm_bom_ecm-plnt NO INTERVALS NO-EXTENSION,
                s_mtno FOR ztbm_bom_ecm-mtno,
                s_comp FOR ztbm_bom_ecm-comp.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Initializaton
INITIALIZATION.
  PERFORM initialization.

*---// Check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM get_data.

TOP-OF-PAGE.
  PERFORM display_header.

START-OF-SELECTION.
  PERFORM check_rtn.
  PERFORM set_processing_itab.
  PERFORM update_material_master.
  PERFORM bom_creation.
  PERFORM table_update.
  PERFORM display_data.
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

*---// Date
  MOVE: sy-datum   TO w_datuv,
        '99991231' TO w_datub.
ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  get_interface_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_interface_data.
*---// Read BOM Interface information.
  SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_bom_ecm
    FROM ztbm_bom_ecm
   WHERE plnt IN s_plnt
     AND mtno IN s_mtno
     AND comp IN s_comp
     AND zresult IN (space,c_legacy_err,c_sap_err).
*     AND zresult IN (space).

  SORT it_bom_ecm BY mtno plnt usag altn.
  LOOP AT it_bom_ecm.
    CONCATENATE it_bom_ecm-zsdat it_bom_ecm-zstim
           INTO it_bom_ecm-create_seq.

    ON CHANGE OF it_bom_ecm-mtno OR it_bom_ecm-plnt OR
                 it_bom_ecm-usag OR it_bom_ecm-altn.
      SELECT SINGLE * FROM mara WHERE matnr = it_bom_ecm-mtno
                                  AND mtart = 'FERT'.
      IF sy-subrc EQ 0.
        MOVE: it_bom_ecm-mtno  TO it_fsc-mtno,
              it_bom_ecm-plnt  TO it_fsc-plnt,
              it_bom_ecm-usag  TO it_fsc-usag,
              it_bom_ecm-altn  TO it_fsc-altn,
              it_bom_ecm-datf  TO it_fsc-datf.

        COLLECT it_fsc.

        MOVE: it_bom_ecm-mtno  TO it_engtm-mtno,
              it_bom_ecm-plnt  TO it_engtm-plnt,
              it_bom_ecm-usag  TO it_engtm-usag,
              it_bom_ecm-altn  TO it_engtm-altn,
              it_bom_ecm-datf  TO it_engtm-datf,
              c_engine         TO it_engtm-atnam,
              c_engtxt         TO it_engtm-zinfo.

        COLLECT it_engtm.

        MOVE: c_tm            TO it_engtm-atnam,
              c_tmtxt         TO it_engtm-zinfo.

        COLLECT it_engtm.
      ENDIF.
    ENDON.

    MODIFY it_bom_ecm.
  ENDLOOP.
ENDFORM.                    " get_interface_data
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_interface_data.
  PERFORM get_other_bom_components.
  PERFORM get_engine_inf_data.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_other_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_other_bom_components.
  PERFORM get_engine.
  PERFORM get_tm.
  PERFORM get_sub_material.
ENDFORM.                    " get_other_bom
*&---------------------------------------------------------------------*
*&      Form  get_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_engine.
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

  LOOP AT it_engtm WHERE atnam EQ c_engine.
    CLEAR: lt_matnr,  lt_matnr[],
           lt_atwrt,  lt_atwrt[],
           lt_engine, lt_engine[].

    CONCATENATE '0' it_engtm-altn INTO lw_stlal.
    CONCATENATE it_engtm-mtno lw_stlal INTO lw_worko SEPARATED BY space.

    SELECT a~matnr
      INTO TABLE lt_matnr
      FROM mara AS a INNER JOIN makt AS b
                             ON a~matnr EQ b~matnr
     WHERE a~mtart EQ 'WOHD'
       AND b~maktx EQ lw_worko
       AND b~spras EQ sy-langu.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_engtm-flag,
            text-b01  TO it_engtm-zmsg.
      MODIFY it_engtm.
      CONTINUE.
    ENDIF.

    LOOP AT lt_matnr.
      PERFORM read_cabn USING    it_engtm-atnam
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
      MOVE: lt_engine-engine TO it_engtm-comp_w.

      IF sy-tabix EQ 2.
        MOVE: c_sap_err TO it_engtm-flag,
              text-b22  TO it_engtm-zmsg.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_engtm-flag,
            text-b01  TO it_engtm-zmsg.
    ENDIF.

    IF it_engtm-flag EQ space.
      SELECT SINGLE * FROM marc WHERE matnr = it_engtm-comp_w
                                  AND werks = it_engtm-plnt.
      IF sy-subrc NE 0.
        MOVE: c_sap_err TO it_engtm-flag,
              text-b03  TO it_engtm-zmsg.
      ENDIF.
    ENDIF.

    MODIFY it_engtm.
  ENDLOOP.
ENDFORM.                    " get_engine
*&---------------------------------------------------------------------*
*&      Form  read_cabn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ENGTM_ATNAM  text
*      <--P_L_ATINN  text
*----------------------------------------------------------------------*
FORM read_cabn USING    pw_atnam
               CHANGING pw_atinn.
  SELECT SINGLE atinn INTO pw_atinn
                      FROM cabn
                     WHERE atnam EQ pw_atnam.
ENDFORM.                    " read_cabn
*&---------------------------------------------------------------------*
*&      Form  get_tm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tm.
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

  LOOP AT it_engtm WHERE atnam EQ c_tm.
    CLEAR: lt_matnr,  lt_matnr[],
           lt_atwrt,  lt_atwrt[],
           lt_tm,     lt_tm[].

    CONCATENATE '0' it_engtm-altn INTO lw_stlal.
    CONCATENATE it_engtm-mtno lw_stlal INTO lw_worko SEPARATED BY space.

    SELECT a~matnr
      INTO TABLE lt_matnr
      FROM mara AS a INNER JOIN makt AS b
                             ON a~matnr EQ b~matnr
     WHERE a~mtart EQ 'WOHD'
       AND b~maktx EQ lw_worko
       AND b~spras EQ sy-langu.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_engtm-flag,
            text-b02  TO it_engtm-zmsg.
      MODIFY it_engtm.
      CONTINUE.
    ENDIF.

    LOOP AT lt_matnr.
      PERFORM read_cabn USING    it_engtm-atnam
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
      MOVE: lt_tm-tm TO it_engtm-comp_w.

      IF sy-tabix EQ 2.
        MOVE: c_sap_err TO it_engtm-flag,
              text-b23  TO it_engtm-zmsg.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_engtm-flag,
            text-b02  TO it_engtm-zmsg.
    ENDIF.

    IF it_engtm-flag EQ space.
      SELECT SINGLE * FROM marc WHERE matnr = it_engtm-comp_w
                                  AND werks = it_engtm-plnt.
      IF sy-subrc NE 0.
        MOVE: c_sap_err TO it_engtm-flag,
              text-b04  TO it_engtm-zmsg.
      ENDIF.
    ENDIF.

    MODIFY it_engtm.
  ENDLOOP.
ENDFORM.                    " get_tm
*&---------------------------------------------------------------------*
*&      Form  get_sub_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_material.
  PERFORM get_sub_bom_value.

  LOOP AT it_fsc.
    PERFORM set_sub_material.
  ENDLOOP.
ENDFORM.                    " get_sub_material
*&---------------------------------------------------------------------*
*&      Form  get_sub_bom_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_bom_value.
  CLEAR: it_sub_val, it_sub_val[].

  SELECT a~werks a~matnr a~sequ a~z_nation a~mtart a~z_car a~z_year
         a~z_bt  a~z_tl  a~z_ec a~z_ft     a~z_tm  a~z_st  a~z_color
         b~matnr AS check
    INTO CORRESPONDING FIELDS OF TABLE it_sub_val
    FROM ztbm_sub_bom_vel AS a LEFT OUTER JOIN marc AS b
      ON a~mandt = b~mandt
     AND a~werks = b~werks
     AND a~matnr = b~matnr.

  SORT it_sub_val BY matnr werks z_nation sequ DESCENDING.
ENDFORM.                    " get_sub_bom_value
*&---------------------------------------------------------------------*
*&      Form  set_sub_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sub_material.
  LOOP AT it_sub_val WHERE werks = it_fsc-plnt.
*---// Pattern matching checking
    CASE it_fsc-mtno+4(2).
      WHEN 'XX'.                         "BIP
        IF NOT it_sub_val-z_nation CS 'XX'.
          CONTINUE.
        ENDIF.
      WHEN 'XY'.                         "BIW
        IF NOT it_sub_val-z_nation CS 'XY'.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.
        IF NOT ( it_fsc-mtno+1(5)  CP it_sub_val-z_nation AND
                 it_fsc-mtno+6(2)  CP it_sub_val-z_car    AND
                 it_fsc-mtno(1)    CP it_sub_val-z_year   AND
                 it_fsc-mtno+8(1)  CP it_sub_val-z_bt     AND
                 it_fsc-mtno+9(1)  CP it_sub_val-z_tl     AND
                 it_fsc-mtno+10(1) CP it_sub_val-z_ec     AND
                 it_fsc-mtno+11(1) CP it_sub_val-z_ft     AND
                 it_fsc-mtno+12(1) CP it_sub_val-z_tm   ).
          CONTINUE.
        ENDIF.
    ENDCASE.

    MOVE: it_fsc-mtno      TO it_sub_mat_alc-mtno,
          it_fsc-plnt      TO it_sub_mat_alc-plnt,
          it_fsc-usag      TO it_sub_mat_alc-usag,
          it_fsc-altn      TO it_sub_mat_alc-altn,
          it_sub_val-matnr TO it_sub_mat_alc-comp.

    IF it_sub_val-check IS INITIAL.
      MOVE: c_sap_err TO it_sub_mat_alc-flag,
            text-b05  TO it_sub_mat_alc-zmsg.
    ENDIF.

    APPEND it_sub_mat_alc.
  ENDLOOP.
ENDFORM.                    " set_sub_material
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_ecm_bom.
  PERFORM check_others.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_ecm_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ecm_bom.
  DATA: lw_continue VALUE c_check.

  SORT it_bom_ecm BY idoc item.
  LOOP AT it_bom_ecm.
    MOVE: c_check TO lw_continue.

    CLEAR: it_bom_ecm-aenf,    it_bom_ecm-aent,
           it_bom_ecm-zresult, it_bom_ecm-zmsg.

    IF it_bom_ecm-datf > it_bom_ecm-datt.
      MOVE: it_bom_ecm-datf TO it_bom_ecm-datt.
    ENDIF.

    PERFORM material_master_check  USING lw_continue.
    PERFORM color_dependency_check USING lw_continue.

    IF it_bom_ecm-bqty NE 1.
      MOVE: c_legacy_err TO it_bom_ecm-zresult,
            text-b17     TO it_bom_ecm-zmsg.
    ENDIF.

    MODIFY it_bom_ecm.
  ENDLOOP.
ENDFORM.                    " check_ecm_bom
*&---------------------------------------------------------------------*
*&      Form  check_engine_tm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_engine_tm TABLES pt_stb STRUCTURE stpox.
  DATA: lw_index  LIKE sy-tabix.

  LOOP AT it_engtm WHERE mtno = it_fsc-mtno
                     AND plnt = it_fsc-plnt
                     AND usag = it_fsc-usag
                     AND altn = it_fsc-altn.
    CASE it_engtm-atnam.
      WHEN c_engine.
        CLEAR: lw_index.

        LOOP AT pt_stb WHERE zinfo EQ c_engtxt
                          OR idnrk EQ it_engtm-comp_w.
          lw_index = lw_index + 1.

          MOVE: pt_stb-idnrk TO it_engtm-comp_b,
                pt_stb-zinfo TO it_engtm-zinfo.

          IF lw_index = 2.
            MOVE: c_sap_err TO it_engtm-flag,
                  text-b09  TO it_engtm-zmsg.
            EXIT.
          ENDIF.
        ENDLOOP.
      WHEN c_tm.
        CLEAR: lw_index.

        LOOP AT pt_stb WHERE zinfo EQ c_tmtxt.
          lw_index = lw_index + 1.

          MOVE: pt_stb-idnrk TO it_engtm-comp_b,
                pt_stb-zinfo TO it_engtm-zinfo.

          IF lw_index = 2.
            MOVE: c_sap_err TO it_engtm-flag,
                  text-b10  TO it_engtm-zmsg.
            EXIT.
          ENDIF.
        ENDLOOP.
    ENDCASE.

    MODIFY it_engtm.
  ENDLOOP.
ENDFORM.                    " check_engine_tm
*&---------------------------------------------------------------------*
*&      Form  check_sub_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_sub_material TABLES pt_stb STRUCTURE stpox.
  LOOP AT pt_stb WHERE zinfo EQ c_subtxt.
    MOVE: it_fsc-mtno  TO it_sub_mat_sap-mtno,
          pt_stb-werks TO it_sub_mat_sap-plnt,
          pt_stb-stlan TO it_sub_mat_sap-usag,
          pt_stb-stlal TO it_sub_mat_sap-altn,
          pt_stb-idnrk TO it_sub_mat_sap-comp.

    APPEND it_sub_mat_sap.
  ENDLOOP.

  LOOP AT it_sub_mat_alc WHERE mtno = it_fsc-mtno.
    LOOP AT pt_stb WHERE werks = it_sub_mat_alc-plnt
                     AND stlan = it_sub_mat_alc-usag
                     AND stlal = it_sub_mat_alc-altn
                     AND idnrk = it_sub_mat_alc-comp
                     AND zinfo NE c_subtxt.
      MOVE: it_fsc-mtno  TO it_sub_mat_sap-mtno,
            pt_stb-werks TO it_sub_mat_sap-plnt,
            pt_stb-stlan TO it_sub_mat_sap-usag,
            pt_stb-stlal TO it_sub_mat_sap-altn,
            pt_stb-idnrk TO it_sub_mat_sap-comp.

      APPEND it_sub_mat_sap.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " check_sub_material
*&---------------------------------------------------------------------*
*&      Form  color_dependency_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM color_dependency_check USING pw_continue.
  DATA: lw_knnam LIKE cukb-knnam.

  CHECK pw_continue EQ c_check.

  CHECK   it_bom_ecm-clpt EQ 'C' OR
        ( it_bom_ecm-eitm EQ 'M' AND it_bom_ecm-dpid NE space ).

  SELECT SINGLE knnam INTO lw_knnam
    FROM cukb
   WHERE knnam EQ it_bom_ecm-dpid.
  IF sy-subrc NE 0.
    MOVE: c_legacy_err TO it_bom_ecm-zresult,
          text-b08     TO it_bom_ecm-zmsg.

    CLEAR: pw_continue.
  ENDIF.
ENDFORM.                    " color_dependency_check
*&---------------------------------------------------------------------*
*&      Form  material_master_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material_master_check USING pw_continue.
  SELECT SINGLE * FROM marc WHERE matnr = it_bom_ecm-mtno
                              AND werks = it_bom_ecm-plnt.
  IF sy-subrc NE 0.
    MOVE: c_legacy_err TO it_bom_ecm-zresult.
    CONCATENATE text-b06 it_bom_ecm-mtno text-b07 INTO it_bom_ecm-zmsg
      SEPARATED BY space.

    CLEAR: pw_continue.
  ENDIF.

  CHECK pw_continue EQ c_check.

  SELECT SINGLE * FROM marc WHERE matnr = it_bom_ecm-comp
                              AND werks = it_bom_ecm-plnt.
  IF sy-subrc NE 0.
    MOVE: c_legacy_err TO it_bom_ecm-zresult.
    CONCATENATE text-b06 it_bom_ecm-comp text-b07 INTO it_bom_ecm-zmsg
      SEPARATED BY space.

    CLEAR: pw_continue.
  ENDIF.
ENDFORM.                    " material_master_check
*&---------------------------------------------------------------------*
*&      Form  get_sap_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_IT_FSC_MTNO  text
*      -->P_IT_FSC_PLNT  text
*      -->P_IT_FSC_USAG  text
*      -->P_IT_FSC_ALTN  text
*      -->P_IT_FSC_ZEDAT  text
*----------------------------------------------------------------------*
FORM get_sap_bom TABLES pt_stb    STRUCTURE stpox
*                        pt_od     STRUCTURE wa_od
                 USING  pw_topmat LIKE      cstmat
                        pw_matnr pw_werks pw_stlan
                        pw_stlal pw_datum pw_subrc.

  DATA: lw_stlal(2)  TYPE n.

  CHECK it_9001-zresult EQ space.

  CLEAR: pw_topmat, pw_subrc, pt_stb, pt_stb[].
*  , pt_od, pt_od[].

  MOVE: pw_stlal TO lw_stlal,
        lw_stlal TO pw_stlal.
  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid                 = c_capid
            datuv                 = pw_datum
            cuobj                 = '999999999999999999'
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
    MOVE: '4' TO pw_subrc.
  ENDIF.

*  LOOP AT pt_stb.
*    PERFORM read_object_dependency TABLES pt_od
*                                   USING  pt_stb.
*  ENDLOOP.
ENDFORM.                    " get_sap_bom
*&---------------------------------------------------------------------*
*&      Form  check_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_others.
  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lt_od  LIKE wa_od OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc  LIKE sy-subrc.

  LOOP AT it_fsc.
    CLEAR: lw_topmat, lw_subrc,
           lt_stb,    lt_stb[].

    PERFORM get_sap_bom TABLES lt_stb
*    lt_od
                        USING  lw_topmat
                               it_fsc-mtno it_fsc-plnt it_fsc-usag
                               it_fsc-altn it_fsc-datf lw_subrc.

    CHECK lw_subrc EQ 0.

    PERFORM check_engine_tm    TABLES lt_stb.
    PERFORM check_sub_material TABLES lt_stb..
  ENDLOOP.
ENDFORM.                    " check_others
*&---------------------------------------------------------------------*
*&      Form  bom_creation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_creation.
  DATA: lt_stb    LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc  LIKE sy-subrc.

  CLEAR: w_read_bom.

  SORT it_9001 BY zresult create_seq mtno plnt usag altn pref comp suff
                          seqc dpid idoc  item.

  LOOP AT it_9001.
    ON CHANGE OF it_9001-zresult OR it_9001-create_seq OR
                 it_9001-mtno    OR it_9001-plnt       OR
                 it_9001-usag    OR it_9001-altn.
      MOVE: c_check TO w_read_bom.
    ENDON.

    IF w_read_bom EQ c_check.
      PERFORM get_sap_bom TABLES lt_stb
                          USING  lw_topmat
                                 it_9001-mtno it_9001-plnt it_9001-usag
                                 it_9001-altn c_init_date lw_subrc.
*---// If BOM does not exist, create BOM header.
      IF lw_subrc NE 0.
        PERFORM create_bom_header.
        g_first = 'X'.
      ENDIF.
    ENDIF.

*---// Update BOM Item
    PERFORM bom_item_processing TABLES lt_stb
                                USING  lw_topmat.

    IF it_9001-zresult EQ space.
      MOVE: c_success TO it_9001-zresult.
    ENDIF.

    PERFORM calculate_error_count.

    MOVE: sy-uname TO it_9001-zbnam,
          sy-datum TO it_9001-zbdat,
          sy-uzeit TO it_9001-zbtim.

    MODIFY it_9001.

    PERFORM set_error_mark_for_next.

*---// If Update mode is create, read current BOM again
    IF it_9001-upt_mode EQ c_create  AND
       it_9001-zresult  EQ c_success.
      MOVE: c_check TO w_read_bom.
    ELSE.
      MOVE: space   TO w_read_bom.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " bom_creation
*&---------------------------------------------------------------------*
*&      Form  set_processing_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_processing_itab.
  PERFORM set_bom_ecm.
  PERFORM set_engine_tm.
  PERFORM set_sub_mat.
  PERFORM set_material_master.
ENDFORM.                    " set_processing_itab
*&---------------------------------------------------------------------*
*&      Form  set_ecm_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_bom_ecm.
  SORT it_bom_ecm BY idoc mtno plnt usag altn pref comp suff seqc.

  CLEAR: it_bom_ecm.

  LOOP AT it_bom_ecm WHERE zresult EQ c_delete.
    MOVE: c_processed TO it_bom_ecm-zresult.

    MODIFY it_bom_ecm TRANSPORTING zresult
                            WHERE ( zresult EQ space        OR
                                    zresult EQ c_legacy_err OR
                                    zresult EQ c_sap_err       )
                              AND create_seq < it_bom_ecm-create_seq
                              AND mtno =  it_bom_ecm-mtno
                              AND plnt =  it_bom_ecm-plnt
                              AND usag =  it_bom_ecm-usag
                              AND altn =  it_bom_ecm-altn
                              AND pref =  it_bom_ecm-pref
                              AND comp =  it_bom_ecm-comp
                              AND suff =  it_bom_ecm-suff
                              AND dpid =  it_bom_ecm-dpid.
  ENDLOOP.

  LOOP AT it_bom_ecm.
    CLEAR: it_9001.

    PERFORM check_engine_parts.

    MOVE-CORRESPONDING it_bom_ecm TO it_9001.
    MOVE: it_bom_ecm-dpid TO it_9001-dpid.

    IF  it_bom_ecm-clpt EQ 'C' OR
      ( it_bom_ecm-eitm EQ 'M' AND it_bom_ecm-dpid NE space ).
      MOVE: c_check TO it_9001-color_flg.
    ENDIF.

    APPEND it_9001.
    MODIFY it_bom_ecm.
  ENDLOOP.
ENDFORM.                    " set_ecm_bom
*&---------------------------------------------------------------------*
*&      Form  create_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_bom_header.
  CHECK it_9001-zresult EQ space.

  PERFORM generate_bdc_bom_header.

  CALL TRANSACTION 'CS01'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '030'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS01' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " create_bom_header
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1598   text
*      -->P_1599   text
*      -->P_1600   text
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
*      -->P_IT_BOM_ZMSG  text
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
*&      Form  update_bom_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LT_OD  text
*----------------------------------------------------------------------*
FORM bom_item_processing TABLES pt_stb    STRUCTURE stpox
                         USING  pw_topmat LIKE      cstmat.

  CHECK it_9001-zresult EQ space.

  PERFORM check_bom_item TABLES pt_stb
                         USING  pw_topmat.

  CASE it_9001-upt_mode.
    WHEN c_create.
      PERFORM create_bom_item.
    WHEN c_update.
      PERFORM update_bom_item TABLES pt_stb.
    WHEN c_delete.
      PERFORM delete_bom_item TABLES pt_stb.
  ENDCASE.
ENDFORM.                    " update_bom_item
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_bom_header.
  DATA: lw_bmeng(17),
        lw_datum(10).

  REFRESH: it_bdc.

  WRITE: it_9001-bqty UNIT it_9001-hunt TO lw_bmeng,
         c_init_date                    TO lw_datum.

  PERFORM dynpro USING:
   'X' 'SAPLCSDI'    '0100',
   ' ' 'RC29N-MATNR' it_9001-mtno,    "NEXT MATERIAL
   ' ' 'RC29N-WERKS' it_9001-plnt,    "PLANT
   ' ' 'RC29N-STLAN' it_9001-usag,    "BOM Usage
   ' ' 'RC29N-STLAL' it_9001-altn,    "ALT BOM
   ' ' 'RC29N-AENNR' space,           "Change number
   ' ' 'RC29N-DATUV' lw_datum,        "Valid from
   ' ' 'BDC_OKCODE'  '/00',

   'X' 'SAPLCSDI'    '0110',
   ' ' 'RC29K-BMENG' lw_bmeng,        "Confirmed quantity
   ' ' 'RC29K-STLST' c_stlst,         "BOM STATUS
   ' ' 'BDC_OKCODE'  '/00',

   'X' 'SAPLCSDI'    '0111',
   ' ' 'BDC_OKCODE'  '/00',

   'X' 'SAPLCSDI'    '0140',
   ' ' 'BDC_OKCODE'  '=FCBU'.
ENDFORM.                    " generate_bdc_bom_header
*&---------------------------------------------------------------------*
*&      Form  create_bom_color_item_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*      -->P_PT_OD  text
*----------------------------------------------------------------------*
FORM create_bom_color_item.
  PERFORM bdc_cs02_color_new_in.
  PERFORM bdc_cs02_new_out.
  g_first = ''.
ENDFORM.                    " create_bom_color_item_new
*&---------------------------------------------------------------------*
*&      Form  create_bom_normal_item_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*----------------------------------------------------------------------*
FORM create_bom_normal_item.
  PERFORM bdc_cs02_normal_new_in.
  PERFORM bdc_cs02_new_out.
  g_first = ''.
ENDFORM.                    " create_bom_normal_item_new
*&---------------------------------------------------------------------*
*&      Form  update_bom_item_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*      -->P_PT_OD  text
*----------------------------------------------------------------------*
FORM update_bom_item TABLES pt_stb    STRUCTURE stpox.
  PERFORM change_change_no TABLES pt_stb.

  CASE it_9001-color_flg.
    WHEN c_check.
      PERFORM bdc_cs02_update_color.
      PERFORM update_pt_stb TABLES pt_stb
                             USING 'OTHERS'.
    WHEN space.
      PERFORM bdc_cs02_update_normal.
  ENDCASE.
ENDFORM.                    " update_bom_item_change
*&---------------------------------------------------------------------*
*&      Form  CREATE_BOM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*      -->P_PT_OD  text
*----------------------------------------------------------------------*
FORM create_bom_item.
  PERFORM create_change_no.

  CASE it_9001-color_flg.
    WHEN c_check.
      PERFORM create_bom_color_item.
    WHEN space.
      PERFORM create_bom_normal_item.
  ENDCASE.
ENDFORM.                    " CREATE_BOM_ITEM
*&---------------------------------------------------------------------*
*&      Form  CHECK_BOM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*----------------------------------------------------------------------*
FORM check_bom_item TABLES pt_stb    STRUCTURE stpox
                    USING  pw_topmat LIKE      cstmat.
  CLEAR: wa_diff_chk.

  IF it_9001-color_flg EQ c_check AND
     it_9001-mtno+6(2) NE it_9001-comp(2).
    PERFORM check_bom_color_item TABLES pt_stb
                                 USING  pw_topmat.
  ELSE.
    PERFORM check_bom_normal_item TABLES pt_stb
                                  USING  pw_topmat.
  ENDIF.
ENDFORM.                    " CHECK_BOM_ITEM
*&---------------------------------------------------------------------*
*&      Form  DELETE_BOM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*----------------------------------------------------------------------*
FORM delete_bom_item TABLES pt_stb    STRUCTURE stpox.
  DATA: lw_datum(10).

  CHECK it_9001-zresult EQ space.

  REFRESH: it_bdc.

  WRITE: it_9001-datuv TO lw_datum.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     space,          "Change number
     ' ' 'RC29N-DATUV'     lw_datum,       "Valid from
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,   "Node number
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',
     ' ' 'BDC_OKCODE'      '=FCDL',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ELSE.
    IF it_9001-color_flg = c_check.
      READ TABLE pt_stb WITH KEY werks = it_9001-plnt
                                 stlan = it_9001-usag
                                 stlal = it_9001-altn
                                 posnr = it_9001-pref
                                 idnrk = it_9001-comp
                                 suff  = it_9001-suff.
      IF sy-subrc NE 0.
        MOVE: c_sap_err TO it_9001-zresult,
              text-m01  TO it_9001-zmsg.
      ELSE.
        DELETE pt_stb INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DELETE_BOM_ITEM
*&---------------------------------------------------------------------*
*&      Form  check_item_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*----------------------------------------------------------------------*
FORM check_item_change USING pw_stb LIKE stpox.
  SELECT SINGLE * FROM aenr WHERE aennr = it_9001-aenf.
  IF sy-subrc NE 0.
    MOVE: c_sap_err TO it_9001-zresult,
          text-m01  TO it_9001-zmsg.
  ENDIF.

  IF pw_stb-datuv NE it_9001-datf OR
     aenr-aegru   NE it_9001-eono(10).
    MOVE: c_update TO wa_diff_chk-from_date_change.
  ENDIF.

*---// If Valid to date is initial,
*---// second BDC failed during item creation.
*---// Second BDC is assignment to-change no.
  IF it_9001-aent IS INITIAL.
    MOVE: c_create TO wa_diff_chk-to_date_change.
  ELSE.
    SELECT SINGLE * FROM aenr WHERE aennr = it_9001-aent.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_9001-zresult,
            text-m01  TO it_9001-zmsg.
    ENDIF.

    IF pw_stb-datub NE it_9001-datt OR
       aenr-aegru   NE it_9001-eono+10(10).
      MOVE: c_update TO wa_diff_chk-to_date_change.
    ENDIF.
  ENDIF.

  IF pw_stb-menge NE it_9001-qnty OR pw_stb-meins NE it_9001-unit OR
     pw_stb-eitm  NE it_9001-eitm OR pw_stb-stgb  NE it_9001-stgb OR
     pw_stb-upgn  NE it_9001-upgn.
    MOVE: c_update TO wa_diff_chk-others_change.
  ENDIF.

ENDFORM.                    " check_item_change
*&---------------------------------------------------------------------*
*&      Form  CHECK_BOM_COLOR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*      -->P_PT_OD  text
*      -->P_PW_TOPMAT  text
*----------------------------------------------------------------------*
FORM check_bom_color_item TABLES pt_stb    STRUCTURE stpox
                          USING  pw_topmat LIKE      cstmat.
  DATA: lw_index TYPE i.


  CHECK it_9001-zresult  EQ space.

  CLEAR: it_od, it_od[].

  LOOP AT pt_stb WHERE werks = it_9001-plnt
                   AND stlan = it_9001-usag
                   AND stlal = it_9001-altn
                   AND posnr = it_9001-pref
                   AND idnrk = it_9001-comp
                   AND suff  = it_9001-suff.
    lw_index = lw_index + 1.

    MOVE: pt_stb-stlkn TO it_9001-stlkn,
          pt_stb-aennr TO it_9001-aenf,
          pt_stb-datuv TO it_9001-datuv,
          pt_stb-aenra TO it_9001-aent,
          pt_stb-datub TO it_9001-datub.

    PERFORM check_item_change_color USING pt_stb.

    IF lw_index = 2.
      MOVE: c_sap_err TO it_9001-zresult,
            text-b11  TO it_9001-zmsg.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    IF it_9001-zmode EQ c_delete.
      MOVE: c_sap_err TO it_9001-zresult,
            text-b14  TO it_9001-zmsg.
      EXIT.
    ENDIF.

** For ECC on 09/04/12
  DATA l_chk(1).
    CLEAR pt_stb.
    LOOP AT pt_stb WHERE werks = it_9001-plnt
                     AND stlan = it_9001-usag
                     AND stlal = it_9001-altn
                     AND datub > it_9001-datf.

      l_chk = 'X'.
      EXIT.
    ENDLOOP.

    IF l_chk = 'X'.
      g_first = space.
    ELSE.
      g_first = 'X'.
    ENDIF.
** End

    MOVE: c_create     TO it_9001-upt_mode.

    MOVE: it_9001-dpid TO it_od-knnam.
    APPEND it_od.
  ELSE.
    PERFORM read_od_value USING pt_stb.

    CASE it_9001-zmode.
      WHEN c_update.
        MOVE: c_update TO it_9001-upt_mode.
      WHEN c_delete.
        READ TABLE it_od INDEX 1.
        IF sy-subrc EQ 0.
          MOVE: c_update TO it_9001-upt_mode.
        ELSE.
          MOVE: c_delete TO it_9001-upt_mode.
        ENDIF.
    ENDCASE.
  ENDIF.
ENDFORM.                    " CHECK_BOM_COLOR_ITEM
*&---------------------------------------------------------------------*
*&      Form  check_bom_normal_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*      -->P_PT_OD  text
*      -->P_PW_TOPMAT  text
*----------------------------------------------------------------------*
FORM check_bom_normal_item TABLES pt_stb    STRUCTURE stpox
                           USING  pw_topmat LIKE      cstmat.
  DATA: lw_index TYPE i.

  LOOP AT pt_stb WHERE werks = it_9001-plnt
                   AND stlan = it_9001-usag
                   AND stlal = it_9001-altn
                   AND posnr = it_9001-pref
                   AND idnrk = it_9001-comp
                   AND suff  = it_9001-suff.
    lw_index = lw_index + 1.

    MOVE: pt_stb-stlkn TO it_9001-stlkn,
          pt_stb-aennr TO it_9001-aenf,
          pt_stb-datuv TO it_9001-datuv,
          pt_stb-aenra TO it_9001-aent,
          pt_stb-datub TO it_9001-datub.

    PERFORM check_item_change USING pt_stb.

    IF lw_index = 2.
      MOVE: c_sap_err TO it_9001-zresult,
            text-b11  TO it_9001-zmsg.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    IF it_9001-zmode EQ c_delete.
      MOVE: c_sap_err TO it_9001-zresult,
            text-b14  TO it_9001-zmsg.
      EXIT.
    ENDIF.

** For ECC on 09/04/12
    DATA l_chk(1).
    CLEAR pt_stb.
    LOOP AT pt_stb WHERE werks = it_9001-plnt
                     AND stlan = it_9001-usag
                     AND stlal = it_9001-altn
                     AND datub > it_9001-datf.

      l_chk = 'X'.
      EXIT.
    ENDLOOP.

    IF l_chk = 'X'.
      g_first = space.
    ELSE.
      g_first = 'X'.
    ENDIF.
****  End

    MOVE: c_create TO it_9001-upt_mode.
  ELSE.
    MOVE: it_9001-zmode TO it_9001-upt_mode.
  ENDIF.
ENDFORM.                    " check_bom_normal_item
*&---------------------------------------------------------------------*
*&      Form  CREATE_CHANGE_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_change_no.
  CHECK it_9001-zresult EQ space.

  PERFORM get_change_no USING 'IN'  it_9001-aenf.
  PERFORM get_change_no USING 'OUT' it_9001-aent.

  PERFORM generate_bdc_cc01 USING it_9001-aenf it_9001-mtno
                                  it_9001-comp it_9001-datf
                                  it_9001-eono(10).

  PERFORM generate_bdc_cc01 USING it_9001-aent it_9001-mtno
                                  it_9001-comp it_9001-datt
                                  it_9001-eono+10(10).
ENDFORM.                    " CREATE_CHANGE_NO
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_CC01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ECM_AENF  text
*      -->P_IT_ECM_NEXT  text
*      -->P_IT_ECM_COMP  text
*      -->P_IT_ECM_DATF  text
*      -->P_IT_ECM_EONO(10)  text
*----------------------------------------------------------------------*
FORM generate_bdc_cc01 USING pw_aennr pw_matnr pw_idnrk
                             pw_datuv pw_aegru.
  DATA: lw_datum(10),
        lw_aetxt   LIKE   aenr-aetxt.

  CHECK it_9001-zresult EQ space.

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
   ' ' 'RC29A-AEGRU'         pw_aegru,          "Reason of Change/EO No
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
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CC01' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " GENERATE_BDC_CC01
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGE_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_AENNR  text
*----------------------------------------------------------------------*
FORM get_change_no USING pw_inout pw_aennr.
  DATA: lw_number(11) TYPE n.

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
    MOVE: c_sap_err TO it_9001-zresult,
          text-b20  TO it_9001-zmsg.
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
ENDFORM.                    " GET_CHANGE_NO
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_CC02_COLOR_NEW_IN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_color_new_in.
  DATA lw_qnty(20).

  CHECK it_9001-zresult EQ space.

  REFRESH: it_bdc.

  WRITE: it_9001-qnty TO lw_qnty UNIT it_9001-unit LEFT-JUSTIFIED.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9001-aenf,    "Change number
     ' ' 'BDC_OKCODE'      '=FCPU'.

** Ecc
IF g_first <> 'X'.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCNP'.
endif.
** end

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,          "CHECK
     ' ' 'RC29P-POSNR(02)' it_9001-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' it_9001-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' lw_qnty,         "Compenent quantity
     ' ' 'RC29P-MEINS(02)' it_9001-unit,    "Compenent Uom
     ' ' 'RC29P-POSTP(02)' it_9001-itca,    "Item category
     ' ' 'BDC_OKCODE'      '/00',


     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131'.

  IF it_9001-usag EQ '1' AND it_9001-plnt NE c_plant.
    PERFORM dynpro USING:
       ' ' 'RC29P-SANKO'     it_9001-sanko,   "Engineering/design
       ' ' 'RC29P-SANFE'     it_9001-sanfe,   "Production Relevant
       ' ' 'RC29P-SANKA'     it_9001-sanka.   "Costing Relevncy
  ENDIF.

  PERFORM dynpro USING:
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZEITM'           it_9001-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'           it_9001-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'           it_9001-suff,    "SUFFIX NO
     ' ' 'ZUPGN'           it_9001-upgn,    "UPG
     ' ' 'ZINFO'           it_9001-zinfo,   "Engine, T/M, SUB
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0140',
     ' ' 'BDC_OKCODE'      '/CS',

     'X' 'SAPLCSDI'        '2130',
     ' ' 'BDC_OKCODE'      '=WIZU',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'RCUKD-KNNAM(01)' it_9001-dpid,
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCSDI'        '2130',
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCSDI'        '0140',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " GENERATE_BDC_CC02_COLOR_NEW_IN
*&---------------------------------------------------------------------*
*&      Form  change_change_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_change_no TABLES pt_stb    STRUCTURE stpox.
  CHECK it_9001-zresult EQ space.

  IF wa_diff_chk-from_date_change EQ c_update.
    PERFORM generate_bdc_cc02 USING it_9001-aenf it_9001-mtno
                                    it_9001-comp it_9001-datf
                                    it_9001-eono(10).
    PERFORM update_pt_stb TABLES pt_stb
                           USING 'IN'.
  ENDIF.

  CASE wa_diff_chk-to_date_change.
    WHEN c_create.
      PERFORM get_change_no USING 'OUT' it_9001-aent.

      PERFORM generate_bdc_cc01 USING it_9001-aent it_9001-mtno
                                      it_9001-comp it_9001-datt
                                      it_9001-eono(10).
      PERFORM bdc_cs02_new_out.

      PERFORM update_pt_stb TABLES pt_stb
                             USING 'OUT'.
    WHEN c_update.
      PERFORM generate_bdc_cc02 USING it_9001-aent it_9001-mtno
                                      it_9001-comp it_9001-datt
                                      it_9001-eono+10(10).
      PERFORM update_pt_stb TABLES pt_stb
                             USING 'OUT'.
  ENDCASE.
ENDFORM.                    " change_change_no
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_cc02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ECM_AENF  text
*      -->P_IT_ECM_NEXT  text
*      -->P_IT_ECM_COMP  text
*      -->P_IT_ECM_DATF  text
*      -->P_IT_ECM_EONO(10)  text
*----------------------------------------------------------------------*
FORM generate_bdc_cc02  USING pw_aennr pw_matnr pw_idnrk
                              pw_datuv pw_aegru.
  DATA: lw_datum(10),
        lw_aetxt   LIKE   aenr-aetxt.

  CHECK it_9001-zresult EQ space.

  REFRESH: it_bdc.

  WRITE: pw_datuv TO lw_datum.
  CONCATENATE pw_matnr '-' pw_idnrk INTO lw_aetxt.


  PERFORM dynpro USING:
   'X' 'SAPMC29C'            '0100',
   ' ' 'RC29A-AENNR'         pw_aennr,          "Change No
   ' ' 'BDC_OKCODE'          '/00',

   'X' 'SAPMC29C'            '0010',
   ' ' 'BDC_OKCODE'          '=DATU',           "Valid from

   'X' 'SAPMC29C'            '0130',
   ' ' 'RC29A-DATUV'         lw_datum,          "Valid from
   ' ' 'BDC_OKCODE'          '=SIDT',           "cOPY

  'X' 'SAPMC29C'            '0010',
  ' ' 'RC29A-AETXT'         lw_aetxt,          "Text
  ' ' 'RC29A-AEGRU'         pw_aegru,          "Reason of Change/EO No
  ' ' 'RC29A-AENST'         '01',              "Change no. status
  ' ' 'RC29A-LVORM'         space,             "Deletion Flag
  ' ' 'BDC_OKCODE'          '=FCBU'.           "Save

  CALL TRANSACTION 'CC02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR NOT ( sy-msgno EQ '002' OR sy-msgno EQ '003' ).
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CC02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " generate_bdc_cc02
*&---------------------------------------------------------------------*
*&      Form  bdc_cc02_normal_new_in
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_normal_new_in.
  DATA: lw_qnty(20).

  CHECK it_9001-zresult EQ space.

  REFRESH: it_bdc.

  WRITE: it_9001-qnty TO lw_qnty UNIT it_9001-unit LEFT-JUSTIFIED.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9001-aenf,    "Change number
     ' ' 'BDC_OKCODE'      '=FCPU'.

** ECC ON 09/04/12
IF g_first <> 'X'.
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCNP'.
endif.
** End
  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X',            "CHECK
     ' ' 'RC29P-POSNR(02)' it_9001-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' it_9001-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' lw_qnty,        "Compenent quantity
     ' ' 'RC29P-MEINS(02)' it_9001-unit,    "Compenent Uom
     ' ' 'RC29P-POSTP(02)' it_9001-itca,    "Item category
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131'.

  IF it_9001-usag EQ '1' AND it_9001-plnt NE c_plant.
    PERFORM dynpro USING:
       ' ' 'RC29P-SANKO'     it_9001-sanko,   "Engineering/design
       ' ' 'RC29P-SANFE'     it_9001-sanfe,   "Production Relevant
       ' ' 'RC29P-SANKA'     it_9001-sanka.   "Costing Relevncy
  ENDIF.

  PERFORM dynpro USING:
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZEITM'           it_9001-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'           it_9001-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'           it_9001-suff,    "SUFFIX NO
     ' ' 'ZUPGN'           it_9001-upgn,    "UPG
     ' ' 'ZINFO'           it_9001-zinfo,   "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0140',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " bdc_cc02_normal_new_in
*&---------------------------------------------------------------------*
*&      Form  bec_cc02_new_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_new_out.
  DATA: lt_stb    LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc  LIKE sy-subrc.

  DATA: lw_index TYPE i.

  CHECK it_9001-zresult EQ space.

  PERFORM get_sap_bom TABLES lt_stb
                      USING  lw_topmat
                             it_9001-mtno it_9001-plnt it_9001-usag
                             it_9001-altn c_init_date lw_subrc.
  IF lw_subrc NE 0.
    MOVE: c_sap_err TO it_9001-zresult.
    CONCATENATE text-b21 text-b11 INTO it_9001-zmsg
      SEPARATED BY space.
    EXIT.
  ENDIF.

  PERFORM get_item_id_for_del TABLES lt_stb.

  CHECK it_9001-zresult EQ space.

  IF it_9001-aent IS INITIAL.
    PERFORM get_change_no USING 'OUT' it_9001-aent.
  ENDIF.

  REFRESH: it_bdc.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9001-aent,    "Change number
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,   "Node number
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',
     ' ' 'BDC_OKCODE'      '=FCDL',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " bec_cc02_new_out
*&---------------------------------------------------------------------*
*&      Form  SET_ENGINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_engine_tm.
  LOOP AT it_engtm.
    CLEAR: it_9001.

    MOVE: it_engtm-mtno  TO it_9001-mtno,
          it_engtm-plnt  TO it_9001-plnt,
          it_engtm-usag  TO it_9001-usag,
          it_engtm-altn  TO it_9001-altn,
          '8000'         TO it_9001-pref,
          it_engtm-zinfo TO it_9001-zinfo,
          c_init_date    TO it_9001-datf,
          c_end_date     TO it_9001-datt,
          'L'            TO it_9001-itca,
          1              TO it_9001-bqty,
          'EA'           TO it_9001-hunt,
          1              TO it_9001-qnty,
          'EA'           TO it_9001-unit,
          it_engtm-zmsg  TO it_9001-zmsg.

    IF it_engtm-flag NE space.
      MOVE: it_engtm-comp_w TO it_9001-comp,
            c_update        TO it_9001-zmode,
            it_engtm-flag   TO it_9001-zresult.
      APPEND it_9001. CONTINUE.
    ENDIF.

    IF it_engtm-comp_b EQ it_engtm-comp_w.
      MOVE: it_engtm-comp_w TO it_9001-comp,
            c_update        TO it_9001-zmode,
            c_success       TO it_9001-zresult.
      APPEND it_9001. CONTINUE.
    ELSE.
      IF it_engtm-comp_b EQ space.
        MOVE: it_engtm-comp_w TO it_9001-comp,
              c_update        TO it_9001-zmode.
        APPEND it_9001. CONTINUE.
      ELSE.
        MOVE: it_engtm-comp_b TO it_9001-comp,
              c_delete        TO it_9001-zmode.
        APPEND it_9001.

        MOVE: it_engtm-comp_w TO it_9001-comp,
              c_update        TO it_9001-zmode.
        APPEND it_9001. CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SET_ENGINE

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
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
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
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
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
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
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
                                  'S' 'ZINFO'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MTNO'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'COMP'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'PLNT'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'USAG'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ALTN'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'PREF'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'SUFF'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'SEQC'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'DPID'        ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'QNTY'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'UNIT'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DATF'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'AENF'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'DATT'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'AENT'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'EONO'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'EITM'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STGB'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'CLPT'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'UPGN'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'BQTY'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'HUNT'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ITCA'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZMODE'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STLKN'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZMSG'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'CREATE_SEQ'  ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'DATUV'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ZRESULT'     ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'DATUB'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'COLOR_FLG'   ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'UPT_MODE'    ' ',
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

                                  'S' 'MAKTX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MTART'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'KZKFG_M'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'PROFL_M'     ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'NORMT_M'     ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'KZKFG_N'     ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PROFL_N'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NORMT_N'     ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZMSG'        ' ',
                                  'E' 'EMPHASIZE'   'C100',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
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
*      -->P_5311   text
*      -->P_5312   text
*      -->P_5313   text
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
*&      Form  TABLE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM table_update.
  LOOP AT it_9001.
    READ TABLE it_bom_ecm WITH KEY idoc = it_9001-idoc
                                   item = it_9001-item
                                   mtno = it_9001-mtno
                                   plnt = it_9001-plnt
                                   usag = it_9001-usag
                                   altn = it_9001-altn
                                   pref = it_9001-pref
                                   comp = it_9001-comp
                                   suff = it_9001-suff
                                   seqc = it_9001-seqc.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    MOVE: it_9001-zresult TO it_bom_ecm-zresult,
          it_9001-zmsg    TO it_bom_ecm-zmsg,
          it_9001-aenf    TO it_bom_ecm-aenf,
          it_9001-aent    TO it_bom_ecm-aent,
          it_9001-zbnam   TO it_bom_ecm-zbnam,
          it_9001-zbdat   TO it_bom_ecm-zbdat,
          it_9001-zbtim   TO it_bom_ecm-zbtim.

    CLEAR: ztbm_bom_ecm.
    MOVE: it_bom_ecm TO ztbm_bom_ecm.

    UPDATE ztbm_bom_ecm.
  ENDLOOP.
ENDFORM.                    " TABLE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_ERROR_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_error_count.
  CASE it_9001-zresult.
    WHEN c_success.
      MOVE: icon_green_light  TO it_9001-icon.
    WHEN c_legacy_err.
      MOVE: icon_yellow_light TO it_9001-icon.
    WHEN c_sap_err.
      MOVE: icon_red_light    TO it_9001-icon.
  ENDCASE.

  CASE it_9001-zinfo.
    WHEN c_engtxt OR c_tmtxt OR c_subtxt.
      CASE it_9001-zresult.
        WHEN c_success.
          w_other_suc = w_other_suc + 1.
        WHEN c_sap_err.
          w_other_sap = w_other_sap + 1.
      ENDCASE.
      w_other_cnt = w_other_cnt + 1.
    WHEN OTHERS.
      CASE it_9001-zresult.
        WHEN c_success.
          w_ecm_suc = w_ecm_suc + 1.
        WHEN c_legacy_err.
          w_ecm_leg = w_ecm_leg + 1.
        WHEN c_sap_err.
          w_ecm_sap = w_ecm_sap + 1.
      ENDCASE.
      w_ecm_cnt = w_ecm_cnt + 1.
  ENDCASE.

  CASE it_9001-zresult.
    WHEN c_success.
      w_tot_suc = w_tot_suc + 1.
    WHEN c_legacy_err.
      w_tot_leg = w_tot_leg + 1.
    WHEN c_sap_err.
      w_tot_sap = w_tot_sap + 1.
  ENDCASE.
  w_tot_cnt = w_tot_cnt + 1.
ENDFORM.                    " CALCULATE_ERROR_COUNT
*&---------------------------------------------------------------------*
*&      Form  set_sub_mat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sub_mat.
  LOOP AT it_sub_mat_sap.
    READ TABLE it_sub_mat_alc WITH KEY mtno = it_sub_mat_sap-mtno
                                       plnt = it_sub_mat_sap-plnt
                                       usag = it_sub_mat_sap-usag
                                       altn = it_sub_mat_sap-altn
                                       comp = it_sub_mat_sap-comp.
    IF sy-subrc NE 0.
      PERFORM append_it_ecm_form_sub USING it_sub_mat_sap c_delete.
    ENDIF.
  ENDLOOP.

  LOOP AT it_sub_mat_alc.
    PERFORM append_it_ecm_form_sub USING it_sub_mat_alc c_update.
  ENDLOOP.
ENDFORM.                    " set_sub_mat
*&---------------------------------------------------------------------*
*&      Form  append_it_ecm_form_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUB_MAT_SAP  text
*      -->P_C_DELETE  text
*----------------------------------------------------------------------*
FORM append_it_ecm_form_sub USING pw_sub_mat LIKE it_sub_mat_sap
                                  pw_mode.
  CLEAR: it_9001.

  MOVE: pw_sub_mat-mtno  TO it_9001-mtno,
        pw_sub_mat-plnt  TO it_9001-plnt,
        pw_sub_mat-usag  TO it_9001-usag,
        pw_sub_mat-altn  TO it_9001-altn,
        '9000'           TO it_9001-pref,
        pw_sub_mat-comp  TO it_9001-comp,
        pw_sub_mat-flag  TO it_9001-zresult,
        pw_sub_mat-zmsg  TO it_9001-zmsg,
        c_subtxt         TO it_9001-zinfo,
        c_init_date      TO it_9001-datf,
        c_end_date       TO it_9001-datt,
        'L'              TO it_9001-itca,
        1                TO it_9001-bqty,
        'EA'             TO it_9001-hunt,
        1                TO it_9001-qnty,
        'EA'             TO it_9001-unit,
        pw_mode          TO it_9001-zmode.

  APPEND it_9001.
ENDFORM.                    " append_it_ecm_form_sub
*&---------------------------------------------------------------------*
*&      Form  set_error_mark_for_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_error_mark_for_next.
  IF it_9001-zresult EQ c_sap_err OR
     it_9001-zresult EQ c_legacy_err.
    MOVE: c_sap_err TO it_9001-zresult,
          text-b24  TO it_9001-zmsg.

    MODIFY it_9001 TRANSPORTING zresult zmsg
                          WHERE ( zresult EQ space        OR
                                  zresult EQ c_legacy_err OR
                                  zresult EQ c_sap_err       )
                            AND create_seq > it_bom_ecm-create_seq
                            AND mtno       = it_bom_ecm-mtno
                            AND plnt       = it_bom_ecm-plnt
                            AND usag       = it_bom_ecm-usag
                            AND altn       = it_bom_ecm-altn
                            AND pref       = it_bom_ecm-pref
                            AND comp       = it_bom_ecm-comp
                            AND suff       = it_bom_ecm-suff.
  ENDIF.
ENDFORM.                    " set_error_mark_for_next
*&---------------------------------------------------------------------*
*&      Form  update_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_material_master.
  DATA: lt_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  SORT it_9002 BY mtart matnr.
  LOOP AT it_9002.
    CLEAR: bapimathead, bapi_mara, bapi_marax,
           lt_bapiret2, lt_bapiret2[].

    IF it_9002-icon EQ icon_red_light.
      w_matl_err = w_matl_err + 1.
      w_matl_cnt = w_matl_cnt + 1.
      CONTINUE.
    ENDIF.

    IF NOT ( it_9002-profl_m NE it_9002-profl_n OR
             it_9002-normt_m NE it_9002-normt_n OR
             it_9002-kzkfg_m NE it_9002-kzkfg_n ).
      MOVE: c_success           TO it_9002-flag,
            icon_green_light    TO it_9002-icon.
      w_matl_suc = w_matl_suc + 1.
      w_matl_cnt = w_matl_cnt + 1.
    ENDIF.

    MOVE: it_9002-matnr   TO bapimathead-material,
          'X'             TO bapimathead-basic_view,
          it_9002-normt_n TO bapi_mara-std_descr,
          'X'             TO bapi_marax-std_descr,
          it_9002-profl_n TO bapi_mara-hazmatprof,
          'X'             TO bapi_marax-hazmatprof.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = bapimathead
              clientdata     = bapi_mara
              clientdatax    = bapi_marax
         TABLES
              returnmessages = lt_bapiret2.
    LOOP AT lt_bapiret2 WHERE type = 'E'
                           OR type = 'A'.
    ENDLOOP.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      MOVE: c_sap_err           TO it_9002-flag,
            icon_red_light      TO it_9002-icon,
            lt_bapiret2-message TO it_9002-zmsg.
      w_matl_err = w_matl_err + 1.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
      IF it_9002-kzkfg_m NE it_9002-kzkfg_n." OR
        PERFORM change_configurable_field.
      ELSE.
        MOVE: c_success           TO it_9002-flag,
              icon_green_light    TO it_9002-icon.
        w_matl_suc = w_matl_suc + 1.
      ENDIF.
    ENDIF.

    w_matl_cnt = w_matl_cnt + 1.

    MODIFY it_9002.
  ENDLOOP.
ENDFORM.                    " update_material_master
*&---------------------------------------------------------------------*
*&      Form  set_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_material_master.
  DATA: BEGIN OF lt_matnr OCCURS 0,
          matnr   LIKE   mara-matnr,
        END   OF lt_matnr.

  DATA: BEGIN OF lt_color_next OCCURS 0,
          matnr   LIKE   mara-matnr,
        END   OF lt_color_next.

  CLEAR: it_9002, it_9002[].

  LOOP AT it_9001.
    MOVE: it_9001-comp TO lt_matnr-matnr.
    COLLECT lt_matnr.

    IF it_9001-color_flg EQ c_check OR
* Begin of changes - UD1K923301
* Mark Parent material of component as configurable material
* in Material master.
      ( it_9001-PLNT eq 'P001' and it_9001-USAG eq '1'
       and it_9001-eitm eq 'C' and it_9001-stgb eq 'P' ).
* End of changes - UD1K923301

      MOVE: it_9001-mtno TO lt_matnr-matnr.
      COLLECT lt_matnr.
      MOVE: it_9001-mtno TO lt_color_next-matnr.
      COLLECT lt_color_next.
    ENDIF.
  ENDLOOP.

  SORT lt_color_next BY matnr.

  LOOP AT it_fsc.
    MOVE: it_fsc-mtno TO lt_matnr-matnr.
    COLLECT lt_matnr.
  ENDLOOP.

  READ TABLE lt_matnr INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'NIGIMISIBURALJOTTO' TO lt_matnr-matnr.
    APPEND lt_matnr.
  ENDIF.

  SELECT a~matnr b~maktx a~mtart
         a~profl AS profl_m a~normt AS normt_m a~kzkfg AS kzkfg_m
    INTO CORRESPONDING FIELDS OF TABLE it_9002
    FROM mara AS a INNER JOIN makt AS b
                      ON a~mandt EQ b~mandt
                     AND a~matnr EQ b~matnr
                     AND b~spras EQ sy-langu
     FOR ALL ENTRIES IN lt_matnr
   WHERE a~matnr EQ lt_matnr-matnr.

  SORT it_9002 BY matnr.
  LOOP AT it_9002.
    IF it_9002-mtart EQ 'FERT'.
      MOVE: 'M'             TO it_9002-profl_n,
            'X'             TO it_9002-kzkfg_n,
            it_9002-normt_m TO it_9002-normt_n.
      MODIFY it_9002. CONTINUE.
    ELSE.
      CLEAR: it_9001.
      READ TABLE it_9001 WITH KEY comp = it_9002-matnr.

      READ TABLE lt_color_next WITH KEY matnr = it_9002-matnr
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE: 'X'             TO it_9002-kzkfg_n,
              it_9002-profl_m TO it_9002-profl_n,
              it_9002-normt_m TO it_9002-normt_n.
      ELSE.
        MOVE: it_9002-kzkfg_m TO it_9002-kzkfg_n,
              it_9002-profl_m TO it_9002-profl_n,
              it_9001-stgb    TO it_9002-normt_n.
      ENDIF.
    ENDIF.

    MODIFY it_9002.
  ENDLOOP.
ENDFORM.                    " set_material_master
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  IF sy-batch EQ c_check.           "Batch job
    PERFORM display_ecm_result.
    PERFORM display_matl_master.
  ELSE.
    SORT it_9001 BY zinfo mtno plnt usag altn pref comp suff dpid
                    create_seq.
    CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  display_ecm_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_ecm_result.
  DATA: lw_color_flg,
        lw_color_mod TYPE i.

  MOVE: 'BOM' TO w_header_flg.

  SORT it_9001 BY zresult zinfo mtno plnt usag altn pref comp
                          suff  dpid create_seq.
  LOOP AT it_9001.
    SET LEFT SCROLL-BOUNDARY COLUMN 58.

    lw_color_mod = sy-tabix MOD 6.
    IF lw_color_mod EQ 0.
      WRITE:/ sy-uline.
    ENDIF.

    PERFORM display_bom_1st_key.
    PERFORM display_bom_1st_body USING lw_color_flg lw_color_mod.
    PERFORM display_bom_2nd      USING lw_color_flg.
  ENDLOOP.
  IF sy-subrc NE 0.
    WRITE:/ text-b25.
  ENDIF.

  WRITE:/ sy-uline.
ENDFORM.                    " display_ecm_result
*&---------------------------------------------------------------------*
*&      Form  display_matl_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_matl_master.
  DATA: lw_color_flg,
        lw_color_mod TYPE i.

  NEW-PAGE.

  MOVE: 'MATL' TO w_header_flg.

  LOOP AT it_9002.
    lw_color_mod = sy-tabix MOD 6.
    IF lw_color_mod EQ 0.
      WRITE:/ sy-uline.
    ENDIF.

    PERFORM display_matl_1st_key.
    PERFORM display_matl_1st_body USING lw_color_flg lw_color_mod.
  ENDLOOP.

  WRITE:/ sy-uline.
ENDFORM.                    " display_matl_master
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color_body USING pw_color_flg pw_color_mod.
  pw_color_mod = sy-tabix MOD 6.
  IF pw_color_mod = 0.
    IF pw_color_flg EQ 'X'.
      CLEAR: pw_color_flg.
    ELSE.
      MOVE: 'X' TO pw_color_flg.
    ENDIF.
  ENDIF.

  IF pw_color_flg IS INITIAL.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.
ENDFORM.                    " SET_COLOR_BODY
*&---------------------------------------------------------------------*
*&      Form  display_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_header.
  CASE w_header_flg.
    WHEN 'BOM'.
      PERFORM display_bom_header.
    WHEN 'MATL'.
      PERFORM display_matl_master_header.
  ENDCASE.
ENDFORM.                    " display_header
*&---------------------------------------------------------------------*
*&      Form  display_bom_1st_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bom_1st_key.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE:/1(10) it_9001-zinfo,
          (18) it_9001-mtno,
          (18) it_9001-comp,
               it_9001-icon AS ICON.
ENDFORM.                    " display_bom_1st_key
*&---------------------------------------------------------------------*
*&      Form  display_bom_1st_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bom_1st_body USING pw_color_flg pw_color_mod.
  PERFORM set_color_body USING pw_color_flg pw_color_mod.

  WRITE:       it_9001-plnt,
          (03) it_9001-usag,
          (03) it_9001-altn,
               it_9001-pref,
               it_9001-suff,
               it_9001-seqc,
          (20) it_9001-dpid,
          (10) it_9001-qnty UNIT it_9001-unit,
               it_9001-unit,
               it_9001-datf,
               it_9001-aenf,
               it_9001-datt,
               it_9001-aent,
               it_9001-eono,
          (02) it_9001-eitm,
          (02) it_9001-stgb,
          (02) it_9001-clpt,
               it_9001-upgn,
          (10) it_9001-bqty UNIT it_9001-hunt,
               it_9001-hunt,
          (03) it_9001-itca,
          (03) it_9001-zmode,
          (17) it_9001-stlkn.
ENDFORM.                    " display_bom_1st_body
*&---------------------------------------------------------------------*
*&      Form  display_bom_2nd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bom_2nd USING pw_color_flg.
  IF it_9001-zresult EQ c_legacy_err OR
     it_9001-zresult EQ c_sap_err.
    FORMAT COLOR COL_KEY INTENSIFIED ON.

    WRITE:/35 it_9001-idoc,
              it_9001-item.

    IF pw_color_flg IS INITIAL.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ENDIF.

    WRITE: 55 it_9001-zmsg.
  ENDIF.
ENDFORM.                    " display_bom_2nd
*&---------------------------------------------------------------------*
*&      Form  display_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bom_header.
  SET LEFT SCROLL-BOUNDARY COLUMN 58.

  WRITE:/070 text-h01,
        /070 text-h04.
  SKIP.

  WRITE:/110 icon_sum          AS ICON, (08) 'SUM',
             icon_green_light  AS ICON, (07) 'Success',
             icon_red_light    AS ICON, (07) 'SAP Err',
             icon_yellow_light AS ICON, (07) 'Leg Err'.

  WRITE:/        'Plant     :', s_plnt-low,
         100     'Total   :',   (12) w_tot_cnt,       (12) w_tot_suc,
            (12) w_tot_sap,     (12) w_tot_leg.

  WRITE:/        'Material  :', (18) s_mtno-low, '~', (18) s_mtno-high,
         100     'ECM I/F :',   (12) w_ecm_cnt,       (12) w_ecm_suc,
            (12) w_ecm_sap,     (12) w_ecm_leg.

  WRITE:/        'Component :', (18) s_comp-low, '~', (18) s_comp-high,
         100     'Others  :',   (12) w_other_cnt,     (12) w_other_suc,
            (12) w_other_sap.

  WRITE:/        'Component :', (18) s_comp-low, '~', (18) s_comp-high,
         100     'Others  :',   (12) w_matl_cnt,      (12) w_matl_suc,
            (12) w_matl_err.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE:/ sy-uline.
  WRITE:/ text-h02, text-h03.
  WRITE:/ sy-uline.
ENDFORM.                    " display_bom_header
*&---------------------------------------------------------------------*
*&      Form  display_matl_master_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_matl_master_header.
  SET LEFT SCROLL-BOUNDARY COLUMN 58.

  WRITE:/060 text-h05,
        /060 text-h06.
  SKIP.

  WRITE:/110 icon_sum          AS ICON, (08) 'SUM',
             icon_green_light  AS ICON, (07) 'Success',
             icon_red_light    AS ICON, (07) 'SAP Err',
             icon_yellow_light AS ICON, (07) 'Leg Err'.

  WRITE:/100     'Total     :',  (12) w_tot_cnt,       (12) w_tot_suc,
            (12) w_tot_sap,      (12) w_tot_leg.

  WRITE:/        'Plant     :', s_plnt-low,
         100     'ECM I/F   :',  (12) w_ecm_cnt,       (12) w_ecm_suc,
            (12) w_ecm_sap,      (12) w_ecm_leg.

  WRITE:/        'Material  :',  (18) s_mtno-low, '~', (18) s_mtno-high,
         100     'Others    :',  (12) w_other_cnt,     (12) w_other_suc,
            (12) w_other_sap.

  WRITE:/        'Component :',  (18) s_comp-low, '~', (18) s_comp-high,
         100     'MatlMast  :',  (12) w_matl_cnt,      (12) w_matl_suc,
            (12) w_matl_err.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE:/ sy-uline.
  WRITE:/ text-h07.
  WRITE:/ sy-uline.
ENDFORM.                    " display_matl_master_header
*&---------------------------------------------------------------------*
*&      Form  get_item_id_for_del
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LT_OD  text
*----------------------------------------------------------------------*
FORM get_item_id_for_del TABLES pt_stb    STRUCTURE stpox.
  DATA: lw_index TYPE i.

  LOOP AT pt_stb WHERE werks = it_9001-plnt
                   AND stlan = it_9001-usag
                   AND stlal = it_9001-altn
                   AND posnr = it_9001-pref
                   AND idnrk = it_9001-comp
                   AND suff  = it_9001-suff.
    lw_index = lw_index + 1.

    MOVE: pt_stb-stlkn TO it_9001-stlkn.

    IF lw_index = 2.
      MOVE: c_sap_err TO it_9001-zresult.
      CONCATENATE text-b21 text-b11 INTO it_9001-zmsg
        SEPARATED BY space.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: c_sap_err TO it_9001-zresult.
    CONCATENATE text-b21 text-b14 INTO it_9001-zmsg
      SEPARATED BY space.
    EXIT.
  ENDIF.
ENDFORM.                    " get_item_id_for_del
*&---------------------------------------------------------------------*
*&      Form  display_matl_1st_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_matl_1st_key.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE:/1(18) it_9002-matnr,
               it_9002-maktx,
               it_9002-icon AS ICON.
ENDFORM.                    " display_matl_1st_key
*&---------------------------------------------------------------------*
*&      Form  display_matl_1st_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_COLOR_FLG  text
*      -->P_LW_COLOR_MOD  text
*----------------------------------------------------------------------*
FORM display_matl_1st_body USING pw_color_flg pw_color_mod.
  PERFORM set_color_body USING pw_color_flg pw_color_mod.

  WRITE:  (05) it_9002-mtart,
          (06) it_9002-kzkfg_m,
          (09) it_9002-profl_m,
          (14) it_9002-normt_m,
          (06) it_9002-kzkfg_n,
          (09) it_9002-profl_n,
          (14) it_9002-normt_n,
         (115) it_9002-zmsg.
ENDFORM.                    " display_matl_1st_body
*&---------------------------------------------------------------------*
*&      Form  change_configurable_field
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
     ' ' 'RMMG1-MATNR'         it_9002-matnr,
     ' ' 'BDC_OKCODE'          '=AUSW',

     'X' 'SAPLMGMM'            '0070',
     ' ' 'MSICHTAUSW-KZSEL(2)' 'X',
     ' ' 'BDC_OKCODE'          '/00',


     'X' 'SAPLMGMM'            '5004',
     ' ' 'MARA-KZKFG'          it_9002-kzkfg_n,
     ' ' 'BDC_OKCODE'          '=BU'.

  CALL TRANSACTION 'MM02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '801'.
    MOVE: c_sap_err           TO it_9002-flag,
          icon_red_light      TO it_9002-icon.
    PERFORM get_err_msg USING 'MM02' it_9002-zmsg.
    w_matl_err = w_matl_err + 1.
  ELSE.
    MOVE: c_success           TO it_9002-flag,
          icon_green_light    TO it_9002-icon.
    w_matl_suc = w_matl_suc + 1.
  ENDIF.
ENDFORM.                    " change_configurable_field
*&---------------------------------------------------------------------*
*&      Form  check_engine_parts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_engine_parts.

** FOR E002
*  CHECK it_bom_ecm-plnt EQ c_engine_plant.
  CHECK it_bom_ecm-plnt NE c_plant.
** END FOR E002

*  CASE it_bom_ecm-eitm.
*    WHEN 'G' OR 'K'.
*      CHECK it_bom_ecm-zresult EQ space.
*
*      PERFORM get_where_userd_list TABLES it_matnr.
*      PERFORM check_n_value_for_engine.
*
*
*    WHEN OTHERS.
*      MOVE: 'X'   TO it_9001-sanko,
*            space TO it_9001-sanfe,
*            space TO it_9001-sanka.
*  ENDCASE.

  CASE it_bom_ecm-eitm.
    WHEN 'G' OR 'K'.
      MOVE: space TO it_9001-sanko,
            'X'   TO it_9001-sanfe,
            'X'   TO it_9001-sanka.
    WHEN OTHERS.
** changed on 10/31/2006 by Furong
** FOR E002
*      IF it_bom_ecm-plnt = 'E001' AND it_bom_ecm-stgb = '7'.
*        MOVE: space TO it_9001-sanko,
*           'X'   TO it_9001-sanfe,
*           'X'   TO it_9001-sanka.
*
*      ELSE.
** END FOR E002
        MOVE: 'X'   TO it_9001-sanko,
              space TO it_9001-sanfe,
              space TO it_9001-sanka.
*      ENDIF.
*      MOVE: 'X'   TO it_9001-sanko,
*            space TO it_9001-sanfe,
*            space TO it_9001-sanka.
** end of change
  ENDCASE.
ENDFORM.                    " check_engine_parts
*&---------------------------------------------------------------------*
*&      Form  get_where_userd_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MATNR  text
*----------------------------------------------------------------------*
FORM get_where_userd_list TABLES pt_matnr STRUCTURE it_matnr.
  DATA: lw_topmat LIKE mc29s.
  DATA: lt_wultb   LIKE   stpov   OCCURS 0 WITH HEADER LINE,
        lt_stpov   LIKE   stpov   OCCURS 0 WITH HEADER LINE,
        lt_equicat LIKE   cscequi OCCURS 0 WITH HEADER LINE,
        lt_kndcat  LIKE   cscknd  OCCURS 0 WITH HEADER LINE,
        lt_matcat  LIKE   cscmat  OCCURS 0 WITH HEADER LINE,
        lt_stdcat  LIKE   cscstd  OCCURS 0 WITH HEADER LINE,
        lt_tplcat  LIKE   csctpl  OCCURS 0 WITH HEADER LINE.

  CLEAR: it_inf_compare, it_inf_compare[].

  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            datub                      = it_bom_ecm-datf
            datuv                      = it_bom_ecm-datt
            matnr                      = it_bom_ecm-comp
            werks                      = it_bom_ecm-plnt
            stlan                      = it_bom_ecm-usag
       IMPORTING
            topmat                     = lw_topmat
       TABLES
            wultb                      = lt_wultb
            equicat                    = lt_equicat
            kndcat                     = lt_kndcat
            matcat                     = lt_matcat
            stdcat                     = lt_stdcat
            tplcat                     = lt_tplcat
       EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.
  LOOP AT lt_wultb.
    IF lt_wultb-vwalt NE it_bom_ecm-altn.
      DELETE lt_wultb. CONTINUE.
    ENDIF.

    READ TABLE it_inf WITH KEY valu1 = lt_wultb-idnrk.
    IF sy-subrc EQ 0.
      MOVE: it_inf-item(4) TO it_inf_compare-item.
      COLLECT it_inf_compare.

      DELETE lt_wultb. CONTINUE.
    ENDIF.
  ENDLOOP.

  READ TABLE it_inf WITH KEY valu1 = it_bom_ecm-mtno.
  IF sy-subrc EQ 0.
    MOVE: it_inf-item(4) TO it_inf_compare-item.
    COLLECT it_inf_compare.
  ENDIF.

  LOOP AT lt_wultb.
    PERFORM get_where_used_list_multi_lvl USING lt_wultb-idnrk.
  ENDLOOP.
ENDFORM.                    " get_where_userd_list
*&---------------------------------------------------------------------*
*&      Form  get_engine_inf_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_engine_inf_data.
  SELECT valu1 item INTO CORRESPONDING FIELDS OF TABLE it_inf
    FROM ztbm_fsc_cre_inf
   WHERE item  LIKE 'EN_6%'
      OR item  LIKE 'EN_7%'.

  SORT it_inf BY valu1 item.
ENDFORM.                    " get_engine_inf_data
*&---------------------------------------------------------------------*
*&      Form  GET_WHERE_USED_LIST_MULTI_Lvl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_WULTB_IDNRK  text
*----------------------------------------------------------------------*
FORM get_where_used_list_multi_lvl USING pw_matnr.
  DATA: lw_topmat LIKE mc29s.
  DATA: lt_wultb   LIKE   stpov   OCCURS 0 WITH HEADER LINE,
        lt_stpov   LIKE   stpov   OCCURS 0 WITH HEADER LINE,
        lt_equicat LIKE   cscequi OCCURS 0 WITH HEADER LINE,
        lt_kndcat  LIKE   cscknd  OCCURS 0 WITH HEADER LINE,
        lt_matcat  LIKE   cscmat  OCCURS 0 WITH HEADER LINE,
        lt_stdcat  LIKE   cscstd  OCCURS 0 WITH HEADER LINE,
        lt_tplcat  LIKE   csctpl  OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            datub                      = it_bom_ecm-datf
            datuv                      = it_bom_ecm-datt
            matnr                      = pw_matnr
            werks                      = it_bom_ecm-plnt
            stlan                      = it_bom_ecm-usag
       IMPORTING
            topmat                     = lw_topmat
       TABLES
            wultb                      = lt_wultb
            equicat                    = lt_equicat
            kndcat                     = lt_kndcat
            matcat                     = lt_matcat
            stdcat                     = lt_stdcat
            tplcat                     = lt_tplcat
       EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.
  LOOP AT lt_wultb.
    IF lt_wultb-vwalt NE it_bom_ecm-altn.
      DELETE lt_wultb. CONTINUE.
    ENDIF.

    READ TABLE it_inf WITH KEY valu1 = lt_wultb-idnrk.
    IF sy-subrc EQ 0.
      MOVE: it_inf-item(4) TO it_inf_compare-item.
      COLLECT it_inf_compare.
    ELSE.
      PERFORM get_where_used_list_multi_lvl USING lt_wultb-idnrk.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_WHERE_USED_LIST_MULTI_Lvl
*&---------------------------------------------------------------------*
*&      Form  check_n_value_for_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_n_value_for_engine.
  LOOP AT it_inf_compare.
    CASE it_bom_ecm-eitm.
      WHEN 'G'.                        "for MIP
        IF it_inf_compare-item EQ 'EN_6'.
          MOVE: space TO it_9001-sanko,
                'X'   TO it_9001-sanfe,
                'X'   TO it_9001-sanka.
        ELSE.
          MOVE: 'X'   TO it_9001-sanko,
                space TO it_9001-sanfe,
                space TO it_9001-sanka.
        ENDIF.
      WHEN 'K'.                        "for ASSY
        IF it_inf_compare-item EQ 'EN_7'.
          MOVE: space TO it_9001-sanko,
                'X'   TO it_9001-sanfe,
                'X'   TO it_9001-sanka.
        ELSE.
          MOVE: 'X'   TO it_9001-sanko,
                space TO it_9001-sanfe,
                space TO it_9001-sanka.
        ENDIF.
    ENDCASE.

    IF sy-tabix EQ 2.
      MOVE: c_sap_err TO it_bom_ecm-zresult,
            text-b26  TO it_bom_ecm-zmsg.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE: c_sap_err TO it_bom_ecm-zresult,
          text-b28  TO it_bom_ecm-zmsg.
  ENDIF.
ENDFORM.                    " check_n_value_for_engine
*&---------------------------------------------------------------------*
*&      Form  read_od_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_od_value USING pw_stb LIKE stpox.
  DATA: BEGIN OF lt_knnam OCCURS 0,
          knnam LIKE cukb-knnam,
        END   OF lt_knnam.

  CLEAR: it_od, it_od[].

  CHECK NOT pw_stb-knobj IS INITIAL.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_knnam
    FROM cuob AS a INNER JOIN cukb AS b
                      ON a~mandt = b~mandt
                     AND a~knnum = b~knnum
                     AND a~adzhl = b~adzhl
   WHERE a~kntab =  'STPO'
     AND a~knobj =  pw_stb-knobj
     AND a~lkenz =  space
     AND b~lkenz =  space.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  LOOP AT lt_knnam.
    CLEAR: it_od.

*    MOVE: it_9001-mtno TO it_od-mtno,
*          it_9001-plnt TO it_od-plnt,
*          it_9001-usag TO it_od-usag,
*          it_9001-altn TO it_od-altn,
*          pw_stb-posnr TO it_od-pref,
*          pw_stb-idnrk TO it_od-comp,
*          pw_stb-knobj TO it_od-knobj,
*          pw_stb-suff  TO it_od-suff,
*          pw_stb-stlkn TO it_od-stlkn.
    MOVE: lt_knnam-knnam      TO it_od-knnam.

    APPEND it_od.
  ENDLOOP.

  CASE it_9001-zmode.
    WHEN c_update.
      MOVE: it_9001-dpid TO it_od-knnam.
      COLLECT it_od.
    WHEN c_delete.
      DELETE it_od WHERE knnam = it_9001-dpid.
  ENDCASE.
ENDFORM.                    " read_od_value
*&---------------------------------------------------------------------*
*&      Form  check_item_change_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PT_STB  text
*----------------------------------------------------------------------*
FORM check_item_change_color USING pw_stb LIKE stpox.
  SELECT SINGLE * FROM aenr WHERE aennr = it_9001-aenf.
  IF sy-subrc NE 0.
    MOVE: c_sap_err TO it_9001-zresult,
          text-m01  TO it_9001-zmsg.
  ENDIF.

  IF pw_stb-datuv >  it_9001-datf OR
     aenr-aegru   NE it_9001-eono(10).
    MOVE: c_update TO wa_diff_chk-from_date_change.
  ENDIF.

*---// If Valid to date is initial,
*---// second BDC failed during item creation.
*---// Second BDC is assignment to-change no.
  IF it_9001-aent IS INITIAL.
    MOVE: c_create TO wa_diff_chk-to_date_change.
  ELSE.
    SELECT SINGLE * FROM aenr WHERE aennr = it_9001-aent.
    IF sy-subrc NE 0.
      MOVE: c_sap_err TO it_9001-zresult,
            text-m01  TO it_9001-zmsg.
    ENDIF.

    IF pw_stb-datub <  it_9001-datt OR
       aenr-aegru   NE it_9001-eono+10(10).
      MOVE: c_update TO wa_diff_chk-to_date_change.
    ENDIF.
  ENDIF.

  MOVE: c_update TO wa_diff_chk-others_change.
ENDFORM.                    " check_item_change_color
*&---------------------------------------------------------------------*
*&      Form  bdc_cs02_update_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_update_normal.
  DATA lw_qnty(20).

  CHECK it_9001-zresult           EQ space    AND
        wa_diff_chk-others_change EQ c_update.

  REFRESH: it_bdc.

  WRITE: it_9001-qnty TO lw_qnty UNIT it_9001-unit LEFT-JUSTIFIED.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9001-aenf,    "Change number
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,   "Item No
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',            "CHECK
     ' ' 'RC29P-POSNR(01)' it_9001-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(01)' it_9001-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(01)' lw_qnty,        "Compenent quantity
     ' ' 'RC29P-MEINS(01)' it_9001-unit,    "Compenent Uom
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131'.

  IF it_9001-usag EQ '1' AND it_9001-plnt NE c_plant.
    PERFORM dynpro USING:
       ' ' 'RC29P-SANKO'     it_9001-sanko,   "Engineering/design
       ' ' 'RC29P-SANFE'     it_9001-sanfe,   "Production Relevant
       ' ' 'RC29P-SANKA'     it_9001-sanka.   "Costing Relevncy
  ENDIF.

  PERFORM dynpro USING:
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZEITM'           it_9001-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'           it_9001-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'           it_9001-suff,    "SUFFIX NO
     ' ' 'ZUPGN'           it_9001-upgn,    "UPG
     ' ' 'ZINFO'           it_9001-zinfo,   "Info
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " bdc_cs02_update_normal
*&---------------------------------------------------------------------*
*&      Form  bdc_cs02_update_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_update_color.
  DATA lw_qnty(20).
 DATA l_field(15).
  DATA l_cnt(02) TYPE n.

  CHECK it_9001-zresult           EQ space    AND
        wa_diff_chk-others_change EQ c_update.

  REFRESH: it_bdc.

  WRITE: it_9001-qnty TO lw_qnty UNIT it_9001-unit LEFT-JUSTIFIED.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9001-mtno,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9001-plnt,    "PLANT
     ' ' 'RC29N-STLAN'     it_9001-usag,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9001-altn,    "ALT BOM
     ' ' 'RC29N-AENNR'     it_9001-aenf,    "Change number
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=SETP',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,   "Item No
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',            "CHECK
     ' ' 'RC29P-POSNR(01)' it_9001-pref,    "BOM item number
     ' ' 'RC29P-IDNRK(01)' it_9001-comp,    "BOM compenent
     ' ' 'RC29P-MENGE(01)' lw_qnty,        "Compenent quantity
     ' ' 'RC29P-MEINS(01)' it_9001-unit,    "Compenent Uom
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131'.

  IF it_9001-usag EQ '1' AND it_9001-plnt NE c_plant.
    PERFORM dynpro USING:
       ' ' 'RC29P-SANKO'     it_9001-sanko,   "Engineering/design
       ' ' 'RC29P-SANFE'     it_9001-sanfe,   "Production Relevant
       ' ' 'RC29P-SANKA'     it_9001-sanka.   "Costing Relevncy
  ENDIF.

  PERFORM dynpro USING:
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZEITM'           it_9001-eitm,    "END ITEM TYPE
     ' ' 'ZSTGB'           it_9001-stgb,    "STRUCTURE TYPE
     ' ' 'ZSUFF'           it_9001-suff,    "SUFFIX NO
     ' ' 'ZUPGN'           it_9001-upgn,    "UPG
     ' ' 'ZINFO'           it_9001-zinfo,   "Info
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0708',
     ' ' 'RC29P-SELPI'     it_9001-stlkn,
     ' ' 'BDC_OKCODE'      '=CLWI',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "
     ' ' 'BDC_OKCODE'      '=WIZU',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=MRKA',

     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=DELZ'.

** ECC6.0 BDC Logic change : New Entries Func removed
** : KDM01 - Start
  LOOP AT it_od.

    l_cnt = l_cnt + 1.
    CONCATENATE 'RCUKD-KNNAM(' l_cnt ')' INTO l_field.

    PERFORM dynpro USING:
       'X' 'SAPLCUKD'        '0130',
       ' ' l_field           it_od-knnam,
       ' ' 'BDC_OKCODE'      '/00'.

    IF l_cnt >= 10.
      l_cnt = 0.
      PERFORM dynpro USING:
         'X' 'SAPLCUKD'        '0130',
         ' ' 'BDC_OKCODE'      '=P+'.
    ENDIF.
  ENDLOOP.
*  LOOP AT it_od.
*    PERFORM dynpro USING:
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'BDC_OKCODE'      '=NEWZ',
*
*       'X' 'SAPLCUKD'        '0130',
*       ' ' 'RCUKD-KNNAM(02)' it_od-knnam,
*       ' ' 'BDC_OKCODE'      '/00'.
*  ENDLOOP.
** : KDM01 - End

  PERFORM dynpro USING:
     'X' 'SAPLCUKD'        '0130',
     ' ' 'BDC_OKCODE'      '=BACK',

     'X' 'SAPLCSDI'        '0150',
     ' ' 'BDC_OKCODE'      '=FCBU'.

  CALL TRANSACTION 'CS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '031'.
    MOVE: c_sap_err TO it_9001-zresult.
    PERFORM get_err_msg USING 'CS02' it_9001-zmsg.
  ENDIF.
ENDFORM.                    " bdc_cs02_update_color
*&---------------------------------------------------------------------*
*&      Form  update_pt_stb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4016   text
*----------------------------------------------------------------------*
FORM update_pt_stb TABLES pt_stb STRUCTURE stpox
                    USING p_change_flag.
  CHECK it_9001-zresult EQ space.

  READ TABLE pt_stb WITH KEY werks = it_9001-plnt
                             stlan = it_9001-usag
                             stlal = it_9001-altn
                             posnr = it_9001-pref
                             idnrk = it_9001-comp
                             suff  = it_9001-suff.
  IF sy-subrc NE 0.
    MOVE: c_sap_err TO it_9001-zresult,
          text-m01  TO it_9001-zmsg.
  ENDIF.

  CASE p_change_flag.
    WHEN 'IN'.
      MOVE: it_9001-datf TO pt_stb-datuv.
    WHEN 'OUT'.
      MOVE: it_9001-datt TO pt_stb-datub.
    WHEN 'OTHERS'.
      MOVE: it_9001-qnty TO pt_stb-menge,
            it_9001-unit TO pt_stb-meins,
            it_9001-eitm TO pt_stb-eitm,
            it_9001-stgb TO pt_stb-stgb,
            it_9001-upgn TO pt_stb-upgn.
  ENDCASE.

  MODIFY pt_stb INDEX sy-tabix.
ENDFORM.                    " update_pt_stb
