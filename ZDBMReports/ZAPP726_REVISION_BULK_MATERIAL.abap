************************************************************************
* Program Name      : ZAPP726_REVISION_BULK_MATERIAL
* Author            : Byung Sung Bae
* Creation Date     : 2005.02.08.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : Revision Bulk Material
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zapp726_revision_bulk_material .

INCLUDE <icon>.
TABLES: mara, marc, aenr.
*---// Internal tables
DATA: BEGIN OF it_fsc OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
      END   OF it_fsc.

DATA: BEGIN OF it_sub_val OCCURS 0.
        INCLUDE STRUCTURE ztbm_sub_bom_vel.
DATA:   check   LIKE   mara-matnr,
        maktx   LIKE   makt-maktx,
      END   OF it_sub_val.

DATA: BEGIN OF it_sub_mat_alc OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
        idnrk   LIKE   stpo-idnrk,
        maktx   LIKE   makt-maktx,
        flag,
        zmsg(100),
      END   OF it_sub_mat_alc.

DATA: BEGIN OF it_sub_mat_sap OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
        idnrk   LIKE   stpo-idnrk,
        maktx   LIKE   makt-maktx,
        datuv   LIKE   stpox-datuv,
        aennr   LIKE   stpox-aennr,
        datub   LIKE   stpox-datub,
        aenra   LIKE   stpox-aenra,
        posnr   LIKE   stpox-posnr,
        zinfo   LIKE   stpox-zinfo,
        menge   LIKE   stpox-menge,
        meins   LIKE   stpox-meins,
        stlkn   LIKE   stpo-stlkn,
        flag,
        zmsg(100),
      END   OF it_sub_mat_sap.

DATA: it_9000 LIKE zspp_zapp726 OCCURS 0 WITH HEADER LINE.

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
      w_total(4)     TYPE   n.

*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_subtxt  LIKE stpo-zinfo  VALUE 'BULK',
           c_posnr   LIKE stpo-posnr  VALUE '9000',
           c_capid   LIKE rc29l-capid VALUE 'PP01',      "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_datuv   LIKE sy-datum VALUE '19000101',  "Initial date
           c_datub   LIKE sy-datum VALUE '99991230',  "End date
           c_object  LIKE inri-object VALUE 'ZBM_CHG_NO',"Number Range
           c_ready                    VALUE 'R',
           c_success                  VALUE 'S',         "Success
           c_create                   VALUE 'C',
           c_update                   VALUE 'U',
           c_delete                   VALUE 'D',
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
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM update_rtn.
  PERFORM display_rtn.

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
*&      Form  read_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  PERFORM get_fsc_list.
  PERFORM get_sub_matl_alc.
  PERFORM get_sub_matl_sap.
  PERFORM set_it_9000.
  PERFORM set_count.
ENDFORM.                    " read_DATA
*&---------------------------------------------------------------------*
*&      Form  get_fsc_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fsc_list.
  SELECT a~matnr b~werks b~stlan b~stlal
    INTO CORRESPONDING FIELDS OF TABLE it_fsc
    FROM mara AS a INNER JOIN mast AS b
                      ON a~matnr = b~matnr
                     AND b~stlan = '1'
                   INNER JOIN stko AS c
                      ON b~stlnr = c~stlnr
                     AND b~stlal = c~stlal
                     AND c~stlty = 'M'
                     AND c~stlst = '01'
   WHERE a~matnr IN s_matnr
     AND a~ersda IN s_ersda
     AND a~mtart EQ 'FERT'.
ENDFORM.                    " get_fsc_list
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
         b~matnr AS check c~maktx
    INTO CORRESPONDING FIELDS OF TABLE it_sub_val
    FROM ztbm_sub_bom_vel AS a LEFT OUTER JOIN marc AS b
                                       ON a~mandt = b~mandt
                                      AND a~werks = b~werks
                                      AND a~matnr = b~matnr
                               LEFT OUTER JOIN makt AS c
                                       ON c~mandt = a~mandt
                                      AND c~matnr = a~matnr
                                      AND c~spras = sy-langu.

  SORT it_sub_val BY matnr werks z_nation sequ DESCENDING.
ENDFORM.                    " get_sub_bom_value
*&---------------------------------------------------------------------*
*&      Form  SET_SUB_MATL_ALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_sub_matl_alc.
  LOOP AT it_fsc.
    LOOP AT it_sub_val WHERE werks = it_fsc-werks.
      CLEAR: it_sub_mat_alc.

*---// Pattern matching checking
      CASE it_fsc-matnr+4(2).
        WHEN 'XX'.                         "BIP
          IF NOT it_sub_val-z_nation CS 'XX'.
            CONTINUE.
          ENDIF.
        WHEN 'XY'.                         "BIW
          IF NOT it_sub_val-z_nation CS 'XY'.
            CONTINUE.
          ENDIF.
        WHEN OTHERS.
          IF NOT ( it_fsc-matnr+1(5)  CP it_sub_val-z_nation AND
                   it_fsc-matnr+6(2)  CP it_sub_val-z_car    AND
                   it_fsc-matnr(1)    CP it_sub_val-z_year   AND
                   it_fsc-matnr+8(1)  CP it_sub_val-z_bt     AND
                   it_fsc-matnr+9(1)  CP it_sub_val-z_tl     AND
                   it_fsc-matnr+10(1) CP it_sub_val-z_ec     AND
                   it_fsc-matnr+11(1) CP it_sub_val-z_ft     AND
                   it_fsc-matnr+12(1) CP it_sub_val-z_tm   ).
            CONTINUE.
          ENDIF.
      ENDCASE.

      MOVE: it_fsc-matnr     TO it_sub_mat_alc-matnr,
            it_fsc-werks     TO it_sub_mat_alc-werks,
            it_fsc-stlan     TO it_sub_mat_alc-stlan,
            it_fsc-stlal     TO it_sub_mat_alc-stlal,
            it_sub_val-matnr TO it_sub_mat_alc-idnrk,
            it_sub_val-maktx TO it_sub_mat_alc-maktx.

      IF it_sub_val-check IS INITIAL.
        MOVE: c_error   TO it_sub_mat_alc-flag,
              text-b01  TO it_sub_mat_alc-zmsg.
      ENDIF.

      COLLECT it_sub_mat_alc.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " SET_SUB_MATL_ALC
*&---------------------------------------------------------------------*
*&      Form  get_sub_matl_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_matl_alc.
  PERFORM get_sub_bom_value.
  PERFORM set_sub_matl_alc.
ENDFORM.                    " get_sub_matl_alc
*&---------------------------------------------------------------------*
*&      Form  get_sub_matl_sap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_matl_sap.
  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.
  DATA: lw_subrc.

  LOOP AT it_fsc.
    PERFORM get_sap_bom TABLES lt_stb
                        USING  lw_topmat
                               it_fsc-matnr it_fsc-werks it_fsc-stlan
                               it_fsc-stlal lw_subrc.

    PERFORM check_sub_material TABLES lt_stb
                               USING  lw_subrc.
  ENDLOOP.
ENDFORM.                    " get_sub_matl_sap
*&---------------------------------------------------------------------*
*&      Form  get_sap_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LW_TOPMAT  text
*      -->P_IT_FSC_MATNR  text
*      -->P_IT_FSC_WERKS  text
*      -->P_IT_FSC_STLAN  text
*      -->P_IT_FSC_STLAL  text
*      -->P_LW_SUBRC  text
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
*&      Form  check_sub_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB  text
*      -->P_LW_SUBRC  text
*----------------------------------------------------------------------*
FORM check_sub_material TABLES pt_stb STRUCTURE stpox
                        USING  pw_subrc.
  DATA: lw_index TYPE i.
  DATA: lt_sub_mat_sap LIKE it_sub_mat_sap OCCURS 0 WITH HEADER LINE.

  LOOP AT pt_stb WHERE zinfo EQ c_subtxt.
    CLEAR: lt_sub_mat_sap.

    MOVE-CORRESPONDING pt_stb TO lt_sub_mat_sap.
    MOVE: it_fsc-matnr TO lt_sub_mat_sap-matnr,
          pt_stb-ojtxb TO lt_sub_mat_sap-maktx.

    APPEND lt_sub_mat_sap.
  ENDLOOP.

  LOOP AT it_sub_mat_alc WHERE matnr = it_fsc-matnr.
    LOOP AT pt_stb WHERE werks = it_sub_mat_alc-werks
                     AND stlan = it_sub_mat_alc-stlan
                     AND stlal = it_sub_mat_alc-stlal
                     AND idnrk = it_sub_mat_alc-idnrk
                     AND zinfo = space.
      MOVE-CORRESPONDING pt_stb TO lt_sub_mat_sap.
      MOVE: it_fsc-matnr TO lt_sub_mat_sap-matnr,
            pt_stb-ojtxb TO lt_sub_mat_sap-maktx.

      APPEND lt_sub_mat_sap.
    ENDLOOP.
  ENDLOOP.

  SORT lt_sub_mat_sap BY matnr werks stlan stlal idnrk stlkn.
  LOOP AT lt_sub_mat_sap.
    CLEAR: it_sub_mat_sap.
    MOVE lt_sub_mat_sap TO it_sub_mat_sap.

    lw_index = lw_index + 1.

    AT END OF idnrk.
      IF lw_index >= 2.
        MOVE: c_error  TO it_sub_mat_sap-flag,
              text-b02 TO it_sub_mat_sap-zmsg.
      ENDIF.

      APPEND it_sub_mat_sap.

      CLEAR: lw_index.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " check_sub_material
*&---------------------------------------------------------------------*
*&      Form  set_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000.
  PERFORM set_it_9000_from_sap.
  PERFORM set_it_9000_from_alc.
  PERFORM set_condition.
ENDFORM.                    " set_it_9000
*&---------------------------------------------------------------------*
*&      Form  append_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_DELETE  text
*----------------------------------------------------------------------*
FORM append_it_9000 USING pw_mode.
  MOVE: it_sub_mat_sap-matnr TO it_9000-matnr,
        it_sub_mat_sap-werks TO it_9000-werks,
        it_sub_mat_sap-stlan TO it_9000-stlan,
        it_sub_mat_sap-stlal TO it_9000-stlal,
        it_sub_mat_sap-idnrk TO it_9000-idnrk,
        it_sub_mat_sap-maktx TO it_9000-maktx,
        it_sub_mat_sap-datuv TO it_9000-datuv,
        it_sub_mat_sap-aennr TO it_9000-aennr,
        it_sub_mat_sap-datub TO it_9000-datub,
        it_sub_mat_sap-aenra TO it_9000-aenra,
        it_sub_mat_sap-stlkn TO it_9000-stlkn,
        it_sub_mat_sap-zinfo TO it_9000-zinfo,
        it_sub_mat_sap-flag  TO it_9000-flag,
        it_sub_mat_sap-zmsg  TO it_9000-zmsg,
        pw_mode              TO it_9000-mode.

  APPEND it_9000.
ENDFORM.                    " append_it_9000
*&---------------------------------------------------------------------*
*&      Form  set_it_9000_from_sap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000_from_sap.
  LOOP AT it_sub_mat_sap.
    CLEAR: it_9000.

    CASE it_sub_mat_sap-flag.
      WHEN c_error.
        READ TABLE it_sub_mat_alc WITH KEY matnr = it_sub_mat_sap-matnr
                                           werks = it_sub_mat_sap-werks
                                           stlan = it_sub_mat_sap-stlan
                                           stlal = it_sub_mat_sap-stlal
                                           idnrk = it_sub_mat_sap-idnrk.
        IF sy-subrc EQ 0.
          PERFORM append_it_9000 USING c_update.
        ELSE.
          PERFORM append_it_9000 USING c_delete.
        ENDIF.
      WHEN OTHERS.
        READ TABLE it_sub_mat_alc WITH KEY matnr = it_sub_mat_sap-matnr
                                           werks = it_sub_mat_sap-werks
                                           stlan = it_sub_mat_sap-stlan
                                           stlal = it_sub_mat_sap-stlal
                                           idnrk = it_sub_mat_sap-idnrk.
        IF sy-subrc EQ 0.
          IF it_sub_mat_sap-zinfo NE c_subtxt OR
             it_sub_mat_sap-posnr NE c_posnr  OR
             it_sub_mat_sap-menge NE 1        OR
             it_sub_mat_sap-meins NE 'EA'.
            PERFORM append_it_9000 USING c_update.
          ENDIF.
        ELSE.
          PERFORM append_it_9000 USING c_delete.
        ENDIF.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " set_it_9000_from_sap
*&---------------------------------------------------------------------*
*&      Form  set_it_9000_from_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000_from_alc.
  LOOP AT it_sub_mat_alc.
    READ TABLE it_sub_mat_sap WITH KEY matnr = it_sub_mat_alc-matnr
                                       werks = it_sub_mat_alc-werks
                                       stlan = it_sub_mat_alc-stlan
                                       stlal = it_sub_mat_alc-stlal
                                       idnrk = it_sub_mat_alc-idnrk.
    IF sy-subrc NE 0.
      PERFORM append_it_9000_alc USING c_create.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_it_9000_from_alc
*&---------------------------------------------------------------------*
*&      Form  append_it_9000_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_CREATE  text
*----------------------------------------------------------------------*
FORM append_it_9000_alc USING pw_mode.
  CLEAR: it_9000.

  MOVE: it_sub_mat_alc-matnr TO it_9000-matnr,
        it_sub_mat_alc-werks TO it_9000-werks,
        it_sub_mat_alc-stlan TO it_9000-stlan,
        it_sub_mat_alc-stlal TO it_9000-stlal,
        it_sub_mat_alc-idnrk TO it_9000-idnrk,
        it_sub_mat_alc-maktx TO it_9000-maktx,
        it_sub_mat_alc-flag  TO it_9000-flag,
        it_sub_mat_alc-zmsg  TO it_9000-zmsg,
        pw_mode              TO it_9000-mode.

  APPEND it_9000.
ENDFORM.                    " append_it_9000_alc
*&---------------------------------------------------------------------*
*&      Form  set_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_condition.
  LOOP AT it_9000.
    IF it_9000-flag EQ space.
      MOVE: c_ready TO it_9000-flag.
    ENDIF.

    CASE it_9000-flag.
      WHEN c_error.
        MOVE: icon_red_light TO it_9000-icon.
      WHEN c_ready.
        MOVE: icon_yellow_light TO it_9000-icon.
    ENDCASE.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " set_condition
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
  LOOP AT it_9000 WHERE flag EQ c_ready.
    CASE it_9000-mode.
      WHEN c_create.
        PERFORM create_item.
      WHEN c_update.
        PERFORM update_item.
      WHEN c_delete.
        PERFORM delete_item.
    ENDCASE.

    MODIFY it_9000.
  ENDLOOP.
  PERFORM set_count.
ENDFORM.                    " update_bom
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  SORT it_9000 BY icon matnr werks stlan stlal idnrk.
  CALL SCREEN 9000.
ENDFORM.                    " display_rtn
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
*&      Form  excute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_rtn.
  PERFORM update_bom.

  SORT it_9000 BY icon matnr werks stlan stlal idnrk.

  PERFORM assign_itab_to_alv_9000.
  PERFORM sssign_event_9000.
ENDFORM.                    " excute_rtn
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
*      -->P_1497   text
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

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MODE'        ' ',
                                  ' ' 'COLTEXT'     'MODE',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'DATUV'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'AENNR'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'DATUB'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'AENRA'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ZINFO'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'STLKN'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'FLAG'        ' ',
                                  'E' 'NO_OUT'      'X'.
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
  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSPP_ZAPP726'
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

ENDFORM.                    " sssign_event_9000
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1634   text
*      -->P_1635   text
*      -->P_1636   text
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
*&      Form  create_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_item.
  PERFORM create_change_no.
  PERFORM create_bom_item.
ENDFORM.                    " create_ITEM
*&---------------------------------------------------------------------*
*&      Form  update_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_item.
  DATA: lw_qnty(10),
        lw_datum(10),
        lw_zinfo LIKE stpo-zinfo.

  CHECK it_9000-flag EQ c_ready.

  REFRESH: it_bdc.

  WRITE: it_9000-datuv TO lw_datum.
  MOVE: 1 TO lw_qnty.

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
     ' ' 'RC29P-POSNR(01)' c_posnr,        "BOM item number
     ' ' 'RC29P-IDNRK(01)' it_9000-idnrk,  "BOM compenent
     ' ' 'RC29P-MENGE(01)' lw_qnty,        "Compenent quantity
     ' ' 'RC29P-MEINS(01)' 'EA',           "Compenent Uom
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZINFO'           c_subtxt,   "Info
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
    MOVE: c_success        TO it_9000-flag,
          icon_green_light TO it_9000-icon.

    IF it_9000-datuv IS INITIAL.
      MOVE: c_datuv          TO it_9000-datuv.
    ENDIF.

    IF it_9000-datuv IS INITIAL.
      MOVE: c_datub          TO it_9000-datub.
    ENDIF.
  ENDIF.
ENDFORM.                    " update_ITEM
*&---------------------------------------------------------------------*
*&      Form  DELETE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_item.
  DATA: lw_datum(10).

  CHECK it_9000-flag EQ c_ready.

  REFRESH: it_bdc.

  WRITE: it_9000-datuv TO lw_datum.

  PERFORM dynpro USING:
     'X' 'SAPLCSDI'        '0100',
     ' ' 'RC29N-MATNR'     it_9000-matnr,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS'     it_9000-werks,    "PLANT
     ' ' 'RC29N-STLAN'     it_9000-stlan,    "BOM usage
     ' ' 'RC29N-STLAL'     it_9000-stlal,    "ALT BOM
     ' ' 'RC29N-AENNR'     space,          "Change number
     ' ' 'RC29N-DATUV'     lw_datum,       "Valid from
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
    MOVE: c_error   TO it_9000-flag.
    PERFORM get_err_msg USING 'CS02' it_9000-zmsg.
  ELSE.
    MOVE: c_success        TO it_9000-flag,
          icon_green_light TO it_9000-icon.
  ENDIF.
ENDFORM.                    " DELETE_ITEM
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

  PERFORM generate_bdc_cc01 USING it_9000-aennr it_9000-matnr
                                  it_9000-idnrk c_datuv.

  PERFORM generate_bdc_cc01 USING it_9000-aenra it_9000-matnr
                                  it_9000-idnrk c_datub.
ENDFORM.                    " create_change_no
*&---------------------------------------------------------------------*
*&      Form  get_change_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1818   text
*      -->P_IT_9000_AENNR  text
*----------------------------------------------------------------------*
FORM get_change_no USING pw_inout pw_aennr.
  DATA: lw_number(11) TYPE n.

  CHECK it_9000-flag EQ c_ready.

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
          text-b03       TO it_9000-zmsg.
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
*      -->P_IT_9000_MATNR  text
*      -->P_IT_9000_IDNRK  text
*      -->P_C_DATUV  text
*----------------------------------------------------------------------*
FORM generate_bdc_cc01 USING pw_aennr pw_matnr pw_idnrk
                             pw_datuv.
  DATA: lw_datum(10),
        lw_aetxt   LIKE   aenr-aetxt,
        lw_aegru   LIKE   rc29a-aegru.

  CHECK it_9000-flag EQ c_ready.

  MOVE: c_subtxt TO lw_aegru.

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
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2100   text
*      -->P_IT_9000_ZMSG  text
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
*&      Form  bdc_cs02_create_in
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cs02_create_in.
  DATA: lw_qnty(10),
        lw_zinfo   LIKE   stpo-zinfo.

  CHECK it_9000-flag EQ c_ready.

  REFRESH: it_bdc.

  MOVE: 1 TO lw_qnty.

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
     ' ' 'RC29P-POSNR(02)' c_posnr,         "BOM item number
     ' ' 'RC29P-IDNRK(02)' it_9000-idnrk,   "BOM compenent
     ' ' 'RC29P-MENGE(02)' lw_qnty,         "Compenent quantity
     ' ' 'RC29P-MEINS(02)' 'EA',            "Compenent Uom
     ' ' 'RC29P-POSTP(02)' 'L',             "Item category
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0130',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0131',
     ' ' 'BDC_OKCODE'      '/00',

     'X' 'SAPLCSDI'        '0138',
     ' ' 'ZINFO'           c_subtxt,         "COLOR SEQUENCE
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
ENDFORM.                    " bdc_cs02_create_in
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_cc02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9001_AENF  text
*      -->P_IT_9001_MTNO  text
*      -->P_IT_9001_COMP  text
*      -->P_IT_9001_DATF  text
*      -->P_IT_9001_EONO(10)  text
*----------------------------------------------------------------------*
FORM generate_bdc_cc02 USING pw_aennr pw_matnr pw_idnrk
                             pw_datuv pw_aegru.
  DATA: lw_datum(10),
        lw_aetxt   LIKE   aenr-aetxt.

  CHECK it_9000-flag EQ c_ready.

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
    MOVE: c_error TO it_9000-flag.
    PERFORM get_err_msg USING 'CC02' it_9000-zmsg.
  ENDIF.
ENDFORM.                    " generate_bdc_cc02
*&---------------------------------------------------------------------*
*&      Form  bdc_cs02_create_out
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

  CHECK it_9000-flag EQ c_ready.

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

  CHECK it_9000-flag EQ c_ready.

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
    MOVE: c_success        TO it_9000-flag,
          icon_green_light TO it_9000-icon.

    IF it_9000-datuv IS INITIAL.
      MOVE: c_datuv          TO it_9000-datuv.
    ENDIF.

    IF it_9000-datuv IS INITIAL.
      MOVE: c_datub          TO it_9000-datub.
    ENDIF.
  ENDIF.
ENDFORM.                    " bdc_cs02_create_out
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
                   AND posnr = c_posnr
                   AND idnrk = it_9000-idnrk.
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
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1778   text
*      -->P_1779   text
*      -->P_1780   text
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
*&      Form  set_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_count.
  CLEAR: w_total, w_success, w_error, w_ready.
  LOOP AT it_9000.
    w_total = w_total + 1.

    CASE it_9000-flag.
      WHEN c_success.
        w_success = w_success + 1.
      WHEN c_ready.
        w_ready = w_ready + 1.
      WHEN c_error.
        w_error = w_error + 1.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " set_count
