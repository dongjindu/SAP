************************************************************************
* Program Name      : ZAPP937C_UPDATE_CP_COLOR
* Author            : Byung Sung Bae
* Creation Date     : 2005.11.09.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K918255
* Addl Documentation:
* Description       : Update Cockpit Module-Color Part Mapping Table
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zapp937c_update_cp_color.
INCLUDE: <icon>.
TABLES: ztmm_cp_color, mara.

DATA: zsbm_app937_9000 LIKE zsbm_app937_9000.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsbm_app937_9000
                                     WITH HEADER LINE.

DATA: it_ztmm_cp_color LIKE ztmm_cp_color OCCURS 0
                            WITH HEADER LINE.

DATA: BEGIN OF it_colorless OCCURS 0,
        zstr_vend_c   LIKE   ztbm_modstr-zstr_vend_c,
        zstr_carx_c   LIKE   ztbm_modstr-zstr_carx_c,
        zstr_modu_g   LIKE   ztbm_modstr-zstr_modu_g,
        copit         LIKE   ztmm_cp_color-copit,
        idnrk         LIKE   stpo-idnrk,
        ccn           LIKE   ztbm_moditm-zitm_ccno_no,
      END   OF it_colorless.

DATA: BEGIN OF it_color OCCURS 0,
        part_color   LIKE   ztbm_modc23-zc23_part_ccolr,
        seq          LIKE   ztbm_modc23-zc23_clsf_nsequ,
        fdate        LIKE   ztbm_modc23-zc23_prod_dstrt,
        tdate        LIKE   ztbm_modc23-zc23_prod_dfini,
      END   OF it_color.

DATA: BEGIN OF it_fcode OCCURS 0,
        fcode LIKE rsmpe-func,
      END   OF it_fcode.

DATA: BEGIN OF st_others,
        car      LIKE   ztbm_modc23-zc23_key1_c0000,
        model    LIKE   ztbm_modc23-zc23_key2_c0000,
        option   LIKE   ztbm_modc23-zc23_optn_c0000,
        inkey    LIKE   ztmm_cp_color-inkey,
        icon(4),
        msg(100),
      END   OF st_others.

*---// Global variables
DATA: w_success(5)   TYPE   n,
      w_error(5)     TYPE   n,
      w_ready(5)     TYPE   n,
      w_total(5)     TYPE   n.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

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

CONSTANTS: c_structure(100) VALUE 'ZSBM_APP937_'.

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

*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_werks   LIKE   marc-werks OBLIGATORY MEMORY ID wrk.

SELECT-OPTIONS: s_copit FOR ztmm_cp_color-copit.
*selection-screen skip.
SELECTION-SCREEN ULINE.
*selection-screen skip.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t02.
PARAMETERS: p_update AS CHECKBOX.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl1.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM read_data.

START-OF-SELECTION.
  IF p_update EQ 'X'.
    PERFORM update_cp_color.
  ENDIF.

  PERFORM count_rtn.

  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*---// Select Colorless

  SELECT b~zstr_vend_c b~zstr_carx_c b~zstr_modu_g
         a~mtno AS copit c~zitm_part_no AS idnrk c~zitm_ccno_no AS ccn
    INTO CORRESPONDING FIELDS OF TABLE it_colorless
    FROM ztbm_abxduldt AS a INNER JOIN ztbm_modstr AS b
                               ON a~comp           = b~zstr_assy_part
                              AND b~zstr_eitm_matl = 'C'
                            INNER JOIN ztbm_moditm AS c
                               ON b~zstr_vend_c    = c~zitm_vend_c
                              AND b~zstr_carx_c    = c~zitm_carx_c
                              AND b~zstr_modu_g    = c~zitm_modu_g
                              AND b~zstr_comp_part = c~zitm_part_no
   WHERE plnt EQ p_werks
     AND mtno IN s_copit
   GROUP BY B~ZSTR_VEND_C b~zstr_carx_c  B~ZSTR_MODU_G
            a~mtno        c~zitm_part_no c~zitm_ccno_no.
  IF sy-subrc NE 0.
    IF sy-batch EQ 'X'.
      MESSAGE s000(zz) WITH text-m02.
      LEAVE TO SCREEN sy-dynnr.
    ELSE.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.
  ENDIF.

*---//
  DATA: lw_continue.

  SORT it_colorless BY zstr_vend_c zstr_carx_c zstr_modu_g
                       copit idnrk ccn.
  LOOP AT it_colorless.
    CLEAR: it_9000.

    MOVE: it_colorless-copit TO it_9000-copit,
          sy-uname           TO it_9000-ernam,
          sy-datum           TO it_9000-erdat,
          sy-uzeit           TO it_9000-erzet,
          sy-uname           TO it_9000-aenam,
          sy-datum           TO it_9000-aedat,
          sy-uzeit           TO it_9000-aezet.

    AT NEW copit.
      PERFORM read_mi_options.

      IF st_others-icon EQ space.
        lw_continue = 'X'.
      ELSE.
        lw_continue = space.
      ENDIF.
    ENDAT.

    IF lw_continue EQ space.
      MOVE: it_colorless-idnrk TO it_9000-submt,
            it_colorless-ccn   TO it_9000-ccn,
            st_others-icon     TO it_9000-icon,
            st_others-msg      TO it_9000-msg.

      APPEND it_9000.
      CONTINUE.
    ENDIF.

    IF it_colorless-ccn IS INITIAL.
      MOVE: it_colorless-idnrk TO it_9000-submt,
            st_others-icon     TO it_9000-icon,
            st_others-msg      TO it_9000-msg.

      SELECT SINGLE maktx INTO it_9000-maktx
        FROM makt
       WHERE matnr = it_colorless-idnrk
         AND spras = sy-langu.

      APPEND it_9000.
      CONTINUE.
    ENDIF.

    PERFORM get_part_color.

    PERFORM set_colorpart.
  ENDLOOP.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_mi_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mi_options.
  DATA: BEGIN OF lt_ibsymbol OCCURS 0,
          atnam TYPE cabn-atnam,
          atwrt TYPE ibsymbol-atwrt,
        END OF lt_ibsymbol.

  DATA: lv_fsc LIKE ztbm_modstr-zstr_assy_part,
        lw_int_key(2),                             " Internal Key Color
        lw_length   TYPE   i.

*---// Get FSC
  SELECT zstr_assy_part
    INTO lv_fsc
    FROM ztbm_modstr
   WHERE zstr_vend_c    = it_colorless-zstr_vend_c
     AND zstr_carx_c    = it_colorless-zstr_carx_c
     AND zstr_comp_part = it_colorless-copit.

    CONCATENATE lv_fsc(13) lv_fsc+16(4) INTO lv_fsc
      SEPARATED BY space.

    CLEAR: mara.
    SELECT SINGLE * FROM mara WHERE matnr = lv_fsc.
    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.
  ENDSELECT.
  IF sy-subrc NE 0 OR mara-matnr EQ space.
    MOVE: icon_red_light TO st_others-icon,
          text-m04       TO st_others-msg.
    EXIT.
  ENDIF.

*---// Get MI, Options
  SELECT d~atnam  c~atwrt MAX( d~datuv )
       INTO CORRESPONDING FIELDS OF TABLE lt_ibsymbol
       FROM marc AS a INNER JOIN ibin AS b
                        ON a~cuobj EQ b~instance
                      INNER JOIN v_ibin_syval AS c
                        ON b~in_recno EQ c~in_recno
                      INNER JOIN cabn AS d
                        ON c~atinn EQ d~atinn
       WHERE d~atnam IN ('COLOR_MI','COLOR_OPT1','COLOR_OPT2',
                                    'COLOR_OPT3','COLOR_OPT4')
         AND d~datuv <= sy-datum
         AND a~matnr EQ mara-matnr
         AND a~werks EQ p_werks
       GROUP by d~atnam c~atwrt.
  IF sy-subrc NE 0.
    MOVE: icon_red_light TO st_others-icon,
          text-m06       TO st_others-msg.
    EXIT.
  ENDIF.

  CLEAR: st_others.
  SORT lt_ibsymbol BY atnam atwrt.
  LOOP AT lt_ibsymbol.
    CASE lt_ibsymbol-atnam.
      WHEN 'COLOR_MI'.
        MOVE: lt_ibsymbol-atwrt+4(4) TO st_others-model.
        CONCATENATE lt_ibsymbol-atwrt+1(2) lt_ibsymbol-atwrt(1)
                    lt_ibsymbol-atwrt+3(1)
               INTO st_others-car.
      WHEN OTHERS.
        IF lt_ibsymbol-atwrt NE '-'.
          CONCATENATE st_others-option lt_ibsymbol-atwrt
                 INTO st_others-option.
        ENDIF.
    ENDCASE.
  ENDLOOP.

*---// Get internal key
  MOVE: IT_COLORLESS-COPIT+10(3) TO ST_OTHERS-INKEY.
ENDFORM.                    " read_mi_options
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000' EXCLUDING it_fcode.
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
      CLEAR: sy-ucomm.
      PERFORM execute_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
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

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container,
               'WC_CONTROL_'   p_dynnr INTO v_control,
               'WC_ALV_'       p_dynnr INTO v_alv.

  ASSIGN: (v_container) TO <container>,
          (v_control)   TO <control>,
          (v_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
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
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
*  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
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

  MOVE: sy-repid TO v_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = v_repid
            i_internal_tabname = lw_itab
            i_inclname         = v_repid
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
                                  'S' 'COPIT'       ' ',
                                  ' ' 'COLTEXT'     'Cockpit',
                                  'E' 'KEY'         'X',

                                  'S' 'INKEY'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'SUBMT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'DATAB'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'DATBI'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'CAR'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MODEL'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'OPTION'      ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'CCN'         ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MSG'         ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'ERDAT'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'ERNAM'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'ERZET'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AEDAT'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AENAM'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AEZET'        ' ',
                                  'E' 'NO_OUT'       'X'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
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
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1033   text
*      -->P_1034   text
*      -->P_1035   text
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
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  get_part_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_part_color.
  SELECT zc23_part_ccolr AS part_color
         zc23_prod_dstrt AS fdate
         zc23_prod_dfini AS tdate
         zc23_clsf_nsequ AS seq
    INTO CORRESPONDING FIELDS OF TABLE it_color
    FROM ztbm_modc23
   WHERE zc23_key1_c0000 = st_others-car
     AND zc23_key2_c0000 = st_others-model
     AND zc23_optn_c0000 = st_others-option
     AND zc23_ccno_n0000 = it_colorless-ccn
     AND zc23_keyo_ccolr = st_others-inkey.
  IF sy-subrc NE 0.
    MOVE: it_colorless-idnrk TO it_9000-submt,
          st_others-car      TO it_9000-car,
          st_others-model    TO it_9000-model,
          st_others-option   TO it_9000-option,
          st_others-inkey    TO it_9000-inkey,
          it_colorless-ccn   TO it_9000-ccn,
          icon_red_light     TO it_9000-icon,
          text-m08           TO it_9000-msg.

    SELECT SINGLE maktx INTO it_9000-maktx
      FROM makt
     WHERE matnr = it_colorless-idnrk
       AND spras = sy-langu.

    APPEND it_9000.
    EXIT.
  ENDIF.

  LOOP AT it_color.
    IF it_color-fdate EQ it_color-tdate.
      DELETE it_color.
    ENDIF.
  ENDLOOP.

  READ TABLE it_color INDEX 1.
  IF sy-subrc NE 0.
    MOVE: it_colorless-idnrk TO it_9000-submt,
          st_others-car      TO it_9000-car,
          st_others-model    TO it_9000-model,
          st_others-option   TO it_9000-option,
          st_others-inkey    TO it_9000-inkey,
          it_colorless-ccn   TO it_9000-ccn,
          icon_red_light     TO it_9000-icon,
          text-m08           TO it_9000-msg.

    SELECT SINGLE maktx INTO it_9000-maktx
      FROM makt
     WHERE matnr = it_colorless-idnrk
       AND spras = sy-langu.

    APPEND it_9000.
    EXIT.
  ENDIF.
ENDFORM.                    " get_part_color
*&---------------------------------------------------------------------*
*&      Form  set_colorpart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_colorpart.
  SORT it_color BY part_color seq DESCENDING.
  LOOP AT it_color.
    CONCATENATE it_colorless-idnrk it_color-part_color
           INTO it_9000-submt.

    MOVE: st_others-inkey    TO it_9000-inkey,
          it_color-fdate     TO it_9000-datab,
          st_others-car      TO it_9000-car,
          st_others-model    TO it_9000-model,
          st_others-option   TO it_9000-option,
          st_others-inkey    TO it_9000-inkey,
          it_colorless-ccn   TO it_9000-ccn,
          icon_yellow_light  TO it_9000-icon,
          space              TO it_9000-msg.

    SELECT SINGLE b~maktx INTO it_9000-maktx
      FROM mara AS a LEFT OUTER JOIN makt AS b
                       ON a~matnr = b~matnr
                      AND b~spras = sy-langu
     WHERE a~matnr = it_9000-submt.
    IF sy-subrc NE 0.
      MOVE: icon_red_light TO it_9000-icon,
            text-m12       TO it_9000-msg.
    ENDIF.

    IF it_color-tdate EQ '00000000'.
      MOVE: '99991230' TO it_9000-datbi.
    ELSE.
      MOVE: it_color-tdate TO it_9000-datbi.
    ENDIF.

    APPEND it_9000.
  ENDLOOP.
ENDFORM.                    " set_colorpart
*&---------------------------------------------------------------------*
*&      Form  count_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn.
  CLEAR: w_total, w_error, w_ready, w_success.
  LOOP AT it_9000.
    CASE it_9000-icon.
      WHEN icon_yellow_light.
        w_ready = w_ready + 1.
      WHEN icon_red_light.
        w_error = w_error + 1.
      WHEN icon_green_light.
        w_success = w_success + 1.
    ENDCASE.

    w_total = w_total + 1.
  ENDLOOP.
ENDFORM.                    " count_rtn
*&---------------------------------------------------------------------*
*&      Form  update_cp_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_cp_color.
  DATA: lv_line(5) TYPE n.

  MOVE: 'EXECUTE' TO it_fcode-fcode.
  APPEND it_fcode.

  PERFORM set_target_data.

  DELETE FROM ztmm_cp_color
   WHERE copit IN s_copit.

  INSERT ztmm_cp_color FROM TABLE it_ztmm_cp_color
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.

  DESCRIBE TABLE it_ztmm_cp_color LINES lv_line.

  MESSAGE s000(zz) WITH text-m10 lv_line text-m11.

  LOOP AT it_ztmm_cp_color.
    READ TABLE it_9000 WITH KEY copit = it_ztmm_cp_color-copit
                                inkey = it_ztmm_cp_color-inkey
                                submt = it_ztmm_cp_color-submt
                                datab = it_ztmm_cp_color-datab.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    IF it_9000-msg NE text-m12.
      MOVE: icon_green_light TO it_9000-icon,
            space            TO it_9000-msg.
    ENDIF.
    MODIFY it_9000 INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " update_cp_color
*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_target_data.
  CLEAR: it_ztmm_cp_color[].
  LOOP AT it_9000.
    CHECK it_9000-icon = icon_yellow_light     OR
          ( it_9000-icon = icon_red_light AND
            it_9000-msg  = text-m12           ).

    CLEAR: it_ztmm_cp_color.
    MOVE-CORRESPONDING it_9000 TO it_ztmm_cp_color.

    APPEND it_ztmm_cp_color.
  ENDLOOP.
ENDFORM.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  execute_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_rtn.
  PERFORM update_cp_color.
  PERFORM count_rtn.
  PERFORM assign_itab_to_alv USING '9000'.
ENDFORM.                    " execute_rtn
