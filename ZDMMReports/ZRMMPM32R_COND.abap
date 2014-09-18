************************************************************************
* Program Name      : ZRMMPM32R_COND
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.03.25.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K908580
* Addl Documentation:
* Description       : Rate Status by FSC
*
* Modification Logs
* Date            Developer       RequestNo   Description
* 2004.03.25.     Sung-Tae Lim    UD1K908580  Initial Coding
* 2005.06.29.     chris           UD1K916532  Include material type ROH1
* Deleted t-code by APM Monitoring (5/4/2012)
************************************************************************
* TODO - ABP logic
REPORT zrmmpm32r_cond.
TABLES: mara, marc, a018,
        zsmm_mmpm31r_fsc.
DATA: zsmm_mmpm32r_9000 LIKE zsmm_mmpm32r_9000.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsmm_mmpm32r_9000
                                     WITH HEADER LINE.

DATA: BEGIN OF it_itab OCCURS 0,
        matnr   LIKE   mara-matnr,
        meins   LIKE   stpo-meins,
        qty01   LIKE   stpo-menge,
        qty02   LIKE   stpo-menge,
        qty03   LIKE   stpo-menge,
        qty04   LIKE   stpo-menge,
        qty05   LIKE   stpo-menge,
        qty06   LIKE   stpo-menge,
        netpr   LIKE   ekpo-netpr,
        peinh   LIKE   ekpo-peinh,
        netwr   LIKE   ekpo-netwr,
      END   OF it_itab.

DATA: BEGIN OF it_header OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
      END   OF it_header.

DATA: BEGIN OF it_lifnr OCCURS 0,
        lifnr   LIKE   lfa1-lifnr,
        knumh   LIKE   konp-knumh,
        kbetr   LIKE   konp-kbetr,
        kpein   LIKE   konp-kpein,
        price   TYPE   f,
      END   OF it_lifnr.

DATA: BEGIN OF it_condition OCCURS 0,
        kbetr LIKE zvmm_info_condi-kbetr,
        kpein LIKE zvmm_info_condi-kpein,
        konwa LIKE zvmm_info_condi-konwa,
        kschl LIKE zvmm_info_condi-kschl,
      END   OF it_condition.

*---// Work area
DATA: w_werks   LIKE   marc-werks,
      w_datuv   LIKE   rc29n-datuv,
      w_9000    LIKE it_9000.


*---// Contants
CONSTANTS: c_check   TYPE c           VALUE 'X',
           c_mtart   LIKE mara-mtart  VALUE 'ROH',
           c_mtart1   LIKE mara-mtart  VALUE 'ROH1',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_werks   LIKE marc-werks  VALUE 'E001',
           c_ekorg   LIKE ekko-ekorg  VALUE 'PU01',
           c_kschl   LIKE konp-kschl  VALUE 'PB00',
           c_stlal   LIKE mast-stlal  VALUE '01',
           c_roh     TYPE i           VALUE 1,
           c_module  TYPE i           VALUE 2,
           c_subpart TYPE i           VALUE 3,
           c_module_stlan LIKE mast-stlan VALUE '2'.

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

CONSTANTS: c_structure(100) VALUE 'ZSMM_MMPM32R_'.

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

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click

ENDCLASS.

START-OF-SELECTION.
  MOVE: sy-datum TO w_datuv,
        'P001'   TO w_werks.

  CALL SCREEN 9000.

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
*---------------------------------------------------------------------*
*       FORM create_alv_object                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_DYNNR                                                       *
*---------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container.
  ASSIGN:      (w_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
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
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
*  w_is_layout-NO_TOTLINE = ' '.
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
  DATA: lw_text(9).

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'KSCHL'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VTEXT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'AMT01'       ' ',
                                  'E' 'NO_OUT'      ' ',

                                  'S' 'RAT01'       ' ',
                                  'E' 'NO_OUT'      ' ',

                                  'S' 'AMT02'       ' ',
                                  ' ' 'EDIT_MASK'   '___.__%',
                                  'E' 'EMPHASIZE'   'C600',

                                  'S' 'RAT02'       ' ',
                                  'E' 'EMPHASIZE'   'C600',

                                  'S' 'AMT03'       ' ',
                                  'E' 'EMPHASIZE'   'C301',

                                  'S' 'RAT03'       ' ',
                                  'E' 'EMPHASIZE'   'C301',

                                  'S' 'AMT04'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'RAT04'       ' ',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'AMT05'       ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'RAT05'       ' ',
                                  'E' 'EMPHASIZE'   'C300',

                                  'S' 'AMT06'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'RAT06'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'WAERS'       ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " set_screen_fields_9000
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
FORM sssign_event USING p_dynnr.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv.
  ASSIGN: (w_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
        EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR ALV_GRID.
**-   toolbar control event
*    SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
  SET HANDLER event_receiver->handle_double_click  FOR <alv>.
**    SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
**    SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID
  .
**    SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
*    SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID
  .
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR ALV_GRID
  .

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
*    CALL METHOD alv_grid->set_toolbar_interactive.
*
*    CALL METHOD cl_gui_control=>set_focus
*                        EXPORTING control = alv_grid.
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
*&      Module  check_screen_field_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_screen_field_9000 INPUT.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc01
                          zsmm_mmpm31r_fsc-ext01 zsmm_mmpm31r_fsc-int01
                          zsmm_mmpm31r_fsc-alt01 zsmm_mmpm31r_fsc-usg01.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc02
                          zsmm_mmpm31r_fsc-ext02 zsmm_mmpm31r_fsc-int02
                          zsmm_mmpm31r_fsc-alt02 zsmm_mmpm31r_fsc-usg02.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc03
                          zsmm_mmpm31r_fsc-ext03 zsmm_mmpm31r_fsc-int03
                          zsmm_mmpm31r_fsc-alt03 zsmm_mmpm31r_fsc-usg03.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc04
                          zsmm_mmpm31r_fsc-ext04 zsmm_mmpm31r_fsc-int04
                          zsmm_mmpm31r_fsc-alt04 zsmm_mmpm31r_fsc-usg04.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc05
                          zsmm_mmpm31r_fsc-ext05 zsmm_mmpm31r_fsc-int05
                          zsmm_mmpm31r_fsc-alt05 zsmm_mmpm31r_fsc-usg05.
  PERFORM check_fsc USING zsmm_mmpm31r_fsc-fsc06
                          zsmm_mmpm31r_fsc-ext06 zsmm_mmpm31r_fsc-int06
                          zsmm_mmpm31r_fsc-alt06 zsmm_mmpm31r_fsc-usg06.
ENDMODULE.                 " check_screen_field_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSMM_MMPM31R_FSC_FSC01  text
*      -->P_ZSMM_MMPM31R_FSC_EXT01  text
*      -->P_ZSMM_MMPM31R_FSC_INT01  text
*      -->P_ZSMM_MMPM31R_FSC_ALT01  text
*      -->P_ZSMM_MMPM31R_FSC_USG01  text
*----------------------------------------------------------------------*
FORM check_fsc USING pw_fsc pw_ext pw_int pw_alt pw_usg.
  CHECK pw_fsc NE space.

  SELECT SINGLE * FROM mara WHERE matnr = pw_fsc.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02 pw_fsc text-m03.
  ENDIF.

  IF mara-lvorm EQ 'X'.
    MESSAGE e000(zz) WITH text-m02 pw_fsc text-m04.
  ENDIF.

  IF mara-mtart NE 'FERT'.
    MESSAGE e000(zz) WITH text-m02 pw_fsc text-m05.
  ENDIF.

  IF pw_ext EQ space.
    MESSAGE e000(zz) WITH text-m06 pw_fsc.
  ENDIF.

  IF pw_int EQ space.
    MESSAGE e000(zz) WITH text-m07 pw_fsc.
  ENDIF.

  IF pw_alt EQ space.
    MESSAGE e000(zz) WITH text-m08 pw_fsc.
  ENDIF.

  IF pw_usg EQ space.
    MESSAGE e000(zz) WITH text-m09 pw_fsc.
  ENDIF.
ENDFORM.                    " check_fsc
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
    WHEN 'REFRESH'.
      CLEAR sy-ucomm.
      PERFORM refresh_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  refresh_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_rtn.
  CLEAR: it_itab, it_itab[],
         it_9000, it_9000[].

  PERFORM read_bom_header.
  PERFORM read_bom.
  PERFORM read_info_record.
ENDFORM.                    " refresh_rtn
*&---------------------------------------------------------------------*
*&      Form  read_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bom_header.
  SELECT a~matnr b~werks c~stlan
    INTO CORRESPONDING FIELDS OF TABLE it_header
    FROM mara AS a INNER JOIN marc AS b
                      ON a~matnr EQ b~matnr
                     AND b~lvorm EQ space
                   INNER JOIN mast AS c
                      ON b~matnr EQ c~matnr
                     AND b~werks EQ c~werks
                     AND c~stlal EQ c_stlal
                   INNER JOIN stko AS d
                      ON c~stlnr EQ d~stlnr
                     AND c~stlal EQ d~stlal
                     AND d~stlty EQ 'M'
                     AND d~lkenz EQ space
                     AND d~loekz EQ space
                     AND d~stlst EQ '01'
                     AND d~datuv <= w_datuv
   WHERE a~mtart NE c_mtart and a~mtart ne c_mtart1.

  SORT it_header BY matnr werks stlan.
ENDFORM.                    " read_bom_header
*&---------------------------------------------------------------------*
*&      Form  read_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bom.
  PERFORM read_fsc_bom USING '01'
                             zsmm_mmpm31r_fsc-fsc01
                             zsmm_mmpm31r_fsc-ext01
                             zsmm_mmpm31r_fsc-int01
                             zsmm_mmpm31r_fsc-usg01
                             zsmm_mmpm31r_fsc-alt01.
  PERFORM read_fsc_bom USING '02'
                             zsmm_mmpm31r_fsc-fsc02
                             zsmm_mmpm31r_fsc-ext02
                             zsmm_mmpm31r_fsc-int02
                             zsmm_mmpm31r_fsc-usg02
                             zsmm_mmpm31r_fsc-alt02.
  PERFORM read_fsc_bom USING '03'
                             zsmm_mmpm31r_fsc-fsc03
                             zsmm_mmpm31r_fsc-ext03
                             zsmm_mmpm31r_fsc-int03
                             zsmm_mmpm31r_fsc-usg03
                             zsmm_mmpm31r_fsc-alt03.
  PERFORM read_fsc_bom USING '04'
                             zsmm_mmpm31r_fsc-fsc04
                             zsmm_mmpm31r_fsc-ext04
                             zsmm_mmpm31r_fsc-int04
                             zsmm_mmpm31r_fsc-usg04
                             zsmm_mmpm31r_fsc-alt04.
  PERFORM read_fsc_bom USING '05'
                             zsmm_mmpm31r_fsc-fsc05
                             zsmm_mmpm31r_fsc-ext05
                             zsmm_mmpm31r_fsc-int05
                             zsmm_mmpm31r_fsc-usg05
                             zsmm_mmpm31r_fsc-alt05.
  PERFORM read_fsc_bom USING '06'
                             zsmm_mmpm31r_fsc-fsc06
                             zsmm_mmpm31r_fsc-ext06
                             zsmm_mmpm31r_fsc-int06
                             zsmm_mmpm31r_fsc-usg06
                             zsmm_mmpm31r_fsc-alt06.
ENDFORM.                    " read_bom
*&---------------------------------------------------------------------*
*&      Form  read_fsc_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0911   text
*      -->P_ZSMM_MMPM31R_FSC_FSC01  text
*      -->P_ZSMM_MMPM31R_FSC_EXT01  text
*      -->P_ZSMM_MMPM31R_FSC_INT01  text
*      -->P_ZSMM_MMPM31R_FSC_USG01  text
*      -->P_ZSMM_MMPM31R_FSC_ALT01  text
*----------------------------------------------------------------------*
FORM read_fsc_bom USING pw_index pw_matnr pw_atwre
                        pw_atwri pw_stlan pw_stlal.
  DATA: lw_odseq LIKE zspp_zrpp301r-odseq.
  DATA: lw_knnam LIKE cukb-knnam,
        lw_index LIKE sy-tabix,
        lw_quantity(50).

  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat,
        lw_stlan  LIKE stpox-stlan,
        lw_subrc  LIKE sy-subrc.

  FIELD-SYMBOLS: <quantity>.

  CHECK pw_matnr NE space.

  PERFORM display_progress_bar USING pw_matnr pw_index.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            cuobj                 = c_cuobj
            datuv                 = w_datuv
            mktls                 = 'X'
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = pw_stlal
            svwvo                 = 'X'
            werks                 = w_werks
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
  IF sy-subrc <> 0 OR  lw_topmat-stlal NE pw_stlal.
    EXIT.
  ENDIF.

  LOOP AT lt_stb.
    CLEAR: it_itab, lw_knnam.

    CONCATENATE 'IT_ITAB-QTY' pw_index INTO lw_quantity.
    ASSIGN (lw_quantity) TO <quantity>.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_itab WITH KEY matnr = lt_stb-idnrk.
    IF sy-subrc EQ 0.
      MOVE: sy-tabix TO lw_index.
      PERFORM check_object_dependency USING lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            pw_atwre     pw_atwri
                                            lw_knnam     pw_matnr.
      IF lw_knnam EQ space AND lt_stb-knobj > 0.
        CONTINUE.
      ENDIF.

      <quantity> = <quantity> + lt_stb-menge.
      MODIFY it_itab INDEX lw_index.
    ELSE.
      READ TABLE it_header WITH KEY matnr = lt_stb-idnrk
                                    werks = lt_stb-werks
                                    stlan = pw_stlan
                           BINARY SEARCH.
      IF sy-subrc NE 0.
        CHECK lt_stb-mtart EQ c_mtart or lt_stb-mtart eq c_mtart1.

        PERFORM check_object_dependency USING lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk lt_stb-werks
                                              pw_atwre     pw_atwri
                                              lw_knnam     pw_matnr.
        IF lw_knnam EQ space AND lt_stb-knobj > 0.
          CONTINUE.
        ENDIF.

        MOVE: lt_stb-idnrk TO it_itab-matnr,
              lt_stb-menge TO <quantity>,
              lt_stb-meins TO it_itab-meins.

        APPEND it_itab.
      ELSE.
        CASE lt_stb-zinfo.
          WHEN 'ENG'.
** Changed on 12/19/11 for E002
*            PERFORM bom_explosion USING pw_index c_werks lt_stb-idnrk
*                                        pw_stlan lt_stb-menge
*                                        pw_atwre pw_atwri pw_matnr.
            PERFORM bom_explosion USING pw_index lt_stb-WERKS lt_stb-idnrk
                                        pw_stlan lt_stb-menge
                                        pw_atwre pw_atwri pw_matnr.
** end
          WHEN OTHERS.
            PERFORM bom_explosion USING pw_index w_werks lt_stb-idnrk
                                        pw_stlan lt_stb-menge
                                        pw_atwre pw_atwri pw_matnr.
        ENDCASE.

      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_fsc_bom
*&---------------------------------------------------------------------*
*&      Form  check_object_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STB_KNOBJ  text
*      -->P_LT_STB_MTART  text
*      -->P_LT_STB_IDNRK  text
*      -->P_LT_STB_WERKS  text
*      -->P_PW_ATWRE  text
*      -->P_PW_ATWRI  text
*      -->P_LW_KNNAM  text
*      -->P_PW_MATNR  text
*----------------------------------------------------------------------*
FORM check_object_dependency USING pw_knobj pw_mtart
                                   pw_matnr pw_werks
                                   pw_atwre pw_atwri
                                   pw_knnam pw_fsc.
  CHECK pw_knobj > 0.

  CASE pw_mtart.
    WHEN 'ROH'.
      SELECT SINGLE * FROM marc WHERE matnr = pw_matnr
                                  AND werks = pw_werks.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      IF marc-dispo EQ 'M01'.         "Module Part
        MOVE pw_atwri TO pw_knnam.
        PERFORM compare_dependency USING c_module pw_knnam
                                         pw_atwre pw_atwri pw_knobj.
      ELSE.
        PERFORM get_dependency USING pw_fsc pw_knnam pw_werks.
        PERFORM compare_dependency USING c_roh    pw_knnam
                                         pw_atwre pw_atwri pw_knobj.
      ENDIF.
    WHEN 'ROH1'.
      MOVE: pw_atwre TO pw_knnam.
      PERFORM compare_dependency USING c_subpart pw_knnam
                                       pw_atwre  pw_atwri pw_knobj.
  ENDCASE.
ENDFORM.                    " check_object_dependency
*&---------------------------------------------------------------------*
*&      Form  compare_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_MODULE  text
*      -->P_PW_KNNAM  text
*      -->P_PW_ATWRE  text
*      -->P_PW_ATWRI  text
*      -->P_PW_KNOBJ  text
*----------------------------------------------------------------------*
FORM compare_dependency USING pw_type  pw_knnam
                              pw_atwre pw_atwri pw_knobj.
  DATA: l_knnam LIKE cukb-knnam.
  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  pw_knobj
     AND datuv <= w_datuv.
  IF sy-subrc NE 0.
    MOVE: '9999999999' TO lt_cuob-knnum.
    APPEND lt_cuob.
  ENDIF.

  CASE pw_type.
    WHEN c_roh.
      WRITE: pw_atwre  TO l_knnam(3),
             pw_knnam  TO l_knnam+3.
      SELECT knnam INTO pw_knnam
        FROM cukb
         FOR ALL ENTRIES IN lt_cuob
       WHERE knnum =  lt_cuob-knnum
         AND adzhl =  lt_cuob-adzhl
         AND knnam =  l_knnam
         AND datuv <= w_datuv.
      ENDSELECT.
      IF sy-subrc NE 0.
        WRITE: pw_atwri  TO l_knnam(3),
               pw_knnam  TO l_knnam+3.
        SELECT knnam INTO pw_knnam
          FROM cukb
           FOR ALL ENTRIES IN lt_cuob
         WHERE knnum =  lt_cuob-knnum
           AND adzhl =  lt_cuob-adzhl
           AND knnam =  l_knnam
           AND datuv <= w_datuv.
        ENDSELECT.
        IF sy-subrc NE 0.
          CLEAR: pw_knnam.
        ENDIF.
      ENDIF.
    WHEN c_module OR c_subpart.
      MOVE: pw_knnam TO l_knnam.

      SELECT knnam INTO pw_knnam
        FROM cukb
         FOR ALL ENTRIES IN lt_cuob
       WHERE knnum =  lt_cuob-knnum
         AND adzhl =  lt_cuob-adzhl
         AND knnam =  l_knnam
         AND datuv <= w_datuv.
      ENDSELECT.
      IF sy-subrc NE 0.
        CLEAR: pw_knnam.
      ENDIF.
  ENDCASE.
ENDFORM.                    " compare_dependency
*&---------------------------------------------------------------------*
*&      Form  get_dependency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_FSC  text
*      -->P_PW_KNNAM  text
*      -->P_PW_WERKS  text
*----------------------------------------------------------------------*
FORM get_dependency USING pw_matnr pw_knnam pw_werks.
  DATA: l_in_recno LIKE ibin-in_recno.

  DATA: BEGIN OF lt_cabn OCCURS 7,
          atwrt TYPE v_ibin_syval-atwrt,
          atnam TYPE cabn-atnam,
        END   OF lt_cabn.

  SELECT SINGLE * FROM marc WHERE matnr = pw_matnr
                              AND werks = pw_werks.
  IF sy-subrc NE 0.
    CLEAR: pw_knnam. EXIT.
  ENDIF.

  SELECT SINGLE in_recno INTO l_in_recno
                         FROM ibin
                        WHERE instance EQ marc-cuobj.
  IF sy-subrc NE 0.
    CLEAR: pw_knnam. EXIT.
  ENDIF.

  SELECT a~atwrt b~atnam
    INTO TABLE lt_cabn
    FROM v_ibin_syval AS a INNER JOIN cabn AS b
                              ON a~atinn EQ b~atinn
   WHERE a~in_recno EQ l_in_recno.

  SORT lt_cabn BY atnam.

  LOOP AT lt_cabn WHERE atnam NE 'COLOREXT'
                    AND atnam NE 'COLORINT'
                    AND atnam NE 'COLOR_MI'.
    IF lt_cabn-atwrt EQ '-'.
      CLEAR: lt_cabn-atwrt.
    ENDIF.

    CONCATENATE pw_knnam lt_cabn-atwrt INTO pw_knnam.
  ENDLOOP.

  WRITE: pw_knnam TO pw_knnam+10,
         space    TO pw_knnam(10).

  READ TABLE lt_cabn WITH KEY atnam = 'COLOR_MI'.
  IF sy-subrc EQ 0.
    WRITE: lt_cabn-atwrt TO pw_knnam(10).
  ENDIF.
ENDFORM.                    " get_dependency
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_INDEX  text
*      -->P_C_WERKS  text
*      -->P_LT_STB_IDNRK  text
*      -->P_PW_STLAN  text
*      -->P_LT_STB_MENGE  text
*      -->P_PW_ATWRE  text
*      -->P_PW_ATWRI  text
*      -->P_PW_MATNR  text
*----------------------------------------------------------------------*
FORM bom_explosion USING pw_index pw_werks pw_matnr pw_stlan pw_menge
                         pw_atwre pw_atwri pw_fsc.
  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat,
        lw_subrc LIKE sy-subrc,
        lw_knnam LIKE cukb-knnam,
        lw_menge LIKE stpo-menge,
        lw_index LIKE sy-tabix,
        lw_quantity(50).

  FIELD-SYMBOLS <quantity>.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = w_datuv
            mktls                 = 'X'
            cuobj                 = c_cuobj
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = c_stlal
            svwvo                 = 'X'
            werks                 = pw_werks
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
  IF sy-subrc <> 0 OR  lw_topmat-stlal NE c_stlal.
    EXIT.
  ENDIF.

  LOOP AT lt_stb.
    CLEAR: it_itab, lw_knnam.

    CONCATENATE 'IT_ITAB-QTY' pw_index INTO lw_quantity.
    ASSIGN (lw_quantity) TO <quantity>.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    lw_menge = pw_menge * lt_stb-menge.

    READ TABLE it_itab WITH KEY matnr = lt_stb-idnrk.
    IF sy-subrc EQ 0.
      MOVE: sy-tabix TO lw_index.
      PERFORM check_object_dependency USING lt_stb-knobj lt_stb-mtart
                                            lt_stb-idnrk lt_stb-werks
                                            pw_atwre     pw_atwri
                                            lw_knnam     pw_fsc.
      IF lw_knnam EQ space AND lt_stb-knobj > 0.
        CONTINUE.
      ENDIF.

      <quantity> = <quantity> + lw_menge.
      MODIFY it_itab INDEX lw_index.
    ELSE.
      READ TABLE it_header WITH KEY matnr = lt_stb-idnrk
                                    werks = lt_stb-werks
                                    stlan = pw_stlan
                           BINARY SEARCH.
      IF sy-subrc NE 0.
        CHECK lt_stb-mtart EQ c_mtart or lt_stb-mtart eq c_mtart1.

        PERFORM check_object_dependency USING lt_stb-knobj lt_stb-mtart
                                              lt_stb-idnrk lt_stb-werks
                                              pw_atwre     pw_atwri
                                              lw_knnam     pw_fsc.
        IF lw_knnam EQ space AND lt_stb-knobj > 0.
          CONTINUE.
        ENDIF.

        MOVE: lt_stb-idnrk TO it_itab-matnr,
              lw_menge     TO <quantity>,
              lt_stb-meins TO it_itab-meins.

        APPEND it_itab.
      ELSE.
        PERFORM bom_explosion USING pw_index pw_werks lt_stb-idnrk
                                    pw_stlan lw_menge
                                    pw_atwre pw_atwri
                                    pw_fsc.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_MATNR  text
*      -->P_PW_INDEX  text
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_MATNR  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING pw_text pw_index.
  DATA: lw_text(50),
        lw_percentage(2) TYPE n.

  CONCATENATE text-m10 pw_text INTO lw_text.

  lw_percentage = pw_index * 10.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_info_record.
  SORT it_itab BY matnr.

  LOOP AT it_itab.
    CLEAR: it_condition, it_condition[].

    PERFORM get_info_record.
    PERFORM set_amount.
    MODIFY it_itab.
  ENDLOOP.

  READ TABLE it_9000 WITH KEY kschl = c_kschl.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE: it_9000 TO w_9000.

  LOOP AT it_9000 WHERE kschl NE c_kschl.
    SELECT SINGLE vtext INTO it_9000-vtext
      FROM t685t
     WHERE spras = sy-langu
       AND kvewe = 'A'
       AND kappl = 'M'
       AND kschl = it_9000-kschl.

    PERFORM set_rate.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " read_info_record
*&---------------------------------------------------------------------*
*&      Form  GET_INFO_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_record.
*---// If S/A does not exist and the material has 2 more info record,
*---// select info record that has chipper price .
*---// MM guy said
*---// "All of material's purchaning UoM is same as BOM's UoM,
*---//  and all of currency is the same"
*---// So below logic does not include any conversion logic.
  DATA: lw_lifnr LIKE lfa1-lifnr.

  CLEAR: it_lifnr, it_lifnr[].

  SELECT lifnr
    INTO CORRESPONDING FIELDS OF it_lifnr
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr =  it_itab-matnr
     AND a~loekz =  ' '
     AND b~werks =  ' '
     AND b~ekorg =  c_ekorg
     AND b~loekz =  ' '.

    SELECT SINGLE *
      FROM a018
     WHERE kappl =  'M'
       AND kschl =  'PB00'
       AND matnr =  it_itab-matnr
       AND lifnr =  it_lifnr-lifnr
       AND ekorg =  c_ekorg
       AND esokz =  '0'
       AND datab <= w_datuv
       AND datbi >= w_datuv.
    IF sy-subrc EQ 0.
      MOVE: a018-knumh TO it_lifnr-knumh.

      APPEND it_lifnr.
    ENDIF.
  ENDSELECT.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

*---// Select one Vendor from S/A, P/O
  READ TABLE it_lifnr INDEX 2.
  IF sy-subrc EQ 0.
    PERFORM select_vendor TABLES it_lifnr
                          USING  lw_lifnr.
    IF lw_lifnr NE space.
      DELETE it_lifnr WHERE lifnr NE lw_lifnr.
    ENDIF.
  ENDIF.

  LOOP AT it_lifnr.
    SELECT SINGLE kbetr kpein
      INTO CORRESPONDING FIELDS OF it_lifnr
      FROM zvmm_info_condi
     WHERE knumh = it_lifnr-knumh
       AND kschl = c_kschl
       AND loevm_ko = ' '.
    IF sy-subrc NE 0.
      DELETE it_lifnr.
      CONTINUE.
    ENDIF.

    it_lifnr-price = it_lifnr-kbetr / it_lifnr-kpein.

    MODIFY it_lifnr.
  ENDLOOP.

  SORT it_lifnr BY price.
  READ TABLE it_lifnr INDEX 1.
  DELETE it_lifnr WHERE lifnr NE it_lifnr-lifnr.

*---// Read price and conditions

  SELECT kbetr kpein konwa kschl
    INTO CORRESPONDING FIELDS OF TABLE it_condition
    FROM zvmm_info_condi
   WHERE knumh = it_lifnr-knumh
     AND ( kschl =    c_kschl OR
           kschl LIKE 'ZP%'   )
     AND loevm_ko = ' '.
ENDFORM.                    " GET_INFO_RECORD
*&---------------------------------------------------------------------*
*&      Form  select_vendor_from_sa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*      -->P_LW_LIFNR  text
*----------------------------------------------------------------------*
FORM select_vendor_from_sa TABLES pt_lifnr STRUCTURE it_lifnr
                           USING  pw_lifnr.

  DATA: BEGIN OF lt_sa_price OCCURS 0,
          lifnr   LIKE   lfa1-lifnr,
          netpr   LIKE   ekpo-netpr,
          peinh   LIKE   ekpo-peinh,
          price   TYPE   f,
        END   OF lt_sa_price.

  SELECT lifnr netpr peinh
    INTO CORRESPONDING FIELDS OF TABLE lt_sa_price
    FROM ekko AS a INNER JOIN ekpo AS b
                      ON a~mandt EQ b~mandt
                     AND a~ebeln EQ b~ebeln
     FOR ALL ENTRIES IN pt_lifnr
   WHERE matnr   EQ it_itab-matnr
     AND lifnr   EQ pt_lifnr-lifnr
     AND a~bstyp EQ 'L'
     AND werks   EQ w_werks
     AND a~loekz EQ space
     AND a~autlf EQ space
     AND b~loekz EQ space
     AND b~elikz EQ space
     AND kdatb   <= w_datuv
     AND kdate   >= w_datuv.
  IF sy-subrc EQ 0.
    LOOP AT lt_sa_price.
      lt_sa_price-price = lt_sa_price-netpr / lt_sa_price-peinh.

      MODIFY lt_sa_price.
    ENDLOOP.
  ENDIF.

  SORT lt_sa_price BY price.

  READ TABLE lt_sa_price INDEX 1.

  MOVE: lt_sa_price-lifnr TO pw_lifnr.
ENDFORM.                    " select_vendor_from_sa
*&---------------------------------------------------------------------*
*&      Form  SET_AMOUNT_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_amount.
  DATA: lw_kpein    LIKE konp-kpein.

  SORT it_condition BY kschl.
  LOOP AT it_condition.
    IF it_condition-kschl EQ c_kschl.
      MOVE: it_condition-kbetr TO it_itab-netpr,
            it_condition-kpein TO it_itab-peinh.
      it_itab-netwr = it_itab-netpr / it_itab-peinh * it_itab-qty01.
      MOVE: it_condition-kpein TO lw_kpein.
    ENDIF.
    READ TABLE it_9000 WITH KEY kschl = it_condition-kschl.
    IF sy-subrc EQ 0.
      PERFORM calculate_amount USING lw_kpein.

      MODIFY it_9000 INDEX sy-tabix.
    ELSE.
      CLEAR: it_9000.

      MOVE: it_condition-kschl TO it_9000-kschl,
            it_condition-konwa TO it_9000-waers.

      PERFORM calculate_amount USING lw_kpein.

      APPEND it_9000.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SET_AMOUNT_RATE
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_amount USING pw_kpein.
  DATA: lw_index(2) TYPE n,
        lw_quantity(50),
        lw_amount(50).

  FIELD-SYMBOLS: <quantity>, <amount>.

  DO 6 TIMES.
    MOVE: sy-index TO lw_index.

    CONCATENATE: 'IT_ITAB-QTY' lw_index INTO lw_quantity,
                 'IT_9000-AMT' lw_index INTO lw_amount.
    ASSIGN: (lw_quantity) TO <quantity>,
            (lw_amount)   TO <amount>.

    <amount> = <amount> + it_condition-kbetr / pw_kpein * <quantity>.

  ENDDO.
ENDFORM.                    " CALCULATE_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  set_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_rate.
  DATA: lw_index(2) TYPE n,
        lw_sum(50),
        lw_amt(50),
        lw_rate(50).

  FIELD-SYMBOLS: <amt>, <rate>, <sum>.

  DO 6 TIMES.
    MOVE: sy-index TO lw_index.

    CONCATENATE: 'IT_9000-AMT' lw_index INTO lw_amt,
                 'IT_9000-RAT' lw_index INTO lw_rate,
                 'W_9000-AMT'  lw_index INTO lw_sum.

    ASSIGN: (lw_amt)  TO <amt>,
            (lw_rate) TO <rate>,
            (lw_sum)  TO <sum>.

    <rate> = <amt> / <sum> * 100.
  ENDDO.
ENDFORM.                    " set_rate
*&---------------------------------------------------------------------*
*&      Form  SELECT_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*      -->P_LW_LIFNR  text
*----------------------------------------------------------------------*
FORM select_vendor TABLES pt_lifnr STRUCTURE it_lifnr
                   USING  pw_lifnr.
  PERFORM select_vendor_from_sa TABLES pt_lifnr
                                USING  pw_lifnr.

  CHECK pw_lifnr EQ space.

  PERFORM select_vendor_from_po TABLES pt_lifnr
                                USING  pw_lifnr.
ENDFORM.                    " SELECT_VENDOR
*&---------------------------------------------------------------------*
*&      Form  select_vendor_from_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIFNR  text
*      -->P_LW_LIFNR  text
*----------------------------------------------------------------------*
FORM select_vendor_from_po TABLES pt_lifnr STRUCTURE it_lifnr
                           USING  pw_lifnr.

  DATA: BEGIN OF lt_sa_price OCCURS 0,
          lifnr   LIKE   lfa1-lifnr,
          netpr   LIKE   ekpo-netpr,
          peinh   LIKE   ekpo-peinh,
          price   TYPE   f,
        END   OF lt_sa_price.

  SELECT lifnr netpr peinh
    INTO CORRESPONDING FIELDS OF TABLE lt_sa_price
    FROM ekko AS a INNER JOIN ekpo AS b
                      ON a~mandt EQ b~mandt
                     AND a~ebeln EQ b~ebeln
     FOR ALL ENTRIES IN pt_lifnr
   WHERE matnr   EQ it_itab-matnr
     AND lifnr   EQ pt_lifnr-lifnr
     AND a~bstyp EQ 'F'
     AND werks   EQ w_werks
     AND a~loekz EQ space
     AND a~autlf EQ space
     AND b~loekz EQ space
     AND b~elikz EQ space.
  IF sy-subrc EQ 0.
    LOOP AT lt_sa_price.
      lt_sa_price-price = lt_sa_price-netpr / lt_sa_price-peinh.

      MODIFY lt_sa_price.
    ENDLOOP.
  ENDIF.

  SORT lt_sa_price BY price.

  READ TABLE lt_sa_price INDEX 1.

  MOVE: lt_sa_price-lifnr TO pw_lifnr.
ENDFORM.                    " select_vendor_from_PO
