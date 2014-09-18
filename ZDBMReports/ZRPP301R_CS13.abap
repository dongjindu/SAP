************************************************************************
* Program Name      : ZRPP301R_CS13
* Author            : Bae, Byung sung
* Creation Date     : 2004.10.28.
* Specifications By : Bae, Byung sung
* Development Request No : UD1K912720
* Addl Documentation:
* Description       : Legacy Format BOM list
*
* Modification Logs
* Date       Developer        RequestNo    Description
*
* 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
*
*  1. Sort by usage, material, component
*  2. Insert Module indicator (MI).
*  3. Insert phantom indicator (MARC-SOBSL)
*  4. Insert structure type indicator (STPO-STGB)
*  5. Object dependency ID
*  6. Include the active date in effective date only
*  7. check Sort string.
*
*  Multi-level and End-item level BOM
*     include MIP(HALB) material.MARC-BESKZ = 'E'
*        AND MARC-SOBSL IS SPACE.
*
************************************************************************
REPORT zrpp301r_cs13 NO STANDARD PAGE HEADING LINE-SIZE 255.
TABLES: mara, t416, t001w, mast, marc, cabn.

*---// Internal Tables
DATA: BEGIN OF it_mast OCCURS 0,
        matnr   LIKE   mast-matnr,
        werks   LIKE   mast-werks,
        stlnr   LIKE   mast-stlnr,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
      END   OF it_mast.

DATA: it_9000 LIKE zspp_zrpp301r OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_od OCCURS 0,
        matnr   LIKE   mast-matnr,
        werks   LIKE   mast-werks,
        stlnr   LIKE   mast-stlnr,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
        posnr   LIKE   stpo-posnr,
        suff    LIKE   stpo-suff,
        idnrk   LIKE   stpo-idnrk,
        odseq   LIKE   zspp_zrpp301r-odseq,
        knobj   LIKE   stpo-knobj,
        knnam   LIKE   cukb-knnam,
        mi      LIKE   zspp_zrpp301r-MI,
      END   OF it_od.

DATA dynpread LIKE dynpread OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF valuetab OCCURS 0,
          value(80).
DATA: END OF valuetab.

DATA: BEGIN OF fields OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF fields.

DATA: BEGIN OF dynpfields  OCCURS 0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: BEGIN OF select_values OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF select_values.

DATA: BEGIN OF it_bom OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        stlan   LIKE   mast-stlan,
        stlal   LIKE   mast-stlal,
      END   OF it_bom.
* 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

type-pools: vrm.
DATA: it_val type vrm_values,
      w_line like line of it_val.
DATA : BEGIN OF IT_ALTBOM OCCURS 0,
         STLAL LIKE MAST-STLAL, " ALTERNATE BOM.
         STLAN like MAST-STLAN, "BOM usage.
       END OF IT_ALTBOM.
DATA: BEGIN OF it_scrfield OCCURS   0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF it_scrfield.
DATA: ZMI(2) TYPE C,
      l_dyname like sy-repid,
       l_dynumb like sy-dynnr.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

*---// Global variables and structures
DATA: select_index LIKE sy-tabix.


*---// Constants
CONSTANTS: c_check                    VALUE 'X',
           c_capid   LIKE rc29l-capid VALUE 'PP01', "Application
           c_datuv   LIKE sy-datum    VALUE '19000101',
           c_cuobj   LIKE marc-cuobj  VALUE '999999999999999999',
           c_roh     TYPE i           VALUE 1,
           c_stlal   LIKE mast-stlal  VALUE '01',
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
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY MEMORY ID wrk.
SELECT-OPTIONS: s_matnr FOR mara-matnr OBLIGATORY.
PARAMETERS: p_stlan LIKE mast-stlan  OBLIGATORY MEMORY ID csv,
            S_stlal LIKE mast-stlal   .
*SELECT-OPTIONS: s_stlal FOR mast-stlal NO-EXTENSION NO INTERVALS.
*parameters: s_stlal(2) type c as listbox visible length 15.
PARAMETERS:     p_datuv LIKE rc29n-datuv DEFAULT sy-datum OBLIGATORY..
*SELECT-OPTIONS: s_matnr FOR mara-matnr OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-t05.
PARAMETERS: p_rdo1 RADIOBUTTON GROUP r1,
            p_rdo2 RADIOBUTTON GROUP r1,
            p_rdo3 RADIOBUTTON GROUP r1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-t06.
PARAMETERS: p_od   AS CHECKBOX USER-COMMAND zod.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl3.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t04.
PARAMETERS: p_atwre LIKE  cawn-atwrt MODIF ID gr1,
            p_atwri LIKE  cawn-atwrt MODIF ID gr1.
SELECTION-SCREEN END   OF BLOCK bl2.

INITIALIZATION.



AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_stlal.
  PERFORM help_request_s_stlal.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_atwre.
  PERFORM help_request_p_atwre.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_atwri.
  PERFORM help_request_p_atwri.

*---// Check input fields & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM get_bom_data.

*---// Display data
START-OF-SELECTION.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
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
  PERFORM check_others.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.
  SELECT SINGLE * FROM t001w WHERE werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " CHECK_WERKS
*&---------------------------------------------------------------------*
*&      Form  CHECK_STLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_stlan.
  SELECT SINGLE * FROM t416 WHERE stlan = p_stlan.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " CHECK_STLAN
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " CHECK_MATNR
*&---------------------------------------------------------------------*
*&      Form  get_bom_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_data.
  DATA: lw_odseq LIKE zspp_zrpp301r-odseq.

  PERFORM set_target_material.
  PERFORM set_bom_header.

  CASE c_check.
    WHEN p_rdo1.
      PERFORM get_single_level_bom.
    WHEN p_rdo2.
      PERFORM get_single_level_bom.
      PERFORM get_sub_item.
    WHEN p_rdo3.
      PERFORM get_single_level_bom.
      PERFORM get_end_item.
  ENDCASE.


  IF p_od EQ c_check.
    PERFORM get_object_dependency.
  ENDIF.




  SORT it_9000 BY matnr werks stlan stlal posnr suff idnrk
                  datuv datub knnam.
  LOOP AT it_9000.
    AT NEW idnrk.
      CLEAR: lw_odseq.
    ENDAT.

    lw_odseq = lw_odseq + 1.

    MOVE: lw_odseq TO it_9000-odseq.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " get_bom_data
*&---------------------------------------------------------------------*
*&      Form  set_target_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_target_material.

  SELECT a~matnr a~werks a~stlan a~stlal
    INTO CORRESPONDING FIELDS OF TABLE it_mast
    FROM mast AS a INNER JOIN stko AS b
                      ON a~stlnr = b~stlnr
                     AND a~stlal = b~stlal
                     AND b~stlty = 'M'
                     AND b~stlst = '01'
   WHERE a~matnr IN s_matnr
     AND a~werks EQ p_werks
     AND a~stlan EQ p_stlan
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
*     AND a~stlal IN s_stlal.
      AND a~stlal EQ s_stlal.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.
ENDFORM.                    " set_target_material
*&---------------------------------------------------------------------*
*&      Form  get_single_level_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_single_level_bom.
  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_stlan LIKE stpox-stlan,
        lw_subrc LIKE sy-subrc.

  LOOP AT it_mast.
    CLEAR: lt_stb, lt_stb[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              aumng                 = 0
              capid                 = c_capid
              cuovs                 = '0'
              datuv                 = p_datuv
*              emeng                 = p_emeng
              mktls                 = 'X'
*              cuobj                 = c_cuobj
              mtnrv                 = it_mast-matnr
              stpst                 = 0
              stlan                 = it_mast-stlan
              stlal                 = it_mast-stlal
              svwvo                 = 'X'
              werks                 = it_mast-werks
              vrsvo                 = 'X'
         TABLES
              stb                   = lt_stb
*              matcat                = selpool
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
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_stb WHERE datuv <= p_datuv and "haseeb
                         datub > p_datuv.

      CLEAR: it_9000, it_od, lw_subrc.
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
*      MOVE-CORRESPONDING lt_stb TO it_od.
*      MOVE: it_mast-matnr       TO it_od-matnr.
*      COLLECT it_od.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

      READ TABLE it_9000 WITH KEY matnr = it_mast-matnr
                                  werks = lt_stb-werks
                                  stlan = lt_stb-stlan
                                  stlal = lt_stb-stlal
                                  posnr = lt_stb-posnr
                                  suff  = lt_stb-suff
                                  idnrk = lt_stb-idnrk
                                  stlkn = lt_stb-stlkn.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING lt_stb TO it_9000.
      MOVE: it_mast-matnr TO it_9000-matnr.

*---// Module check
      IF it_mast-matnr+6(2) EQ lt_stb-idnrk(2).    "Module
        MOVE: c_module_stlan TO lt_stb-stlan,
              c_module       TO it_9000-node_type.
      ENDIF.

      READ TABLE it_bom WITH KEY matnr = lt_stb-idnrk
                                 werks = lt_stb-werks
                                 stlan = lt_stb-stlan
                                 stlal = c_stlal
                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        MOVE: c_check     TO it_9000-stlkz.
      ENDIF.

*----- Object Dependency check
      IF NOT lt_stb-knobj IS INITIAL.
        MOVE: c_check TO it_9000-kzbez.
*        PERFORM get_object_dependency USING it_9000 lw_subrc.
*
*        IF lw_subrc NE space.
*          CONTINUE.
*        ENDIF.

      ENDIF.
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
      case lt_stb-idnrk+3(2) .
        WHEN 'CP'.
          ZMI = 'CP'.
        WHEN 'RC'.
          ZMI = 'RC'.
        WHEN 'FM'.
          ZMI = 'FM'.
        WHEN 'DR'.
          ZMI = 'DR'.
        WHEN 'FC'.
          ZMI = 'FC'.
        WHEN 'TW'.
          ZMI = 'TW'.
      endcase.
      move: zmi to it_9000-mi.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
      APPEND it_9000.
      clear zmi.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " get_single_level_bom
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY matnr werks stlan stlal posnr suff idnrk odseq.
  CALL SCREEN 9000.
ENDFORM.                    " display_data
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
*      -->P_0625   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  w_repid = sy-repid.
  lw_itab = p_itab.
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

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

                                  'S' 'STLAN'       ' ',
                                  'E' 'KEY'         'X',
*HASEEB module indicator
                                  'S' 'MI'          ' ',
                                  'E' 'KEY'         'X',
*HASEEB

                                  'S' 'STLAL'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'POSNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'SUFF'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'IDNRK'       ' ',
                                  'E' 'KEY'         'X',
*HASEEB  phantom indicataor
                                  'S' 'MENGE'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'MEINS'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'DATUV'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'DATUB'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'EITM'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'STGB'        ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'MTART'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'SORTF'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'SOBSL'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'KZBEZ'       ' ',
                                  'E' 'CHECKBOX'    'X',

                                  'S' 'STLKZ'       ' ',
                                  'E' 'CHECKBOX'    'X',

                                  'S' 'ODSEQ'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'KNOBJ'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'KNNAM'       ' ',
                                  'E' 'KEY'         ' ',

                                  'S' 'STLTY'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'WERKS'       ' ',
                                  'E' 'NO_OUT'      'X',
*HASEEB

                                  'S' 'NODE_TYPE'   ' ',
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
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

*  SORT it_9000 BY flag matnr.
  SORT it_9000 by STLAN MATNR IDNRK.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSPP_ZRPP301R'
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0771   text
*      -->P_0772   text
*      -->P_0773   text
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
*&      Form  get_sub_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sub_item.
  DATA: lt_one_level LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  lt_one_level[] = it_9000[].
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

*  LOOP AT lt_one_level.
*    IF lt_one_level-node_type EQ c_module.
*      PERFORM bom_explosion USING lt_one_level-idnrk c_module_stlan.
*    ELSE.
*      PERFORM bom_explosion USING lt_one_level-idnrk p_stlan.
*    ENDIF.
*  ENDLOOP.
  LOOP AT lt_one_level.
    case lt_one_level-idnrk+3(2) .
      WHEN 'CP'.
        ZMI = 'CP'.
      WHEN 'RC'.
        ZMI = 'RC'.
      WHEN 'FM'.
        ZMI = 'FM'.
      WHEN 'DR'.
        ZMI = 'DR'.
      WHEN 'FC'.
        ZMI = 'FC'.
      WHEN 'TW'.
        ZMI = 'TW'.
    endcase.
    IF lt_one_level-node_type EQ c_module.
      PERFORM bom_explosion USING lt_one_level-idnrk c_module_stlan .
    ELSE.
      PERFORM bom_explosion USING lt_one_level-idnrk p_stlan.
    ENDIF.
  ENDLOOP.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
ENDFORM.                    " get_sub_item
*&---------------------------------------------------------------------*
*&      Form  get_end_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_end_item.
  DATA: lt_one_level LIKE it_9000 OCCURS 0 WITH HEADER LINE.
  DATA: lw_stlan LIKE mast-stlan.

  lt_one_level[] = it_9000[].

  CLEAR: it_9000[], it_9000.

  LOOP AT lt_one_level.
*    IF lt_one_level-node_type EQ c_module.
*      MOVE: c_module_stlan     TO lw_stlan.
*    ELSE.
    MOVE: lt_one_level-stlan TO lw_stlan.
*    ENDIF.
*MOVE: '1' TO lw_stlan.

    READ TABLE it_bom WITH KEY matnr = lt_one_level-idnrk
                               werks = lt_one_level-werks
                               stlan = lw_stlan
*                               stlal = c_stlal
                      BINARY SEARCH.
    IF sy-subrc EQ 0.

      PERFORM bom_explosion USING lt_one_level-idnrk lw_stlan.

    ELSE.
      APPEND lt_one_level TO it_9000.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_end_item
*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ONE_LEVEL_MATNR  text
*----------------------------------------------------------------------*
FORM bom_explosion USING pw_matnr pw_stlan.
  DATA: lt_stb TYPE  stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat,
        lw_subrc LIKE sy-subrc.

*CALL FUNCTION 'CSEP_MAT_BOM_READ'
*  EXPORTING
*   MATERIAL             = pw_matnr
*   PLANT                = p_werks
*   BOM_USAGE            = pw_stlan
*   ALTERNATIVE          = S_stlal
*   VALID_FROM           = p_datuv
* TABLES
*   T_STPO               = lt_stb
* EXCEPTIONS
*   ERROR                = 1
*   OTHERS               = 2 .

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  .


  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            aumng                 = 0
            capid                 = c_capid
            cuovs                 = '0'
            datuv                 = p_datuv  "c_datuv
            mktls                 = 'X'
*            cuobj                 = c_cuobj
            mtnrv                 = pw_matnr
            stpst                 = 0
            stlan                 = pw_stlan
            stlal                 = S_stlal  "c_stlal
            svwvo                 = 'X'
            werks                 = p_werks
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

  LOOP AT lt_stb WHERE datuv <= p_datuv and "haseeb
                       datub > p_datuv.
    CLEAR: it_9000, it_od, lw_subrc, it_od.

*     MOVE-CORRESPONDING lt_stb TO it_od.
*    MOVE: it_mast-matnr       TO it_od-matnr.
*    COLLECT it_od.

*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431

    MOVE-CORRESPONDING lt_stb TO it_od.
    MOVE: it_mast-matnr       TO it_od-matnr.
    move: ZMI TO IT_OD-MI.
    APPEND IT_OD.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
    READ TABLE it_9000 WITH KEY matnr = pw_matnr
                                werks = lt_stb-werks
                                stlan = lt_stb-stlan
                                stlal = lt_stb-stlal
                                posnr = lt_stb-posnr
                                suff  = lt_stb-suff
                                idnrk = lt_stb-idnrk
                                stlkn = lt_stb-stlkn.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING lt_stb TO it_9000.
    MOVE: pw_matnr TO it_9000-matnr.

    CASE c_check.
      WHEN p_rdo2.
        READ TABLE it_bom WITH KEY matnr = it_9000-idnrk
                                   werks = it_9000-werks
                                   stlan = pw_stlan
*                                   stlal = '01' "c_stlal
                          BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE: c_check     TO it_9000-stlkz.
        ENDIF.
*11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
        READ table it_od with key idnrk = it_9000-idnrk
                                  stlan = 2.
        if sy-subrc eq 0.
          it_9000-mi = it_od-mi.
        endif.
*END 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
        APPEND it_9000.

        PERFORM bom_explosion USING lt_stb-idnrk pw_stlan.

      WHEN p_rdo3.
        READ TABLE it_bom WITH KEY matnr = it_9000-idnrk
                                   werks = it_9000-werks
                                   stlan = pw_stlan
*                                   stlal =  '01'
                          BINARY SEARCH.
*haseeb
        IF sy-subrc EQ 0.
           if lt_stb-dumps <> ' '. "OR it_9000-STLKZ = 'X'.
              PERFORM bom_explosion USING lt_stb-idnrk pw_stlan.
           else.
              APPEND it_9000.
           endif.
*haseeb
        ELSE.

          APPEND it_9000.

        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " bom_explosion
*&---------------------------------------------------------------------*
*&      Form  GET_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_object_dependency.
* USING pw_9000 LIKE it_9000
*                                 pw_subrc.
  DATA: l_knnam LIKE cukb-knnam.

*  CHECK p_od EQ c_check.

  IF p_atwre IS INITIAL AND
     p_atwri IS INITIAL.
    PERFORM get_all_dependency.
  ELSE.
    PERFORM get_single_dependency USING l_knnam.
  ENDIF.
ENDFORM.                    " GET_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  GET_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_MODULE  text
*      -->P_L_KNNAM  text
*----------------------------------------------------------------------*
FORM get_dependency USING pw_knnam.
  DATA: l_in_recno LIKE ibin-in_recno.

  DATA: BEGIN OF lt_cabn OCCURS 7,
          atwrt TYPE v_ibin_syval-atwrt,
          atnam TYPE cabn-atnam,
        END   OF lt_cabn.

  SELECT SINGLE * FROM marc WHERE matnr = s_matnr-low
                              AND werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  SELECT SINGLE in_recno INTO l_in_recno
                         FROM ibin
                        WHERE instance EQ marc-cuobj.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
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
ENDFORM.                    " GET_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  GET_SINGLE_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_single_dependency USING pw_knnam.
*---// If material type is 'ROH1', dependency = P_ATWRE.
*---// If material is module part, dependency = P_ATWRE + '_' + P_ATWRI
*---// If material is other 'ROH', dependency is in Vehicle Master

  LOOP AT it_9000 WHERE knobj > 0.
    CASE it_9000-mtart.
      WHEN 'ROH'.
        SELECT SINGLE * FROM marc WHERE matnr = it_9000-idnrk
                                    AND werks = p_werks.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        IF marc-dispo EQ 'M01'.         "Module Part
          MOVE: p_atwri TO pw_knnam.
          PERFORM get_collect_dependency USING c_module pw_knnam.
        ELSE.
          PERFORM get_dependency USING pw_knnam.
          PERFORM get_collect_dependency USING c_roh pw_knnam.
        ENDIF.
      WHEN 'ROH1'.
        MOVE: p_atwre TO pw_knnam.
        PERFORM get_collect_dependency USING c_subpart pw_knnam.
    ENDCASE.

    IF pw_knnam IS INITIAL.
      DELETE it_9000.
    ELSE.
      MOVE: pw_knnam TO it_9000-knnam.
      MODIFY it_9000.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_SINGLE_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  GET_COLLECT_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*----------------------------------------------------------------------*
FORM get_collect_dependency USING pw_type pw_knnam.
  DATA: l_knnam LIKE cukb-knnam.
  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE lt_cuob
    FROM cuob
   WHERE kntab =  'STPO'
     AND knobj =  it_9000-knobj
     AND datuv <= p_datuv.
  IF sy-subrc NE 0.
    MOVE: '9999999999' TO lt_cuob-knnum.
    APPEND lt_cuob.
  ENDIF.

  CASE pw_type.
    WHEN c_roh.
      WRITE: p_atwre   TO l_knnam(3),
             pw_knnam  TO l_knnam+3.
      SELECT knnam INTO pw_knnam
        FROM cukb
         FOR ALL ENTRIES IN lt_cuob
       WHERE knnum =  lt_cuob-knnum
         AND adzhl =  lt_cuob-adzhl
         AND knnam =  l_knnam
         AND datuv <= p_datuv.
      ENDSELECT.
      IF sy-subrc NE 0.
        WRITE: p_atwri  TO l_knnam(3),
               pw_knnam  TO l_knnam+3.
        SELECT knnam INTO pw_knnam
          FROM cukb
           FOR ALL ENTRIES IN lt_cuob
         WHERE knnum =  lt_cuob-knnum
           AND adzhl =  lt_cuob-adzhl
           AND knnam =  l_knnam
           AND datuv <= p_datuv.
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
         AND datuv <= p_datuv.
      ENDSELECT.
      IF sy-subrc NE 0.
        CLEAR: pw_knnam.
      ENDIF.
  ENDCASE.
ENDFORM.                    " GET_COLLECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  GET_ALL_DEPENDENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_dependency.
* USING pw_9000 LIKE it_9000.
  DATA: lw_odseq LIKE sy-tabix.

  DATA: lt_9000_tmp LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  DATA: lt_cuob LIKE cuob OCCURS 0 WITH HEADER LINE.

  DATA: lt_cukb LIKE cukb OCCURS 0 WITH HEADER LINE.

  lt_9000_tmp[] = it_9000[].

  SORT it_9000 BY matnr idnrk.

  LOOP AT lt_9000_tmp WHERE knobj > 0.

    SELECT * INTO TABLE lt_cuob
      FROM cuob
     WHERE kntab =  'STPO'
       AND knobj =  lt_9000_tmp-knobj
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

    LOOP AT lt_cukb.
      CLEAR: it_9000.

      MOVE: sy-tabix TO lw_odseq.

      IF sy-tabix EQ 1.
        READ TABLE it_9000 WITH KEY lt_9000_tmp.
*                           BINARY SEARCH.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        MOVE: lt_cukb-knnam TO it_9000-knnam,
              lw_odseq      TO it_9000-odseq.

        MODIFY it_9000 INDEX sy-tabix.
      ELSE.
        MOVE: lt_9000_tmp   TO it_9000,
              lt_cukb-knnam TO it_9000-knnam,
              lw_odseq      TO it_9000-odseq.
        APPEND it_9000.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_ALL_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CHECK_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_others.
  CHECK p_od EQ 'X'.

  READ TABLE s_matnr INDEX 2.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m07 text-m08.
  ENDIF.

  IF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
    MESSAGE e000(zz) WITH text-m07 text-m08.
  ENDIF.

  IF mara-mtart NE 'FERT'.
    MESSAGE e000(zz) WITH text-m07 text-m08.
  ENDIF.

  CHECK NOT ( p_atwre IS INITIAL AND p_atwri IS INITIAL ).

  READ TABLE s_matnr INDEX 2.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m07 text-m08.
  ENDIF.

ENDFORM.                    " CHECK_OTHERS
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  IF p_od IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'GR1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    CLEAR: p_atwre, p_atwri.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'GR1'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SCREEN_MODIFY
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
*&      Form  HELP_REQUEST_P_ATWRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_p_atwre.
  DATA it_ksml   LIKE TABLE OF ksml  WITH HEADER LINE.
  DATA : BEGIN OF it_cawn OCCURS 0,
          atwrt   LIKE  cawn-atwrt,
          atwtb   LIKE  cawnt-atwtb.
  DATA : END OF it_cawn.

  DATA :  l_cuobj   LIKE  inob-cuobj,
          l_clint   LIKE  klah-clint.

  READ TABLE s_matnr INDEX 2.
  CHECK sy-subrc NE 0.

  CHECK NOT ( s_matnr-low NE ' ' AND s_matnr-high NE ' ' ).

  CLEAR: mara.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  CHECK mara-mtart EQ 'FERT'.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: 'S_MATNR-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_matnr-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE cuobj
         INTO l_cuobj
         FROM inob
         WHERE klart EQ '300'
           AND obtab EQ 'MARA'
           AND objek EQ s_matnr-low.

  SELECT SINGLE clint
         INTO l_clint
         FROM kssk
         WHERE objek EQ l_cuobj
           AND mafid EQ 'O'
           AND klart EQ '300'.

  SELECT *
         INTO TABLE it_ksml
         FROM ksml
         WHERE clint EQ l_clint.

  DATA l_tabix   LIKE sy-tabix.
  LOOP AT it_ksml.
    l_tabix = sy-tabix.
    SELECT SINGLE *
              FROM cabn
              WHERE atinn EQ it_ksml-imerk
                AND atnam EQ 'COLOREXT'.
    IF sy-subrc NE 0.
      DELETE it_ksml INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE it_ksml INDEX 1.
  SELECT a~atwrt
         b~atwtb
         INTO TABLE it_cawn
         FROM cawn AS a INNER JOIN cawnt AS b
                        ON  a~atinn EQ b~atinn
                        AND a~atzhl EQ b~atzhl
         WHERE a~atinn EQ it_ksml-omerk.
  SORT it_cawn.
  it_cawn-atwrt = 'No entry'.
  INSERT it_cawn INDEX 1.
  CLEAR: it_cawn.
  LOOP AT it_cawn.
    valuetab-value = it_cawn-atwrt.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = it_cawn-atwtb.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'CAWN'  'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM help_values_get.

  IF select_index > 0.
    READ TABLE it_cawn   INDEX select_index.
    PERFORM value_update USING:
            'X'   'P_ATWRE' it_cawn-atwrt 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_P_ATWRE
*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1975   text
*----------------------------------------------------------------------*
FORM value_read USING  p_name.
  dynpread-fieldname = p_name.
  APPEND dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpread
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            invalid_parameter    = 7
            undefind_error       = 8
            double_conversion    = 9
            OTHERS               = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " VALUE_READ
*&---------------------------------------------------------------------*
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2222   text
*      -->P_2223   text
*      -->P_2224   text
*----------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  APPEND fields.      CLEAR fields.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_values_get.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display                   = ' '
       IMPORTING
            index                     = select_index
       TABLES
            fields                    = fields
            select_values             = select_values
            valuetab                  = valuetab
       EXCEPTIONS
            field_not_in_ddic         = 1
            more_then_one_selectfield = 2
            no_selectfield            = 3
            OTHERS                    = 4.
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2256   text
*      -->P_2257   text
*      -->P_IT_CAWN_ATWRT  text
*      -->P_0      text
*----------------------------------------------------------------------*
FORM value_update USING  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
  CLEAR dynpfields.
  dynpfields-fieldname  = p_fieldname.
  dynpfields-fieldvalue = p_fieldvalue.
  IF p_stepl > 0.
    dynpfields-stepl = p_stepl.
  ENDIF.
  APPEND dynpfields.      CLEAR dynpfields.

  IF p_process EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              dyname               = sy-cprog
              dynumb               = sy-dynnr
         TABLES
              dynpfields           = dynpfields
         EXCEPTIONS
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              OTHERS               = 8.
    REFRESH dynpfields.
  ENDIF.
ENDFORM.                    " VALUE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_P_ATWRI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_p_atwri.
  DATA it_ksml   LIKE TABLE OF ksml  WITH HEADER LINE.
  DATA : BEGIN OF it_cawn OCCURS 0,
          atwrt   LIKE  cawn-atwrt,
          atwtb   LIKE  cawnt-atwtb.
  DATA : END OF it_cawn.

  DATA :  l_cuobj   LIKE  inob-cuobj,
          l_clint   LIKE  klah-clint.

  READ TABLE s_matnr INDEX 2.
  CHECK sy-subrc NE 0.

  CHECK NOT ( s_matnr-low NE ' ' AND s_matnr-high NE ' ' ).

  CLEAR: mara.


  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  CHECK mara-mtart EQ 'FERT'.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.
  CLEAR : it_scrfield.
  it_scrfield-fieldname  = 'S_MATNR-LOW'.   "Storge type
  it_scrfield-fieldvalue = S_MATNR-LOW.
  APPEND it_scrfield.
  CLEAR : it_scrfield.
  it_scrfield-fieldname  = 'P_STLAN'.   "Storge type
  it_scrfield-fieldvalue =  P_STLAN.
  APPEND it_scrfield.
  CLEAR IT_SCRFIELD.

  l_dyname =  sy-repid.
  l_dynumb = sy-dynnr.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = l_dyname
            DYNUMB     = l_dynumb
       TABLES
            DYNPFIELDS = it_scrfield.
*

  PERFORM value_read USING: 'S_MATNR-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_matnr-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE cuobj
         INTO l_cuobj
         FROM inob
         WHERE klart EQ '300'
           AND obtab EQ 'MARA'
           AND objek EQ s_matnr-low.

  SELECT SINGLE clint
         INTO l_clint
         FROM kssk
         WHERE objek EQ l_cuobj
           AND mafid EQ 'O'
           AND klart EQ '300'.

  SELECT *
         INTO TABLE it_ksml
         FROM ksml
         WHERE clint EQ l_clint.

  DATA l_tabix   LIKE sy-tabix.
  LOOP AT it_ksml.
    l_tabix = sy-tabix.
    SELECT SINGLE *
              FROM cabn
              WHERE atinn EQ it_ksml-imerk
                AND atnam EQ 'COLORINT'.
    IF sy-subrc NE 0.
      DELETE it_ksml INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE it_ksml INDEX 1.
  SELECT a~atwrt
         b~atwtb
         INTO TABLE it_cawn
         FROM cawn AS a INNER JOIN cawnt AS b
                        ON  a~atinn EQ b~atinn
                        AND a~atzhl EQ b~atzhl
         WHERE a~atinn EQ it_ksml-omerk.
  SORT it_cawn.
  it_cawn-atwrt = 'No entry'.
  INSERT it_cawn INDEX 1.
  CLEAR: it_cawn.
  LOOP AT it_cawn.
    valuetab-value = it_cawn-atwrt.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = it_cawn-atwtb.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'CAWN' 'ATWRT'  'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM help_values_get.

  IF select_index > 0.
    READ TABLE it_cawn   INDEX select_index.
    PERFORM value_update USING:
            'X'   'P_ATWRI' it_cawn-atwrt 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_P_ATWRI
*&---------------------------------------------------------------------*
*&      Form  set_bom_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_bom_header.
*  SELECT a~matnr b~werks c~stlan c~stlal
*    INTO CORRESPONDING FIELDS OF TABLE it_bom
*    FROM mara AS a INNER JOIN marc AS b
*                      ON a~matnr EQ b~matnr
*                     AND b~lvorm EQ space
*                   INNER JOIN mast AS c
*                      ON b~matnr EQ c~matnr
*                     AND b~werks EQ c~werks
*                     AND c~stlal EQ c_stlal
*                   INNER JOIN stko AS d
*                      ON c~stlnr EQ d~stlnr
*                     AND c~stlal EQ d~stlal
*                     AND d~stlty EQ 'M'
*                     AND d~lkenz EQ space
*                     AND d~loekz EQ space
*                     AND d~stlst EQ '01'
**                     AND d~datuv <= p_datuv
*   WHERE a~mtart IN ('ROH','ROH1','HALB')
*     AND c~stlan IN ('1','2').
  SELECT a~matnr b~werks c~stlan c~stlal
      INTO CORRESPONDING FIELDS OF TABLE it_bom
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
                       AND d~datuv <= p_datuv
     WHERE a~mtart IN ('ROH','ROH1','HALB')
       AND c~stlan IN ('1','2')
       OR b~beskz EQ 'E' "haseeb
       OR b~sobsl eq space.

  SORT it_bom BY matnr werks stlan stlal.
ENDFORM.                    " set_bom_header
*&---------------------------------------------------------------------*
*&      Form  help_request_s_stlal
*&---------------------------------------------------------------------*
*       Help request F4 for bom usage. by Haseeb.
* 11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_s_stlal.
  DATA : w_sfield   LIKE  help_info-fieldname,
        it_fields  LIKE  help_value OCCURS 1 WITH HEADER LINE,
        w_svalue   LIKE  help_info-fldvalue,
        w_idx    LIKE  sy-tabix,
        zmatnr like mara-matnr.

  CLEAR : it_scrfield, it_scrfield[].
  it_scrfield-fieldname  = 'S_MATNR-LOW'.   "Storge type
  it_scrfield-fieldvalue = S_MATNR-LOW.
  APPEND it_scrfield.
  CLEAR : it_scrfield.
  it_scrfield-fieldname  = 'P_STLAN'.   "Storge type
  it_scrfield-fieldvalue =  P_STLAN.
  APPEND it_scrfield.
  CLEAR : it_scrfield.

  l_dyname =  sy-repid.
  l_dynumb = sy-dynnr.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME     = l_dyname
            DYNUMB     = l_dynumb
       TABLES
            DYNPFIELDS = it_scrfield.
*
  Loop at it_scrfield.
    if it_scrfield-fieldname = 'S_MATNR-LOW'.
      zmatnr = it_scrfield-fieldvalue.
    else.
      p_stlan = it_scrfield-fieldvalue.
    endif.
  endloop.
*  SELECT  STLAL  STLAN INTO TABLE IT_ALTBOM FROM MAST WHERE
*             MATNR EQ S_MATNR-LOW AND STLAN EQ p_stlan.
  SELECT  STLAL  STLAN INTO TABLE IT_ALTBOM FROM MAST WHERE
             MATNR EQ zmatnr AND STLAN EQ  p_stlan.
  IF SY-SUBRC EQ 0.
    CLEAR : it_fields, it_fields[].
    it_fields-tabname   = 'MAST'.
    it_fields-fieldname = 'STLAL'.
    it_fields-selectflag = ' '.

    APPEND it_fields.
    CLEAR it_fields.
    it_fields-tabname   = 'MAST'.
    it_fields-fieldname = 'STLAN'.
    it_fields-selectflag = ' '.
    APPEND it_fields.
    CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
         EXPORTING
              selectfield  = w_sfield
         IMPORTING
              ind          = w_idx
              select_value = w_svalue
         TABLES
              fields       = it_fields
              full_table   = it_ALTBOM.
  ENDIF.
  CHECK w_idx > 0 .
  CLEAR it_ALTBOM.
  READ TABLE it_ALTBOM INDEX w_idx.
  PERFORM fill_storage_type.


ENDFORM.                    " help_request_s_stlal
*&---------------------------------------------------------------------*
*&      Form  fill_storage_type
*&---------------------------------------------------------------------*
*       11-28-2006 Haseeb Mohammad  UD1K923143 , HDTicket #6BGC116431
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_storage_type.
  CLEAR : it_scrfield, it_scrfield[].
  it_scrfield-fieldname  = 'S_STLAL'.   "Storge type
  it_scrfield-fieldvalue = it_ALTBOM-STLAL.
  APPEND it_scrfield.
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = it_scrfield
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
  CHECK sy-subrc = 0.
  LOOP AT it_scrfield.
    CASE it_scrfield-fieldname.
      WHEN 'S_STLAL'.
        S_STLAL = it_scrfield-fieldvalue.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " fill_storage_type
