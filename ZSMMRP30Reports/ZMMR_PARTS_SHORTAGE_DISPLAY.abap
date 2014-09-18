************************************************************************
* Program Name      : ZMMR_PARTS_SHORTAGE_DISPLAY
* Author            : Furong Wang
* Creation Date     : 04/2010
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zmmr_parts_shortage_display   MESSAGE-ID zmmm.

TYPE-POOLS: slis .
TABLES: mara, marc, pvbe, lfa1, ztpp_inputplan_h.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------

DATA: BEGIN OF it_short OCCURS 0.
        INCLUDE STRUCTURE zmms_short.
DATA: lfimg    LIKE lips-lfimg.    "CH.Jeong 10/21/2013
DATA: lights,
      color    TYPE slis_t_specialcol_alv,
      ven_name LIKE lfa1-name1,
      END OF it_short.

DATA: BEGIN OF it_hour OCCURS 0,
     seq(2),
     date LIKE sy-datum,
     time LIKE sy-uzeit,
     hrs(2) TYPE n,
     END OF  it_hour.

* CH.Jeong on 10/21/2013 : ASN Qty (Advanced Shipping Notification)
*  DATA: lt_temp LIKE TABLE OF ztmm_hour_short WITH HEADER LINE.
DATA: BEGIN OF lt_temp OCCURS 0.
INCLUDE        STRUCTURE ztmm_hour_short.
DATA: lfimg    LIKE      lips-lfimg.  "CH.Jeong 10/21/2013
DATA: END OF lt_temp.

*--------------------------------------------------------------------*
*   CLASS Definitiion                                               *
*--------------------------------------------------------------------*
CLASS lcl_alv_grid DEFINITION  INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.
    CLASS-DATA f_alv  TYPE c.

*    METHODS : set_fixed_column,
    METHODS : set_optimizer.

ENDCLASS.                    "lcl_alv_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION.

*  METHOD set_fixed_column.
*    CALL METHOD me->set_fixed_cols
*      EXPORTING
*        cols = 5.
*  ENDMETHOD.                    "SET_FIXED_COLUMN

  METHOD set_optimizer.
    CALL METHOD me->optimize_all_cols
      EXPORTING
        include_header = 1.
  ENDMETHOD.                    "SET_OPTIMIZER

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION

* End on 10/21/2013

DATA: w_updating(50),
      w_new_program(1).

** ALV
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      it_exclude TYPE ui_functions.

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_800 TYPE scrfname VALUE 'ALV_CONTAINER_800',
* CH.Jeong on 10/21/2013
      alv_grid_800          TYPE REF TO cl_gui_alv_grid,
**      alv_grid_800          TYPE REF TO lcl_alv_grid,
* End on 10/21/2013
*      grid_container_800    TYPE REF TO cl_gui_custom_container.
      grid_dock_container_800 TYPE REF TO cl_gui_docking_container.

DATA: g_lights_fieldname  TYPE slis_fieldname VALUE 'LIGHTS'.

DATA: ok_code LIKE sy-ucomm,
      w_code LIKE sy-ucomm,
      w_old_code LIKE sy-ucomm,
      w_cnt   TYPE   i,
      w_base_date LIKE sy-datum,
      w_base_time LIKE sy-uzeit,
      w_zrun_date LIKE sy-datum,
      w_zrun_time LIKE sy-uzeit,
      w_input_date LIKE sy-datum,
      w_input_time LIKE sy-uzeit,
      l_uph TYPE zmeng13_2, "LIKE IT_SHORT-RP01,
      w_repid LIKE sy-repid,
      w_dynnr LIKE sy-dynnr.

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_gui_timer DEFINITION INHERITING FROM cl_gui_control.

  PUBLIC SECTION.

    CONSTANTS:  eventid_finished TYPE i VALUE 1 .

    CLASS-DATA: interval TYPE i VALUE '0'.

    EVENTS:     finished .

    METHODS:
*             show_alv,
             cancel
                  EXCEPTIONS
                     error,
             constructor
                 IMPORTING
                     lifetime TYPE i OPTIONAL
                     value(shellstyle) TYPE i OPTIONAL
                     value(parent) TYPE REF TO cl_gui_container OPTIONAL
                 EXCEPTIONS
                     error,
             run
                 EXCEPTIONS
                     error,
             dispatch REDEFINITION.


ENDCLASS.                    "lcl_gui_timer DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
                on_finished
                       FOR EVENT finished OF lcl_gui_timer.

ENDCLASS.                    "lcl_event_handler DEFINITION


DATA: timer_container TYPE REF TO cl_gui_custom_container,
      gui_timer TYPE REF TO lcl_gui_timer,
      event_handler TYPE REF TO lcl_event_handler,
      timeout_interval TYPE i.
*      L_ALV TYPE REF TO CL_GUI_ALV_GRID,
*      FIRST_CALL(1) TYPE C,
**      ok_code     TYPE syucomm,
*      L_IS_STABLE TYPE LVC_S_STBL.

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_finished.

* Start Timer again

    gui_timer->interval = timeout_interval.
    CALL METHOD gui_timer->run.

* cause PAI
    CALL METHOD cl_gui_cfw=>set_new_ok_code
      EXPORTING
        new_code = 'REFR'.

  ENDMETHOD.                    "on_finished

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_gui_timer IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_gui_timer IMPLEMENTATION.

  METHOD constructor.

    TYPE-POOLS: sfes.

    DATA clsid(80).
    DATA event_tab TYPE cntl_simple_events.
    DATA event_tab_line TYPE cntl_simple_event.

    IF clsid IS INITIAL.
      DATA: return,
            guitype TYPE i.

      guitype = 0.
      CALL FUNCTION 'GUI_HAS_OBJECTS'
        EXPORTING
          object_model = sfes_obj_activex
        IMPORTING
          return       = return
        EXCEPTIONS
          OTHERS       = 1.
      IF sy-subrc NE 0.
        RAISE error.
      ENDIF.

      IF return = 'X'.
        guitype = 1.
      ENDIF.
      IF guitype = 0.
        CALL FUNCTION 'GUI_HAS_OBJECTS'
          EXPORTING
            object_model = sfes_obj_javabeans
          IMPORTING
            return       = return
          EXCEPTIONS
            OTHERS       = 1.
        IF sy-subrc NE 0.
          RAISE error.
        ENDIF.

        IF return = 'X'.
          guitype = 2.
        ENDIF.
      ENDIF.

      CASE guitype.
        WHEN 1.
          clsid = 'Sapgui.InfoCtrl.1'.
        WHEN 2.
          clsid = 'com.sap.components.controls.sapImage.SapImage'.
      ENDCASE.
    ENDIF.

    CALL METHOD super->constructor
      EXPORTING
        clsid      = clsid
        shellstyle = 0
        parent     = cl_gui_container=>default_screen
        autoalign  = space
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

    CALL METHOD cl_gui_cfw=>subscribe
      EXPORTING
        shellid = h_control-shellid
        ref     = me
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.

* Register the events
    event_tab_line-eventid = lcl_gui_timer=>eventid_finished.
    APPEND event_tab_line TO event_tab.

    CALL METHOD set_registered_events
      EXPORTING
        events = event_tab.

  ENDMETHOD.                    "constructor

  METHOD cancel.

    CALL METHOD call_method
      EXPORTING
        method     = 'SetTimer'
        p_count    = 1
        p1         = -1
        queue_only = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.


  ENDMETHOD.                    "cancel

  METHOD run.

    CALL METHOD call_method
      EXPORTING
        method     = 'SetTimer'
        p_count    = 1
        p1         = interval
        queue_only = 'X'
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc NE 0.
      RAISE error.
    ENDIF.


  ENDMETHOD.                    "run

  METHOD dispatch .

    CASE eventid.
      WHEN eventid_finished.
        RAISE EVENT finished.
    ENDCASE.

    CLEAR timer_container.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETERS:   P_WERKS   TYPE MARC-WERKS MEMORY ID WRK OBLIGATORY
*DEFAULT
* 'P001'.
SELECT-OPTIONS :
    s_matnr   FOR mara-matnr,
    s_mtart   FOR mara-mtart,
    s_matkl   FOR mara-matkl,
    s_dispo   FOR marc-dispo,                  "MRP controller
    s_lgpro   FOR marc-lgpro,
    s_prvbe   FOR pvbe-prvbe,
    s_sortf   FOR ztpp_inputplan_h-status,
    s_lifnr   FOR lfa1-lifnr MODIF ID noi.
*    S_MODEL   FOR ZTPP_INPUTPLAN_H-MODL,
*    S_BODYNO   FOR ZTPP_INPUTPLAN_H-BODY_SER.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 25(5) text-all FOR FIELD pc_all .
SELECTION-SCREEN POSITION 18.
PARAMETERS  pc_all  RADIOBUTTON GROUP icon DEFAULT 'X'.
SELECTION-SCREEN COMMENT 32(5) text-red FOR FIELD pc_red .
PARAMETERS  pc_red  RADIOBUTTON GROUP icon .
SELECTION-SCREEN COMMENT 46(5) text-yel FOR FIELD pc_yel .
PARAMETERS  pc_yel  RADIOBUTTON GROUP icon .
SELECTION-SCREEN COMMENT 60(5) text-gre FOR FIELD pc_gre .
PARAMETERS  pc_gre  RADIOBUTTON GROUP icon .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) text-011  .
SELECTION-SCREEN COMMENT 31(2) text-012 FOR FIELD p_red1 .
PARAMETERS :  p_red1 TYPE mdkp-berw1 DEFAULT '10'.
SELECTION-SCREEN COMMENT 45(2) text-012 FOR FIELD p_yel1 .
PARAMETERS :  p_yel1 TYPE mdkp-berw1 DEFAULT '20'.
SELECTION-SCREEN COMMENT 59(2) text-012 FOR FIELD p_gre1 .
PARAMETERS :  p_gre1 TYPE mdkp-berw1 DEFAULT '999'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.


SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECTION-SCREEN : BEGIN OF LINE.

PARAMETERS : p_topb AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(10) text-top FOR FIELD p_topb.
PARAMETERS: p_topv TYPE  i DEFAULT '20'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS : p_lesb AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(10) text-les FOR FIELD p_lesb.
PARAMETERS: p_lesv TYPE  i DEFAULT '20'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE.
PARAMETERS : p_grtb AS CHECKBOX.
SELECTION-SCREEN COMMENT 8(10) text-grt FOR FIELD p_grtb.
PARAMETERS: p_grtv TYPE  i DEFAULT '4'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK block6 WITH FRAME TITLE text-006.
PARAMETERS :
    p_intrv TYPE i DEFAULT '10'.
SELECTION-SCREEN END OF BLOCK block6.

SELECTION-SCREEN BEGIN OF BLOCK block5 WITH FRAME TITLE text-006.
PARAMETERS :
    p_cogi(1).
SELECTION-SCREEN END OF BLOCK block5.



*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM init_data.
*----------------------------------------------------------------------

* Set F4 values for MRP Controller
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_dispo-low.
  DATA : BEGIN OF value_tab OCCURS 0,
         dispo  LIKE marc-dispo,
         desc LIKE t024d-dsnam,
         END OF value_tab.

  SELECT DISTINCT a~dispo dsnam
     INTO TABLE value_tab
     FROM marc AS a
     INNER JOIN t024d AS b
     ON a~dispo = b~dispo
     WHERE a~werks = 'P001'
     AND a~dispo <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DISPO'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'DISPO'
      window_title    = 'MRP Controller'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_dispo-high.
  DATA : BEGIN OF value_tab OCCURS 0,
         dispo  LIKE marc-dispo,
         desc LIKE t024d-dsnam,
         END OF value_tab.

  SELECT DISTINCT a~dispo dsnam
     INTO TABLE value_tab
     FROM marc AS a
     INNER JOIN t024d AS b
     ON a~dispo = b~dispo
     WHERE a~werks = 'P001'
     AND a~dispo <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DISPO'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'DISPO'
      window_title    = 'MRP Controller'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.

* Set F4 values for Prod Storage Location
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgpro-low.
  DATA : BEGIN OF value_tab OCCURS 0,
         lgpro  LIKE marc-lgpro,
         desc LIKE t001l-lgobe,
         END OF value_tab.

  SELECT DISTINCT a~lgpro lgobe INTO TABLE value_tab
     FROM marc AS a
     INNER JOIN t001l AS b
     ON a~lgpro = b~lgort
     WHERE a~werks = 'P001'
       AND lgpro <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LGPRO'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'LGPRO'
      window_title    = 'Prod Storage Loca'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgpro-high.
  DATA : BEGIN OF value_tab OCCURS 0,
         lgpro  LIKE marc-lgpro,
         desc LIKE t001l-lgobe,
         END OF value_tab.

  SELECT DISTINCT a~lgpro lgobe INTO TABLE value_tab
     FROM marc AS a
     INNER JOIN t001l AS b
     ON a~lgpro = b~lgort
     WHERE a~werks = 'P001'
       AND lgpro <> ' '.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LGPRO'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
      dynprofield     = 'LGPRO'
      window_title    = 'Prod Storage Loca'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab
    EXCEPTIONS
      parameter_error = 1.

* Set F4 values for Supply Area
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_PRVBE-LOW.
*  DATA : BEGIN OF VALUE_TAB OCCURS 0,
*         PRVBE  LIKE MARC-VSPVB,
*         DESC LIKE PVBE-PVBTX,
*         END OF VALUE_TAB.
*
*  SELECT DISTINCT A~VSPVB PVBTX
*     INTO TABLE VALUE_TAB
*     FROM MARC AS A
*     INNER JOIN PVBE AS B
*     ON A~VSPVB = B~PRVBE
*     WHERE A~WERKS = 'P001'
*     and A~PRVBE <> ' '.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*       EXPORTING
*            RETFIELD        = 'PRVBE'
*            DYNPPROG        = w_REPID
*            DYNPNR          = w_DYNNR
*            DYNPROFIELD     = 'PRVBE'
*            WINDOW_TITLE    = 'Supply Area'
*            VALUE_ORG       = 'S'
*       TABLES
*            VALUE_TAB       = VALUE_TAB
*       EXCEPTIONS
*            PARAMETER_ERROR = 1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------

  PERFORM get_data.

  CALL SCREEN 0800.

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
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
      MESSAGE e000(zz) WITH 'Check field catalog' p_gubun p_field.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname .
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*&---------------------------------------------------------------------*
*&      Form  GET_VIN_SCHUDLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

** CH.Jeong on 10/21/2013 : go to the global variables
**  DATA: lt_temp LIKE TABLE OF ztmm_hour_short WITH HEADER LINE.
** End on 10/21/2013

  DATA:  l_index LIKE sy-tabid,
         l_cn(2) TYPE n,
         l_cn_c(10),
         l_dec(3),
         l_qty_c(13),
         l_text(20),
         l_total_stock LIKE ztmm_hour_short-ln_ohqty.

  DATA: l_input14(14),
        l_run14(14),
        l_date_c(8),
        l_time_c(6).

  REFRESH: it_short.
  CLEAR: w_updating.

  timeout_interval = p_intrv * 60.

  CLEAR   : lt_temp.
  REFRESH : lt_temp[].

  SELECT * INTO TABLE lt_temp
     FROM ztmm_hour_short
     WHERE matnr IN s_matnr
       AND mtart IN s_mtart
       AND matkl IN s_matkl
       AND dispo IN s_dispo
       AND lgpro IN s_lgpro
       AND prvbe IN s_prvbe
       AND rp IN s_sortf
       AND lifnr IN s_lifnr.

  READ TABLE lt_temp INDEX 1.
  w_base_date = lt_temp-zsdat.
  w_base_time = lt_temp-zstim.

  w_zrun_date = lt_temp-zedat.
  w_zrun_time = lt_temp-zetim.

** Furong on 06/08/12
* LOOP AT LT_TEMP.
*    L_TOTAL_STOCK = L_TOTAL_STOCK + LT_TEMP-LN_OHQTY.
*   ENDLOOP.

  IF  p_cogi IS INITIAL.
    LOOP AT lt_temp.
      l_total_stock = l_total_stock + lt_temp-ln_ohqty.
    ENDLOOP.
  ELSE.
    LOOP AT lt_temp.
      l_index = sy-tabix.
      lt_temp-ln_ohqty = lt_temp-ohqty_cogi.
      lt_temp-hr_oh = lt_temp-hr_qty_cogi.

      l_total_stock = l_total_stock + lt_temp-ln_ohqty.
      MODIFY lt_temp INDEX l_index.
    ENDLOOP.
  ENDIF.
** End on 06/08/12
  IF l_total_stock IS INITIAL.
    w_updating = '- Updating Stock... Please Open Again'.
  ENDIF.

*  SELECT ZEDAT ZETIM INTO (W_INPUT_DATE, W_INPUT_TIME) UP TO 1 ROWS
*     FROM ZTPP_INPUTPLAN_H.
*  ENDSELECT.
*
*  L_DATE_C = W_INPUT_DATE.
*  L_TIME_C = W_INPUT_TIME.
*  CONCATENATE L_DATE_C L_TIME_C INTO L_INPUT14.
*
*  L_DATE_C = W_ZRUN_DATE.
*  L_TIME_C = W_ZRUN_TIME.
*
*  CONCATENATE L_DATE_C L_TIME_C INTO L_RUN14.
*
*  IF L_RUN14 < L_INPUT14.
*    W_UPDATING = '- Updating Stock... Please Open Again'.
*  ENDIF.

* CH.Jeong on 10/21/2013 : ASN Qty (Advanced Shipping Notification)
  PERFORM get_asn_qty  TABLES lt_temp.
* End on 10/21/2013


  LOOP AT lt_temp.

    CLEAR: it_short, l_cn.

    MOVE-CORRESPONDING lt_temp TO it_short.
    SELECT SINGLE name1 INTO it_short-ven_name
      FROM lfa1
      WHERE lifnr = it_short-lifnr.

    IF it_short-hr_oh <= 0.
      it_short-lights = '1'.  "red
      l_qty_c = it_short-hr_oh.
      SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
      l_cn = l_cn_c + 1.
      WHILE l_cn <= 40.
        CONCATENATE 'RP' l_cn INTO l_text.
        PERFORM set_cell_color USING '6'
                                     '1'
                                 l_text
                              CHANGING it_short-color[].
        l_cn = l_cn + 1.
        CLEAR: l_text.

      ENDWHILE.
      APPEND it_short.
      CONTINUE.
    ENDIF.

    CASE 'X'.
      WHEN pc_all.
        IF it_short-hr_oh <= p_red1.
          it_short-lights = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '6'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.
          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.
          APPEND it_short.
        ENDIF.
        IF it_short-hr_oh <=  p_yel1 AND it_short-hr_oh > p_red1.
          it_short-lights = '2'.  "yellow
*          PERFORM SET_CELL_COLOR USING    '3'
*                                            '1'
*                                        'HR_OH'
*                                     CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.
          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.

          APPEND it_short.
        ENDIF.

        IF it_short-hr_oh <= p_gre1 AND it_short-hr_oh > p_yel1 .
          it_short-lights = '3'.  "green
*          PERFORM SET_CELL_COLOR USING    '5'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.
          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.

          APPEND it_short.
        ENDIF.

      WHEN pc_red.
        IF it_short-hr_oh <= p_red1.
          it_short-lights = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '6'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.

          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.

          APPEND it_short.
        ENDIF.
      WHEN pc_yel.
        IF it_short-hr_oh <= p_yel1.
          it_short-lights = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '3'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.

          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.

          APPEND it_short.
        ENDIF.

      WHEN pc_gre.
        IF it_short-hr_oh <= p_gre1.
          it_short-lights = '1'.  "red
*          PERFORM SET_CELL_COLOR USING    '5'
*                                          '1'
*                                      'HR_OH'
*                                   CHANGING IT_SHORT-COLOR[].
*          L_CN = IT_SHORT-HR_OH + 1.
          l_qty_c = it_short-hr_oh.
          SPLIT l_qty_c AT '.' INTO l_cn_c l_dec.
          l_cn = l_cn_c + 1.

          WHILE l_cn <= 40.
            CONCATENATE 'RP' l_cn INTO l_text.
            PERFORM set_cell_color USING '6'
                                         '1'
                                     l_text
                                  CHANGING it_short-color[].
            l_cn = l_cn + 1.
            CLEAR: l_text.

          ENDWHILE.

          APPEND it_short.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

  ENDLOOP.

** Changed by Furong on 07/20/10
*  SORT IT_SHORT BY HR_OH MATNR.
** End of change.

  IF p_topb = 'X' AND p_topv > 0.
    l_index = p_topv + 1.
    DELETE it_short FROM l_index.
  ENDIF.

  IF p_lesb = 'X' AND p_grtb = 'X'.
    DELETE it_short WHERE hr_oh >= p_lesv
                      OR hr_oh <= p_grtv.
  ELSE.
    IF  p_lesb = 'X'.
      DELETE it_short WHERE hr_oh >= p_lesv.
    ENDIF.
    IF p_grtb = 'X' .
      DELETE it_short WHERE hr_oh <= p_grtv.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_VIN_SCHUDLE

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_0800 OUTPUT.

  IF grid_dock_container_800 IS INITIAL.
    PERFORM create_container_object_800.
    PERFORM set_attributes_alv_grid_800.
*    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM build_field_catalog_800 USING 'IT_SHORT'.
    PERFORM assign_itab_to_alv_800.
  ELSE.
    CALL METHOD alv_grid_800->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_object_800.

** Change on 08/27/13
  w_repid = sy-repid.
  w_dynnr = sy-dynnr.
  CREATE OBJECT grid_dock_container_800
    EXPORTING
      repid     = w_repid
      dynnr     = w_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.
** End on 08/27/13

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT alv_grid_800
    EXPORTING
*  I_PARENT      = GRID_CONTAINER_800
      i_parent      = grid_dock_container_800
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_800
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_800.
  DATA: l_date_c(10),
        l_time(8),
        l_uph_c(6).


  CLEAR : wa_is_layout, wa_variant.
  CONCATENATE w_base_date+4(2) '/' w_base_date+6(2) '/' w_base_date+0(4)
                                                       INTO l_date_c.
  CONCATENATE w_base_time+0(2) ':' w_base_time+2(2) ':' w_base_time+4(2)
                                                       INTO l_time.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  wa_is_layout-ctab_fname  = 'COLOR'.
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-excp_fname = 'LIGHTS'.

  CONCATENATE 'As of' l_date_c l_time INTO wa_is_layout-grid_title
  SEPARATED BY space.

  CONCATENATE wa_is_layout-grid_title w_updating INTO
  wa_is_layout-grid_title
  SEPARATED BY space.

*  L_UPH_C =  L_UPH.
*  WA_IS_LAYOUT-GRID_TITLE+50(5) = 'UPH :'.
*  MOVE: L_UPH_C TO WA_IS_LAYOUT-GRID_TITLE+56(10).

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

  wa_is_layout-zebra             = 'X'.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_800
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3194   text
*----------------------------------------------------------------------*
FORM build_field_catalog_800 USING p_itab.
  DATA: lw_itab TYPE slis_tabname.

  DATA: l_cn(2) TYPE n,
  l_rp(30),
  l_hr(10).


  CLEAR   : it_fieldcat,   it_fieldname.
  REFRESH : it_fieldcat[], it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.


  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Part',
                                  'E' 'OUTPUTLEN'   '10',

                                 'S' 'MAKTX'    ' ',
                                 ' ' 'KEY'         'X',
                                 ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'HR_OH'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DECIMALS_O'  '1',
                                  ' ' 'COLTEXT'     'On Hand',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'VEN_NAME'    ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'RP'    ' ',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '3',

                                 'S' 'PRVBE'    ' ',
                                 ' ' 'COLTEXT'     'Supp Area',
                                  'E' 'OUTPUTLEN'   '2',

                                 'S' 'DSNAM'    ' ',
                                  ' ' 'COLTEXT'     'MRP Controller',
                                  'E' 'OUTPUTLEN'   '10',

                                'S' 'MATKL'    ' ',
                                  ' ' 'COLTEXT'     'Matl Grp',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'MEINS'    ' ',
                                  ' ' 'COLTEXT'     'BUn',
                                  'E' 'OUTPUTLEN'   '2',

*                               CH.Jeong on 10/21/2013 , ASN Qty
                               'S'  'LFIMG'       ' ',
                               ' '  'COLTEXT'     'ASN Qty',
                               ' '  'DECIMALS_O'  '0',
                               'E'  'OUTPUTLEN'   '5',
*                               End on 10/21/2013

                                 'S' 'LN_OHQTY'    ' ',
                                 ' ' 'COLTEXT'     'Stock',
                                 ' ' 'DECIMALS_O'  '0',
                                 'E' 'OUTPUTLEN'   '8'.
*
*                                 'S' 'WH_OHQTY'    ' ',
*                                  ' ' 'COLTEXT'     'WH Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13',
*
*                                 'S' 'ADJ_QTY'    ' ',
*                                  ' ' 'COLTEXT'     'Adj Qty',
*                                 ' ' 'DECIMALS_O'  '0',
*                                 'E' 'OUTPUTLEN'   '13'.

  l_cn = '00'.
  DO 40 TIMES.
    l_cn = l_cn + 1.

    CONCATENATE 'RP' l_cn INTO l_rp.
    CONCATENATE  l_cn 'Hr' INTO l_hr SEPARATED BY space.

    PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                   'S' l_rp        ' ',
                                   ' ' 'COLTEXT'     l_hr,
                                   ' ' 'DECIMALS_O'  '0',
                                   'E' 'OUTPUTLEN'   '5'.
    CLEAR: l_rp.
  ENDDO.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'TOTAL'      ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  ' ' 'DECIMALS_O'  '0',
                                  'E' 'OUTPUTLEN'   '7'.

ENDFORM.                    " BUILD_FIELD_CATALOG_800

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_800.
  CALL METHOD alv_grid_800->set_table_for_first_display
    EXPORTING
      is_layout       = wa_is_layout
      i_save          = wa_save
      is_variant      = wa_variant
    CHANGING
      it_fieldcatalog = it_fieldcat[]
      it_outtab       = it_short[]
      it_sort         = it_sort[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV_800
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  w_code = ok_code.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFR'.
      SUBMIT zmmr_parts_shortage_display
       WITH s_matnr IN s_matnr
       WITH s_mtart IN s_mtart
       WITH s_matkl IN s_matkl
       WITH s_dispo IN s_dispo
       WITH s_lgpro IN s_lgpro
       WITH s_prvbe IN s_prvbe
       WITH s_sortf IN s_sortf
       WITH s_lifnr IN s_lifnr
       WITH pc_all = pc_all
       WITH pc_red = pc_red
       WITH pc_yel = pc_yel
       WITH pc_gre = pc_gre
       WITH p_red1 = p_red1
       WITH p_yel1 = p_yel1
       WITH p_gre1 = p_gre1
       WITH p_topb = p_topb
       WITH p_topv = p_topv
       WITH p_lesb = p_lesb
       WITH p_lesv = p_lesv
       WITH p_grtb = p_grtb
       WITH p_grtv = p_grtv
       WITH p_intrv = p_intrv
       with p_cogi  = p_cogi.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  IF p_cogi IS INITIAL.
    SET TITLEBAR 'ST200'.
  ELSE.
    SET TITLEBAR 'ST201'.
  ENDIF.
ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  set_cell_color
*&---------------------------------------------------------------------*
*       Set Cell Color
*----------------------------------------------------------------------*
FORM set_cell_color  USING    u_col
                              u_int
                              u_field
                     CHANGING color_tab
                              TYPE slis_t_specialcol_alv.
*----------------------------------------------------------------------*
* No  Colour
*  0  COL_BACKGROUND
*  1  COL_HEADING
*  2  COL_NORMAL
*  3  COL_TOTAL
*  4  COL_KEY
*  5  COL_POSITIVE
*  6  COL_NEGATIVE
*  7  COL_GROUP
*----------------------------------------------------------------------*
  DATA : l_color TYPE slis_specialcol_alv.
  l_color-fieldname = u_field.
  l_color-color-col = u_col.
  l_color-color-int = u_int.
  APPEND l_color TO color_tab.
ENDFORM.                    " set_cell_color

*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  w_repid = sy-repid.
  w_dynnr = sy-dynnr.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Module  SET_TIMER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_timer OUTPUT.
  IF timer_container IS INITIAL.
    CREATE OBJECT:

       timer_container
             EXPORTING
                  container_name = 'TI_CONTAINER',
       gui_timer
             EXPORTING
                  parent = timer_container.

    SET HANDLER event_handler->on_finished FOR gui_timer.

    gui_timer->interval = timeout_interval.
    CALL METHOD gui_timer->run.
*  if w_new_program IS INITIAL.
*     w_new_program  = 'X'.
*  ELSE.
*       SUBMIT ZMMR_PARTS_SHORTAGE_DISPLAY
*       WITH S_MATNR IN S_MATNR
*       WITH S_MTART IN S_MTART
*       WITH S_MATKL IN S_MATKL
*       WITH S_DISPO IN S_DISPO
*       WITH S_LGPRO IN S_LGPRO
*       WITH S_PRVBE IN S_PRVBE
*       WITH S_SORTF IN S_SORTF
*       WITH S_LIFNR IN S_LIFNR
*       WITH PC_ALL = PC_ALL
*       WITH PC_RED = PC_RED
*       WITH PC_YEL = PC_YEL
*       WITH PC_GRE = PC_GRE
*       WITH P_RED1 = P_RED1
*       WITH P_YEL1 = P_YEL1
*       WITH P_GRE1 = P_GRE1
*       WITH P_TOPB = P_TOPB
*       WITH P_TOPV = P_TOPV
*       WITH P_LESB = P_LESB
*       WITH P_LESV = P_LESV
*       WITH P_GRTB = P_GRTB
*       WITH P_GRTV = P_GRTV.
*  ENDIF.
  ENDIF.

ENDMODULE.                 " SET_TIMER  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  GET_ASN_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_ASN  text
*      -->PT_TEMP  text
*----------------------------------------------------------------------*
FORM get_asn_qty    TABLES pt_temp STRUCTURE lt_temp.

  DATA : BEGIN OF lt_asn OCCURS 0,
       vbeln  TYPE likp-vbeln,     "Delivery
       posnr  TYPE lips-posnr,     "Delivery item
       lfdat  TYPE likp-lfdat,     "Delivery Date
       lfart  TYPE likp-lfart,     "Delivery Type
       matnr  TYPE lips-matnr,     "Material Number

       lfimg  TYPE lips-lfimg,     "Actual quantity delivered
       meins  TYPE lips-meins,     "Base Unit of Measure
       wbsta  TYPE vbup-wbsta.     "Goods movement status
  DATA : END OF  lt_asn.

  DATA :
    l_the_date   TYPE sy-datum,
    l_days_after TYPE i,
    lt_tmp_asn   LIKE TABLE OF lt_asn WITH HEADER LINE.
  RANGES :
    lt_lfdat     FOR sy-datum.

* "initialize result
  CLEAR   : lt_asn.
  REFRESH : lt_asn[].

  l_days_after = 3.
  CLEAR : l_the_date.
  PERFORM get_factory_calendar_days   USING l_days_after
                                   CHANGING l_the_date.

  CLEAR   : lt_lfdat.
  REFRESH : lt_lfdat[].
  lt_lfdat-sign   = 'I'.
  lt_lfdat-option = 'BT'.
  lt_lfdat-low    = sy-datum.
  lt_lfdat-high   = l_the_date.
  APPEND lt_lfdat.

  SELECT b~wbsta
         c~lfart  c~lfdat           "Delivery Type, Delivery date
         d~vbeln  d~posnr           "Delivery
         d~matnr  d~lfimg  d~meins  "material, Qty
    FROM vbuk AS a
         INNER JOIN  vbup AS b  ON a~vbeln = b~vbeln
         INNER JOIN  likp AS c  ON a~vbeln = c~vbeln
         INNER JOIN  lips AS d  ON c~vbeln = d~vbeln AND
                                   b~posnr = d~posnr
    INTO CORRESPONDING FIELDS OF TABLE lt_tmp_asn
   WHERE b~wbsta =  'A'         "('A'  Not yet processed)
     AND c~lfart =  'EL'        "('EL' Inbound delivery)
     AND c~lfdat IN lt_lfdat.


  SORT lt_tmp_asn BY matnr.

  CLEAR : lt_tmp_asn.
  LOOP AT lt_tmp_asn.

    CLEAR : lt_asn.
    lt_asn-matnr = lt_tmp_asn-matnr.
    lt_asn-lfimg = lt_tmp_asn-lfimg.  "Delivery Qty
*   -------
    COLLECT lt_asn.
*   -------
    CLEAR : lt_tmp_asn.
  ENDLOOP.

* "for Binary search
  SORT lt_asn BY matnr.

  CLEAR : pt_temp.
  LOOP AT pt_temp.

    CLEAR : lt_asn.
    READ TABLE lt_asn WITH KEY matnr = pt_temp-matnr  BINARY SEARCH.
    IF sy-subrc = 0.
      pt_temp-lfimg = lt_asn-lfimg.

      MODIFY pt_temp TRANSPORTING lfimg.

    ENDIF.

    CLEAR : pt_temp.
  ENDLOOP.

ENDFORM.                    " GET_ASN_QTY

*&---------------------------------------------------------------------*
*&      Form  GET_FACTORY_CALENDAR_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_DAY  text
*----------------------------------------------------------------------*
FORM get_factory_calendar_days    USING p_days_after   TYPE i
                               CHANGING p_the_date     TYPE sy-datum.

  DATA :
    lt_dates   TYPE TABLE OF rke_dat    WITH HEADER LINE,
    l_datbi    TYPE          kona-datbi.

* "initialize result
  CLEAR : p_the_date.

  CLEAR : l_datbi.
  l_datbi = sy-datum + 90.   "setting about 3 months

* 'HM' (HMMA Vehicle Factory Calendar)
  CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
    EXPORTING
      i_datab               = sy-datum
      i_datbi               = l_datbi
      i_factid              = 'HM'    "Factory calendar ID
    TABLES
      eth_dats              = lt_dates
    EXCEPTIONS
      date_conversion_error = 1
      OTHERS                = 2.

  IF NOT lt_dates[] IS INITIAL.
    SORT lt_dates   BY periodat.

    CLEAR : lt_dates.
    READ TABLE lt_dates   INDEX p_days_after.
    IF sy-subrc = 0.

      p_the_date = lt_dates-periodat.

    ENDIF.
  ENDIF.

ENDFORM.                    " GET_FACTORY_CALENDAR_DAYS
