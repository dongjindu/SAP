************************************************************************
* Program Name      : ZPPR_IF_MONITORING
* Author            :
* Creation Date     : 12/04/12
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  yppr_if_monitoring MESSAGE-ID zm_hma.
INCLUDE: <icon>.
TABLES: dd02l, dd03l,  ztpp_if_monitor.

*---// Internal Tables
DATA: BEGIN OF st_0100,
        ifgrp       LIKE ztpp_if_monitor-ifgrp,  " I/F Group
        ifgrp_text  LIKE dd07t-ddtext,           " I/F Group Text
        ifid        LIKE ztpp_if_monitor-ifid,   " I/F ID
        modul       LIKE ztpp_if_monitor-modul,  " Module
        tabname     LIKE ztpp_if_monitor-tabname," I/F Table
        tabtext     LIKE dd02t-ddtext,           " I/F Desc
        ddtext      LIKE dd02t-ddtext,           " Table Desc
        znifflg     LIKE ztpp_if_monitor-znifflg," I/F Flag
        znifdat     LIKE ztpp_if_monitor-znifdat," I/F Date Field
        tabhis      LIKE ztpp_if_monitor-tabhis, " I/F History
        tabmsg      LIKE ztpp_if_monitor-tabmsg, " I/F Message
        fname       LIKE ztpp_if_monitor-fname,  " RFC Functiona
        ifsts_icon(4),                           " I/F Status Icon
        direc       LIKE ztpp_if_monitor-direc,  " Direction
        direc_text  LIKE dd07t-ddtext,           " Direction Text
        direc_icon(4),                           " Direction Icon
        sourc       LIKE ztpp_if_monitor-sourc,  " Source System
        targt       LIKE ztpp_if_monitor-targt,  " Target System
        zgo         LIKE ztpp_if_monitor-zgo,    " I/F On/Off Switch
        zgo_text    LIKE dd07t-ddtext,         " I/F On/Off Switch Text
        total       TYPE i,                      " Total Count
        success     TYPE i,                      " Success Count
        initial     TYPE i,                      " Initial Count
        process     TYPE i,                      " Process Count
        error       TYPE i,                      " Error Count
        erdat       LIKE sy-datum,               " Created on
        incharge    LIKE ztpp_if_monitor-incharge, " Person in charge
      END   OF st_0100.

DATA: it_0100 LIKE st_0100 OCCURS 0 WITH HEADER LINE.

DATA: it_ifsts LIKE it_0100 OCCURS 0 WITH HEADER LINE.

DATA: it_fcode    TYPE STANDARD TABLE OF sy-ucomm,
      it_fcode_if TYPE STANDARD TABLE OF sy-ucomm.

*---// Global variable
DATA: g_field     TYPE string,
      g_group     TYPE string,
      g_condition TYPE string,
      g_no_of_if  TYPE i,
      g_err_only.

*---// Ranges
RANGES: r_datum    FOR sy-datum,
        r_modul    FOR ztpp_if_monitor-modul,
        r_incharge FOR ztpp_if_monitor-incharge,
        r_targt    FOR ztpp_if_monitor-targt,
        r_ifgrp    FOR ztpp_if_monitor-ifgrp,
        r_ifid     FOR ztpp_if_monitor-ifid.

*---// Button
DATA: g_datum_ext(4)    VALUE icon_enter_more,
      g_modul_ext(4)    VALUE icon_enter_more,
      g_incharge_ext(4) VALUE icon_enter_more,
      g_targt_ext(4)    VALUE icon_enter_more,
      g_ifgrp_ext(4)    VALUE icon_enter_more,
      g_ifid_ext(4)     VALUE icon_enter_more.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_0100   TYPE        scrfname VALUE 'CC_0100_ALV',
      wc_alv_0100       TYPE REF TO cl_gui_alv_grid,
      wc_container_0100 TYPE REF TO cl_gui_custom_container,
      it_sort_0100      TYPE lvc_t_sort   WITH HEADER LINE.

DATA: wc_control_0500   TYPE        scrfname VALUE 'CC_0500_ALV',
      wc_alv_0500       TYPE REF TO cl_gui_alv_grid,
      wc_container_0500 TYPE REF TO cl_gui_custom_container,
      it_sort_0500      TYPE lvc_t_sort   WITH HEADER LINE.

DATA: wc_control_0600   TYPE        scrfname VALUE 'CC_0600_ALV',
      wc_alv_0600       TYPE REF TO cl_gui_alv_grid,
      wc_container_0600 TYPE REF TO cl_gui_custom_container,
      it_sort_0600      TYPE lvc_t_sort   WITH HEADER LINE.

DATA: wc_control_0700   TYPE        scrfname VALUE 'CC_0700_ALV',
      wc_alv_0700       TYPE REF TO cl_gui_alv_grid,
      wc_container_0700 TYPE REF TO cl_gui_custom_container,
      it_sort_0700      TYPE lvc_t_sort   WITH HEADER LINE.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
      v_sort(50),
      v_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container>      TYPE REF TO cl_gui_custom_container,
               <control>        TYPE        scrfname,
               <alv>            TYPE REF TO cl_gui_alv_grid,
               <itab>           TYPE STANDARD TABLE,
               <itab_header>    TYPE any,
               <history>        TYPE STANDARD TABLE,
               <history_header> TYPE any,
               <message>        TYPE STANDARD TABLE,
               <message_header> TYPE any,
               <sort>           TYPE lvc_t_sort.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_his TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_msg TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_table        TYPE REF TO data,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ST_'.

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
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_0100 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

INITIALIZATION.
*  AUTHORITY-CHECK OBJECT 'ZPP_REPORT'
*           ID 'ZPP_REPORT' FIELD 'ZPPR0083'
*           ID 'ACTVT' FIELD '02'.
*
*  IF sy-subrc <> 0.
*    APPEND 'NEW' TO it_fcode.
*    APPEND 'EDIT' TO it_fcode.
*    APPEND 'DELETE' TO it_fcode.
*  ENDIF.

START-OF-SELECTION.
  PERFORM set_default.
  PERFORM read_data.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  SELECT a~tabname ifgrp direc sourc targt zgo tabtext
         a~tabhis a~tabmsg a~ifid a~znifflg a~znifdat a~fname
         b~ddtext a~modul  a~incharge
    INTO CORRESPONDING FIELDS OF TABLE it_ifsts
    FROM ztpp_if_monitor AS a LEFT OUTER JOIN dd02t AS b
                               ON b~tabname    = a~tabname
                              AND b~ddlanguage = sy-langu
                              AND b~as4local   = 'A'
                              AND b~as4vers    = '0000'
   WHERE ifgrp    IN r_ifgrp
     AND modul    IN r_modul
     AND incharge IN r_incharge
     AND targt    IN r_targt
     AND ifid     IN r_ifid.
  IF sy-subrc NE 0.
    MESSAGE s003 WITH text-m02.
*    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  DESCRIBE TABLE it_ifsts LINES g_no_of_if.

  CLEAR: it_0100, it_0100[].

  SORT it_0100 BY ifgrp ifid tabname erdat.

  PERFORM read_count_record.

  IF g_err_only EQ 'X'.
    DELETE it_0100 WHERE error EQ 0.
  ENDIF.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  set_default
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_default.
  REFRESH r_datum.
  MOVE: 'I'      TO r_datum-sign,
        'BT'     TO r_datum-option,
        sy-datum TO r_datum-high.
  r_datum-low = sy-datum - 1.
  APPEND r_datum.

  MOVE: icon_display_more TO g_datum_ext.

  REFRESH r_incharge.
  MOVE: 'I'      TO r_incharge-sign,
        'EQ'     TO r_incharge-option,
        sy-uname TO r_incharge-low.
  APPEND r_incharge.

  MOVE: icon_display_more TO g_incharge_ext.
ENDFORM.                    " set_default
*&---------------------------------------------------------------------*
*&      Form  read_count_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_count_record .
  DATA: BEGIN OF lt_count OCCURS 0,
          erdat_d  LIKE sy-datum,
          erdat_c(10),
          ifresult TYPE zresult,
          count    TYPE   i,
        END   OF lt_count.

  READ TABLE r_datum INDEX 1.

  LOOP AT it_ifsts.
    IF it_ifsts-tabtext IS INITIAL.
      MOVE: it_ifsts-ddtext TO it_ifsts-tabtext.
    ENDIF.

    PERFORM set_count_conditions.

    SELECT (g_field)
      INTO CORRESPONDING FIELDS OF TABLE lt_count
      FROM (it_ifsts-tabname)
     WHERE (g_condition)
     GROUP BY (g_group).
    IF sy-subrc NE 0.
      MOVE: it_ifsts    TO it_0100,
            r_datum-low TO it_0100-erdat.
      APPEND it_0100.

      CONTINUE.
    ENDIF.

    LOOP AT lt_count.
      IF lt_count-erdat_d IS INITIAL.
        MOVE: lt_count-erdat_c TO lt_count-erdat_d.
      ENDIF.

      READ TABLE it_0100 WITH KEY ifgrp   = it_ifsts-ifgrp
                                  tabname = it_ifsts-tabname
                                  erdat   = lt_count-erdat_d.
      IF sy-subrc NE 0.
        MOVE: it_ifsts         TO it_0100,
              lt_count-erdat_d TO it_0100-erdat.

        CASE lt_count-ifresult.
          WHEN 'S' OR 'Z' OR 'Y'.
            MOVE: lt_count-count TO it_0100-success.
          WHEN ' ' OR 'I'.
            MOVE: lt_count-count TO it_0100-initial.
          WHEN 'E'.
            MOVE: lt_count-count TO it_0100-error.
          WHEN OTHERS.
            MOVE: lt_count-count TO it_0100-process.
        ENDCASE.

        it_0100-total = it_0100-total + lt_count-count.

        APPEND it_0100.
      ELSE.
        CASE lt_count-ifresult.
          WHEN 'S' OR 'Z' OR 'Y'.
            MOVE: lt_count-count TO it_0100-success.
          WHEN ' ' OR 'I'.
            MOVE: lt_count-count TO it_0100-initial.
          WHEN 'E'.
            MOVE: lt_count-count TO it_0100-error.
          WHEN OTHERS.
            MOVE: lt_count-count TO it_0100-process.
        ENDCASE.

        it_0100-total = it_0100-total + lt_count-count.

        MODIFY  it_0100 INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  PERFORM set_icon.
ENDFORM.                    " read_count_record

*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 0100.
      SET PF-STATUS 'G0100' EXCLUDING it_fcode.
      SET TITLEBAR  'T0100'.
    WHEN 0200 OR 0300.
      SET PF-STATUS 'G0200'.
      SET TITLEBAR  'T0200'.
    WHEN 0400.
      SET PF-STATUS 'G0400'.
      SET TITLEBAR  'T0200'.
    WHEN 0500.
      SET PF-STATUS 'G0500' EXCLUDING it_fcode_if.
      SET TITLEBAR  'T0500'.
    WHEN 0600.
      SET PF-STATUS 'G0600'.
      SET TITLEBAR  'T0600' WITH st_0100-tabtext.
    WHEN 0700.
      SET PF-STATUS 'G0700'.
      SET TITLEBAR  'T0700' WITH st_0100-tabtext.
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
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      CLEAR: sy-ucomm.
      PERFORM refresh_rtn.
    WHEN 'DETAIL'.
      CLEAR: sy-ucomm.
      PERFORM detail_rtn.
    WHEN 'ONOFF'.
      CLEAR: sy-ucomm.
      PERFORM onoff_rtn.
    WHEN 'NEW'.
      CLEAR: sy-ucomm.
      PERFORM new_rtn.
    WHEN 'EDIT'.
      CLEAR: sy-ucomm.
      PERFORM edit_rtn.
    WHEN 'DELETE'.
      CLEAR: sy-ucomm.
      PERFORM delete_rtn.
    WHEN 'DATUM_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'DATUM' 'SYST-DATUM'.
    WHEN 'MODUL_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'MODUL' 'ZTPP_IF_MONITOR-MODUL'.
    WHEN 'INCHARGE_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'INCHARGE'
                                    'ZTPP_IF_MONITOR-INCHARGE'.
    WHEN 'TARGT_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'TARGT' 'ZTPP_IF_MONITOR-TARGT'.
    WHEN 'IFID_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'IFID' 'ZTPP_IF_MONITOR-IFID'.
  ENDCASE.
ENDMODULE.                 " user_command_0100  INPUT
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
    PERFORM set_sort_total_field USING p_dynnr.
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
    EXPORTING
      container_name              = <control>
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
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
    WHEN '0100'.
      PERFORM set_attributes_alv_0100.
    WHEN '0200'.
      PERFORM set_attributes_alv_0100.
    WHEN '0300'.
      PERFORM set_attributes_alv_0100.
    WHEN '0500' OR '0600' OR '0700'.
      PERFORM set_attributes_alv_0500.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_0100.
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
*  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
*  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_is_layout-zebra      = 'X'.         " Emphasize C250
*  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
*  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = space.
  v_variant-handle      = space.
  v_variant-log_group   = space.
  v_variant-variant     = space.
  v_variant-text        = space.

ENDFORM.                    " set_attributes_alv_0100
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

  DATA: l_grid(100).

  FIELD-SYMBOLS: <lfs_grid>      TYPE REF TO   cl_gui_alv_grid.

  CHECK p_dynnr EQ '0100'.

  CONCATENATE: 'WC_ALV_' p_dynnr INTO l_grid.
  ASSIGN:      (l_grid)          TO   <lfs_grid>.

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.

  CALL METHOD <lfs_grid>->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcat[].

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
    WHEN '0100'.
      PERFORM set_screen_fields_0100.
    WHEN '0200' OR '0300'.
      PERFORM set_screen_fields_0200.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_0100.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'IFGRP_TEXT'  ' ',
                                  ' ' 'COLTEXT'     'Group',
                                  ' ' 'OUTPUTLEN'   '8',
                                  'E' 'KEY'         'X',

                                  'S' 'TARGT'       ' ',
                                  ' ' 'COLTEXT'     'System',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'KEY'         'X',

                                  'S' 'DIREC_ICON'  ' ',
                                  ' ' 'COLTEXT'     'I/O',
                                  ' ' 'OUTPUTLEN'   '4',
                                  ' ' 'JUST'        'C',
                                  'E' 'KEY'         'X',

*                                  'S' 'ZGO_TEXT'    ' ',
*                                  ' ' 'COLTEXT'     'On/Off',
*                                  ' ' 'OUTPUTLEN'   '6',
*                                  'E' 'KEY'         'X',

                                  'S' 'IFID'        ' ',
                                  ' ' 'COLTEXT'     'I/F ID',
                                  ' ' 'OUTPUTLEN'   '6',
                                  ' ' 'JUST'        'C',
                                  'E' 'KEY'         'X',

                                  'S' 'MODUL'       ' ',
                                  ' ' 'COLTEXT'     'Module',
                                  ' ' 'OUTPUTLEN'   '5',
                                  ' ' 'JUST'        'C',
                                  'E' 'KEY'         'X',

                                  'S' 'TABNAME'     ' ',
                                  ' ' 'COLTEXT'     'I/F Table',
                                  ' ' 'OUTPUTLEN'   '12',
                                  'E' 'KEY'         'X',

                                  'S' 'TABTEXT'     ' ',
                                  ' ' 'COLTEXT'     'I/F Description',
                                  ' ' 'OUTPUTLEN'   '25',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'IFSTS_ICON'  ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  ' ' 'OUTPUTLEN'   '4',
                                  ' ' 'JUST'        'C',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'COLTEXT'     'I/F Date',
                                  ' ' 'OUTPUTLEN'   '10',
                                  ' ' 'JUST'        'C',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'TOTAL'       ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'SUCCESS'     ' ',
                                  ' ' 'COLTEXT'     'Succ #',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'INITIAL'     ' ',
                                  ' ' 'COLTEXT'     'Init #',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'PROCESS'     ' ',
                                  ' ' 'COLTEXT'     'Proc #',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'ERROR'       ' ',
                                  ' ' 'COLTEXT'     'Err #',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'EMPHASIZE'   'C500',

                                  'S' 'FNAME'       ' ',
                                  ' ' 'COLTEXT'     'RFC Function',
                                  ' ' 'OUTPUTLEN'   '20',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'TABHIS'     ' ',
                                  ' ' 'COLTEXT'     'I/F History',
                                  ' ' 'OUTPUTLEN'   '12',
                                  'E' 'EMPHASIZE'   'C400',

*                                  'S' 'TABMSG'     ' ',
*                                  ' ' 'COLTEXT'     'I/F Message',
*                                  ' ' 'OUTPUTLEN'   '12',
*                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZNIFFLG'     ' ',
                                  ' ' 'COLTEXT'     'I/F Flag Field',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZNIFDAT'     ' ',
                                  ' ' 'COLTEXT'     'I/F Date Field',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'INCHARGE'    ' ',
                                  ' ' 'COLTEXT'     'Person in charge',
                                  ' ' 'OUTPUTLEN'   '8',
                                  'E' 'EMPHASIZE'   'C400'.

ENDFORM.                    " set_screen_fields_0100
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
               'IT_'        p_dynnr '[]' INTO v_itab,
               'IT_SORT_'   p_dynnr '[]' INTO v_sort.

  ASSIGN: (v_alv)       TO <alv>,
          (v_itab)      TO <itab>,
          (v_sort)      TO <sort>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
      i_structure_name = v_structure
      is_layout        = v_is_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = 'X'
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_sort          = <sort>
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

*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.
  SET HANDLER event_receiver->handle_double_click  FOR <alv>.
**-   toolbar control event
*  SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_ONF4          FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_MENU_BUTTON  FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_AFTER_USER_COMMAND FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_BUTTON_CLICK FOR ALV_GRID.
*  SET HANDLER EVENT_RECEIVER->HANDLE_BEFORE_USER_COMMAND FOR ALV_GRID.
* SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED_FINISHED FOR ALV_GRID.

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
  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO v_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e003 WITH 'Check filed catalog' p_field.
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
*&      Form  dbl_click_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_0100 USING p_column_name             "Column Name
                          ps_row_no  LIKE sy-tabix. "Numeric Row ID
  DATA : lw_sel_index LIKE sy-tabix.

  MOVE: ps_row_no TO lw_sel_index.

  READ TABLE it_0100 INDEX lw_sel_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CLEAR: st_0100.
  MOVE: it_0100-ifid       TO st_0100-ifid,
        it_0100-tabname    TO st_0100-tabname,
        it_0100-tabtext    TO st_0100-tabtext,
        it_0100-modul      TO st_0100-modul,
        it_0100-incharge   TO st_0100-incharge,
        it_0100-ifgrp_text TO st_0100-ifgrp_text,
        it_0100-targt      TO st_0100-targt,
        it_0100-znifflg    TO st_0100-znifflg,
        it_0100-znifdat    TO st_0100-znifdat,
        it_0100-tabhis     TO st_0100-tabhis,
        it_0100-tabmsg     TO st_0100-tabmsg,
        it_0100-direc_text TO st_0100-direc_text,
        it_0100-erdat      TO st_0100-erdat.

  PERFORM set_field_list_for_0500.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcat[]
    IMPORTING
      ep_table        = it_table.

  ASSIGN it_table->* TO <itab>.

  PERFORM fill_table USING p_column_name.

  IF wc_container_0500 IS INITIAL.
    PERFORM create_container_n_object USING '0500'.
  ENDIF.

  CALL METHOD wc_alv_0500->set_table_for_first_display
    EXPORTING
      i_structure_name = it_0100-tabname
      is_layout        = v_is_layout
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_outtab        = <itab>.

  CALL SCREEN 0500.
ENDFORM.                    " dbl_click_0100
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0100_VINNO  text
*----------------------------------------------------------------------*
FORM display_vehicle_master USING pv_vinno.
  SET PARAMETER ID 'ZMODEL' FIELD pv_vinno(4).
  SET PARAMETER ID 'ZBODY'  FIELD pv_vinno+4(6).

*  CALL TRANSACTION 'ZPPR0047'.
  CALL TRANSACTION 'ZPPR01500T'.

ENDFORM.                    " DISPLAY_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_FSC_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0100_FSC  text
*----------------------------------------------------------------------*
FORM display_fsc_master USING pv_fsc.
  SET PARAMETER ID 'MAT' FIELD pv_fsc.

  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_FSC_MASTER
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_0200 .
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'VINNO'       ' ',
                                  ' ' 'COL_POS'     '1',
                                  'E' 'KEY'         'X',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'COL_POS'     '2',
                                  'E' 'KEY'         'X',

                                  'S' 'SEQ'         ' ',
                                  ' ' 'COL_POS'     '3',
                                  'E' 'KEY'         'X',

                                  'S' 'RP'          ' ',
                                  ' ' 'COL_POS'     '4',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  ' ' 'COL_POS'     '5',
                                  'E' 'KEY'         'X',

                                  'S' 'RPFLG'       ' ',
                                  'E' 'COL_POS'     '6',

                                  'S' 'VORNR'       ' ',
                                  'E' 'COL_POS'     '7',

                                  'S' 'BFFLG'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'COL_POS'     '8',

                                  'S' 'SCRAP'       ' ',
                                  ' ' 'CHECKBOX'    'X',
                                  'E' 'COL_POS'     '9',

                                  'S' 'ZRESULT'      ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AENAM'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AEDAT'        ' ',
                                  'E' 'NO_OUT'       'X',

                                  'S' 'AEZET'        ' ',
                                  'E' 'NO_OUT'       'X'.
ENDFORM.                    " set_screen_fields_0200
*&---------------------------------------------------------------------*
*&      Form  REFRESH_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_rtn .
  PERFORM read_data.

  PERFORM build_field_catalog USING '0100'.
  PERFORM assign_itab_to_alv USING '0100'.
ENDFORM.                    " REFRESH_rtn
*&---------------------------------------------------------------------*
*&      Form  NEW_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_rtn .
  CLEAR: st_0100.

  MOVE: 'IFRESULT' TO st_0100-znifflg,
        'ERDAT'    TO st_0100-znifdat.

  CALL SCREEN 0200 STARTING AT  30  5
                   ENDING   AT  80 16.
ENDFORM.                    " NEW_RTN
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_rtn .
  SELECT SINGLE * FROM dd02l WHERE tabname   = st_0100-tabname
                               AND as4local  = 'A'
                               AND as4vers   = '0000'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m15 st_0100-tabname text-m16.
  ENDIF.

  SELECT SINGLE * FROM dd03l WHERE tabname   = st_0100-tabname
                               AND as4local  = 'A'
                               AND as4vers   = '0000'
                               AND fieldname = st_0100-znifdat.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH 'Table'  st_0100-tabname
                             text-m03 st_0100-znifflg.
  ENDIF.

  SELECT SINGLE * FROM dd03l WHERE tabname   = st_0100-tabname
                               AND as4local  = 'A'
                               AND as4vers   = '0000'
                               AND fieldname = st_0100-znifflg.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH 'Table'  st_0100-tabname
                             text-m03 st_0100-znifflg.
  ENDIF.

  IF st_0100-tabhis IS NOT INITIAL.
    PERFORM check_history_table.
  ENDIF.

  IF st_0100-tabmsg IS NOT INITIAL.
    PERFORM check_message_table.
  ENDIF.

  CLEAR: ztpp_if_monitor.

  MOVE: st_0100-ifid     TO ztpp_if_monitor-ifid,
        st_0100-tabname  TO ztpp_if_monitor-tabname,
        st_0100-tabtext  TO ztpp_if_monitor-tabtext,
        st_0100-znifflg  TO ztpp_if_monitor-znifflg,
        st_0100-znifdat  TO ztpp_if_monitor-znifdat,
        st_0100-tabhis   TO ztpp_if_monitor-tabhis,
        st_0100-tabmsg   TO ztpp_if_monitor-tabmsg,
        st_0100-fname    TO ztpp_if_monitor-fname,
        st_0100-ifgrp    TO ztpp_if_monitor-ifgrp,
        st_0100-direc    TO ztpp_if_monitor-direc,
        st_0100-sourc    TO ztpp_if_monitor-sourc,
        st_0100-targt    TO ztpp_if_monitor-targt,
        st_0100-modul    TO ztpp_if_monitor-modul,
        st_0100-incharge TO ztpp_if_monitor-incharge,
        sy-uname         TO ztpp_if_monitor-ernam,
        sy-datum         TO ztpp_if_monitor-erdat,
        sy-uzeit         TO ztpp_if_monitor-erzet,
        sy-uname         TO ztpp_if_monitor-aenam,
        sy-datum         TO ztpp_if_monitor-aedat,
        sy-uzeit         TO ztpp_if_monitor-aezet.

  INSERT ztpp_if_monitor.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m04.
  ENDIF.

  PERFORM read_data.

  PERFORM build_field_catalog USING '0100'.
  PERFORM assign_itab_to_alv USING '0100'.

  LEAVE TO SCREEN 0.
ENDFORM.                    " save_rtn
*&---------------------------------------------------------------------*
*&      Form  EDIT_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM edit_rtn .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE it_0100 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  SELECT SINGLE * FROM ztpp_if_monitor WHERE ifid    = it_0100-ifid
                                         AND tabname = it_0100-tabname.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m06.
  ENDIF.

  CLEAR: st_0100.
  MOVE-CORRESPONDING ztpp_if_monitor TO st_0100.

  CALL SCREEN 0300 STARTING AT  30  5
                   ENDING   AT  80 17.
ENDFORM.                    " EDIT_rtn
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_0300_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_0300_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0300_rtn .
  SELECT SINGLE * FROM dd03l WHERE tabname   = st_0100-tabname
                               AND as4local  = 'A'
                               AND as4vers   = '0000'
                               AND fieldname = st_0100-znifdat.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH 'Table'  st_0100-tabname
                             text-m03 st_0100-znifdat.
  ENDIF.

  SELECT SINGLE * FROM dd03l WHERE tabname   = st_0100-tabname
                               AND as4local  = 'A'
                               AND as4vers   = '0000'
                               AND fieldname = st_0100-znifflg.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH 'Table'  st_0100-tabname
                             text-m03 st_0100-znifflg.
  ENDIF.

  IF st_0100-tabhis IS NOT INITIAL.
    PERFORM check_history_table.
  ENDIF.

  IF st_0100-tabmsg IS NOT INITIAL.
    PERFORM check_message_table.
  ENDIF.

  MOVE: st_0100-ifid    TO ztpp_if_monitor-ifid,
        st_0100-tabtext TO ztpp_if_monitor-tabtext,
        st_0100-tabmsg  TO ztpp_if_monitor-tabmsg,
        st_0100-znifflg TO ztpp_if_monitor-znifflg,
        st_0100-znifdat TO ztpp_if_monitor-znifdat,
        st_0100-tabhis  TO ztpp_if_monitor-tabhis,
        st_0100-tabmsg  TO ztpp_if_monitor-tabmsg,
        st_0100-fname   TO ztpp_if_monitor-fname,
        st_0100-ifgrp   TO ztpp_if_monitor-ifgrp,
        st_0100-direc   TO ztpp_if_monitor-direc,
        st_0100-sourc   TO ztpp_if_monitor-sourc,
        st_0100-targt   TO ztpp_if_monitor-targt,
        st_0100-modul    TO ztpp_if_monitor-modul,
        st_0100-incharge TO ztpp_if_monitor-incharge,
        sy-uname        TO ztpp_if_monitor-aenam,
        sy-datum        TO ztpp_if_monitor-aedat,
        sy-uzeit        TO ztpp_if_monitor-aezet.

  UPDATE ztpp_if_monitor.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m07.
  ENDIF.

  PERFORM read_data.

  PERFORM build_field_catalog USING '0100'.
  PERFORM assign_itab_to_alv USING '0100'.

  LEAVE TO SCREEN 0.
ENDFORM.                    " save_0300_rtn
*&---------------------------------------------------------------------*
*&      Form  DELETE_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_rtn .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE it_0100 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  SELECT SINGLE * FROM ztpp_if_monitor WHERE ifid    = it_0100-ifid
                                         AND tabname = it_0100-tabname.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m06.
  ENDIF.

  CLEAR: st_0100.
  MOVE-CORRESPONDING ztpp_if_monitor TO st_0100.

  CALL SCREEN 0400 STARTING AT  30  5
                   ENDING   AT  80 17.
ENDFORM.                    " DELETE_rtn
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'DELETE'.
      CLEAR: sy-ucomm.
      PERFORM delete_0400_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  delete_0300_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_0400_rtn .
  DELETE FROM ztpp_if_monitor WHERE tabname = st_0100-tabname.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m08.
  ENDIF.

  PERFORM read_data.

  PERFORM build_field_catalog USING '0100'.
  PERFORM assign_itab_to_alv USING '0100'.

  LEAVE TO SCREEN 0.
ENDFORM.                    " delete_0300_rtn
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_icon .
  DATA: lv_ddtext LIKE dd07t-ddtext.

  LOOP AT it_0100.
    IF it_0100-error > 0.
      MOVE: icon_led_red   TO it_0100-ifsts_icon.
    ELSE.
      MOVE: icon_led_green TO it_0100-ifsts_icon.
    ENDIF.

    CASE it_0100-direc.
      WHEN 'O'.
        MOVE: icon_arrow_left TO it_0100-direc_icon.
      WHEN 'I'.
        MOVE: icon_arrow_right TO it_0100-direc_icon.
    ENDCASE.

    CLEAR: lv_ddtext.
    SELECT SINGLE ddtext INTO lv_ddtext
      FROM dd07t
     WHERE domname    = 'ZCIFGRP'
       AND ddlanguage = sy-langu
       AND as4local   = 'A'
       AND as4vers    = space
       AND domvalue_l = it_0100-ifgrp.

    MOVE: lv_ddtext TO it_0100-ifgrp_text.

    CLEAR: lv_ddtext.
    SELECT SINGLE ddtext INTO lv_ddtext
      FROM dd07t
     WHERE domname    = 'ZCONOFF'
       AND ddlanguage = sy-langu
       AND as4local   = 'A'
       AND as4vers    = space
       AND domvalue_l = it_0100-zgo.

    MOVE: lv_ddtext TO it_0100-zgo_text.

    CLEAR: lv_ddtext.
    SELECT SINGLE ddtext INTO lv_ddtext
      FROM dd07t
     WHERE domname    = 'ZCDIRECTION01'
       AND ddlanguage = sy-langu
       AND as4local   = 'A'
       AND as4vers    = space
       AND domvalue_l = it_0100-direc.

    MOVE: lv_ddtext TO it_0100-direc_text.

    MODIFY it_0100.
  ENDLOOP.
ENDFORM.                    " set_icon
*&---------------------------------------------------------------------*
*&      Form  DETAIL_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_rtn .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE it_0100 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  CLEAR: st_0100.
  MOVE: it_0100-ifid       TO st_0100-ifid,
        it_0100-tabname    TO st_0100-tabname,
        it_0100-tabtext    TO st_0100-tabtext,
        it_0100-modul      TO st_0100-modul,
        it_0100-incharge   TO st_0100-incharge,
        it_0100-ifgrp_text TO st_0100-ifgrp_text,
        it_0100-targt      TO st_0100-targt,
        it_0100-znifflg    TO st_0100-znifflg,
        it_0100-znifdat    TO st_0100-znifdat,
        it_0100-tabhis     TO st_0100-tabhis,
        it_0100-tabmsg     TO st_0100-tabmsg,
        it_0100-direc_text TO st_0100-direc_text,
        it_0100-erdat      TO st_0100-erdat.

  REFRESH it_fcode_if. CLEAR it_fcode_if.
  IF st_0100-tabhis IS INITIAL.
    APPEND 'HISTORY' TO it_fcode_if.
  ENDIF.

  IF st_0100-tabmsg IS INITIAL.
    APPEND 'MESSAGE' TO it_fcode_if.
  ENDIF.

  PERFORM set_field_list_for_0500.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcat[]
    IMPORTING
      ep_table        = it_table.

  ASSIGN it_table->* TO <itab>.

  PERFORM fill_table USING 'TOTAL'.

  IF wc_container_0500 IS INITIAL.
    PERFORM create_container_n_object USING '0500'.
  ENDIF.

  CALL METHOD wc_alv_0500->set_table_for_first_display
    EXPORTING
      i_structure_name = it_0100-tabname
      i_default        = space
      is_layout        = v_is_layout
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_outtab        = <itab>.

  CALL SCREEN 0500.
ENDFORM.                    " DETAIL_RTN
*&---------------------------------------------------------------------*
*&      Form  set_field_list_for_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_list_for_0500 .
  DATA: BEGIN OF lt_tabinfo OCCURS 0,
          fieldname   LIKE   dd03l-fieldname,
          position    LIKE   dd03l-position,
          ddtext      LIKE   dd03t-ddtext,
          scrtext_s   LIKE   dd04t-scrtext_s,
          scrtext_m   LIKE   dd04t-scrtext_m,
          scrtext_l   LIKE   dd04t-scrtext_l,
          leng        LIKE   dd03l-leng,
          reftable    LIKE   dd03l-reftable,
          reffield    LIKE   dd03l-reffield,
          datatype    LIKE   dd03l-datatype,
          inttype     LIKE   dd03l-inttype,
          keyflag     LIKE   dd03l-keyflag,
          rollname    LIKE   dd03l-rollname,
        END   OF lt_tabinfo.

  SELECT a~fieldname a~position b~ddtext a~leng
         a~inttype   a~keyflag  a~reftable a~reffield a~datatype
         a~rollname  b~scrtext_s b~scrtext_m b~scrtext_l
    INTO CORRESPONDING FIELDS OF TABLE lt_tabinfo
    FROM dd03l AS a LEFT OUTER JOIN dd04t AS b
                       ON a~rollname   = b~rollname
                      AND a~as4local   = b~as4local
                      AND a~as4vers    = b~as4vers
                      AND b~ddlanguage = sy-langu
   WHERE a~tabname   =  it_0100-tabname
     AND a~as4local  =  'A'
     AND a~as4vers   =  '0000'
     AND a~comptype  IN ('E',space).
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m09.
  ENDIF.

  CLEAR: it_fieldcat, it_fieldcat[].
  SORT lt_tabinfo BY position.
  LOOP AT lt_tabinfo.
    CLEAR: it_fieldcat.
    MOVE: lt_tabinfo-fieldname TO it_fieldcat-fieldname,
          it_0100-tabname      TO it_fieldcat-ref_table,
          lt_tabinfo-fieldname TO it_fieldcat-ref_field,
          lt_tabinfo-ddtext    TO it_fieldcat-coltext,
          lt_tabinfo-scrtext_s TO it_fieldcat-scrtext_s,
          lt_tabinfo-scrtext_m TO it_fieldcat-scrtext_m,
          lt_tabinfo-scrtext_l TO it_fieldcat-scrtext_l,
*          lt_tabinfo-leng      TO it_fieldcat-outputlen,
*          lt_tabinfo-inttype   TO it_fieldcat-inttype,
          lt_tabinfo-datatype  TO it_fieldcat-datatype,
          lt_tabinfo-keyflag   TO it_fieldcat-key,
          'C250'               TO it_fieldcat-emphasize.

    IF it_fieldcat-scrtext_s IS NOT INITIAL.
      CLEAR: it_fieldcat-coltext.
    ENDIF.

    IF it_fieldcat-scrtext_s IS INITIAL.
      SELECT SINGLE ddtext INTO it_fieldcat-coltext
        FROM dd03t
       WHERE tabname    = it_0100-tabname
         AND ddlanguage = sy-langu
         AND as4local   = 'A'
         AND fieldname  = it_fieldcat-fieldname.
      IF sy-subrc NE 0.
        SELECT SINGLE scrtext_s scrtext_m scrtext_l
          INTO (it_fieldcat-scrtext_s,
                it_fieldcat-scrtext_m,it_fieldcat-scrtext_l)
          FROM dd04t
         WHERE rollname   = lt_tabinfo-rollname
           AND ddlanguage = 'E'
           AND as4local   = 'A'
           AND as4vers    = '0000'.
        IF sy-subrc NE 0.
          SELECT SINGLE ddtext INTO it_fieldcat-coltext
            FROM dd03t
           WHERE tabname    = it_0100-tabname
             AND ddlanguage = 'E'
             AND as4local   = 'A'
             AND fieldname  = it_fieldcat-fieldname.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE lt_tabinfo-datatype.
      WHEN 'QUAN'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat-qfieldname.
        ENDIF.
      WHEN 'CURR'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat-cfieldname.
        ENDIF.
    ENDCASE.

    IF it_fieldcat-fieldname EQ 'MANDT'.
      MOVE: 'X' TO it_fieldcat-no_out.
    ENDIF.

    APPEND it_fieldcat.
  ENDLOOP.
ENDFORM.                    " set_field_list_for_0500
*&---------------------------------------------------------------------*
*&      Form  fill_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_table USING p_column_name.
  CLEAR: g_condition.
  CONCATENATE it_0100-znifdat '= IT_0100-ERDAT'
         INTO g_condition SEPARATED BY space.

  CASE p_column_name.
    WHEN 'TOTAL'.
      "N/A
    WHEN 'SUCCESS'.
      CONCATENATE g_condition 'AND' it_0100-znifflg
                  'IN (' '''S'',' '''Z'',' '''Y'' )'
             INTO g_condition SEPARATED BY space.
    WHEN 'INITIAL'.
      CONCATENATE g_condition 'AND' it_0100-znifflg
                  'IN (' '''I'',' ''' '' )'
             INTO g_condition SEPARATED BY space.
    WHEN 'PROCESS'.
      CONCATENATE g_condition 'AND NOT' it_0100-znifflg
                  'IN (' '''S'',' '''Z'',' '''Y'',' '''I'','
                         ''' '',' '''E'' )'
             INTO g_condition SEPARATED BY space.
    WHEN 'ERROR'.
      CONCATENATE g_condition 'AND' it_0100-znifflg 'EQ' '''E'''
             INTO g_condition SEPARATED BY space.
    WHEN 'OTHERS'.
      "N/A
  ENDCASE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE <itab>
    FROM (it_0100-tabname)
   WHERE (g_condition).
ENDFORM.                    " fill_table
*&---------------------------------------------------------------------*
*&      Module  user_command_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'HISTORY'.
      CLEAR: sy-ucomm.
      PERFORM display_history.
    WHEN 'MESSAGE'.
      CLEAR: sy-ucomm.
      PERFORM display_message.
  ENDCASE.
ENDMODULE.                 " user_command_0500  INPUT
*&---------------------------------------------------------------------*
*&      Form  onoff_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM onoff_rtn .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE it_0100 INDEX lt_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

*---// If use this function, target system must be 'MES'
  IF it_0100-targt NE 'MES'.
    MESSAGE e003 WITH text-m10.
  ENDIF.

*---// popup
  DATA: lv_msg(40),
        lv_ans.
  IF it_0100-zgo IS INITIAL.
    MOVE: text-m11 TO lv_msg.
  ELSE.
    MOVE: text-m12 TO lv_msg.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = text-h01
      text_question  = lv_msg
      text_button_1  = text-yes
      text_button_2  = text-no1
      default_button = '2'
    IMPORTING
      answer         = lv_ans.

  CHECK lv_ans EQ '1'.

  IF it_0100-zgo IS INITIAL.
    MOVE: 'X'   TO it_0100-zgo.
  ELSE.
    MOVE: space TO it_0100-zgo.
  ENDIF.

  UPDATE ztpp_if_monitor
     SET zgo = it_0100-zgo
   WHERE tabname EQ it_0100-tabname.

  COMMIT WORK AND WAIT.

  PERFORM read_data.

  PERFORM build_field_catalog USING '0100'.
  PERFORM assign_itab_to_alv USING '0100'.
ENDFORM.                    " onoff_rtn
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_0500
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_0500 .
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
  v_is_layout-sel_mode   = 'X'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_is_layout-zebra      = 'X'.         " Emphasize C250
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_0500
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_history .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE <itab> INDEX lt_rows-index ASSIGNING <itab_header>.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

*  ASSIGN COMPONENT 'TABHIS' OF STRUCTURE <itab_header> TO <tabhis>.
*  IF sy-subrc NE 0.
*    MESSAGE e003 WITH text-m01.
*  ENDIF.
*
  IF st_0100-tabhis IS INITIAL.
    MESSAGE s003 WITH text-m13.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  PERFORM set_condition.
  PERFORM set_field_list_for_history.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcat_his[]
    IMPORTING
      ep_table        = it_table.

  ASSIGN it_table->* TO <history>.

  PERFORM fill_table_history.

  IF wc_container_0600 IS INITIAL.
    PERFORM create_container_n_object USING '0600'.
  ENDIF.

  CALL METHOD wc_alv_0600->set_table_for_first_display
    EXPORTING
      i_structure_name = it_0100-tabhis
      i_default        = space
      is_layout        = v_is_layout
    CHANGING
      it_fieldcatalog  = it_fieldcat_his[]
      it_outtab        = <history>.

  CALL SCREEN 0600.
ENDFORM.                    " DISPLAY_HISTORY
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_LIST_FOR_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_list_for_history .
  DATA: BEGIN OF lt_tabinfo OCCURS 0,
          fieldname   LIKE   dd03l-fieldname,
          position    LIKE   dd03l-position,
          ddtext      LIKE   dd03t-ddtext,
          scrtext_s   LIKE   dd04t-scrtext_s,
          scrtext_m   LIKE   dd04t-scrtext_m,
          scrtext_l   LIKE   dd04t-scrtext_l,
          leng        LIKE   dd03l-leng,
          reftable    LIKE   dd03l-reftable,
          reffield    LIKE   dd03l-reffield,
          datatype    LIKE   dd03l-datatype,
          inttype     LIKE   dd03l-inttype,
          keyflag     LIKE   dd03l-keyflag,
          rollname    LIKE   dd03l-rollname,
        END   OF lt_tabinfo.

  SELECT a~fieldname a~position b~ddtext a~leng
         a~inttype   a~keyflag  a~reftable a~reffield a~datatype
         a~rollname  b~scrtext_s b~scrtext_m b~scrtext_l
    INTO CORRESPONDING FIELDS OF TABLE lt_tabinfo
    FROM dd03l AS a LEFT OUTER JOIN dd04t AS b
                       ON a~rollname   = b~rollname
                      AND a~as4local   = b~as4local
                      AND a~as4vers    = b~as4vers
                      AND b~ddlanguage = sy-langu
   WHERE a~tabname   =  st_0100-tabhis
     AND a~as4local  =  'A'
     AND a~as4vers   =  '0000'
     AND a~comptype  IN ('E',space).
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m09.
  ENDIF.

  CLEAR: it_fieldcat_his, it_fieldcat_his[].
  SORT lt_tabinfo BY position.
  LOOP AT lt_tabinfo.
    CLEAR: it_fieldcat_his.
    MOVE: lt_tabinfo-fieldname TO it_fieldcat_his-fieldname,
          it_0100-tabhis       TO it_fieldcat_his-ref_table,
          lt_tabinfo-fieldname TO it_fieldcat_his-ref_field,
          lt_tabinfo-ddtext    TO it_fieldcat_his-coltext,
          lt_tabinfo-scrtext_s TO it_fieldcat_his-scrtext_s,
          lt_tabinfo-scrtext_m TO it_fieldcat_his-scrtext_m,
          lt_tabinfo-scrtext_l TO it_fieldcat_his-scrtext_l,
*          lt_tabinfo-leng      TO it_fieldcat_his-outputlen,
*          lt_tabinfo-inttype   TO it_fieldcat_his-inttype,
          lt_tabinfo-datatype  TO it_fieldcat_his-datatype,
          lt_tabinfo-keyflag   TO it_fieldcat_his-key,
          'C250'               TO it_fieldcat_his-emphasize.

    IF it_fieldcat_his-scrtext_s IS NOT INITIAL.
      CLEAR: it_fieldcat_his-coltext.
    ENDIF.

    IF it_fieldcat_his-scrtext_s IS INITIAL.
      SELECT SINGLE ddtext INTO it_fieldcat_his-coltext
        FROM dd03t
       WHERE tabname    = it_0100-tabhis
         AND ddlanguage = sy-langu
         AND as4local   = 'A'
         AND fieldname  = it_fieldcat_his-fieldname.
      IF sy-subrc NE 0.
        SELECT SINGLE scrtext_s scrtext_m scrtext_l
          INTO (it_fieldcat_his-scrtext_s,
                it_fieldcat_his-scrtext_m,it_fieldcat_his-scrtext_l)
          FROM dd04t
         WHERE rollname   = lt_tabinfo-rollname
           AND ddlanguage = 'E'
           AND as4local   = 'A'
           AND as4vers    = '0000'.
        IF sy-subrc NE 0.
          SELECT SINGLE ddtext INTO it_fieldcat_his-coltext
            FROM dd03t
           WHERE tabname    = it_0100-tabhis
             AND ddlanguage = 'E'
             AND as4local   = 'A'
             AND fieldname  = it_fieldcat_his-fieldname.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE lt_tabinfo-datatype.
      WHEN 'QUAN'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat_his-qfieldname.
        ENDIF.
      WHEN 'CURR'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat_his-cfieldname.
        ENDIF.
    ENDCASE.

    IF it_fieldcat_his-fieldname EQ 'MANDT'.
      MOVE: 'X' TO it_fieldcat_his-no_out.
    ENDIF.

    APPEND it_fieldcat_his.
  ENDLOOP.
ENDFORM.                    " SET_FIELD_LIST_FOR_HISTORY
*&---------------------------------------------------------------------*
*&      Form  FILL_TABLE_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_table_history .

  SELECT * INTO TABLE <history>
    FROM (st_0100-tabhis)
   WHERE (g_condition).

ENDFORM.                    " FILL_TABLE_HISTORY
*&---------------------------------------------------------------------*
*&      Form  SET_HISTORY_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_condition .
  DATA: l_length TYPE i,
        l_value  TYPE string.

  FIELD-SYMBOLS: <lfs_field>.

  CLEAR: g_condition.

  LOOP AT it_fieldcat WHERE key = 'X'.
    IF st_0100-tabmsg EQ 'ZHINMMT9021'.
      CHECK it_fieldcat-fieldname EQ 'ZNORDER' OR
            it_fieldcat-fieldname EQ 'ZSEQ'.
    ENDIF.

    ASSIGN COMPONENT it_fieldcat-fieldname
                  OF STRUCTURE <itab_header> TO <lfs_field>.
    IF sy-subrc NE 0.
      MESSAGE e003 WITH text-m01.
    ENDIF.

    MOVE: <lfs_field> TO l_value.

    CONCATENATE g_condition it_fieldcat-fieldname '= '''
           INTO g_condition SEPARATED BY space.
    CONCATENATE g_condition l_value ''' AND'
           INTO g_condition.
  ENDLOOP.

  l_length = strlen( g_condition ).
  l_length = l_length - 3.
  g_condition = g_condition(l_length).
ENDFORM.                    " SET_HISTORY_CONDITION
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_sort_total_field USING p_dynnr.
  DATA: lst_sort LIKE it_sort_0100.

  CONCATENATE: 'WC_ALV_'       p_dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CONCATENATE: 'IT_SORT_' p_dynnr '[]' INTO v_sort.
  ASSIGN:      (v_sort)               TO   <sort>.

  CALL METHOD <alv>->get_sort_criteria
    IMPORTING
      et_sort = <sort>.

  CASE p_dynnr.
    WHEN '0100'.
      REFRESH <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'IFGRP_TEXT'.
      APPEND lst_sort TO <sort>.

*      CLEAR : lst_sort.
*      lst_sort-fieldname = 'MODUL'.
*      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'TARGT'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'DIREC_ICON'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'IFID'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'TABNAME'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'TABTEXT'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'FNAME'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'TABHIS'.
      APPEND lst_sort TO <sort>.

*      CLEAR : lst_sort.
*      lst_sort-fieldname = 'TABMSG'.
*      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'ZNIFFLG'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'ZNIFDAT'.
      APPEND lst_sort TO <sort>.

      CLEAR : lst_sort.
      lst_sort-fieldname = 'INCHARGE'.
      APPEND lst_sort TO <sort>.
  ENDCASE.

  CALL METHOD <alv>->set_sort_criteria
    EXPORTING
      it_sort = <sort>.

ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message .
  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m05.
  ENDIF.

  READ TABLE <itab> INDEX lt_rows-index ASSIGNING <itab_header>.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  IF st_0100-tabmsg IS INITIAL.
    MESSAGE s003 WITH text-m14.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  PERFORM set_condition.
  PERFORM set_field_list_for_message.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog = it_fieldcat_msg[]
    IMPORTING
      ep_table        = it_table.

  ASSIGN it_table->* TO <message>.

  PERFORM fill_table_message.

  IF wc_container_0700 IS INITIAL.
    PERFORM create_container_n_object USING '0700'.
  ENDIF.

  CALL METHOD wc_alv_0700->set_table_for_first_display
    EXPORTING
      i_structure_name = it_0100-tabmsg
      is_layout        = v_is_layout
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat_msg[]
      it_outtab        = <message>.

  CALL SCREEN 0700.
ENDFORM.                    " DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_LIST_FOR_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_list_for_message .
  DATA: BEGIN OF lt_tabinfo OCCURS 0,
          fieldname   LIKE   dd03l-fieldname,
          position    LIKE   dd03l-position,
          ddtext      LIKE   dd03t-ddtext,
          scrtext_s   LIKE   dd04t-scrtext_s,
          scrtext_m   LIKE   dd04t-scrtext_m,
          scrtext_l   LIKE   dd04t-scrtext_l,
          leng        LIKE   dd03l-leng,
          reftable    LIKE   dd03l-reftable,
          reffield    LIKE   dd03l-reffield,
          datatype    LIKE   dd03l-datatype,
          inttype     LIKE   dd03l-inttype,
          keyflag     LIKE   dd03l-keyflag,
          rollname    LIKE   dd03l-rollname,
        END   OF lt_tabinfo.

  SELECT a~fieldname a~position b~ddtext a~leng
         a~inttype   a~keyflag  a~reftable a~reffield a~datatype
         a~rollname  b~scrtext_s b~scrtext_m b~scrtext_l
    INTO CORRESPONDING FIELDS OF TABLE lt_tabinfo
    FROM dd03l AS a LEFT OUTER JOIN dd04t AS b
                       ON a~rollname   = b~rollname
                      AND a~as4local   = b~as4local
                      AND a~as4vers    = b~as4vers
                      AND b~ddlanguage = sy-langu
   WHERE a~tabname   =  st_0100-tabmsg
     AND a~as4local  =  'A'
     AND a~as4vers   =  '0000'
     AND a~comptype  IN ('E',space).
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m09.
  ENDIF.

  CLEAR: it_fieldcat_msg, it_fieldcat_msg[].
  SORT lt_tabinfo BY position.
  LOOP AT lt_tabinfo.
    CLEAR: it_fieldcat_msg.
    MOVE: lt_tabinfo-fieldname TO it_fieldcat_msg-fieldname,
          it_0100-tabmsg       TO it_fieldcat_msg-ref_table,
          lt_tabinfo-fieldname TO it_fieldcat_msg-ref_field,
          lt_tabinfo-ddtext    TO it_fieldcat_msg-coltext,
          lt_tabinfo-scrtext_s TO it_fieldcat_msg-scrtext_s,
          lt_tabinfo-scrtext_m TO it_fieldcat_msg-scrtext_m,
          lt_tabinfo-scrtext_l TO it_fieldcat_msg-scrtext_l,
*          lt_tabinfo-leng      TO it_fieldcat_msg-outputlen,
*          lt_tabinfo-inttype   TO it_fieldcat_msg-inttype,
          lt_tabinfo-datatype  TO it_fieldcat_msg-datatype,
          lt_tabinfo-keyflag   TO it_fieldcat_msg-key,
          'C250'               TO it_fieldcat_msg-emphasize.

    IF it_fieldcat_msg-scrtext_s IS NOT INITIAL.
      CLEAR: it_fieldcat_msg-coltext.
    ENDIF.

    IF it_fieldcat_msg-scrtext_s IS INITIAL.
      SELECT SINGLE ddtext INTO it_fieldcat_msg-coltext
        FROM dd03t
       WHERE tabname    = it_0100-tabmsg
         AND ddlanguage = sy-langu
         AND as4local   = 'A'
         AND fieldname  = it_fieldcat_msg-fieldname.
      IF sy-subrc NE 0.
        SELECT SINGLE scrtext_s scrtext_m scrtext_l
          INTO (it_fieldcat_msg-scrtext_s,
                it_fieldcat_msg-scrtext_m,it_fieldcat_msg-scrtext_l)
          FROM dd04t
         WHERE rollname   = lt_tabinfo-rollname
           AND ddlanguage = 'E'
           AND as4local   = 'A'
           AND as4vers    = '0000'.
        IF sy-subrc NE 0.
          SELECT SINGLE ddtext INTO it_fieldcat_msg-coltext
            FROM dd03t
           WHERE tabname    = it_0100-tabmsg
             AND ddlanguage = 'E'
             AND as4local   = 'A'
             AND fieldname  = it_fieldcat_msg-fieldname.
        ENDIF.
      ENDIF.
    ENDIF.

    CASE lt_tabinfo-datatype.
      WHEN 'QUAN'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat_msg-qfieldname.
        ENDIF.
      WHEN 'CURR'.
        READ TABLE lt_tabinfo WITH KEY fieldname = lt_tabinfo-reffield
                              TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          MOVE: lt_tabinfo-reffield TO it_fieldcat_msg-cfieldname.
        ENDIF.
    ENDCASE.

    IF it_fieldcat_msg-fieldname EQ 'MANDT'.
      MOVE: 'X' TO it_fieldcat_msg-no_out.
    ENDIF.

    APPEND it_fieldcat_msg.
  ENDLOOP.
ENDFORM.                    " SET_FIELD_LIST_FOR_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  FILL_TABLE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_table_message .
  SELECT * INTO TABLE <message>
    FROM (st_0100-tabmsg)
   WHERE (g_condition).
ENDFORM.                    " FILL_TABLE_MESSAGE
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0700 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0700  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_HISTORY_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_history_table .
  DATA: lt_field LIKE dd03l OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE * FROM dd02l WHERE tabname   = st_0100-tabhis
                               AND as4local  = 'A'
                               AND as4vers   = '0000'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m15 st_0100-tabhis text-m16.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_field
    FROM dd03l
   WHERE tabname = st_0100-tabname
     AND as4local  = 'A'
     AND as4vers   = '0000'
     AND keyflag   = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  LOOP AT lt_field.
    SELECT SINGLE *
      FROM dd03l
     WHERE tabname   = st_0100-tabhis
       AND fieldname = lt_field-fieldname
       AND as4local  = 'A'
       AND as4vers   = '0000'
       AND keyflag   = 'X'.
    IF sy-subrc NE 0.
      MESSAGE e003 WITH text-m17.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_HISTORY_TABLE
*&---------------------------------------------------------------------*
*&      Form  CHECK_MESSAGE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_message_table .
  DATA: lt_field LIKE dd03l OCCURS 0 WITH HEADER LINE.

  SELECT SINGLE * FROM dd02l WHERE tabname   = st_0100-tabmsg
                               AND as4local  = 'A'
                               AND as4vers   = '0000'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m15 st_0100-tabmsg text-m16.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_field
    FROM dd03l
   WHERE tabname = st_0100-tabname
     AND as4local  = 'A'
     AND as4vers   = '0000'
     AND keyflag   = 'X'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH text-m01.
  ENDIF.

  LOOP AT lt_field.
    SELECT SINGLE *
      FROM dd03l
     WHERE tabname   = st_0100-tabmsg
       AND fieldname = lt_field-fieldname
       AND as4local  = 'A'
       AND as4vers   = '0000'
       AND keyflag   = 'X'.
    IF sy-subrc NE 0.
      MESSAGE e003 WITH text-m18.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_MESSAGE_TABLE
*&---------------------------------------------------------------------*
*&      Module  SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_selection_condition INPUT.
  PERFORM set_ranges_first_line USING 'R_DATUM'    r_datum.
  PERFORM set_ranges_first_line USING 'R_MODUL'    r_modul.
  PERFORM set_ranges_first_line USING 'R_INCHARGE' r_incharge.
  PERFORM set_ranges_first_line USING 'R_IFGRP'    r_ifgrp.
  PERFORM set_ranges_first_line USING 'R_TARGT'    r_targt.
  PERFORM set_ranges_first_line USING 'R_IFID'     r_ifid.
ENDMODULE.                 " SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5198   text
*      -->P_R_DATUM  text
*----------------------------------------------------------------------*
FORM set_ranges_first_line  USING pv_range pv_range_val.
  DATA: l_range        TYPE string,
        l_range_sign   TYPE string,
        l_range_option TYPE string,
        l_range_low    TYPE string,
        l_range_high   TYPE string,
        l_button       TYPE string.

  FIELD-SYMBOLS: <lfs_range_it>     TYPE STANDARD TABLE,
                 <lfs_range_sign>,
                 <lfs_range_option>,
                 <lfs_range_low>,
                 <lfs_range_high>,
                 <lfs_button>.

  CONCATENATE: pv_range '[]'          INTO l_range,
               pv_range '-SIGN'       INTO l_range_sign,
               pv_range '-OPTION'     INTO l_range_option,
               pv_range '-LOW'        INTO l_range_low,
               pv_range '-HIGH'       INTO l_range_high,
               'G_' pv_range+2 '_EXT' INTO l_button.

  ASSIGN: (l_range)        TO <lfs_range_it>,
          (l_range_sign)   TO <lfs_range_sign>,
          (l_range_option) TO <lfs_range_option>,
          (l_range_low)    TO <lfs_range_low>,
          (l_range_high)   TO <lfs_range_high>,
          (l_button)       TO <lfs_button>.

  READ TABLE <lfs_range_it> INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    IF <lfs_range_high> IS INITIAL.
      IF <lfs_range_low> IS INITIAL.
        DELETE <lfs_range_it> INDEX 1.
      ELSEIF <lfs_range_low> CA '*+'.
        MOVE: 'I' TO <lfs_range_sign>, 'CP' TO <lfs_range_option>.
        MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
      ELSE.
        MOVE: 'I' TO <lfs_range_sign>, 'EQ' TO <lfs_range_option>.
        MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <lfs_range_sign>, 'BT' TO <lfs_range_option>.
      MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
    ENDIF.
  ELSE.
    IF <lfs_range_high> IS INITIAL.
      IF <lfs_range_low> IS INITIAL.
        " N/A
      ELSEIF <lfs_range_low> CA '*+'.
        MOVE: 'I' TO <lfs_range_sign>, 'CP' TO <lfs_range_option>.
        APPEND pv_range_val TO <lfs_range_it>.
      ELSE.
        MOVE: 'I' TO <lfs_range_sign>, 'EQ' TO <lfs_range_option>.
        APPEND pv_range_val TO <lfs_range_it>.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <lfs_range_sign>, 'BT' TO <lfs_range_option>.
      APPEND pv_range_val TO <lfs_range_it>.
    ENDIF.
  ENDIF.

  READ TABLE <lfs_range_it> INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    MOVE: icon_display_more TO <lfs_button>.
  ELSE.
    MOVE: icon_enter_more TO <lfs_button>.
  ENDIF.
ENDFORM.                    " SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*&      Form  CLICK_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0962   text
*      -->P_0963   text
*----------------------------------------------------------------------*
FORM click_extension USING p_field p_ref.
  DATA: ls_ref      LIKE rstabfield,
        l_range_h(50),               " Body of ranges
        l_range_b(50),               " Header of ranges
        l_button(50).

  FIELD-SYMBOLS: <lfs_range_h>,
                 <lfs_range_b> TYPE STANDARD TABLE,
                 <lfs_button>.

  SPLIT p_ref AT '-' INTO ls_ref-tablename ls_ref-fieldname.

  CONCATENATE 'R_' p_field INTO l_range_h.
  ASSIGN (l_range_h) TO <lfs_range_h>.

  CONCATENATE 'R_' p_field '[]' INTO l_range_b.
  ASSIGN (l_range_b) TO <lfs_range_b>.

  CONCATENATE 'G_' p_field '_EXT' INTO l_button.
  ASSIGN (l_button) TO <lfs_button>.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      tab_and_field = ls_ref
    TABLES
      range         = <lfs_range_b>
    EXCEPTIONS
      cancelled     = 1
      OTHERS        = 2.

  READ TABLE <lfs_range_b> INTO <lfs_range_h> INDEX 1.
  IF sy-subrc EQ 0.
    MOVE: icon_display_more TO <lfs_button>.
  ELSE.
    CLEAR: <lfs_range_h>.
    MOVE: icon_enter_more   TO <lfs_button>.
  ENDIF.
ENDFORM.                    " CLICK_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  SET_COUNT_CONDITIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_count_conditions .
  SELECT SINGLE * FROM dd03l WHERE tabname   = it_ifsts-tabname
                               AND fieldname = it_ifsts-znifdat
                               AND as4local  = 'A'
                               AND as4vers   = '0000'.
  IF sy-subrc NE 0.
    MESSAGE e003 WITH 'Table'  it_ifsts-tabname
                             text-m03 it_ifsts-znifdat.
  ENDIF.

  CASE dd03l-inttype.
    WHEN 'C'.
      CLEAR: g_field.
      CONCATENATE it_ifsts-znifdat 'AS ERDAT_C'
                  it_ifsts-znifflg 'AS IFRESULT COUNT(*) AS COUNT'
             INTO g_field SEPARATED BY space.
    WHEN 'D'.
      CLEAR: g_field.
      CONCATENATE it_ifsts-znifdat 'AS ERDAT_D'
                  it_ifsts-znifflg 'AS IFRESULT COUNT(*) AS COUNT'
             INTO g_field SEPARATED BY space.
  ENDCASE.


  CLEAR: g_condition.
  CONCATENATE it_ifsts-znifdat 'IN R_DATUM'
         INTO g_condition SEPARATED BY space.

  CLEAR: g_group.
  CONCATENATE it_ifsts-znifdat
              it_ifsts-znifflg
         INTO g_group SEPARATED BY space.
ENDFORM.                    " SET_COUNT_CONDITIONS
