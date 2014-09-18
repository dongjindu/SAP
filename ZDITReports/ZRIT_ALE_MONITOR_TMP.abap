*&---------------------------------------------------------------------*
*& Report  ZRIT_ALE_MONITOR                                            *
*&---------------------------------------------------------------------*
*& Program: ZRIT_ALE_MONITOR                                           *
*& Type   : Report                                                     *
*& Author : I.G.Moon                                                   *
*& Title  : ALE Monitoring                                             *
*&---------------------------------------------------------------------*
*  MODIFICATION LOG
************************************************************************
*  DATE      Developer      RequestNo.      Description
*  01/20/11  I.G. MOON                      Recreate
************************************************************************

REPORT  zrit_ale_monitor.

**-------------------------------------------------------------------*
**  TABLE DEFINE
**-------------------------------------------------------------------*
TABLES : edidc, tedtt, edimsgt.

INCLUDE zrpp_common_alvc.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.



CONSTANTS: gc_formname_top_of_page TYPE slis_formname
                                   VALUE 'TOP_OF_PAGE',
           gc_var_save       TYPE c VALUE  'A',
           gc_pf_status_set  TYPE slis_formname VALUE 'PF_STATUS_SET',
           gc_user_command   TYPE slis_formname VALUE 'USER_COMMAND'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA: gt_list_top_of_page  TYPE slis_t_listheader,
      gt_list_top_of_page1 TYPE slis_t_listheader,
      gs_fieldcat          TYPE slis_fieldcat_alv,
      gt_events            TYPE slis_t_event,
      gt_specialcol        TYPE slis_t_specialcol_alv,
      gs_specialcol        TYPE slis_specialcol_alv.

DATA: gv_default(1)  TYPE c,
      gs_variant  LIKE disvariant,
      gs_variant1 LIKE disvariant,
      gv_repid    LIKE sy-repid.

* for ALV Grid
DATA : gt_exclude   TYPE ui_functions,
       gt_exclude1  TYPE ui_functions,
       gs_print     TYPE lvc_s_prnt,
       gs_fcat      TYPE lvc_s_fcat,
       gt_fcat      TYPE lvc_t_fcat,
       gs_layo      TYPE lvc_s_layo,
       gs_fcat1     TYPE lvc_s_fcat,
       gt_fcat1     TYPE lvc_t_fcat,
       gs_layo1     TYPE lvc_s_layo,
       gs_sort_alv  TYPE slis_sortinfo_alv,
       gt_sort_alv  TYPE slis_t_sortinfo_alv.

DATA : save_ok_code TYPE sy-ucomm.

* Define internal tables &sstructures for Possible Entry
DATA : gs_values TYPE seahlpres,
       gt_fields TYPE TABLE OF dfies WITH HEADER LINE,
       gt_values TYPE TABLE OF seahlpres WITH HEADER LINE,
       gs_fields TYPE dfies.

**-------------------------------------------------------------------*
**  DATA DEFINE
**-------------------------------------------------------------------*
DATA: stable        TYPE lvc_s_stbl.

DEFINE __set_refresh_mode.
  stable-row = &1.
  stable-col = &1.
END-OF-DEFINITION.

INCLUDE <icon>.

DATA: d_tab TYPE REF TO data,
      lt_fcat TYPE TABLE OF lvc_s_fcat,
      ls_fcat LIKE LINE OF lt_fcat,
*        LS_ORI LIKE LINE OF LT_ORI,
      lv_time(2) TYPE c,
      lv_line(2) TYPE c.

FIELD-SYMBOLS : <new_tab> TYPE table.

DATA : t_field(50).
FIELD-SYMBOLS : <status> .

*** Data References
DATA: new_line TYPE REF TO data.

*&---------------------------------------------------------------------*
*& VARIABLES
*&---------------------------------------------------------------------*

DATA: gt_data LIKE TABLE OF edidc WITH HEADER LINE.
DATA max_display TYPE i VALUE 300.
DATA: own_sys TYPE REF TO bd_ls_mon.
DATA aud_idoc_tab TYPE bdmon_disp1.

*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_datum   FOR  sy-datum DEFAULT sy-datum,
                 s_uptim  FOR edidc-updtim,
                 s_mestyp FOR edidc-mestyp,
                 s_sndprn FOR edidc-sndprn,
                 s_status FOR edidc-status,
                 s_dirct  FOR edidc-direct,
                 s_docnm  FOR edidc-docnum.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS p_auto AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

PARAMETERS     : p_logi  AS CHECKBOX,
                 p_vend  AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b3.

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
             show_alv,
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
      timeout_interval TYPE i VALUE '5',
      l_alv TYPE REF TO cl_gui_alv_grid,
      first_call(1) TYPE c,
      ok_code     TYPE syucomm,
      l_is_stable TYPE lvc_s_stbl.

WHILE ok_code NE 'EXIT'.
  CALL SCREEN 101.
ENDWHILE.

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
        new_code = 'AUTO'.

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

  METHOD show_alv.
    PERFORM refersh.
  ENDMETHOD.                    "show_alv

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

    CALL METHOD timer_container->free
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.

    CLEAR timer_container.

  ENDMETHOD.                    "dispatch

ENDCLASS.                    "lcl_gui_timer IMPLEMENTATION

**********************************************************************

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  gv_repid = sy-repid.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_auto EQ true.

*    __cls s_datum.
*
*    s_datum = 'IBT'.
*    s_datum-high = sy-datum.
*    s_datum-low = sy-datum - 1.
*    append s_datum.

    WHILE ok_code NE 'BACK' AND ok_code NE 'EXIT'.
      CALL SCREEN 101.
    ENDWHILE.
  ELSE.
    CALL SCREEN 101.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*FORM  CREATE_OBJECT
*---------------------------------------------------------------------*
*TEXT :
*---------------------------------------------------------------------*
FORM p1000_create_object .

*  CLEAR lt_tab[].
*
*  lt_tab[] = <new_tab>[].


  gs_o_layout-report = g_repid_c = gv_repid .

  CLEAR : gs_layout.

*    gs_layout-stylefname = 'H_STYLE'.
  gs_layout-edit      = ' '.
  gs_layout-zebra      = 'X'.
*    gs_layout-sel_mode   = 'B'.
*    gs_layout-box_fname  = 'MARK'.
  gs_layout-cwidth_opt = 'X'.

  PERFORM get_filedcat_alv  USING gt_fieldcat[].

  IF g_docking_container IS INITIAL.

    CREATE OBJECT g_docking_container
      EXPORTING
        repid     = gv_repid
        dynnr     = '0101'
        side      = cl_gui_docking_container=>dock_at_bottom
        extension = 2000.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_docking_container.


    PERFORM p1010_set_grid_events  USING g_grid 'X'.
    PERFORM set_input_con USING g_grid ' ' '0'.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
        i_default            = 'X'
        is_layout            = gs_layout
        is_variant           = gs_o_layout
        it_toolbar_excluding = gt_excl_func
      CHANGING
        it_fieldcatalog      = lt_fcat[]
        it_outtab            = <new_tab>
        it_sort              = gt_sort[].

  ELSE.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
        i_default            = 'X'
        is_layout            = gs_layout
        is_variant           = gs_o_layout
        it_toolbar_excluding = gt_excl_func
      CHANGING
        it_fieldcatalog      = lt_fcat[]
        it_outtab            = <new_tab>
        it_sort              = gt_sort[].

  ENDIF.


ENDFORM.                    "CREATE_OBJECT

*&---------------------------------------------------------------------*
*&      Form  P1010_SET_GRID_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_GRID  text
*      -->P_0097   text
*----------------------------------------------------------------------*
FORM p1010_set_grid_events
  USING p_grid TYPE REF TO cl_gui_alv_grid
           p_toolbar.

  DATA : p_object TYPE REF TO cl_alv_event_toolbar_set,
         p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol,
         ps_row_no     TYPE lvc_s_roid,
         pr_event_data TYPE REF TO cl_alv_event_data,
         pt_bad_cells  TYPE lvc_t_modi.

  CREATE OBJECT g_events.

*****  Register Event Handler
*_DOUBLE CLICK
  SET HANDLER g_events->double_click FOR p_grid.
*  PERFORM EVENT_DOUBLE_CLICK USING '' '' ''.

* HOTSPOT
  SET HANDLER g_events->hotspot_click FOR p_grid.

*_DATA CHANGED
  SET HANDLER g_events->data_changed FOR p_grid.
  PERFORM event_data_changed
          USING p_er_data_changed '' '' '' ''.

*_DATA CHANGED FINISHED
  SET HANDLER g_events->data_changed_finished FOR p_grid.
  PERFORM event_data_changed_finis
          USING ''.

  SET HANDLER g_events->print_top_of_page FOR p_grid.

  CHECK NOT p_toolbar IS INITIAL.
  SET HANDLER g_events->user_command FOR p_grid.
  PERFORM  event_ucomm
          USING ''
                '' .

  SET HANDLER g_events->toolbar FOR p_grid.
  PERFORM  event_toolbar
          USING p_object
                '' ''.

ENDFORM.                    " P1010_SET_GRID_EVENTS

*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0243   text
*      -->P_0244   text
*----------------------------------------------------------------------*
FORM event_ucomm   USING   e_ucomm LIKE sy-ucomm
                                                  p_check.
**---------------------------------------------------------------
*  CHECK P_CHECK EQ 'X'.
**
**---------------------------------------------------------------
*  CASE E_UCOMM.
**___ÀçÀü¼Û
*    WHEN '&CS03'.
*      PERFORM P3000_CALL_CS03.
*  ENDCASE.

ENDFORM.                    " P1020_EVENT_UCOMM
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJECT  text
*      -->P_0254   text
*      -->P_0255   text
*----------------------------------------------------------------------*
FORM event_toolbar
   USING  e_object TYPE REF TO cl_alv_event_toolbar_set
               e_interactive TYPE c
               p_check.

*---------------------------------------------------------------
  CHECK p_check EQ 'X' .

*---------------------------------------------------------------
*_º¯°æÀÏ¶§¸¸ Ãß°¡ ¹öÆ° »ðÀÓ
*  CHECK S_CHANGE IS NOT INITIAL.

  DATA : ls_toolbar  TYPE stb_button.

*_SET : BUTTON TYPE - SEPARATOR
  CLEAR : ls_toolbar.
  ls_toolbar-butn_type = 3.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

  CLEAR ls_toolbar.
*  LS_TOOLBAR-FUNCTION = '&MMBE'.
*  LS_TOOLBAR-ICON = ICON_BIW_REPORT.
*  LS_TOOLBAR-QUICKINFO = 'Àç°íÁ¶È¸'.
*  LS_TOOLBAR-TEXT = ' Àç°í Á¶È¸'.
*  APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

ENDFORM.                    " P1030_EVENT_TOOLBAR
*&--------------------------------------------------------------------*
*&      Form  EVENT_DOUBLE_CLICK
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_0201   text
*      -->P_0202   text
*      -->P_0203   text
*---------------------------------------------------------------------*
FORM event_double_click  USING    value(p_0201)
                                             value(p_0202)
                                             value(p_0203).

  IF sy-dynnr EQ '0101'.
    PERFORM p3100_call_rbdmon00.
    CALL SCREEN 0200.
  ELSEIF sy-dynnr EQ '0200'.
    PERFORM show_idoc_data USING 'I'.
  ENDIF.




ENDFORM.                    " P1040_EVENT_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  EVENT_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM event_hotspot_click  USING e_row_id e_column_id.

  "READ TABLE GT_DATA INDEX E_ROW_ID .
*  CASE E_COLUMN_ID .
*    WHEN 'LIFNR'.
*      SET PARAMETER ID 'LIF' FIELD GT_DATA-LIFNR.
*      SET PARAMETER ID 'EKO' FIELD GT_DATA-EKORG.
*      CALL TRANSACTION 'MK03' AND SKIP FIRST SCREEN.
*  ENDCASE.

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ER_DATA_CHANGED  text
*      -->P_0213   text
*      -->P_0214   text
*      -->P_0215   text
*      -->P_0216   text
*----------------------------------------------------------------------*
FORM event_data_changed  USING    p_data_changed
                                        value(p_0213)
                                        value(p_0214)
                                        value(p_0215)
                                        value(p_0216).

ENDFORM.                    " P1050_EVENT_DATA_CHANGED

*&---------------------------------------------------------------------*
*&      Form  EVENT_DATA_CHANGED_FINIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0225   text
*----------------------------------------------------------------------*
FORM event_data_changed_finis  USING    value(p_0225).

ENDFORM.                    " P1060_EVENT_DATA_CHANGED_FINIS

*&---------------------------------------------------------------------*
*&      Form  GET_FILEDCAT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM get_filedcat_alv  USING gt_fieldcat TYPE lvc_t_fcat.

  DATA : ls_fieldcat TYPE lvc_s_fcat.
  DATA : lt_fieldcat TYPE slis_t_fieldcat_alv,
         l_fieldcat  TYPE slis_fieldcat_alv.

  CLEAR : lt_fieldcat ,lt_fieldcat[].

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = gv_repid
      i_internal_tabname     = 'GT_DATA'
*      I_BYPASSING_BUFFER     = 'X'
      i_inclname             = gv_repid
    CHANGING
      ct_fieldcat            = lt_fieldcat[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CLEAR : gt_fieldcat , gt_fieldcat[].

  LOOP AT lt_fieldcat INTO l_fieldcat.
    CLEAR : ls_fieldcat.
    MOVE-CORRESPONDING l_fieldcat TO ls_fieldcat.

    ls_fieldcat-reptext   = l_fieldcat-seltext_s.
    ls_fieldcat-ref_table = l_fieldcat-ref_tabname.
    ls_fieldcat-key       = space.

  ENDLOOP.

ENDFORM.                    " GET_FILEDCAT_ALV

*&---------------------------------------------------------------------*
*&      Form  P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_POS[]  text
*      -->P_G_GRID  text
*----------------------------------------------------------------------*
FORM p1200_call_grid_display  TABLES   p_table
                          USING    p_grid TYPE REF TO cl_gui_alv_grid  .

  CALL METHOD p_grid->set_table_for_first_display
    EXPORTING
      i_save               = 'A'
      i_default            = 'X'
      is_layout            = gs_layout
      i_structure_name     = 'GT_DATA'
      is_variant           = gs_o_layout
      it_toolbar_excluding = gt_excl_func
    CHANGING
      it_fieldcatalog      = gt_fieldcat
      it_sort              = gt_sort
      it_outtab            = p_table[].

ENDFORM.                    " P1200_CALL_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CALL_GRID_DISPLAY_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_DATA[]  text
*----------------------------------------------------------------------*
FORM call_grid_display_old .
  DATA: l_struct    LIKE dd02l-tabname.

  l_struct = 'GT_DATA'.
*-----> SET OBJECT
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      i_structure_name              = l_struct
      is_variant                    = gs_o_layout
      i_save                        = 'A'
      is_layout                     = gs_layout
    CHANGING
      it_outtab                     = gt_data[]
      it_fieldcatalog               = gt_fieldcat[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4 .

ENDFORM.                    " CALL_GRID_DISPLAY_OLD
*&---------------------------------------------------------------------*
*&      Form  P1100_CREATE_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p1100_create_object.
  DATA ai TYPE bdmod1tp.
  DATA: fieldcat TYPE lvc_t_fcat, fc TYPE lvc_s_fcat.
  DATA  layout TYPE lvc_s_layo.

* Prepare Control
  SET PF-STATUS 'S200'.

  IF g_alv_container  IS INITIAL.

    CREATE OBJECT    g_alv_container
    EXPORTING container_name = 'CUST_200'.

  ENDIF.

  IF g_grid1 IS INITIAL.
    CREATE OBJECT g_grid1
           EXPORTING i_parent = g_alv_container .

  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name       = 'BDMOD1TP'
       CHANGING
            ct_fieldcat            = fieldcat
       EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
  CHECK sy-subrc = 0.

* Fill Fieldcatalog
  LOOP AT fieldcat INTO fc.
    IF fc-fieldname = 'DOCNUM'.
      fc-col_pos = 1.
      fc-outputlen = 16.
      fc-key = 'X'.
    ELSEIF fc-fieldname = 'STATUS'.
      fc-col_pos = 2.
      fc-outputlen = 4.
      fc-just = 'C'.
    ELSEIF fc-fieldname = 'DOCNUM_PR'.
      fc-coltext = 'IDoc-Nr. Partner'(017).
      fc-seltext = 'IDoc-Nr. Partner'(017).
      fc-emphasize = 'C1'.
      fc-col_pos = 3.
      fc-outputlen = 16.
*      LOOP AT AUD_IDOC_TAB INTO AI WHERE NOT DOCNUM_PR IS INITIAL.
*      ENDLOOP.
*      IF SY-SUBRC <> 0.
*        FC-NO_OUT = 'X'.
*        FC-TECH = 'X'.
*      ENDIF.
    ELSEIF fc-fieldname = 'STATUS_PR'.
      fc-emphasize = 'C1'.
      fc-col_pos = 4.
      fc-outputlen = 4.
      fc-just = 'C'.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT docnum_pr IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
        fc-tech = 'X'.
      ENDIF.
    ELSEIF fc-fieldname = 'MESTYP'.
      fc-col_pos = 5.
      fc-outputlen = 15.
    ELSEIF fc-fieldname = 'MESCOD'.
      fc-col_pos = 6.
      fc-outputlen = 5.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT mescod IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
      ENDIF.
    ELSEIF fc-fieldname = 'MESFCT'.
      fc-col_pos = 7.
      fc-outputlen = 5.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT mesfct IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
      ENDIF.
    ELSEIF fc-fieldname = 'OBJNAME'.
      fc-col_pos = 8.
      fc-outputlen = 16.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT objname IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
      ENDIF.
      fc-coltext = 'Objekttyp'(016).
      fc-seltext = 'Objekttyp'(016).
    ELSEIF fc-fieldname = 'METHODNAME'.
      fc-col_pos = 9.
      fc-outputlen = 16.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT methodname IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
        fc-tech = 'X'.
      ENDIF.
      fc-coltext = 'Methode'(021).
      fc-seltext = 'Methode'(021).
    ELSEIF fc-fieldname = 'OBJKEY'.
      fc-col_pos = 10.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT objkey IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
        fc-tech = 'X'.
      ENDIF.
      fc-outputlen = 25.
      fc-coltext = 'Objektschlüssel'(022).
      fc-seltext = 'Objektschlüssel'(022).
    ELSEIF fc-fieldname = 'STATXT'.
      fc-col_pos = 11.
      fc-outputlen = 50.
    ELSEIF fc-fieldname = 'PARTNER'.
      fc-col_pos = 12.
    ELSEIF fc-fieldname = 'PARPRT'.
      fc-col_pos = 13.
      fc-outputlen = 5.
      fc-no_out = 'X'.
    ELSEIF fc-fieldname = 'PARPFC'.
      fc-col_pos = 14.
      fc-outputlen = 5.
      fc-no_out = 'X'.
    ELSEIF fc-fieldname = 'CREDAT'.
      fc-col_pos = 15.
    ELSEIF fc-fieldname = 'CRETIM'.
      fc-col_pos = 16.
    ELSEIF fc-fieldname = 'IDOCTP'.
      fc-col_pos = 17.
      fc-outputlen = 15.
    ELSEIF fc-fieldname = 'CIMTYP'.
      fc-col_pos = 18.
      LOOP AT aud_idoc_tab INTO ai WHERE NOT cimtyp IS initial.
      ENDLOOP.
      IF sy-subrc <> 0.
        fc-no_out = 'X'.
      ENDIF.
      fc-outputlen = 15.
    ELSEIF fc-fieldname = 'MAXSEGNUM'.
      fc-col_pos = 19.
      fc-outputlen = 7.
    ELSEIF fc-fieldname = 'UPDDAT'.
      fc-col_pos = 20.
      fc-no_out = 'X'.
    ELSEIF fc-fieldname = 'UPDTIM'.
      fc-col_pos = 21.
      fc-no_out = 'X'.
    ELSEIF fc-fieldname = 'TEST'.
      fc-col_pos = 22.
      fc-outputlen = 4.
      fc-no_out = 'X'.
    ENDIF.

    MODIFY fieldcat FROM fc.
  ENDLOOP.

  layout-zebra = 'X'.
  layout-grid_title = 'IDoc-Auswahl'(019).
*  layout-NO_HGRIDLN = 'X'.
*  layout-NO_vGRIDLN = 'X'.
  layout-no_rowmark = 'X'.
  layout-sel_mode = 'D'.

  PERFORM p1010_set_grid_events
          USING g_grid1 ' '.

  CALL METHOD g_grid1->set_table_for_first_display
       EXPORTING i_structure_name = 'BDMOD1TP'
                 is_layout        = layout
       CHANGING  it_outtab        = aud_idoc_tab
                 it_fieldcatalog  = fieldcat.

  CALL METHOD cl_gui_cfw=>flush.


ENDFORM.                    " P1100_CREATE_OBJECT


*---------------------------------------------------------------------*
*       FORM p2000_get_data                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM p2000_get_data.

  DATA : BEGIN OF lt_key OCCURS 0,
          status LIKE edidc-status,
          sndprn LIKE edidc-sndprn,
          sndprt LIKE edidc-sndprt,
          rcvprn LIKE edidc-rcvprn,
*         RCVPRT LIKE EDIDC-RCVPRT,
          mestyp LIKE edidc-mestyp,
          n_stt  TYPE p,
         END OF lt_key.
*          DESC_SNDPRN LIKE TEDTT-DESCRP,
  DATA : lt_data LIKE TABLE OF lt_key WITH HEADER LINE.
  DATA : BEGIN OF lt_status OCCURS 0,
           status LIKE edidc-status,
         END OF lt_status.

  DATA : lt_edidc LIKE TABLE OF edidc WITH HEADER LINE,
         lt_edim LIKE TABLE OF edimsgt WITH HEADER LINE,
         lt_teds LIKE TABLE OF teds2 WITH HEADER LINE.

  PERFORM show_progress     USING 'Data gathering...' '5'.

  SELECT sndprn rcvprn mestyp status sndprt
    INTO CORRESPONDING FIELDS OF TABLE lt_key
    FROM edidc
    WHERE "CREDAT IN S_DATUM
          upddat IN s_datum
      AND updtim IN s_uptim
      AND mestyp IN s_mestyp
      AND sndprn IN s_sndprn
      AND status IN s_status
      AND docnum IN s_docnm
      AND rcvprt = 'LS'
      AND direct IN s_dirct. "inbound/outbound

  PERFORM show_progress     USING 'Data gathering...' '20'.

  SORT lt_key BY status.
  LOOP AT lt_key.
    lt_status-status = lt_key-status.
    AT END OF status.
      APPEND lt_status.
    ENDAT.
  ENDLOOP.

  SORT lt_key BY sndprn mestyp status.
  LOOP AT lt_key.
    lt_key-n_stt = 1.
    MOVE-CORRESPONDING lt_key TO lt_data.
    COLLECT lt_data.
  ENDLOOP.

  PERFORM show_progress     USING 'Data gathering...' '40'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE  lt_edim
    FROM edimsgt
    FOR ALL ENTRIES IN lt_data
    WHERE mestyp EQ lt_data-mestyp
      AND langua EQ sy-langu.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_teds
    FROM teds2
    FOR ALL ENTRIES IN lt_data
    WHERE status EQ lt_data-status
      AND langua EQ sy-langu.

  CLEAR : ls_fcat .
  ls_fcat-fieldname = 'SNDPRN'.
  ls_fcat-ref_table = 'BDSER'.
  ls_fcat-ref_field = 'SNDPRN'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '10'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'DESCRP'.
  ls_fcat-ref_table = 'TEDTT'.
  ls_fcat-ref_field = 'DESCRP'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '60'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'RCVPRN'.
  ls_fcat-ref_table = 'BDSER'.
  ls_fcat-ref_field = 'RCVPRN'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '10'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'DESRCV'.
  ls_fcat-ref_table = 'TEDTT'.
  ls_fcat-ref_field = 'DESCRP'.
  ls_fcat-inttype  = 'C'.
  ls_fcat-intlen  = '60'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'MESTYP'.
  ls_fcat-ref_table = 'EDIDC'.
  ls_fcat-ref_field = 'MESTYP'.
  ls_fcat-datatype  = 'C'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  ls_fcat-fieldname = 'MESDESC'.
  ls_fcat-ref_table = 'EDIMSGT'.
  ls_fcat-ref_field = 'DESCRP'.
  ls_fcat-datatype  = 'C'.
  ls_fcat-key  = 'X'.
  APPEND ls_fcat TO lt_fcat.

  PERFORM show_progress     USING 'Data gathering...' '80'.

  CLEAR : ls_fcat.
  DATA : i_field(10), i_cnt TYPE i.

  LOOP AT lt_status .

    CONCATENATE 'STATUS' lt_status-status INTO i_field.

    ls_fcat-fieldname = i_field.
    ls_fcat-seltext = lt_status-status.
    ls_fcat-reptext = lt_status-status.
    ls_fcat-ref_table = 'MSEG'.
    ls_fcat-ref_field = 'MENGE'.
    ls_fcat-quantity = 'EA'.
    READ TABLE lt_teds WITH KEY status = lt_status-status.
    IF sy-subrc EQ 0 .
      ls_fcat-tooltip = lt_teds-descrp .
    ELSE.
      ls_fcat-tooltip = ''.
    ENDIF.

    IF lt_status-status EQ '03'.
      ls_fcat-emphasize = 'C511'.
    ELSEIF lt_status-status EQ '51'.
      ls_fcat-emphasize = 'C611'.
    ELSE.
      ls_fcat-emphasize = 'C311'.
    ENDIF.

    APPEND ls_fcat TO lt_fcat.
    CLEAR : ls_fcat.
  ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_fcat
      IMPORTING
        ep_table        = d_tab.

  ASSIGN d_tab->* TO <new_tab>.

  PERFORM show_progress     USING 'Data gathering...' '90'.

  CREATE DATA new_line LIKE LINE OF <new_tab>.
  ASSIGN new_line->* TO <status>.

  FIELD-SYMBOLS : <fs_wa> TYPE ANY.
  DATA : f_txt(50), pnrdesc LIKE tedststruc-name1.

  SORT lt_data BY sndprn mestyp.

  LOOP AT lt_data .

    CONCATENATE 'SNDPRN' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    MOVE lt_data-sndprn TO <fs_wa>.
*    <FS_WA> =  LT_DATA-SNDPRN.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-DESCRP' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'DESCRP' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    CALL FUNCTION 'EDI_PARTNER_GET_DESCRIPTION'
         EXPORTING
              pi_partyp = lt_data-sndprt
              pi_parnum = lt_data-sndprn
         IMPORTING
              pe_descrp = pnrdesc
         EXCEPTIONS
              OTHERS    = 0.
    <fs_wa> = pnrdesc.

    UNASSIGN <fs_wa>.
    CONCATENATE 'RCVPRN' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.
    MOVE lt_data-rcvprn TO <fs_wa>.

    UNASSIGN <fs_wa>.
    CONCATENATE 'DESRCV' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.
    CALL FUNCTION 'EDI_PARTNER_GET_DESCRIPTION'
         EXPORTING
              pi_partyp = 'LS'
              pi_parnum = lt_data-rcvprn
         IMPORTING
              pe_descrp = pnrdesc
         EXCEPTIONS
              OTHERS    = 0.
    <fs_wa> = pnrdesc.

    UNASSIGN <fs_wa>.
    CONCATENATE 'NEW_LINE->*-MESTYP' '' INTO f_txt.
    ASSIGN (f_txt) TO <fs_wa>.


    CONCATENATE 'MESTYP' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    <fs_wa> = lt_data-mestyp.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-MESDESC' '' INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'MESDESC' '' INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.

    READ TABLE lt_edim WITH KEY mestyp = lt_data-mestyp.
    IF sy-subrc = 0 .
      <fs_wa> = lt_edim-descrp.
    ENDIF.

    UNASSIGN <fs_wa>.
*    CONCATENATE 'NEW_LINE->*-STATUS' LT_DATA-STATUS INTO F_TXT.
*    ASSIGN (F_TXT) TO <FS_WA>.

    CONCATENATE 'STATUS' lt_data-status  INTO f_txt.
    ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_wa>.


    <fs_wa> = lt_data-n_stt .

    COLLECT <status> INTO <new_tab>.

    CLEAR <status>.
    UNASSIGN <fs_wa>.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2100_CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p2100_clear_data.
  IF d_tab IS INITIAL.
  ELSE.
*  CLEAR : d_tab, lt_fcat, ls_fcat.
    CLEAR : lt_fcat, ls_fcat.
    CLEAR : lt_fcat[].
    CLEAR : <new_tab>, <new_tab>[].
    UNASSIGN <new_tab>.
  ENDIF.
ENDFORM.                    " P2100_CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_CALL_PBDPROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3000_call_rbdproc.
  DATA: gt_seltab TYPE STANDARD TABLE OF rsparams
                  WITH HEADER LINE.
  DATA  seltab LIKE LINE OF gt_seltab.
  DATA: lv_row TYPE i, f_txt(100),
        lv_sndprn LIKE edidc-sndprn,
        lv_mestyp LIKE edidc-mestyp.

  FIELD-SYMBOLS : <fs_sndprn> TYPE ANY,
                  <fs_mestyp> TYPE ANY.

  RANGES : r_status FOR edidc-status.
  CLEAR : gt_seltab[], seltab, <status>.

  CALL METHOD g_grid->get_current_cell
    IMPORTING
      e_row = lv_row.
  READ TABLE <new_tab> INTO <status> INDEX lv_row.

  CONCATENATE 'SNDPRN' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_sndprn>.
  lv_sndprn = <fs_sndprn>.

  CONCATENATE 'MESTYP' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_mestyp>.
  lv_mestyp = <fs_mestyp>.

  r_status-sign = 'I'.
  r_status-option = 'EQ'.
  r_status-low = '30'. APPEND r_status.
  r_status-low = '02'. APPEND r_status.
  r_status-low = '04'. APPEND r_status.
  r_status-low = '05'. APPEND r_status.
  r_status-low = '25'. APPEND r_status.
  r_status-low = '29'. APPEND r_status.
  r_status-low = '26'. APPEND r_status.
  r_status-low = '32'. APPEND r_status.
  r_status-low = '51'. APPEND r_status.
  r_status-low = '56'. APPEND r_status.
  r_status-low = '61'. APPEND r_status.
  r_status-low = '63'. APPEND r_status.
  r_status-low = '65'. APPEND r_status.
  r_status-low = '60'. APPEND r_status.
  r_status-low = '64'. APPEND r_status.
  r_status-low = '66'. APPEND r_status.
  r_status-low = '69'. APPEND r_status.


  SELECT docnum INTO CORRESPONDING FIELDS OF TABLE gt_data
    FROM edidc
  WHERE "CREDAT IN S_DATUM
        upddat IN s_datum
    AND updtim IN s_uptim
    AND mestyp EQ lv_mestyp
    AND sndprn EQ lv_sndprn
    AND status IN r_status
    AND rcvprt EQ 'LS'
      AND direct IN s_dirct. "inbound/outbound


  LOOP AT gt_data.
    seltab-selname  = 'P_IDOC'.
    seltab-sign     = 'I'.
    seltab-option   = 'EQ'.
    seltab-low      =  gt_data-docnum.

    APPEND seltab TO gt_seltab.
  ENDLOOP.

  IF NOT gt_seltab[] IS INITIAL.
    SUBMIT rbdprocess WITH SELECTION-TABLE gt_seltab
                        AND RETURN.
  ELSE.
*    MESSAGE-ID a891.
  ENDIF.
*
ENDFORM.                    " P3000_CALL_PBDPROC
*&---------------------------------------------------------------------*
*&      Form  P3100_CALL_RBDMON00
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM p3100_call_rbdmon00.

  DATA: lv_row TYPE i, f_txt(100),
        lv_sndprn LIKE edidc-sndprn,
        lv_mestyp LIKE edidc-mestyp,
        cnt TYPE i, answer.
  CLEAR :  <status>.
  FIELD-SYMBOLS : <fs_sndprn> TYPE ANY,
              <fs_mestyp> TYPE ANY.

  CALL METHOD g_grid->get_current_cell
    IMPORTING
      e_row = lv_row.
  READ TABLE <new_tab> INTO <status> INDEX lv_row.

  CONCATENATE 'SNDPRN' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_sndprn>.
  lv_sndprn = <fs_sndprn>.

  CONCATENATE 'MESTYP' '' INTO f_txt.
  ASSIGN COMPONENT f_txt OF STRUCTURE <status> TO <fs_mestyp>.
  lv_mestyp = <fs_mestyp>.

*  SELECT docnum INTO CORRESPONDING FIELDS OF TABLE gt_data
  SELECT docnum status mestyp credat cretim sndprn rcvprn
                   idoctp cimtyp maxsegnum upddat updtim
                   mescod mesfct sndprt sndpfc rcvprt rcvpfc test
INTO CORRESPONDING FIELDS OF TABLE gt_data
  FROM edidc
WHERE "CREDAT IN S_DATUM
      upddat IN s_datum
    AND updtim IN s_uptim
    AND mestyp EQ lv_mestyp
    AND sndprn EQ lv_sndprn
    AND rcvprt EQ 'LS'
    AND direct IN s_dirct. "inbound/outbound

*    SELECT DOCNUM STATUS MESTYP CREDAT CRETIM SNDPRN RCVPRN
*                     IDOCTP CIMTYP MAXSEGNUM UPDDAT UPDTIM
*                     MESCOD MESFCT SNDPRT SNDPFC RCVPRT RCVPFC TEST
*              INTO TABLE GT_DATA FROM EDIDC
*              WHERE  CREDAT IN S_DATUM
*              AND MESTYP EQ LV_MESTYP
*              AND SNDPRN EQ LV_SNDPRN
*              AND  DIRECT = '2' .

  DESCRIBE TABLE gt_data LINES cnt .

  DATA : att_text(255), num_text(70).

  IF cnt > max_display.
    att_text =
    'Total lines are &. Do you really want to display iDocs?'(020).
    WRITE cnt TO num_text.
    SHIFT num_text LEFT DELETING LEADING space.

    REPLACE '&' WITH num_text INTO att_text.
    CONDENSE att_text.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
         EXPORTING
              text_question         = att_text
              default_button        = '2'
              display_cancel_button = ''
         IMPORTING
              answer                = answer.

    IF answer = '2' OR answer = 'A'.
      sy-subrc = 4.
      EXIT.
    ENDIF.
  ENDIF.

  DATA : aud_line LIKE LINE OF aud_idoc_tab.
  LOOP AT gt_data.
    aud_line-docnum = gt_data-docnum.
    aud_line-status = gt_data-status.
    aud_line-mestyp = gt_data-mestyp.
    aud_line-credat = gt_data-credat.
    aud_line-cretim = gt_data-cretim.
    aud_line-partner = gt_data-sndprn.
    aud_line-idoctp = gt_data-idoctp.
    aud_line-maxsegnum = gt_data-maxsegnum.
    aud_line-idoctp = gt_data-idoctp.
    aud_line-cimtyp = gt_data-cimtyp.

    APPEND aud_line TO aud_idoc_tab.
  ENDLOOP.

ENDFORM.                    " P3100_CALL_RBDMON00

*&---------------------------------------------------------------------*
*&      Form  SHOW_IDOC_DATA
*&---------------------------------------------------------------------*
*  Achtung: Hier noch keine Mehrsystemfähigkeit!
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_idoc_data USING detail TYPE c.
  DATA ai TYPE bdmod1tp.
  DATA: e_row TYPE i, e_col TYPE i.
  DATA: es_row_id TYPE lvc_s_row, es_col_id TYPE lvc_s_col.
  DATA: logsys TYPE logsys, docnum TYPE edi_docnum.
  DATA: d_obj TYPE borident.

  CALL METHOD g_grid1->get_current_cell
    IMPORTING e_row = e_row
              e_col = e_col
              es_row_id = es_row_id
              es_col_id = es_col_id.

  CALL METHOD cl_gui_cfw=>flush.

  IF ( es_col_id-fieldname <> 'DOCNUM'
       AND es_col_id-fieldname <> 'STATXT'
       AND es_col_id-fieldname <> 'DOCNUM_PR' ) OR e_row = 0.
    MESSAGE w897(b1).
* Es ist kein IDoc markiert.
    EXIT.
  ENDIF.

  READ TABLE aud_idoc_tab INTO ai INDEX e_row.
  CHECK sy-subrc = 0.

  IF es_col_id-fieldname = 'DOCNUM'.
    docnum = ai-docnum.
  ELSEIF es_col_id-fieldname = 'DOCNUM_PR'.
    logsys = ai-partner.
    IF ai-docnum_pr IS INITIAL OR ai-docnum_pr = '?'.
      MESSAGE w897(b1).
* Es ist kein IDoc markiert.
      EXIT.
    ENDIF.
    docnum = ai-docnum_pr.
  ENDIF.


  CASE detail.
    WHEN 'E'.
      docnum = ai-docnum.
      PERFORM show_err_long_text USING docnum.
    WHEN 'R'.                          " Show object Relations
      d_obj-objkey = docnum.
      d_obj-objtype = 'IDOC'.
      d_obj-logsys = logsys.


      CALL FUNCTION 'SREL_DISPLAY_LIST_OF_NEIGHBORS'
           EXPORTING
                object         = d_obj
*         ROLETYPE       =
*         I_MESSAGE      = 'X'
           EXCEPTIONS
                no_logsys      = 1
                internal_error = 2
                OTHERS         = 3
                .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    WHEN 'I'.                          " Show IDoc
      PERFORM display_single_idoc USING docnum logsys.
  ENDCASE.

  " SHOW_LINKAGE
ENDFORM.                    " SHOW_IDOC_DATA

*---------------------------------------------------------------------*
*       FORM display_single_idoc                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  docnum                                                        *
*  -->  logsys                                                        *
*---------------------------------------------------------------------*
FORM display_single_idoc USING docnum TYPE edi_docnum
                               logsys TYPE logsys.

  DATA: rfc_destination TYPE rfcdes-rfcdest.
  DATA  e_msg(50) TYPE c.

*  IF LOGSYS IS INITIAL OR LOGSYS = BD_LS_MON=>OWN_LOGICAL_SYSTEM.
**    CALL FUNCTION 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
*    CALL FUNCTION 'BAPI_IDOCAPPL_DISPLAY'
*         EXPORTING
*              IDOCNUMBER            = DOCNUM
*         EXCEPTIONS
*              SYSTEM_FAILURE        = 01
*              COMMUNICATION_FAILURE = 02. "#EC *
*    EXIT.
*  ENDIF.

* RFC-Destination bestimmen
  CALL FUNCTION 'OBJ_METHOD_GET_RFC_DESTINATION'
       EXPORTING
            object_type                   = 'IDOCALEAUD'
            method                        = 'DISPLAY'
            logical_system                = logsys
       IMPORTING
            rfc_destination               = rfc_destination
       EXCEPTIONS
            no_rfc_destination_maintained = 1
            error_reading_method_props    = 2
            OTHERS                        = 3.

  IF sy-subrc <> 0.
*Für das logische System & konnte keine RFC-Destination ermittelt werden
    MESSAGE s555(b1) WITH logsys 'IDOCALEAUD' 'DISPLAY'.
    EXIT.
  ENDIF.

* Check wether system is type R/3
  SELECT COUNT( * ) FROM rfcdes
    WHERE rfcdest = rfc_destination
      AND rfctype = '3'.
  IF sy-subrc <> 0.
    MESSAGE s086(b1) WITH rfc_destination.
    EXIT.
  ENDIF.

* RFC-Berechtigung prüfen
  CALL FUNCTION 'AUTHORITY_CHECK_RFC'
       EXPORTING
            userid           = sy-uname
       EXCEPTIONS
            user_dont_exist  = 1
            rfc_no_authority = 2.
  IF sy-subrc <> 0.
* Keine Berechtigung zur Ausführung der Aktion
    MESSAGE s227(b1) WITH '' 'S_RFC' ''.
    EXIT.
* Fehlerhandling
  ENDIF.

* show remote idoc

  CALL FUNCTION 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
     DESTINATION rfc_destination
       EXPORTING
            docnum  = docnum
         EXCEPTIONS
              system_failure = 1 MESSAGE e_msg
              communication_failure = 2 MESSAGE e_msg
              OTHERS = 3.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1 OR 2.
      MESSAGE e778(b1)
        WITH 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
             rfc_destination '' e_msg.
    WHEN 3.
      MESSAGE e778(b1)
        WITH 'RSEIDOC2_CALL_VIA_RFC_DOCNUM'
             rfc_destination 'EXCEPTION RAISED'.            "#EC *
  ENDCASE.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM show_err_long_text                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DOCNUM                                                        *
*---------------------------------------------------------------------*
FORM show_err_long_text
  USING docnum TYPE edi_docnum.

  DATA info_langtext TYPE help_info.
  DATA: BEGIN OF int_dselc OCCURS 0.
          INCLUDE STRUCTURE dselc.
  DATA: END OF int_dselc.

  DATA: BEGIN OF int_dval OCCURS 0.
          INCLUDE STRUCTURE dval.
  DATA: END OF int_dval.

* Workaround, runs only local!
  DATA edids_tab TYPE TABLE OF edids.
  DATA edids TYPE edids.
  DATA: buffer LIKE help_info-message.

  SELECT * INTO TABLE edids_tab FROM edids
       WHERE docnum = docnum.
  CHECK sy-subrc = 0.

  SORT edids_tab BY countr DESCENDING.
  READ TABLE edids_tab INTO edids INDEX 1.

  info_langtext-call = 'D'.
  info_langtext-object = 'N'.
  info_langtext-spras = sy-langu.
  info_langtext-messageid = edids-stamid. "ii-stamid.
  info_langtext-messagenr = edids-stamno. "ii-stamno.
  info_langtext-msgv1     = edids-stapa1.                   "ii-stapa1.
  info_langtext-msgv2     = edids-stapa2.                   "ii-stapa2.
  info_langtext-msgv3     = edids-stapa3.                   "ii-stapa3.
  info_langtext-msgv4     = edids-stapa4.                   "ii-stapa4.
  info_langtext-program   = 'RBDMON00'.
  info_langtext-dynpro    = sy-dynnr.
  info_langtext-docuid    = 'NA'.
  info_langtext-cucol     = 9.
  info_langtext-curow     = 2.
*  info_langtext-message   = edids-statxt. "ii-statxt.
  CALL FUNCTION 'FORMAT_MESSAGE'
       EXPORTING
            id        = info_langtext-messageid
            lang      = '-D'
            no        = info_langtext-messagenr
            v1        = info_langtext-msgv1
            v2        = info_langtext-msgv2
            v3        = info_langtext-msgv3
            v4        = info_langtext-msgv4
       IMPORTING
            msg       = buffer
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  info_langtext-message   = buffer.
  info_langtext-title     = 'SAP R/3'.
  CALL FUNCTION 'HELP_START'
       EXPORTING
            help_infos   = info_langtext
       TABLES
            dynpselect   = int_dselc
            dynpvaluetab = int_dval
       EXCEPTIONS
            OTHERS       = 1.

ENDFORM.                    " SHOW_ERR_LONG_TEXT
*&---------------------------------------------------------------------*
*&      Form  GET_IDOC_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_idoc_objects.
  DATA obj TYPE borident.
  DATA ai TYPE bdmod1tp.
  DATA: nb_tab TYPE TABLE OF neighbor, nb TYPE neighbor.
  DATA objtyp TYPE swo_objtyp.
  DATA ai_status TYPE i.

  LOOP AT aud_idoc_tab INTO ai.
    obj-objkey  = ai-docnum.
    obj-objtype = 'IDOC'.

    CLEAR objtyp.
    IF NOT ai-objname IS INITIAL.
      CALL METHOD own_sys->objname2obj
        EXPORTING
          objname    = ai-objname
        IMPORTING
          objtype    = objtyp.
    ENDIF.

    REFRESH nb_tab.
    CALL FUNCTION 'SREL_GET_NEXT_NEIGHBORS'
      EXPORTING
        object               = obj
*   ROLETYPE             =
*   RELATIONTYPE         =
*   MAX_HOPS             = 1
      TABLES
        neighbors            = nb_tab
      EXCEPTIONS
        internal_error       = 1
        no_logsys            = 2
        OTHERS               = 3.
    CHECK sy-subrc = 0.

    IF NOT objtyp IS INITIAL.
      READ TABLE nb_tab INTO nb
        WITH KEY objtype = objtyp.
      IF sy-subrc = 0.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    ai_status = ai-status.
    IF ai-objkey IS INITIAL AND ai_status < 50.
      READ TABLE nb_tab INTO nb
        WITH KEY roletype = 'OUTBELEG'.
      IF sy-subrc = 0.
        CLEAR ai-methodname.
        CALL METHOD own_sys->obj2objname
          EXPORTING
            objtype    = nb-objtype
          IMPORTING
            objname    = ai-objname.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    ai_status = ai-status.
    IF ai-objkey IS INITIAL AND ai_status >= 50.
      READ TABLE nb_tab INTO nb
        WITH KEY roletype = 'INBELEG'.
      IF sy-subrc = 0.
        CLEAR ai-methodname.
        CALL METHOD own_sys->obj2objname
          EXPORTING
            objtype    = nb-objtype
          IMPORTING
            objname    = ai-objname.
        ai-objkey = nb-objkey.
      ENDIF.
    ENDIF.

    IF ai-objname IS INITIAL.
      ai-objname = '-'.
    ENDIF.
    IF ai-objkey IS INITIAL.
      ai-objkey = '-'.
    ENDIF.
    MODIFY aud_idoc_tab FROM ai.
  ENDLOOP.

  PERFORM p1100_create_object.

ENDFORM.                    " GET_IDOC_OBJECTS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: ucomm TYPE TABLE OF sy-ucomm,
        tmp_maktx LIKE makt-maktx.

  DATA : lv_datum(20).

  REFRESH ucomm.
  SET PF-STATUS 'S100' EXCLUDING ucomm.
  SET TITLEBAR  'T100' .

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  PERFORM p1000_create_object.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.

  SET PF-STATUS 'S200'.
  SET TITLEBAR 'T200'.

  IF g_alv_container IS INITIAL.
    PERFORM p1100_create_object.
  ENDIF.

ENDMODULE.                             " PBO_0200  OUTPUT

*----------------------------------------------------------------------*
* MODULE  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lv_cnt TYPE p.
  DATA lv_ans(1).
  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REF'.
      PERFORM refersh.
    WHEN 'PROC'.
      PERFORM p3000_call_rbdproc.
    WHEN 'DISPLAY'.
      PERFORM p3100_call_rbdmon00.
      CALL SCREEN 0200.
  ENDCASE.

ENDMODULE.                 " user_command_0100  INPUT

*----------------------------------------------------------------------*
* MODULE  EXIT  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE exit INPUT.
*  PERFORM P1110_DESTROY_OBJECT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  pai_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0200 INPUT.
  CASE ok_code.
    WHEN 'DISPLAY'.
      PERFORM show_idoc_data USING 'I'.
    WHEN 'LINKAGE'.
      PERFORM show_idoc_data USING 'R'.
    WHEN 'SHOWERR'.
      PERFORM show_idoc_data USING 'E'.
    WHEN 'OBJECT'.
      PERFORM get_idoc_objects.
    WHEN 'EXIT' OR 'BACK' OR 'CANCEL'.
      IF NOT g_alv_container IS INITIAL.
        " destroy alv container (detroys contained alv control, too)
        CALL METHOD g_alv_container->free
          EXCEPTIONS
            cntl_system_error = 1
            cntl_error        = 2.
      ENDIF.
      FREE: g_grid1, g_alv_container.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                 " pai_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  refersh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refersh.
  PERFORM p2100_clear_data.
  PERFORM p2000_get_data.
  PERFORM p1000_create_object.
ENDFORM.                    " refersh
*&---------------------------------------------------------------------*
*&      Module  set_pfstatus  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_pfstatus OUTPUT.

  SET PF-STATUS 'S100'.
  SET TITLEBAR  'T100'.

ENDMODULE.                 " set_pfstatus  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  pbo_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0101 OUTPUT.

  IF timer_container IS INITIAL.
    CREATE OBJECT:

       timer_container
             EXPORTING
                  container_name = 'TI_CONTAINER',
       gui_timer
             EXPORTING
                  parent = timer_container.

    SET HANDLER event_handler->on_finished FOR gui_timer.

    IF p_auto EQ true.
      gui_timer->interval = timeout_interval.
    ENDIF.

    CALL METHOD gui_timer->run.
    CALL METHOD gui_timer->show_alv.

  ENDIF.

ENDMODULE.                 " pbo_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0101 INPUT.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REF'.
      PERFORM refersh.
    WHEN 'PROC'.
      PERFORM p3000_call_rbdproc.
    WHEN 'DISPLAY'.
      PERFORM p3100_call_rbdmon00.
      CALL SCREEN 0200.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.                 " exit_0101  INPUT

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
