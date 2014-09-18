*----------------------------------------------------------------------*
***INCLUDE ZRSD_OSR_SIGNOFF_ALV .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_PLAN'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
          EXPORTING container_name = wa_custom_control
          EXCEPTIONS
           cntl_error = 1
           cntl_system_error = 2
           create_error = 3
           lifetime_error = 4
           lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'WORK_ORDER'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
 data: lw_itab(30).

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  lw_itab = p_itab.

  gv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = gv_repid
            i_internal_tabname = lw_itab
            i_inclname         = gv_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                'S' 'WORK_ORDER'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Work Order',
                                  'E' 'OUTPUTLEN'   '14',

                                   'S' 'EXTC'       ' ',
                                  ' ' 'COLTEXT'     'Ext',
                                  'E' 'OUTPUTLEN'   '3',

                                  'S' 'INTC'       ' ',
                                  ' ' 'COLTEXT'     'Int',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'STATUS'         ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  'E' 'OUTPUTLEN'   '2',

                                  'S' 'MI'        ' ',
                                  ' ' 'COLTEXT'     'MI',
                                   'E' 'OUTPUTLEN'   '9',

                                  'S' 'OCNN'       ' ',
                                  ' ' 'COLTEXT'     'OCNN',
                                   'E' 'OUTPUTLEN'   '4',

                                  'S' 'RSNUM'         ' ',
                                  ' ' 'COLTEXT'     'Resch date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MODL'       ' ',
                                  ' ' 'COLTEXT'     'Model',
                                  'E' 'OUTPUTLEN'   '5',

                                   'S' 'BODY_SER'       ' ',
                                  ' ' 'COLTEXT'     'Body No',
                                  'E' 'OUTPUTLEN'   '7',

                                  'S' 'SEQ_SERIAL'       ' ',
                                  ' ' 'COLTEXT'     'Seq serial',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'ZVIN'       ' ',
                                  ' ' 'COLTEXT'     'ZVIN',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'DATE_SOFF'       ' ',
                                  ' ' 'COLTEXT'     'S.off Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZSER_NO'       ' ',
                                  ' ' 'COLTEXT'     'ZSER_NO',
                                  'E' 'OUTPUTLEN'   '18'.




ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40),
         w_cnt type i.

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
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
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display

   EXPORTING   is_layout        = wa_is_layout
               i_save           = wa_save
               is_variant       = wa_variant
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        =  it_plan[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
   CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK' or 'CANC'.
      LEAVE TO SCREEN 0.
  endcase.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
