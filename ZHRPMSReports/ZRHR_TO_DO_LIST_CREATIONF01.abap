*&---------------------------------------------------------------------*
*&  Include           ZRHR_TO_DO_LIST_CREATIONF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_value .

  IF p_date IS INITIAL.
    CONCATENATE sy-datum(6) '01' INTO p_date.
  ENDIF.

ENDFORM.                    " SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_button .

  sscrfields-functxt_01  = 'Display'.

  icon_name = 'ICON_DISPLAY'.
  icon_text = 'Display'.
  info_text = 'Display'.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                  = icon_name
      text                  = icon_text
      info                  = info_text
    IMPORTING
      result                = sscrfields-functxt_01
    EXCEPTIONS
      icon_not_found        = 1
      outputfield_too_short = 2
      OTHERS                = 3.

ENDFORM.                    " SET_BUTTON
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_100 .

  IF gr_cont IS INITIAL.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_cont.

    PERFORM set_layout.     " set layout
    PERFORM set_fcat.       " set fieldcatalog
    PERFORM set_excl_func.  " set exclude function

    " Edit Event
    IF sy-batch IS INITIAL.
      " Event Cell Modified
      CALL METHOD gr_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      " Enter Event
      CALL METHOD gr_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.
    ENDIF.

    " ALV Event Handler Setting
    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_data_changed FOR gr_grid.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layo
*       i_structure_name              = 'ZSHR_TODO'
        it_toolbar_excluding          = gt_excl_func
      CHANGING
        it_outtab                     = gt_result[]
        it_fieldcatalog               = gt_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    " set edit cells
    LOOP AT gt_fcat WHERE ( fieldname = 'BEGDA' OR fieldname = 'ENDDA' OR
                            fieldname = 'APPEE' OR fieldname = 'APPER' OR
                            fieldname = 'CORI1' OR fieldname = 'EVAI2' OR
                            fieldname = 'CORI2' OR fieldname = 'EVAI3' OR
                            fieldname = 'CORI3' OR fieldname = 'APPRI' OR
                            fieldname = 'CORI4' OR fieldname = 'HRTMI' OR
                            fieldname = 'ERROR' OR fieldname = 'ETEXT' ).
      IF g_change_flag IS INITIAL.
        gt_fcat-edit = ' '.
        MODIFY gt_fcat INDEX sy-tabix.
      ELSE.
        gt_fcat-edit = 'X'.
        MODIFY gt_fcat INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    CALL METHOD gr_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = gt_fcat[].

    PERFORM refresh_alv.

  ENDIF.

ENDFORM.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout .

  gs_layo-zebra = 'X'.
  gs_layo-col_opt = 'X'.
  gs_layo-sel_mode = 'D'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fcat .

  CLEAR: gt_fcat[].
  " Flag
  gt_fcat-fieldname = 'LIGHT'.
  gt_fcat-coltext = text-t16.
  gt_fcat-col_pos = 1.
  gt_fcat-outputlen = 5.
  gt_fcat-just = 'C'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Duration(From)
  gt_fcat-fieldname = 'BEGDA'.
  gt_fcat-coltext = text-t01.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'BEGDA'.
  gt_fcat-col_pos = 2.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Duration(To)
  gt_fcat-fieldname = 'ENDDA'.
  gt_fcat-coltext = text-t02.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'ENDDA'.
  gt_fcat-col_pos = 3.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Team Member
  gt_fcat-fieldname = 'APPEE'.
  gt_fcat-coltext = text-t03.
*  gt_fcat-key = 'X'.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'APPEE'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 4.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPEENM'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 5.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Evaluator
  gt_fcat-fieldname = 'APPER'.
  gt_fcat-coltext = text-t05.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'APPER'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 6.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPERNM'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 7.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 1st Coordinator
  gt_fcat-fieldname = 'CORI1'.
  gt_fcat-coltext = text-t06.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'CORI1'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 8.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM1'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 9.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Evaluator
  gt_fcat-fieldname = 'EVAI2'.
  gt_fcat-coltext = text-t07.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'EVAI2'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 10.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM2'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 11.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2nd Coordinator
  gt_fcat-fieldname = 'CORI2'.
  gt_fcat-coltext = text-t08.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'CORI2'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 12.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM2'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 13.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Evaluator
  gt_fcat-fieldname = 'EVAI3'.
  gt_fcat-coltext = text-t09.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'EVAI3'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'EVANM3'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 15.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3rd Coordinator
  gt_fcat-fieldname = 'CORI3'.
  gt_fcat-coltext = text-t10.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'CORI3'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM3'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 17.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Approver
  gt_fcat-fieldname = 'APPRI'.
  gt_fcat-coltext = text-t11.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'APPRI'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'APPRNM'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 4th Coordinator
  gt_fcat-fieldname = 'CORI4'.
  gt_fcat-coltext = text-t12.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'CORI4'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 20.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'CORNM4'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 21.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " HR Team
  gt_fcat-fieldname = 'HRTMI'.
  gt_fcat-coltext = text-t13.
  gt_fcat-f4availabl = 'X'.
  gt_fcat-ref_table = 'ZSHR_TODO'.
  gt_fcat-ref_field = 'HRTMI'.
  gt_fcat-just = 'C'.
  gt_fcat-col_pos = 22.
  gt_fcat-outputlen = 6.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Name
  gt_fcat-fieldname = 'HRTMNM'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 23.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Error Type
  gt_fcat-fieldname = 'ERROR'.
  gt_fcat-coltext = text-t14.
  gt_fcat-col_pos = 24.
  gt_fcat-outputlen = 5.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Error Message
  gt_fcat-fieldname = 'ETEXT'.
  gt_fcat-coltext = text-t15.
  gt_fcat-col_pos = 25.
  gt_fcat-outputlen = 30.
  APPEND gt_fcat.CLEAR: gt_fcat.

ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  SET_EXCL_FUNC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_excl_func .

  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO gt_excl_func.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO gt_excl_func.

ENDFORM.                    " SET_EXCL_FUNC
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_excpt    TYPE TABLE OF zthr_excpt WITH HEADER LINE,
        lt_applv    TYPE TABLE OF zthr_applv WITH HEADER LINE,
        lt_applve   TYPE TABLE OF zthr_applve WITH HEADER LINE,
        lt_clfaj    TYPE TABLE OF zthr_clfaj WITH HEADER LINE.

  DATA: BEGIN OF lt_p0000 OCCURS 0,
          pernr     TYPE p0000-pernr,
          begda     TYPE p0000-begda,
          massn     TYPE p0000-massn,
          massg     TYPE p0000-massg,
          stat2     TYPE p0000-stat2,
        END OF lt_p0000.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr     TYPE p0001-pernr,
          orgeh     TYPE p0001-orgeh,
          plans     TYPE p0001-plans,
          stell     TYPE p0001-stell,
        END OF lt_p0001.

  DATA: l_where     TYPE string,
        l_apprv     TYPE persno,
        l_orgeh     TYPE orgeh,
        l_orglv     TYPE zdhr_orglv.

  CLEAR: gt_result[].

*  " set where condition
*  IF p_date < sy-datum.
*    CONCATENATE 'ENDDA >=' ' `' p_date '`' INTO l_where.
*    CONCATENATE l_where ' AND BEGDA <=' ' `' sy-datum '`' INTO l_where.
*  ELSEIF p_date >= sy-datum.
*    CONCATENATE 'ENDDA >=' ' `' p_date '`' INTO l_where.
*    CONCATENATE l_where ' AND BEGDA <=' ' `' p_date '`' INTO l_where.
*  ENDIF.

*****************************************************
*  Get Data
*****************************************************
  " get Active Team Member
  CLEAR lt_p0000[].
  SELECT pernr
         begda
         massn
         massg
         stat2
    FROM pa0000
    INTO TABLE lt_p0000
    WHERE pernr IN s_pernr
      AND endda = '99991231'
      AND stat2 = '3'.

  CHECK lines( lt_p0000 ) > 0.
  SORT lt_p0000 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0000 COMPARING pernr.

  " get American, Salary Exempt, Salary Non-Exempt
  CLEAR lt_p0001[].
  SELECT pernr
         orgeh
         plans
         stell
    FROM pa0001
    INTO TABLE lt_p0001
    FOR ALL ENTRIES IN lt_p0000
    WHERE pernr = lt_p0000-pernr
      AND endda = '99991231'
      AND persg = '1'
      AND persk IN ('U2', 'U3').

  CHECK lines( lt_p0001 ) > 0.
  SORT lt_p0001 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0001 COMPARING pernr.

  " get To-Do Exception List
  CLEAR lt_excpt[].
  SELECT * FROM zthr_excpt
    INTO TABLE lt_excpt
    WHERE begda <= sy-datum
      AND endda >= sy-datum.
  SORT lt_excpt BY appee.

  " get Approver Level Exception
  CLEAR lt_applve[].
  SELECT * FROM zthr_applve INTO TABLE lt_applve.
  SORT lt_applve BY clfid orgeh.

  " get Approver Level
  CLEAR lt_applv[].
  SELECT * FROM zthr_applv INTO TABLE lt_applv.
  SORT lt_applv BY clfid.

  " get Classfication and Job
  CLEAR lt_clfaj[].
  SELECT * FROM zthr_clfaj INTO TABLE lt_clfaj.
  SORT lt_clfaj BY jobid.

*****************************************************
*  Export Data Setting
*****************************************************
  LOOP AT lt_p0001.
    CLEAR: gt_result.

*&-- fill appraisal period
    " read p0000
    READ TABLE lt_p0000 WITH KEY pernr = lt_p0001-pernr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      IF lt_p0000-massn EQ 'Z0' OR    " Hiring
         lt_p0000-massn EQ 'Z3'.      " Organizational Reassignment
        IF lt_p0000-massg EQ '02' OR  " Promotion
           lt_p0000-massg EQ '03' OR  " Lateral Move
           lt_p0000-massg EQ '05' OR  " Reclassification
           lt_p0000-massg EQ '08' OR  " Re-organization
           lt_p0000-massg EQ '13'.    " Career Opportunity Program
          IF lt_p0000-begda(6) >= p_date(6).
            PERFORM fill_appraisal_period USING lt_p0000-begda
                                                lt_p0001-pernr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gt_result-begda IS INITIAL AND gt_result-endda IS INITIAL.
      PERFORM fill_appraisal_period USING p_date
                                          lt_p0001-pernr.
    ENDIF.

*&-- fill team member
    gt_result-appee = lt_p0001-pernr.
    PERFORM get_ename USING gt_result-begda
                            gt_result-appee
                            'GT_RESULT-APPEENM'.

*&-- Check Exception List
    READ TABLE lt_excpt WITH KEY appee = lt_p0001-pernr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " fill exception data
      PERFORM fill_exception_list USING lt_excpt.
      APPEND gt_result.
      CONTINUE.
    ENDIF.

*&-- fill HR Team
    IF gt_result-hrtmi IS INITIAL.
      gt_result-hrtmi = p_hrtm.
      PERFORM get_ename USING gt_result-begda
                              gt_result-hrtmi
                              'GT_RESULT-HRTMNM'.
    ENDIF.

*&-- Get Org Struc
    PERFORM get_org_struc USING gt_result-begda
                                lt_p0001-orgeh.
*&-- fill Approver
    " read Classification ID
    READ TABLE lt_clfaj WITH KEY jobid = lt_p0001-stell
                        BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR: l_apprv, l_orgeh.
      " check Approver Lever Exception
      PERFORM check_approver_excep TABLES   lt_applve
                                   USING    gt_result-begda
                                            lt_p0001-orgeh
                                            lt_clfaj-clfid
                                   CHANGING l_apprv
                                            l_orgeh
                                            l_orglv.
      IF l_apprv IS NOT INITIAL.
        gt_result-appri = l_apprv.
        gt_result-eorg4 = l_orgeh.
        gt_result-eorglv4 = l_orglv.
        PERFORM get_ename USING gt_result-begda
                                gt_result-appri
                                'GT_RESULT-APPRNM'.
      ELSE.
        " read Approver Org Level
        READ TABLE lt_applv WITH KEY clfid = lt_clfaj-clfid
                            BINARY SEARCH.
        IF sy-subrc = 0.
          PERFORM fill_approver USING gt_result-begda
                                      lt_p0001-orgeh
                                      lt_applv-orglv
                                      lt_clfaj-clfid.
        ENDIF.
      ENDIF.

    ELSE.
      gt_result-error = 'E'.
      MESSAGE s028 INTO gt_result-etext.
      APPEND gt_result.
      CONTINUE.

    ENDIF.

*&-- fill 4th Coordinator
    PERFORM fill_coordinator USING gt_result-begda
                                   gt_result-eorg4
                                   'GT_RESULT-CORI4'
                                   'GT_RESULT-CORNM4'.

*&-- fill 1st Evaluator
    PERFORM fill_1st_evaluator USING gt_result-begda
                                     gt_result-appee
                                     lt_p0001-orgeh.
    IF gt_result-apper EQ gt_result-appri OR
       gt_result-eorglv1 < gt_result-eorglv4 .
      APPEND gt_result.
      CONTINUE.
    ENDIF.

*&-- fill 1st Coordinator
    PERFORM fill_coordinator USING gt_result-begda
                                   gt_result-eorg1
                                   'GT_RESULT-CORI1'
                                   'GT_RESULT-CORNM1'.

*&-- fill 2nd Evaluator
    PERFORM fill_further_evaluator USING gt_result-begda
                                         gt_result-eorg1
                                         'GT_RESULT-EVAI2'
                                         'GT_RESULT-EORG2'
                                         'GT_RESULT-EORGLV2'
                                         'GT_RESULT-EVANM2'.
    IF gt_result-evai2 EQ gt_result-appri OR
       gt_result-eorglv2 < gt_result-eorglv4 ..
      CLEAR: gt_result-evai2, gt_result-eorg2, gt_result-eorglv2, gt_result-evanm2.
      APPEND gt_result.
      CONTINUE.
    ENDIF.

*&-- fill 2nd Coordinator
    PERFORM fill_coordinator USING gt_result-begda
                                   gt_result-eorg2
                                   'GT_RESULT-CORI2'
                                   'GT_RESULT-CORNM2'.

*&-- fill 3rd Evaluator
    PERFORM fill_further_evaluator USING gt_result-begda
                                         gt_result-eorg2
                                         'GT_RESULT-EVAI3'
                                         'GT_RESULT-EORG3'
                                         'GT_RESULT-EORGLV3'
                                         'GT_RESULT-EVANM3'.
    IF gt_result-evai3 EQ gt_result-appri OR
       gt_result-eorglv3 < gt_result-eorglv4 ..
      CLEAR: gt_result-evai3, gt_result-eorg3, gt_result-eorglv3, gt_result-evanm3.
      APPEND gt_result.
      CONTINUE.
    ENDIF.

*&-- fill 3rd Coordinator
    PERFORM fill_coordinator USING gt_result-begda
                                   gt_result-eorg3
                                   'GT_RESULT-CORI3'
                                   'GT_RESULT-CORNM3'.

    APPEND gt_result.

    CLEAR: lt_p0001, lt_p0000, lt_excpt.
  ENDLOOP.

  DATA: l_index               TYPE i.

  FIELD-SYMBOLS: <fs_value1>  TYPE any,
                 <fs_value2>  TYPE any.

  LOOP AT gt_result.
    " Check same TM ID in further participants
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value1>.
      IF sy-subrc = 0.
        CASE sy-index.
          WHEN 9. " 1st Coordinator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 11 OR 15 OR 17 OR 21 OR 23 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 11.  " 2nd Evaluator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 15 OR 17 OR 21 OR 23 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 15.  " 2nd Coordinator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 17 OR 21 OR 23 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 17.  " 3rd Evaluator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 15 OR 21 OR 23 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 21.  " 3rd Coordinator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 15 OR 17 OR 23 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 23.  " Approver
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 15 OR 17 OR 21 OR 27 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 27.  " 4th Coordinator
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 15 OR 17 OR 21 OR 23 OR 29.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

          WHEN 29.  " HR Team
            DO.
              ASSIGN COMPONENT sy-index OF STRUCTURE gt_result TO <fs_value2>.
              IF sy-subrc = 0.
                CASE sy-index.
                  WHEN 9 OR 11 OR 15 OR 17 OR 21 OR 23 OR 27.
                    IF <fs_value1> IS NOT INITIAL AND <fs_value2> IS NOT INITIAL.
                      IF <fs_value1> EQ <fs_value2>.
                        gt_result-error = 'E'.
                        MESSAGE s040 INTO gt_result-etext.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.

        ENDCASE.

      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    " Check 1st Evaluator
    IF gt_result-apper IS INITIAL.
      gt_result-error = 'E'.
      MESSAGE s035 INTO gt_result-etext.
    ENDIF.
    " Check approver
    IF gt_result-appri IS INITIAL.
      gt_result-error = 'E'.
      MESSAGE s034 INTO gt_result-etext.
    ENDIF.
    " Check HR Team
    IF gt_result-hrtmi IS INITIAL.
      gt_result-error = 'E'.
      MESSAGE s036 INTO gt_result-etext.
    ENDIF.

    IF gt_result-error EQ 'E'.
      gt_result-light = '@0A@'.   " Red
    ELSE.
      gt_result-light = '@08@'.   " Green
    ENDIF.

    MODIFY gt_result INDEX sy-tabix TRANSPORTING error etext light.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_APPRAISAL_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_P0000  text
*      -->P_LT_P0001_PERNR  text
*----------------------------------------------------------------------*
FORM fill_appraisal_period  USING p_date  TYPE sy-datum
                                  p_pernr TYPE persno.

  DATA: l_endda   TYPE endda.

  CONCATENATE p_date(4) '1231' INTO l_endda.

  gt_result-begda = p_date.
  gt_result-endda = l_endda.

ENDFORM.                    " FILL_APPRAISAL_PERIOD
*&---------------------------------------------------------------------*
*&      Form  GET_ENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RESULT_BEGDA  text
*      -->P_LT_P0001_PERNR  text
*----------------------------------------------------------------------*
FORM get_ename  USING    p_begda      TYPE begda
                         p_pernr      TYPE persno
                         p_fieldname  TYPE string.

  FIELD-SYMBOLS: <fs_comp>  TYPE any.

  IF p_fieldname IS NOT INITIAL.
    ASSIGN (p_fieldname) TO <fs_comp>.

    IF <fs_comp> IS ASSIGNED.
      " get ename
      SELECT SINGLE ename FROM pa0001
        INTO <fs_comp>
        WHERE pernr = p_pernr
          AND endda >= p_begda
          AND begda <= p_begda.

      UNASSIGN <fs_comp>.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ENAME
*&---------------------------------------------------------------------*
*&      Form  FILL_EXCEPTION_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_EXCPT  text
*----------------------------------------------------------------------*
FORM fill_exception_list  USING    ps_excpt TYPE zthr_excpt.

  IF ps_excpt-apper IS NOT INITIAL.
    gt_result-apper = ps_excpt-apper.
    PERFORM get_ename USING gt_result-begda
                            gt_result-apper
                            'GT_RESULT-APPERNM'.
  ENDIF.

  IF ps_excpt-cori1 IS NOT INITIAL.
    gt_result-cori1 = ps_excpt-cori1.
    PERFORM get_ename USING gt_result-begda
                            gt_result-cori1
                            'GT_RESULT-CORNM1'.
  ENDIF.

  IF ps_excpt-evai2 IS NOT INITIAL.
    gt_result-evai2 = ps_excpt-evai2.
    PERFORM get_ename USING gt_result-begda
                            gt_result-evai2
                            'GT_RESULT-EVANM2'.
  ENDIF.

  IF ps_excpt-cori2 IS NOT INITIAL.
    gt_result-cori2 = ps_excpt-cori2.
    PERFORM get_ename USING gt_result-begda
                            gt_result-cori2
                            'GT_RESULT-CORNM2'.
  ENDIF.

  IF ps_excpt-evai3 IS NOT INITIAL.
    gt_result-evai3 = ps_excpt-evai3.
    PERFORM get_ename USING gt_result-begda
                            gt_result-evai3
                            'GT_RESULT-EVANM3'.
  ENDIF.

  IF ps_excpt-cori3 IS NOT INITIAL.
    gt_result-cori3 = ps_excpt-cori2.
    PERFORM get_ename USING gt_result-begda
                            gt_result-cori3
                            'GT_RESULT-CORNM3'.
  ENDIF.

  IF ps_excpt-appri IS NOT INITIAL.
    gt_result-appri = ps_excpt-appri.
    PERFORM get_ename USING gt_result-begda
                            gt_result-appri
                            'GT_RESULT-APPRNM'.
  ENDIF.

  IF ps_excpt-cori4 IS NOT INITIAL.
    gt_result-cori4 = ps_excpt-cori4.
    PERFORM get_ename USING gt_result-begda
                            gt_result-cori4
                            'GT_RESULT-CORNM4'.
  ENDIF.

  IF ps_excpt-hrtmi IS NOT INITIAL.
    gt_result-hrtmi = ps_excpt-hrtmi.
    PERFORM get_ename USING gt_result-begda
                            gt_result-hrtmi
                            'GT_RESULT-HRTMNM'.
  ENDIF.

ENDFORM.                    " FILL_EXCEPTION_LIST
*&---------------------------------------------------------------------*
*&      Form  GET_ORG_STRUC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P0001_ORGEH  text
*----------------------------------------------------------------------*
FORM get_org_struc  USING p_begda TYPE begda
                          p_orgeh TYPE orgeh.

  DATA: rt_objid    TYPE RANGE OF hrobjid WITH HEADER LINE.

  CLEAR: gt_objec[], gt_struc[].
  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = p_orgeh
      act_wegid      = 'O-O'
      act_plvar      = '01'
      act_begda      = p_begda
      act_endda      = p_begda
    TABLES
      result_objec   = gt_objec
      result_struc   = gt_struc
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR: rt_objid[].
  LOOP AT gt_objec.
    rt_objid-sign = 'I'.
    rt_objid-option = 'EQ'.
    rt_objid-low = gt_objec-objid.
    APPEND rt_objid.CLEAR rt_objid.
  ENDLOOP.

  " get Org Level
  CLEAR: gt_orglv[].
  SELECT objid
         sclas
         sobid
    FROM hrp1001
    INTO TABLE gt_orglv
    WHERE otype = 'O'
      AND objid IN rt_objid
      AND plvar = '01'
      AND rsign = 'A'
      AND relat = 'Z03'
      AND istat = '1'
      AND begda <= p_begda
      AND endda >= p_begda.
  SORT gt_orglv BY objid.

ENDFORM.                    " GET_ORG_STRUC
*&---------------------------------------------------------------------*
*&      Form  FILL_APPROVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_APPLV_SELOP  text
*      -->P_LT_APPLV_ORGLV  text
*----------------------------------------------------------------------*
FORM fill_approver  USING   p_begda TYPE begda
                            p_orgeh TYPE orgeh
                            p_orglv TYPE zdhr_orglv
                            p_clfid TYPE zdhr_clfid.

  DATA: lt_orglv      LIKE TABLE OF gt_orglv WITH HEADER LINE.

  DATA: ls_hrp1001    TYPE hrp1001.

  CHECK p_orglv <= 40.

  " read org
  CLEAR gt_struc.
  READ TABLE gt_struc WITH KEY objid = p_orgeh.
  IF sy-subrc = 0.
    WHILE sy-subrc = 0.
      " read org level
      READ TABLE gt_orglv WITH KEY objid = gt_struc-objid.
      IF sy-subrc = 0 AND gt_orglv-sobid = p_orglv.
        " get chief position
        CLEAR ls_hrp1001.
        SELECT SINGLE * FROM hrp1001
          INTO ls_hrp1001
          WHERE otype = 'O'
            AND objid = gt_orglv-objid
            AND plvar = '01'
            AND rsign = 'B'
            AND relat = '012'
            AND istat = '1'
            AND begda <= p_begda
            AND endda >= p_begda.
        IF sy-subrc = 0.
          " get chief
          SELECT SINGLE * FROM hrp1001
            INTO ls_hrp1001
            WHERE otype = 'S'
              AND objid = ls_hrp1001-sobid
              AND plvar = '01'
              AND rsign = 'A'
              AND relat = '008'
              AND istat = '1'
              AND begda <= p_begda
              AND endda >= p_begda.
          IF sy-subrc = 0.
            IF gt_result-appri IS INITIAL.
              gt_result-appri = ls_hrp1001-sobid.
              gt_result-eorg4 = gt_orglv-objid.
              gt_result-eorglv4 = gt_orglv-sobid.
              PERFORM get_ename USING p_begda
                                      gt_result-appri
                                      'GT_RESULT-APPRNM'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      " read parent org
      READ TABLE gt_struc WITH KEY seqnr = gt_struc-pdown
                          BINARY SEARCH.
    ENDWHILE.

    IF gt_result-appri IS INITIAL.
      CLEAR: lt_orglv[].
      lt_orglv[] = gt_orglv[].
      IF p_clfid EQ 'C02'.      " Manager
        DELETE lt_orglv WHERE sobid <= p_orglv.
        SORT lt_orglv BY sobid ASCENDING.
      ELSEIF p_clfid EQ 'C03'.  " Assistant Manager
        DELETE lt_orglv WHERE sobid >= p_orglv.
        SORT lt_orglv BY sobid DESCENDING.
      ENDIF.

      " read next org level
      CLEAR lt_orglv.
      READ TABLE lt_orglv INDEX 1.
      IF sy-subrc = 0.
        p_orglv = lt_orglv-sobid.
        PERFORM fill_approver USING p_begda
                                    p_orgeh
                                    p_orglv
                                    p_clfid.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " FILL_APPROVER
*&---------------------------------------------------------------------*
*&      Form  FILL_1ST_EVALUATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RESULT_BEGDA  text
*      -->P_LT_P0001_ORGEH  text
*----------------------------------------------------------------------*
FORM fill_1st_evaluator  USING    p_begda TYPE begda
                                  p_appee TYPE persno
                                  p_orgeh TYPE orgeh.

  DATA: ls_hrp1001    TYPE hrp1001.

  " read org
  CLEAR gt_struc.
  READ TABLE gt_struc WITH KEY objid = p_orgeh.
  IF sy-subrc = 0.
    WHILE sy-subrc = 0.
      " get chief position
      CLEAR ls_hrp1001.
      SELECT SINGLE * FROM hrp1001
        INTO ls_hrp1001
        WHERE otype = 'O'
          AND objid = gt_struc-objid
          AND plvar = '01'
          AND rsign = 'B'
          AND relat = '012'
          AND istat = '1'
          AND begda <= p_begda
          AND endda >= p_begda.
      IF sy-subrc = 0.
        " get chief
        SELECT SINGLE * FROM hrp1001
          INTO ls_hrp1001
          WHERE otype = 'S'
            AND objid = ls_hrp1001-sobid
            AND plvar = '01'
            AND rsign = 'A'
            AND relat = '008'
            AND istat = '1'
            AND begda <= p_begda
            AND endda >= p_begda.
        IF sy-subrc = 0 AND ls_hrp1001-sobid NE p_appee.
          IF gt_result-apper IS INITIAL.
            gt_result-apper = ls_hrp1001-sobid.
            gt_result-eorg1 = gt_struc-objid.
            " read org level
            READ TABLE gt_orglv WITH KEY objid = gt_result-eorg1
                                BINARY SEARCH.
            IF sy-subrc = 0.
              gt_result-eorglv1 = gt_orglv-sobid.
            ENDIF.
            PERFORM get_ename USING p_begda
                                    gt_result-apper
                                    'GT_RESULT-APPERNM'.
          ENDIF.
        ENDIF.
      ENDIF.

      " read parent org
      READ TABLE gt_struc WITH KEY seqnr = gt_struc-pdown
                          BINARY SEARCH.
    ENDWHILE.
  ENDIF.

ENDFORM.                    " FILL_1ST_EVALUATOR
*&---------------------------------------------------------------------*
*&      Form  FILL_COORDINATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RESULT_BEGDA  text
*      -->P_GT_RESULT_EORG1  text
*----------------------------------------------------------------------*
FORM fill_coordinator  USING    p_begda           TYPE begda
                                p_orgeh           TYPE orgeh
                                p_fieldname_id    TYPE string
                                p_fieldname_name  TYPE string.

  DATA: ls_hrp1001          TYPE hrp1001.

  FIELD-SYMBOLS: <fs_comp>  TYPE any.

  ASSIGN (p_fieldname_id) TO <fs_comp>.

  IF <fs_comp> IS ASSIGNED.
    " get coordinator position
    CLEAR ls_hrp1001.
    SELECT SINGLE * FROM hrp1001
      INTO ls_hrp1001
      WHERE otype = 'O'
        AND objid = p_orgeh
        AND plvar = '01'
        AND rsign = 'B'
        AND relat = 'Z01'
        AND istat = '1'
        AND begda <= p_begda
        AND endda >= p_begda.
    IF sy-subrc = 0.
      " get coordinator
      SELECT SINGLE * FROM hrp1001
        INTO ls_hrp1001
        WHERE otype = ls_hrp1001-sclas
          AND objid = ls_hrp1001-sobid
          AND plvar = '01'
          AND rsign = 'A'
          AND relat = '008'
          AND istat = '1'
          AND begda <= p_begda
          AND endda >= p_begda.
      IF sy-subrc = 0.
        <fs_comp> = ls_hrp1001-sobid.
        PERFORM get_ename USING p_begda
                                <fs_comp>
                                p_fieldname_name.
      ENDIF.
    ENDIF.

    UNASSIGN <fs_comp>.
  ENDIF.

ENDFORM.                    " FILL_COORDINATOR
*&---------------------------------------------------------------------*
*&      Form  FILL_FURTHER_EVALUATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_RESULT_BEGDA  text
*      -->P_GT_RESULT_EORG1  text
*      -->P_1026   text
*      -->P_1027   text
*      -->P_1028   text
*----------------------------------------------------------------------*
FORM fill_further_evaluator  USING    p_begda           TYPE begda
                                      p_orgeh           TYPE orgeh
                                      p_fieldname_id    TYPE string
                                      p_fieldname_orgeh TYPE string
                                      p_fieldname_orglv TYPE string
                                      p_fieldname_name  TYPE string.

  DATA: ls_hrp1001    TYPE hrp1001.

  FIELD-SYMBOLS: <fs_id>    TYPE any,
                 <fs_orgeh> TYPE any,
                 <fs_orglv> TYPE any.

  ASSIGN: (p_fieldname_id) TO <fs_id>,
          (p_fieldname_orgeh) TO <fs_orgeh>,
          (p_fieldname_orglv) TO <fs_orglv>.

  IF <fs_id> IS ASSIGNED AND <fs_orgeh> IS ASSIGNED.
    READ TABLE gt_struc WITH KEY objid = p_orgeh.
    IF sy-subrc = 0.
      " read parent
      READ TABLE gt_struc WITH KEY seqnr = gt_struc-pdown
                          BINARY SEARCH.
      IF sy-subrc = 0.
        " get chief position
        CLEAR ls_hrp1001.
        SELECT SINGLE * FROM hrp1001
          INTO ls_hrp1001
          WHERE otype = 'O'
            AND objid = gt_struc-objid
            AND plvar = '01'
            AND rsign = 'B'
            AND relat = '012'
            AND istat = '1'
            AND begda <= p_begda
            AND endda >= p_begda.
        IF sy-subrc = 0.
          " get chief
          SELECT SINGLE * FROM hrp1001
            INTO ls_hrp1001
            WHERE otype = ls_hrp1001-sclas
              AND objid = ls_hrp1001-sobid
              AND plvar = '01'
              AND rsign = 'A'
              AND relat = '008'
              AND istat = '1'
              AND begda <= p_begda
              AND endda >= p_begda.
          IF sy-subrc = 0.
            IF <fs_id> IS INITIAL.
              <fs_id> = ls_hrp1001-sobid.
              <fs_orgeh> = gt_struc-objid.
              " read org level
              READ TABLE gt_orglv WITH KEY objid = gt_struc-objid
                                  BINARY SEARCH.
              IF sy-subrc = 0.
                <fs_orglv> = gt_orglv-sobid.
              ENDIF.
              PERFORM get_ename USING p_begda
                                      <fs_id>
                                      p_fieldname_name.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    UNASSIGN: <fs_id>, <fs_orgeh>.
  ENDIF.

ENDFORM.                    " FILL_FURTHER_EVALUATOR
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM handle_data_changed  USING    p_er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

  DATA: ls_mod_cells  TYPE lvc_s_modi.

  CHECK p_er_data_changed->mt_good_cells IS NOT INITIAL.

  LOOP AT p_er_data_changed->mt_good_cells INTO ls_mod_cells.
    CLEAR gt_result.
    READ TABLE gt_result INDEX ls_mod_cells-row_id.
    CASE ls_mod_cells-fieldname.
        " Appraisee
      WHEN 'APPEE'.
        gt_result-appee = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-appee
                                'GT_RESULT-APPEENM'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING appee appeenm.

        " Appraiser
      WHEN 'APPER'.
        gt_result-apper = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-apper
                                'GT_RESULT-APPERNM'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING apper appernm.

        " 1st Coordinator
      WHEN 'CORI1'.
        gt_result-cori1 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-cori1
                                'GT_RESULT-CORNM1'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING cori1 cornm1.

        " 2nd Evaluator
      WHEN 'EVAI2'.
        gt_result-evai2 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-evai2
                                'GT_RESULT-EVANM2'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING evai2 evanm2.

        " 2nd Coordinator
      WHEN 'CORI2'.
        gt_result-cori2 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-cori2
                                'GT_RESULT-CORNM2'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING cori2 cornm2.

        " 3rd Evaluator
      WHEN 'EVAI3'.
        gt_result-evai3 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-evai3
                                'GT_RESULT-EVANM3'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING evai3 evanm3.

        " 3rd Coordinator
      WHEN 'CORI3'.
        gt_result-cori3 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-cori3
                                'GT_RESULT-CORNM3'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING cori3 cornm3.

        " Approver
      WHEN 'APPRI'.
        gt_result-appri = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-appri
                                'GT_RESULT-APPRNM'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING appri apprnm.

        " 4th Coordinator
      WHEN 'CORI4'.
        gt_result-cori4 = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-cori4
                                'GT_RESULT-CORNM4'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING cori4 cornm4.

        " HR Team
      WHEN 'HRTMI'.
        gt_result-hrtmi = ls_mod_cells-value.
        PERFORM get_ename USING gt_result-begda
                                gt_result-hrtmi
                                'GT_RESULT-HRTMNM'.
        MODIFY gt_result INDEX ls_mod_cells-row_id TRANSPORTING hrtmi hrtmnm.

    ENDCASE.
  ENDLOOP.

  PERFORM refresh_alv.

ENDFORM.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_rows .

  DATA: lt_roid TYPE lvc_t_roid,
        ls_roid TYPE lvc_s_roid.

  " get selected row
  CALL METHOD gr_grid->get_selected_rows
    IMPORTING
      et_row_no = lt_roid.

  CLEAR: gt_srows[].
  LOOP AT lt_roid INTO ls_roid.
    gt_srows-row_id = ls_roid-row_id.
    APPEND gt_srows.
  ENDLOOP.

ENDFORM.                    " GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  DELETE_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_rows .

  DATA: l_lines   TYPE i.

  " get selected rows
  PERFORM get_selected_rows.

  IF gt_srows[] IS INITIAL.
    MESSAGE s029 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    SORT gt_srows BY row_id DESCENDING.
    LOOP AT gt_srows.
      DELETE gt_result[] INDEX gt_srows-row_id.
    ENDLOOP.

    DESCRIBE TABLE gt_srows LINES l_lines.
    MESSAGE i030 WITH l_lines.
  ENDIF.

ENDFORM.                    " DELETE_ROWS
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv .

  DATA: ls_stbl TYPE lvc_s_stbl.

  ls_stbl-row = 'X'.
  ls_stbl-col = 'X'.

  CALL METHOD gr_grid->refresh_table_display
    EXPORTING
      is_stable      = ls_stbl
      i_soft_refresh = 'X'
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2.

ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  CHECK_APPROVER_EXCEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_APPLV  text
*      -->P_LT_CLFAJ_CLFID  text
*----------------------------------------------------------------------*
FORM check_approver_excep TABLES    pt_applve STRUCTURE zthr_applve
                          USING     p_begda
                                    p_orgeh
                                    p_clfid
                          CHANGING  p_apprv
                                    p_apprv_orgeh
                                    p_orglv.

  " read org
  CLEAR gt_struc.
  READ TABLE gt_struc WITH KEY objid = p_orgeh.
  IF sy-subrc = 0.
    WHILE sy-subrc = 0.
      " check Approver Level Exception
      READ TABLE pt_applve WITH KEY clfid = p_clfid
                                    orgeh = gt_struc-objid
                           BINARY SEARCH.
      IF sy-subrc = 0.
        p_apprv = pt_applve-apprv.
        " set approver orgeh
        SELECT SINGLE orgeh FROM pa0001
          INTO p_apprv_orgeh
          WHERE pernr = p_apprv
            AND endda >= p_begda
            AND begda <= p_begda.
*        p_apprv_orgeh = pt_applve-orgeh.
        " read org level
        READ TABLE gt_orglv WITH KEY objid = pt_applve-orgeh
                            BINARY SEARCH.
        IF sy-subrc = 0.
          p_orglv = gt_orglv-sobid.
        ENDIF.
        EXIT.
      ENDIF.

      " read parent org
      READ TABLE gt_struc WITH KEY seqnr = gt_struc-pdown
                          BINARY SEARCH.
    ENDWHILE.
  ENDIF.

ENDFORM.                    " CHECK_APPROVER_EXCEP
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save .

  DATA: lt_todo   TYPE TABLE OF zthr_todo WITH HEADER LINE.

  DATA: l_cnt     TYPE i.

  CLEAR lt_todo[].
  SELECT * FROM zthr_todo INTO TABLE lt_todo.
  IF sy-subrc = 0.
    DELETE FROM zthr_todo.
  ENDIF.

  CLEAR l_cnt.
  LOOP AT gt_result.
    l_cnt = l_cnt + 1.
    MOVE-CORRESPONDING gt_result TO lt_todo.
    INSERT zthr_todo FROM lt_todo.
    IF sy-subrc NE 0.
      MESSAGE s032 WITH gt_result-appee DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CLEAR: gt_result, lt_todo.
  ENDLOOP.

  IF l_cnt IS NOT INITIAL.
    MESSAGE i033 WITH l_cnt.
  ENDIF.

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  GET_SAVED_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_saved_data .

  DATA: lt_todo   TYPE TABLE OF zthr_todo WITH HEADER LINE.

  CLEAR lt_todo[].
  SELECT * FROM zthr_todo INTO TABLE lt_todo.
  IF sy-subrc = 0.
    CLEAR gt_result[].
    LOOP AT lt_todo.
      MOVE-CORRESPONDING lt_todo TO gt_result.
      APPEND gt_result.

      CLEAR: lt_todo, gt_result.
    ENDLOOP.

  ELSE.
    MESSAGE s027 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " GET_SAVED_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create .

  DATA: lt_record   TYPE TABLE OF zshr_adc_bdc WITH HEADER LINE.

  IF gt_result[] IS INITIAL.
    EXIT.
  ENDIF.

  " get selected rows
  PERFORM get_selected_rows.

  CLEAR lt_record[].
  IF gt_srows[] IS NOT INITIAL.
    SORT gt_srows BY row_id DESCENDING.
    LOOP AT gt_srows.
      READ TABLE gt_result INDEX gt_srows-row_id.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING gt_result TO lt_record.
        APPEND lt_record.
      ENDIF.

      CLEAR: gt_srows, gt_result.
    ENDLOOP.

  ELSE.
    LOOP AT gt_result.
      MOVE-CORRESPONDING gt_result TO lt_record.
      APPEND lt_record.

      CLEAR: gt_result, lt_record.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'ZFHR_APP_DOC_CREATE_BDC'
    TABLES
      it_record = lt_record.

  LOOP AT lt_record.
    READ TABLE gt_result WITH KEY appee = lt_record-appee.
    IF sy-subrc = 0.
      gt_result-error = lt_record-error.
      gt_result-etext = lt_record-etext.
      IF gt_result-error EQ 'E'.
        gt_result-light = '@0A@'.   " Red
      ELSE.
        gt_result-light = '@08@'.   " Green
      ENDIF.
      MODIFY gt_result INDEX sy-tabix TRANSPORTING error etext light.
    ENDIF.

    CLEAR lt_record.
  ENDLOOP.

ENDFORM.                    " CREATE
