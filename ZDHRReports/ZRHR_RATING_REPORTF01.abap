*&---------------------------------------------------------------------*
*&  Include           ZRHR_STLT_GOAL_REPORTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_init_value .
  data: l_date like sy-datum.

  IF p_year IS INITIAL.
    p_year = sy-datum(4).
    CONCATENATE p_year '0701' into l_date.
    if sy-datum < l_date.
      p_year = p_year - 1.
    endif.
  ENDIF.

ENDFORM.                    " SET_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_droplist_year .

  DATA: l_year        TYPE zdhr_year.

  g_fieldname = 'P_YEAR'.
  l_year = sy-datum(4).

  CLEAR gt_values.
  DO 10 TIMES.
    gs_value-key = l_year.
    gs_value-text = l_year.
    APPEND gs_value TO gt_values.CLEAR gs_value.
    l_year = sy-datum(4) - sy-index.
  ENDDO.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = g_fieldname
      values = gt_values.

ENDFORM.                    " SET_DROPLIST_YEAR
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_alv_100 .

  DATA: lt_exclud TYPE ui_functions.
  DATA: ls_variant TYPE disvariant.

  IF gr_cont IS INITIAL.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_cont.

    PERFORM set_layout.
    PERFORM set_fcat.
    PERFORM set_sort.

    ls_variant-report   = sy-repid.
    ls_variant-username = sy-uname.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layo
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = gt_result
        it_fieldcatalog               = gt_fcat[]
        it_sort                       = gt_sort[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    CALL METHOD gr_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_layout .

  gs_layo-zebra = 'X'.
  gs_layo-sel_mode = 'D'.
  gs_layo-CWIDTH_OPT = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_fcat .

  CLEAR: gt_fcat[].

  " TM ID
  gt_fcat-fieldname = 'APPEE'.
  gt_fcat-coltext = text-t04.
  gt_fcat-col_pos = 1.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " First Name
  gt_fcat-fieldname = 'VORNA'.
  gt_fcat-coltext = text-t05.
  gt_fcat-col_pos = 2.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Last Name
  gt_fcat-fieldname = 'NACHN'.
  gt_fcat-coltext = text-t06.
  gt_fcat-col_pos = 3.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Cost Center ID
  gt_fcat-fieldname = 'KOSTL'.
  gt_fcat-coltext = text-t02.
  gt_fcat-col_pos = 4.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Cost Center Name
  gt_fcat-fieldname = 'KTEXT'.
  gt_fcat-coltext = text-t03.
  gt_fcat-col_pos = 5.
  gt_fcat-outputlen = 20.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Classification
  gt_fcat-fieldname = 'CLFTX'.
  gt_fcat-coltext = text-t07.
  gt_fcat-col_pos = 6.
  gt_fcat-outputlen = 25.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Grade
  gt_fcat-fieldname = 'GRADE'.
  gt_fcat-coltext = text-t08.
  gt_fcat-col_pos = 7.
  gt_fcat-outputlen = 3.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Hiring Date
  gt_fcat-fieldname = 'HIRDA'.
  gt_fcat-coltext = text-t11.
  gt_fcat-col_pos = 8.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Appraisal Year
  gt_fcat-fieldname = 'ZYEAR'.
  gt_fcat-coltext = text-t12.
  gt_fcat-col_pos = 9.
  gt_fcat-outputlen = 4.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Status
  gt_fcat-fieldname = 'STATX'.
  gt_fcat-coltext = text-t27.
  gt_fcat-col_pos = 10.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Duration(From)
  gt_fcat-fieldname = 'STRDA'.
  gt_fcat-coltext = text-t13.
  gt_fcat-col_pos = 11.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Duration(To)
  gt_fcat-fieldname = 'ENDDA'.
  gt_fcat-coltext = text-t14.
  gt_fcat-col_pos = 12.
  gt_fcat-outputlen = 10.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Supervisor ID
  gt_fcat-fieldname = 'APPER'.
  gt_fcat-coltext = text-t09.
  gt_fcat-col_pos = 13.
  gt_fcat-outputlen = 6.
  gt_fcat-just = 'R'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Supervisor Name
  gt_fcat-fieldname = 'APPERNM'.
  gt_fcat-coltext = text-t10.
  gt_fcat-col_pos = 14.
  gt_fcat-outputlen = 30.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Frist Evaluatot
  gt_fcat-fieldname = 'R15'.
  gt_fcat-coltext = text-t31.
  gt_fcat-col_pos = 15.
  gt_fcat-outputlen = 30.
    gt_fcat-NO_ZERO = 'X'.
    gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 2 Evaluatot
  gt_fcat-fieldname = 'R16'.
  gt_fcat-coltext = text-t32.
  gt_fcat-col_pos = 16.
  gt_fcat-outputlen = 30.
    gt_fcat-NO_ZERO = 'X'.
        gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " 3 Evaluatot
  gt_fcat-fieldname = 'R17'.
  gt_fcat-coltext = text-t33.
  gt_fcat-col_pos = 17.
  gt_fcat-outputlen = 30.
    gt_fcat-NO_ZERO = 'X'.
        gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

 " Calculated result
  gt_fcat-fieldname = 'R20'.
  gt_fcat-coltext = text-t30.
  gt_fcat-col_pos = 18.
  gt_fcat-outputlen = 30.
  gt_fcat-NO_ZERO = 'X'.
      gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " Approver
  gt_fcat-fieldname = 'R18'.
  gt_fcat-coltext = text-t34.
  gt_fcat-col_pos = 19.
  gt_fcat-outputlen = 30.
    gt_fcat-NO_ZERO = 'X'.
        gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

  " HR Team
  gt_fcat-fieldname = 'R19'.
  gt_fcat-coltext = text-t35.
  gt_fcat-col_pos = 20.
  gt_fcat-outputlen = 30.
    gt_fcat-NO_ZERO = 'X'.
        gt_fcat-DECIMALS_O = '2'.
  APPEND gt_fcat.CLEAR: gt_fcat.

*  " Competency.
*  gt_fcat-fieldname = 'COMPR'.
*  gt_fcat-coltext = text-t36.
*  gt_fcat-col_pos = 21.
*  gt_fcat-outputlen = 20.
*  APPEND gt_fcat.CLEAR: gt_fcat.
*
*  " Short Term Goal.
*  gt_fcat-fieldname = 'SHORT'.
*  gt_fcat-coltext = text-t28.
*  gt_fcat-col_pos = 21.
*  gt_fcat-outputlen = 80.
*  APPEND gt_fcat.CLEAR: gt_fcat.
*
*  " Long Term Goal.
*  gt_fcat-fieldname = 'LONG'.
*  gt_fcat-coltext = text-t29.
*  gt_fcat-col_pos = 23.
*  gt_fcat-outputlen = 80.
*  APPEND gt_fcat.CLEAR: gt_fcat.


ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_sort .

  CLEAR gt_sort[].
  gt_sort-spos      = '1'.
  gt_sort-fieldname = 'APPEE'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '2'.
  gt_sort-fieldname = 'VORNA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '3'.
  gt_sort-fieldname = 'NACHN'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '4'.
  gt_sort-fieldname = 'KOSTL'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '5'.
  gt_sort-fieldname = 'KTEXT'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '6'.
  gt_sort-fieldname = 'CLFTX'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '7'.
  gt_sort-fieldname = 'GRADE'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '8'.
  gt_sort-fieldname = 'HIRDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '9'.
  gt_sort-fieldname = 'ZYEAR'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '10'.
  gt_sort-fieldname = 'STATX'.
  gt_sort-up        = 'X'.
  gt_sort-level     = '1'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '11'.
  gt_sort-fieldname = 'STRDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '12'.
  gt_sort-fieldname = 'ENDDA'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '13'.
  gt_sort-fieldname = 'APPER'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

  gt_sort-spos      = '14'.
  gt_sort-fieldname = 'APPERNM'.
  gt_sort-up        = 'X'.
  gt_sort-subtot    = 'X'.
  APPEND gt_sort.CLEAR  gt_sort.

*  gt_sort-spos      = '15'.
*  gt_sort-fieldname = 'COMPR'.
*  gt_sort-up        = 'X'.
*  gt_sort-subtot    = 'X'.
*  APPEND gt_sort.CLEAR  gt_sort.

ENDFORM.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .

  DATA: lt_appraisees         TYPE hap_t_hrsobid,
        lt_documents          TYPE hap_t_documents,
        lt_ktext              TYPE TABLE OF cskt WITH HEADER LINE,
        lt_p0041              TYPE TABLE OF pa0041 WITH HEADER LINE.

  DATA: ls_sel_dates          TYPE hap_s_sel_dates,
        ls_sel_status         TYPE hap_s_sel_status,
        ls_sel_with_or_witout TYPE hap_s_sel_with_or_without,
        ls_appraisees         TYPE hrsobid,
        ls_return             TYPE bapiret2,
        ls_documents          TYPE hap_s_documents.

  DATA: BEGIN OF lt_p0000 OCCURS 0,
          pernr               TYPE p0000-pernr,
          stat2               TYPE p0000-stat2,
        END OF lt_p0000.

  DATA: BEGIN OF lt_p0001 OCCURS 0,
          pernr               TYPE pa0001-pernr,
          kostl               TYPE pa0001-kostl,
          plans               TYPE pa0001-plans,
          stell               TYPE pa0001-stell,
        END OF lt_p0001.

  DATA: BEGIN OF lt_p0002 OCCURS 0,
          pernr               TYPE pa0002-pernr,
          vorna               TYPE pa0002-vorna,
          nachn               TYPE pa0002-nachn,
        END OF lt_p0002.

  DATA: BEGIN OF lt_class OCCURS 0,
          clfid               TYPE zthr_clfaj-clfid,
          jobid               TYPE zthr_clfaj-jobid,
          clftx               TYPE zthr_class-clftx,
        END OF lt_class.

  DATA: BEGIN OF lt_hirda OCCURS 0,
          pernr               TYPE pa0000-pernr,
          begda               TYPE pa0000-begda,
        END OF lt_hirda.

  DATA: l_endda               TYPE endda,
        l_index               TYPE n LENGTH 2,
        l_fieldname           TYPE string,
        l_rfc_destination     TYPE rfcdest.

  DATA: rt_kostl              TYPE RANGE OF kostl WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_dar>     TYPE any,
                 <fs_dat>     TYPE any.

  CLEAR: gt_result.

  IF p_year IS INITIAL.
    MESSAGE s026 DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    CONCATENATE p_year '1231' INTO l_endda.
  ENDIF.

 IF p_stats IS INITIAL.
    MESSAGE s000 DISPLAY LIKE 'E' with 'Please select Status'..
    EXIT.
 endif.
************************************************
*   get data
************************************************
  " get classification text
  CLEAR lt_class[].
  SELECT a~clfid
         a~jobid
         b~clftx
    FROM zthr_clfaj AS a INNER JOIN zthr_class AS b
                                 ON a~clfid = b~clfid
    INTO TABLE lt_class.
  SORT lt_class BY jobid.

  " check active
  CLEAR: lt_p0000[].
  SELECT pernr stat2
    INTO TABLE lt_p0000
    FROM pa0000
   WHERE pernr IN s_pernr
     AND endda >= l_endda
     AND begda <= l_endda
     AND stat2 = '3'.

*  CHECK lines( lt_p0000 ) > 0.
  IF lt_p0000[] IS INITIAL.
    MESSAGE s027 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SORT lt_p0000 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0000 COMPARING pernr.

  " check Employee Group, Employee Subgroup
  CLEAR lt_p0001[].
  SELECT pernr
         kostl
         plans
         stell
    INTO TABLE lt_p0001
    FROM pa0001
     FOR ALL ENTRIES IN lt_p0000
   WHERE pernr = lt_p0000-pernr
     AND endda >= l_endda
     AND begda <= l_endda
     AND persg = '1'
     AND persk IN ('U2','U3')
     AND kostl IN s_kostl.

*  CHECK lines( lt_p0001 ) > 0.
  IF lt_p0001[] IS INITIAL.
    MESSAGE s027 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  SORT lt_p0001 BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_p0001 COMPARING pernr.

  " get first name, last name
  CLEAR lt_p0002[].
  SELECT pernr vorna nachn
    INTO TABLE lt_p0002
    FROM pa0002
     FOR ALL ENTRIES IN lt_p0001
   WHERE pernr = lt_p0001-pernr
     AND endda >= l_endda
     AND begda <= l_endda.
  SORT lt_p0002 BY pernr.

  CLEAR rt_kostl[].
  LOOP AT lt_p0001.
    rt_kostl-sign = 'I'.
    rt_kostl-option = 'EQ'.
    rt_kostl-low = lt_p0001-kostl.
    APPEND rt_kostl.CLEAR rt_kostl.
  ENDLOOP.
  SORT rt_kostl BY low.
  DELETE ADJACENT DUPLICATES FROM rt_kostl COMPARING low.

  " get cost center text
  CLEAR lt_ktext[].
  SELECT *
    INTO TABLE lt_ktext
    FROM cskt
   WHERE spras = sy-langu
     AND kokrs = 'H201'
     AND kostl IN rt_kostl
     AND datbi = '99991231'.
  SORT lt_ktext BY kostl.

  " get date specifications
  CLEAR lt_p0041[].
  SELECT *
    INTO TABLE lt_p0041
    FROM pa0041
     FOR ALL ENTRIES IN lt_p0001
   WHERE pernr = lt_p0001-pernr
     AND endda >= l_endda
     AND begda <= l_endda.

  " set hire date
  CLEAR lt_hirda[].
  LOOP AT lt_p0041.
    lt_hirda-pernr = lt_p0041-pernr.
    DO 12 TIMES.
      l_index = l_index + 1.
      CONCATENATE 'LT_P0041-DAR' l_index INTO l_fieldname.
      ASSIGN (l_fieldname) TO <fs_dar>.
      IF <fs_dar> EQ 'Z1'.
        CONCATENATE 'LT_P0041-DAT' l_index INTO l_fieldname.
        ASSIGN (l_fieldname) TO <fs_dat>.
        lt_hirda-begda = <fs_dat>.
        UNASSIGN: <fs_dar>, <fs_dat>.
        EXIT.
      ENDIF.

      UNASSIGN <fs_dar>.
    ENDDO.

    APPEND lt_hirda.
    CLEAR: lt_p0041, lt_hirda, l_index.
  ENDLOOP.

  " set appraisal dates
  CLEAR ls_sel_dates.
  ls_sel_dates-validity_to_date = l_endda.

  " set appraisal status
*  ls_sel_status-ap_status_1 = 'X'.
*  ls_sel_status-ap_status_2 = 'X'.
  ls_sel_status-ap_status_3 = 'X'.
  ls_sel_status-ap_status_4 = 'X'.
  ls_sel_status-ap_status_5 = 'X'.
*  ls_sel_status-ap_status_6 = 'X'.
*  ls_sel_status-ap_status_7 = 'X'.

  CLEAR ls_sel_with_or_witout.
  ls_sel_with_or_witout-sel_display_existing = 'X'.

  " get destination
  CALL FUNCTION 'HRHAP_GET_RFC_DESTINATION'
    IMPORTING
      rfc_destination = l_rfc_destination.

  it_role_id-seq = '14'.

  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z1'.
  APPEND it_role_id.
  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z3'.
  APPEND it_role_id.
  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z5'.
  APPEND it_role_id.
  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z7'.
  APPEND it_role_id.
  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z9'.
  APPEND it_role_id.
  it_role_id-seq = it_role_id-seq + 1.
  it_role_id-role_id = 'Z0'.
  APPEND it_role_id.

  LOOP AT lt_p0001.
    " set appraisal appraisee
    ls_appraisees-plvar = '01'.
    ls_appraisees-otype = 'P'.
    ls_appraisees-sobid = lt_p0001-pernr.
    APPEND ls_appraisees TO lt_appraisees.

    " get appraisal document
    CALL FUNCTION 'HRHAP_RFC_DOCUMENT_GET_LIST'
      DESTINATION l_rfc_destination
      EXPORTING
        plan_version          = '01'
        s_sel_date            = ls_sel_dates
        s_sel_status          = ls_sel_status
        s_sel_with_or_without = ls_sel_with_or_witout
      IMPORTING
        s_return              = ls_return
      TABLES
        t_appraisees          = lt_appraisees
        t_documents           = lt_documents.

    IF ls_return-type EQ 'E'.
      CONTINUE.
    ENDIF.

    " read the most recent appraisal document
    SORT lt_documents BY ap_end_date DESCENDING.
    READ TABLE lt_documents INTO ls_documents
                            INDEX 1.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    " set cost center id
    gs_result-kostl = lt_p0001-kostl.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_result-kostl
      IMPORTING
        output = gs_result-kostl.
    " read cost center text
    READ TABLE lt_ktext WITH KEY kostl = lt_p0001-kostl
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set cost center text
      gs_result-ktext = lt_ktext-ktext.
    ENDIF.

    gs_result-statx = ls_documents-ap_status_name.

    " set appraisee id
    gs_result-appee = ls_documents-appraisee_id.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_result-appee
      IMPORTING
        output = gs_result-appee.

    " read first name, last name
    READ TABLE lt_p0002 WITH KEY pernr = lt_p0001-pernr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set appraisee first name, last name
      gs_result-vorna = lt_p0002-vorna.
      gs_result-nachn = lt_p0002-nachn.
    ENDIF.

    " read classification
    READ TABLE lt_class WITH KEY jobid = lt_p0001-stell
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set classification
      gs_result-clfid = lt_class-clfid.
      gs_result-clftx = lt_class-clftx.
    ENDIF.

    " get grade
    SELECT SINGLE grade
      INTO gs_result-grade
      FROM hrp9870
     WHERE plvar = '01'
       AND otype = 'S'
       AND objid = lt_p0001-plans
       AND istat = '1'
       AND begda <= ls_documents-ap_end_date
       AND endda >= ls_documents-ap_end_date.

    " set supervisor id, name
    gs_result-apper = ls_documents-appraiser_id.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_result-apper
      IMPORTING
        output = gs_result-apper.
    gs_result-appernm = ls_documents-appraiser_name.

    " read hiring date
    READ TABLE lt_hirda WITH KEY pernr = lt_p0001-pernr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      " set hiring date
      gs_result-hirda = lt_hirda-begda.
    ENDIF.

    " set appraisal period
    gs_result-zyear = ls_documents-ap_end_date(4).
    gs_result-strda = ls_documents-ap_start_date.
    gs_result-endda = ls_documents-ap_end_date.
    gs_result-pernr = lt_p0001-pernr.

    " get appraisal detail info
    PERFORM get_detail USING ls_documents.

    CLEAR: lt_p0001, lt_appraisees, ls_appraisees, ls_return,
           lt_documents, ls_documents, gs_result.
  ENDLOOP.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_detail  USING    ps_documents TYPE hap_s_documents.

  DATA: lt_body_elements        TYPE hap_t_body_elements,
        lt_body_tmp    TYPE TABLE OF hap_s_body_elements WITH HEADER LINE,
        lt_body_b      TYPE TABLE OF hap_s_body_elements WITH HEADER LINE,
        lt_body_cells           TYPE hap_t_body_cells,
        lt_body_cell_notes      TYPE hap_t_body_cell_notes,
        lt_body_columns         TYPE hap_t_body_columns,
        lt_hrp1001              TYPE TABLE OF hrp1001 WITH HEADER LINE,
        lt_hrp9871              TYPE TABLE OF hrp9871 WITH HEADER LINE.

  DATA: ls_appraisal_id         TYPE hap_s_appraisal_id,
        ls_body_elements        TYPE hap_s_body_elements,
        ls_body                 TYPE hap_s_body_elements,
        ls_body_b               TYPE hap_s_body_elements,
        ls_body_elements_child  TYPE hap_s_body_elements,
        ls_body_cells           TYPE hap_s_body_cells,
        ls_body_cell_notes      TYPE hap_s_body_cell_notes,
        ls_return               TYPE bal_s_msg.

  DATA: lv_short                TYPE hap_value_txt,
        lv_long                 TYPE hap_value_txt.

  DATA: lt_notes_0015           TYPE hap_t_body_cell_notes,
        lt_notes_0016           TYPE hap_t_body_cell_notes.

  DATA  ls_notes_0015           TYPE hap_s_body_cell_notes.
  DATA  ls_notes_0016           TYPE hap_s_body_cell_notes.

  DATA: rt_objid                TYPE RANGE OF hrobjid WITH HEADER LINE.

  DATA: lt_result               LIKE TABLE OF gs_result.
  DATA: ls_result               LIKE gs_result.

  DATA lv_cnt1                  TYPE sy-index.
  DATA lv_cnt2                  TYPE sy-index.
  DATA lv_cnt3                  TYPE sy-index.
  DATA lv_index                 TYPE sy-index.

  DATA: l_text(50).
  FIELD-SYMBOLS: <fs>.

  CLEAR ls_appraisal_id.
  ls_appraisal_id-appraisal_id = ps_documents-appraisal_id.
  ls_appraisal_id-part_ap_id = ps_documents-part_ap_id.

  " get appraisal detail
  CALL FUNCTION 'HRHAP_DOCUMENT_GET_DETAIL'
    EXPORTING
      plan_version      = ps_documents-plan_version
      s_appraisal_id    = ls_appraisal_id
    IMPORTING
      t_body_columns    = lt_body_columns
      t_body_elements   = lt_body_elements
      t_body_cells      = lt_body_cells
      t_body_cell_notes = lt_body_cell_notes
      s_return          = ls_return.

  IF ls_return-msgty EQ 'E'.
    EXIT.
  ENDIF.

  LOOP AT it_role_id.
    CLEAR: l_rating.

    CALL FUNCTION 'ZFHR_GET_RATING'
      EXPORTING
        i_status        = p_stats
        i_role_id       = it_role_id-role_id
      IMPORTING
*       E_ROW_IID       =
        e_rating        = l_rating
      TABLES
        t_body_elements = lt_body_elements
        t_body_cells    = lt_body_cells.
    .
    IF NOT l_rating IS INITIAL.
      CONCATENATE 'GS_RESULT-R' it_role_id-seq INTO l_text.
      ASSIGN (l_text) TO <fs>.
      <fs> = l_rating.
    ENDIF.
  ENDLOOP.


  " read VA element
*  READ TABLE lt_body_elements INTO ls_body_elements
*                              INDEX 1.
*  IF sy-subrc NE 0.
*    EXIT.
*  ENDIF.
*
*  SORT lt_body_elements   BY row_iid.
*  SORT lt_body_cells      BY row_iid column_iid.
*  SORT lt_body_cell_notes BY row_iid column_iid.
*
*  lt_body_tmp[] = lt_body_elements[].
*  lt_body_b[]   = lt_body_tmp[].
*
*  CLEAR lt_hrp9871[].
*  SELECT *
*    INTO TABLE lt_hrp9871
*    FROM hrp9871
*   WHERE plvar = '01'
*     AND otype = 'VB'
*     AND istat = '4'
*     AND begda <= sy-datum
*     AND endda >= sy-datum
*     AND igid =  '11'.
*
*  SORT lt_hrp9871 BY igid.
*
*  LOOP AT lt_hrp9871.
*    " read vb element
*    READ TABLE lt_body_elements INTO ls_body_elements
*                                WITH KEY element_id = lt_hrp9871-objid.
*    IF sy-subrc = 0.
*      gs_result-compr = ls_body_elements-name.
*      CLEAR: lt_notes_0015[], lt_notes_0016[].
*      LOOP AT lt_body_tmp  INTO ls_body
*                          WHERE row_iid = ls_body_elements-child.
*
*        "Short Term Goal.
*        READ TABLE lt_body_cells INTO ls_body_cells
*                                 WITH KEY row_iid = ls_body-row_iid
*                                          column_iid = '0015'
*                                 BINARY SEARCH.
*        IF sy-subrc = 0.
*          LOOP AT lt_body_cell_notes  INTO ls_body_cell_notes
*                                     WHERE row_iid = ls_body-row_iid
*                                       AND column_iid = '0015'.
*            ADD 1 TO lv_cnt1.
*            APPEND ls_body_cell_notes TO lt_notes_0015.
*          ENDLOOP.
*        ENDIF.
*
*        "Long Term Goal
*        LOOP AT lt_body_b  INTO ls_body_b
*                          WHERE row_iid = ls_body_elements-child.
*          READ TABLE lt_body_cells INTO ls_body_cells
*                                   WITH KEY row_iid = ls_body_b-brother
*                                            column_iid = '0015'
*                                   BINARY SEARCH.
*          IF sy-subrc = 0.
*            LOOP AT lt_body_cell_notes  INTO ls_body_cell_notes
*                                       WHERE row_iid = ls_body_b-brother
*                                         AND column_iid = '0015'.
*              ADD 1 TO lv_cnt2.
*              APPEND ls_body_cell_notes TO lt_notes_0016.
*            ENDLOOP.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.
*
*  CLEAR lt_result[].
*  IF lv_cnt1 = 0 AND lv_cnt2 = 0.
    APPEND gs_result TO gt_result.
*
*  ELSE.
*    IF lv_cnt1 >= lv_cnt2 .
*      DO lv_cnt1 TIMES.
*        APPEND gs_result TO lt_result.
*      ENDDO.
*    ELSE.
*      DO lv_cnt2 TIMES.
*        APPEND gs_result TO lt_result.
*      ENDDO.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " GET_DETAIL
*&---------------------------------------------------------------------*
*&      Form  set_droplist_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_droplist_status.

*    08/14/2013 - T00306  Start
  DATA: lt_values     TYPE vrm_values,
        ls_value      LIKE LINE OF lt_values,
        l_fieldname   TYPE vrm_id,
        l_staus       TYPE hap_ap_status.

  DATA: lt_dd07t TYPE TABLE OF dd07t WITH HEADER LINE.

  l_fieldname = 'P_STATS'.

  SELECT *
     INTO TABLE lt_dd07t
     FROM dd07t
    WHERE domname = 'HAP_AP_STATUS'
      AND ddlanguage = sy-langu
      AND as4local = 'A'
      AND domvalue_l IN ('3','4','5').

  CLEAR lt_values.
  LOOP AT lt_dd07t.
    ls_value-key = lt_dd07t-domvalue_l.
    ls_value-text = lt_dd07t-ddtext.
    APPEND ls_value TO lt_values.
    CLEAR ls_value.
  ENDLOOP.

  SORT lt_values BY key ASCENDING.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_fieldname
      values = lt_values.
*    08/14/2013 - T00306  End

ENDFORM.                    "set_droplist_status
