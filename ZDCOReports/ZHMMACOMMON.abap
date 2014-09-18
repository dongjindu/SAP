*&---------------------------------------------------------------------*
*&  Include           ZDHMMACOMMON
*&---------------------------------------------------------------------*

CONSTANTS: c_vkorg TYPE vkorg       VALUE 'HUS1',
           c_bukrs LIKE t001-bukrs  VALUE 'H201',   " Company code
           c_land1 TYPE land1       VALUE 'US',     " Country key
           c_spras TYPE sy-langu    VALUE 'E'.

* SAPGUI_PROGRESS_INDICATOR
CONSTANTS : c_text(35) VALUE 'Processing...............',
            c_percentage(3) TYPE n VALUE '30'.

CONSTANTS: c_confirm(60) TYPE c VALUE 'Confirmation',
           c_coal(60) TYPE c VALUE 'Close out an Allocation'.

CONSTANTS: c_save_text(400) TYPE c VALUE 'Do you want to save the data?',
           c_dele_text(400) TYPE c VALUE 'Do you want to delete the data?'.


DATA: g_whsale TYPE char01.
*      G_NEWCAR TYPE CHAR01.

** BDC table
DATA: it_bdcdata LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      it_msgtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      gs_options LIKE ctu_params.


*&---------------------------------------------------------------------*
*&      Form  GET_FIXED_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT     text
*      -->P_DOMNAME  text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM get_fixed_value USING p_domname p_value
                           p_error p_text.

  DATA: l_ddtext LIKE dd07t-ddtext,
        l_domvalue_l LIKE dd07t-domvalue_l.

  CLEAR:p_text, p_error.
  IF p_value <> space.
    l_domvalue_l = p_value.
    SELECT SINGLE ddtext INTO l_ddtext
      FROM dd07t
     WHERE domname = p_domname
       AND ddlanguage = sy-langu
       AND as4local = 'A'
       AND domvalue_l = p_value.
    IF sy-subrc = 0.
      p_text = l_ddtext.
    ELSE.
      p_error = 'X'.
    ENDIF.
  ELSE.
    CLEAR p_text.
  ENDIF.

ENDFORM.                    "GET_FIXED_VALUE
*&---------------------------------------------------------------------*
*&      Form  DELETE_LOG_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STRUCT   text
*----------------------------------------------------------------------*
FORM delete_log_info USING p_struct.

  " == Data for Result
  FIELD-SYMBOLS: <fs_outtab> TYPE STANDARD TABLE, " Result data table
                 <fs_outwa>,                      " Result structure
                 <fs_outdata>,                    " Result data
                 <fs_outdata2>.

  ASSIGN p_struct TO <fs_outwa>.

  ASSIGN ('<fs_outwa>-mandt') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-erdat') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-ernam') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-erzet') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-aenam') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-aedat') TO <fs_outdata>.
  CLEAR <fs_outdata>.
  ASSIGN ('<fs_outwa>-aezet') TO <fs_outdata>.
  CLEAR <fs_outdata>.

ENDFORM.                    "DELETE_LOG_INFO
*&---------------------------------------------------------------------*
*&      Form  CHECK_QUESTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_ucomm
                      CHANGING p_answer.

  DATA: l_title(60) TYPE c,
        l_text(400) TYPE c.

  CLEAR p_answer.

  l_title = c_confirm.
  CASE p_ucomm.
    WHEN 'SAVE'.
      l_text = c_save_text.
    WHEN 'DELE'.
      l_text = c_dele_text.
  ENDCASE.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = c_confirm
      text_question         = l_text
      text_button_1         = 'Yes'
      icon_button_1         = ' '
      text_button_2         = 'No'
      icon_button_2         = ' '
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

ENDFORM.                    " CHECK_QUESTION
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TITLE    text
*      -->P_TEXT1    text
*      -->P_TEXT2    text
*      -->C_ANSWER   text
*----------------------------------------------------------------------*
FORM popup_to_confirm_step     USING p_title
                                     p_text1
                                     p_text2
                            CHANGING c_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'A'
      textline1      = p_text1
      textline2      = p_text2
      titel          = p_title
      cancel_display = 'X'
    IMPORTING
      answer         = c_answer.

ENDFORM.                    "POPUP_TO_CONFIRM_STEP
*&---------------------------------------------------------------------*
*&      Form  GET_NAME_FROM_CUSTOMER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_name_from_customer USING p_kunnr
                         CHANGING p_name.
  DATA: l_name(5).

  CLEAR:p_name.

  IF p_kunnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM conversion_exit_alpha_input CHANGING p_kunnr.

  l_name = 'NAME1'.
*  CASE P_TYPE.
*    WHEN 1.
*      L_NAME = 'NAME1'.
*    WHEN 2.
*      L_NAME = 'NAME2'.
*    WHEN 3.
*      EXIT.
*  ENDCASE.

  CLEAR p_name.

  SELECT SINGLE (l_name) INTO p_name
         FROM kna1
         WHERE kunnr = p_kunnr.

ENDFORM.                    "GET_NAME_FROM_CUSTOMER
*&---------------------------------------------------------------------*
*&      Form  CHECK_LINFR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KUNNR    text
*      -->C_ERROR    text
*----------------------------------------------------------------------*
FORM check_kunnr USING p_kunnr
              CHANGING c_error.

  DATA: l_name LIKE kna1-name1.

  IF p_kunnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM conversion_exit_alpha_input CHANGING p_kunnr.

  SELECT SINGLE name1 INTO l_name
         FROM kna1
         WHERE kunnr = p_kunnr.
  IF sy-subrc <> 0.
    c_error = 'X'.
  ENDIF.

ENDFORM.                    "CHECK_LINFR
*&---------------------------------------------------------------------*
*&      Form  GET_NAME_FROM_COMPANY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPE     text
*      -->P_LIFNR    text
*      -->P_NAME     text
*----------------------------------------------------------------------*
FORM get_name_from_company USING p_lifnr
                        CHANGING p_name.
  DATA: l_name(5).

  CLEAR: p_name.

  IF p_lifnr IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR p_name.

  PERFORM conversion_exit_alpha_input CHANGING p_lifnr.

*  CASE P_TYPE.
*    WHEN 1.
*      L_NAME = 'NAME1'.
*    WHEN 2.
*      L_NAME = 'NAME2'.
*    WHEN 3.
*      EXIT.
*  ENDCASE.
  l_name = 'NAME1'.

  SELECT SINGLE (l_name) INTO p_name
         FROM lfa1
         WHERE lifnr = p_lifnr.

*  PERFORM CONVERSION_EXIT_ALPHA_OUTPUT CHANGING P_LIFNR.

*  IF SY-SUBRC <> 0.
*    P_ERROR = 'X'.
*  ENDIF.

ENDFORM.                    "GET_NAME_FROM_COMPANY
*&---------------------------------------------------------------------*
*&      Form  GET_USER_NAME_FROM_ADDRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_USER     text
*      -->P_USERNM   text
*----------------------------------------------------------------------*
FORM get_user_name_from_address USING p_user
                             CHANGING p_usernm.

  IF p_user IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR p_usernm.

  SELECT SINGLE name_first INTO p_usernm
    FROM user_addr
    WHERE bname = p_user.

ENDFORM.                    "GET_USER_NAME_FROM_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_LINFR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIFNR    text
*      -->C_ERROR    text
*----------------------------------------------------------------------*
FORM check_linfr USING p_lifnr
              CHANGING c_error.

  DATA: l_name LIKE kna1-name1.

  IF p_lifnr IS INITIAL.
    EXIT.
  ENDIF.

  PERFORM conversion_exit_alpha_input CHANGING p_lifnr.

  SELECT SINGLE name1 INTO l_name
         FROM lfa1
         WHERE lifnr = p_lifnr.
  IF sy-subrc <> 0.
    c_error = 'X'.
  ENDIF.

ENDFORM.                    "CHECK_LINFR
*&---------------------------------------------------------------------*
*&      Form  CREATE_CUSTOM_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PI_CONT_NAME  text
*      -->PR_CUCTNR     text
*      -->PR_ALVGRD     text
*----------------------------------------------------------------------*
FORM create_custom_container USING pi_cont_name
                                   pr_cuctnr TYPE REF TO cl_gui_custom_container
                                   pr_alvgrd TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT pr_cuctnr
    EXPORTING
      container_name              = pi_cont_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = sy-repid
        txt2  = sy-subrc
        txt1  = 'The control could not be created'(510).
  ENDIF.

*.. Create an instance of alv control
  CREATE OBJECT pr_alvgrd
    EXPORTING
      i_parent      = pr_cuctnr
      i_appl_events = 'X'.

ENDFORM.                    "create_custom_container
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_SORT    text
*      -->P_FIELD    text
*      -->P_UP       text
*      -->P_SPOS     text
*      -->P_SUB      text
*----------------------------------------------------------------------*
FORM build_sort_value USING pt_sort TYPE lvc_t_sort
                            p_field
                            p_up
                            p_spos
                            p_sub.

  DATA: ls_sort TYPE lvc_s_sort.

  ls_sort-fieldname = p_field.
  ls_sort-spos      = p_spos.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_sub.
  ls_sort-group     = 'UL'.

  APPEND ls_sort TO pt_sort.

ENDFORM.                    "build_sort_value
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GRID     text
*      -->P_INDEX    text
*      -->P_LCNT     text
*----------------------------------------------------------------------*
FORM get_selected_row  USING p_grid TYPE REF TO cl_gui_alv_grid
                             p_index
                             p_lcnt.

  DATA: lt_idxrow TYPE lvc_t_row,
        lt_rowno  TYPE lvc_t_roid,
        ls_rowno  TYPE lvc_s_roid.

  DATA: lt_cell TYPE lvc_t_cell,
        ls_cells TYPE lvc_s_cell.

  CALL METHOD p_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_idxrow
      et_row_no     = lt_rowno.

  IF NOT lt_rowno[] IS INITIAL.
    READ TABLE lt_rowno INTO ls_rowno INDEX 1.
    IF sy-subrc = 0.
      p_index = ls_rowno-row_id.
      DESCRIBE TABLE lt_rowno LINES p_lcnt.
      DELETE lt_rowno WHERE row_id < 1.
      EXIT.
    ENDIF.
  ENDIF.

*  CALL METHOD P_GRID->GET_SELECTED_CELLS
*    IMPORTING
*      ET_CELL = LT_CELL.
*
*  READ TABLE LT_CELL INTO LS_CELLS INDEX 1.
*  IF SY-SUBRC = 0.
*    P_INDEX  = LS_CELLS-ROW_ID-INDEX.
*  ELSE.
*    EXIT.
*  ENDIF.

ENDFORM.                    "GET_SELECTED_ROW
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_ROWNO   text
*      -->P_INDEX    text
*      -->P_LCNT     text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES pt_rowno
                       USING  p_grid TYPE REF TO cl_gui_alv_grid
                              p_index p_lcnt.

  DATA: lt_idxrow TYPE lvc_t_row,
        lt_rowno  TYPE lvc_t_roid,
        ls_rowno  TYPE lvc_s_roid.

  DATA: lt_cell TYPE lvc_t_cell,
        ls_cells TYPE lvc_s_cell.


  CALL METHOD p_grid->get_selected_rows
    IMPORTING
      et_index_rows = lt_idxrow
      et_row_no     = lt_rowno.


  IF NOT lt_rowno[] IS INITIAL.
    READ TABLE lt_rowno INTO ls_rowno INDEX 1.
    IF sy-subrc = 0.
      DESCRIBE TABLE lt_rowno LINES p_lcnt.
      p_index = ls_rowno-row_id.

      pt_rowno[] = lt_rowno[].
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    "get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  CHECK_SELECTED_INDEX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INDEX    text
*      -->P_CNT      text
*----------------------------------------------------------------------*
FORM check_falv_selected_index   TABLES pt_table
                               CHANGING c_index.

  FIELD-SYMBOLS: <fs_outtab> TYPE STANDARD TABLE,
                 <fs_outwa>,
                 <fs_value>.
  DATA: ls_line TYPE REF TO data,
        l_tabix LIKE sy-tabix.

  CLEAR: c_index.

  ASSIGN pt_table TO <fs_outtab>.
  CREATE DATA ls_line LIKE LINE OF <fs_outtab>.
  ASSIGN ls_line->* TO <fs_outwa>.

  LOOP AT <fs_outtab> INTO <fs_outwa>.
    l_tabix = sy-tabix.
    ASSIGN ('<FS_OUTWA>-CHECK') TO <fs_value>.
    IF <fs_value> = 'X'.
      c_index = sy-tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "CHECK_SELECTED_INDEX

*&---------------------------------------------------------------------*
*&      Form  SET_ASSIGN_STYLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STYLE       text
*      -->P_STRUCT      text
*      -->P_FIELD       text
*      -->P_STYLEFIELD  text
*----------------------------------------------------------------------*
FORM set_assign_style USING p_style
                            p_struct
                            p_editfield
                            p_stylefield.

  DATA: ls_edit TYPE lvc_s_styl,
        lt_edit TYPE lvc_t_styl.
  DATA: ls_line TYPE REF TO data.
  DATA: l_fsfield TYPE fieldname.

  FIELD-SYMBOLS: <fs_outwa>,   " Structure
                 <fs_styletb> TYPE lvc_t_styl. " Field value

  ASSIGN p_struct TO <fs_outwa>.
  CONCATENATE '<FS_OUTWA>-' p_stylefield INTO l_fsfield.
  ASSIGN (l_fsfield) TO <fs_styletb>.

  ls_edit-fieldname = p_editfield.
  CASE p_style.
    WHEN 'HOTSPOT'.
      ls_edit-style = cl_gui_alv_grid=>mc_style_hotspot.
    WHEN 'HOTSPOT_NO'.
      ls_edit-style = cl_gui_alv_grid=>mc_style_hotspot_no.
    WHEN 'DISABLED'.
      ls_edit-style = cl_gui_alv_grid=>mc_style_disabled.
    WHEN 'ENABLED'.
      ls_edit-style = cl_gui_alv_grid=>mc_style_enabled.
  ENDCASE.
  ls_edit-style2 = space.
  ls_edit-style3 = space.
  ls_edit-style4 = space.
  ls_edit-maxlen = 10.

  INSERT ls_edit INTO TABLE <fs_styletb>.

ENDFORM.                    "SET_ASSIGN_STYLE
*&---------------------------------------------------------------------*
*&      Form  TRANSLATE_FIELD_TO_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->C_RANGE    text
*----------------------------------------------------------------------*
FORM translate_field_to_range TABLES t_range
                               USING p_field.

  CLEAR: t_range, t_range[].

  FIELD-SYMBOLS: <fs_outtab> TYPE STANDARD TABLE,
                 <fs_outwa>,
                 <fs_outdata>.

  CHECK NOT p_field IS INITIAL.

  DATA: ls_line TYPE REF TO data.
  ASSIGN t_range[] TO <fs_outtab>.

  CREATE DATA ls_line LIKE LINE OF <fs_outtab>.
  ASSIGN ls_line->* TO <fs_outwa>.

  <fs_outwa> = 'IEQ'.
  ASSIGN ('<FS_OUTWA>-LOW') TO <fs_outdata>.
  <fs_outdata> = p_field.
  APPEND <fs_outwa> TO <fs_outtab>.

ENDFORM.                    "TRANSLATE_FIELD_TO_RANGE
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELD_TO_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->T_RANGE    text
*      -->P_FIELD    text
*----------------------------------------------------------------------*
FORM append_field_to_range TABLES t_range
                            USING p_type p_field.

  FIELD-SYMBOLS: <fs_outtab> TYPE STANDARD TABLE,
                 <fs_outwa>,
                 <fs_outdata>.

  DATA: ls_line TYPE REF TO data.

  IF p_field IS INITIAL.
    EXIT.
  ENDIF.

  ASSIGN t_range[] TO <fs_outtab>.

  CREATE DATA ls_line LIKE LINE OF <fs_outtab>.
  ASSIGN ls_line->* TO <fs_outwa>.

  CONCATENATE p_type 'EQ' INTO <fs_outwa>.
  ASSIGN ('<FS_OUTWA>-LOW') TO <fs_outdata>.
  <fs_outdata> = p_field.
  APPEND <fs_outwa> TO <fs_outtab>.

ENDFORM.                    "APPEND_FIELD_TO_RANGE
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM conversion_exit_alpha_input CHANGING p_value.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.
ENDFORM.                    "conversion_exit_alpha_input

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_outPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM conversion_exit_alpha_output CHANGING p_value.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_value
    IMPORTING
      output = p_value.
ENDFORM.                    "CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_DATE_OF_THE_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATUM      text
*      -->P_LAST_DATE  text
*----------------------------------------------------------------------*
FORM get_last_date_of_the_month USING p_datum     TYPE datum
                             CHANGING p_last_date TYPE sydatum.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = p_datum
    IMPORTING
      last_day_of_month = p_last_date.

ENDFORM.                    "GET_LAST_DATE_OF_THE_MONTH
*&---------------------------------------------------------------------*
*&      Form  convert_to_tstmp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATLO    text
*      -->P_TIMLO    text
*      -->P_TSTMP    text
*----------------------------------------------------------------------*
FORM convert_to_date  USING p_tstmp
                   CHANGING c_datlo.

  CLEAR: c_datlo.

  IF p_tstmp IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION 'VELO03_CONVERT_FROM_TIMESTAMP'
    EXPORTING
      timestamp_iv = p_tstmp
    IMPORTING
      datlo_ev     = c_datlo.

ENDFORM.                    " CONVERT_TO_TSTMP
*&---------------------------------------------------------------------*
*&      Form  convert_long_to_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TSTMP    text
*      -->C_DATLO    text
*----------------------------------------------------------------------*
FORM convert_long_to_date  USING p_tstmp
                        CHANGING c_datlo
                                 c_timlo.

  CLEAR: c_datlo.

  IF p_tstmp IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION 'VELO03_CONVERT_FROM_TIMESTAMP'
    EXPORTING
      long_timestamp_iv = p_tstmp
    IMPORTING
      datlo_ev          = c_datlo
      timlo_ev          = c_timlo.

ENDFORM.                    "convert_long_to_date
*&---------------------------------------------------------------------*
*&      Form  convert_to_tstmp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATLO    text
*      -->P_TIMLO    text
*      -->P_TSTMP    text
*----------------------------------------------------------------------*
FORM convert_to_tstmp  USING    p_datlo
                                p_timlo
                       CHANGING p_tstmp.

  DATA: l_tstmp TYPE tzonref-tstamps.

  CLEAR: l_tstmp, p_tstmp.

  IF p_datlo IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION 'VELO03_CONVERT_INTO_TIMESTAMP'
    EXPORTING
      datlo_iv     = p_datlo
      timlo_iv     = p_timlo
      tzone_iv     = sy-zonlo
    IMPORTING
      timestamp_ev = l_tstmp.

  p_tstmp = l_tstmp.

ENDFORM.                    "convert_to_tstmp
*&---------------------------------------------------------------------*
*&      Form  convert_long_to_tstmp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATLO    text
*      -->P_TIMLO    text
*      -->P_TSTMP    text
*----------------------------------------------------------------------*
FORM convert_long_to_tstmp  USING p_datlo
                                  p_timlo
                         CHANGING p_tstmp.

  DATA: l_tstmp TYPE tzonref-tstampl.

  CLEAR: l_tstmp, p_tstmp.

  IF p_datlo IS INITIAL.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZKUS_MM_CONVERT_DATE_TO_TSTMP'
    EXPORTING
      datum = p_datlo
      timlo = p_timlo
    IMPORTING
      tstmp = l_tstmp.

  p_tstmp = l_tstmp.

ENDFORM.                    "convert_long_to_tstmp
*&---------------------------------------------------------------------*
*&      Form  REMOVE_NON_NUMERIC_CHAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_INPUT    text
*      -->P_OUTPUT   text
*----------------------------------------------------------------------*
FORM remove_non_numeric_char USING    p_input
                             CHANGING p_output.

  DATA: l_number_in  TYPE char40,
        l_number_out TYPE char40,
        l_allowed_char(37) VALUE '0123456789.'.

  MOVE p_input TO l_number_in.

  CLEAR l_number_out.
  CONDENSE l_number_in NO-GAPS.
  TRANSLATE l_number_in TO UPPER CASE.

  IF l_number_in CN l_allowed_char.
    DO.
      IF l_number_in(1) NE space.
        IF l_number_in(1) CO l_allowed_char.
          CONCATENATE l_number_out l_number_in(1) INTO l_number_out.
        ENDIF.
        SHIFT l_number_in.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    MOVE l_number_out TO p_output.
  ELSE.
    MOVE l_number_in TO p_output.
  ENDIF.
ENDFORM.                    " REMOVE_NON_NUMERIC_CHAR
*&---------------------------------------------------------------------*
*&      Form  get_plant_with_bukrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_WERKS   text
*----------------------------------------------------------------------*
FORM get_plant_with_bukrs  TABLES pr_werks STRUCTURE range_werks
                            USING p_bukrs.

  CLEAR: pr_werks, pr_werks[].

  SELECT bwkey INTO pr_werks-low
    FROM t001k
    WHERE bukrs = p_bukrs.

    pr_werks-sign = 'I'.
    pr_werks-option = 'EQ'.
    APPEND pr_werks.
  ENDSELECT.

ENDFORM.                    "get_plant_with_bukrs
*&---------------------------------------------------------------------*
*&      Form  is_numeric
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_VALUE    text
*      -->C_RESULT   text
*----------------------------------------------------------------------*
FORM is_numeric  USING p_value
              CHANGING c_result.

  DATA: l_value TYPE char30.

  WRITE p_value TO l_value CURRENCY 'USD'.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
    EXPORTING
      input      = l_value
      internal   = 'X'
    IMPORTING
      output     = l_value
    EXCEPTIONS
      no_numeric = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
    c_result = 'X'.
  ELSE.
    CLEAR c_result.
  ENDIF.

ENDFORM.                    "is_numeric
*&---------------------------------------------------------------------*
*&      Form  remove_point
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_VALUE    text
*      -->C_DATE     text
*----------------------------------------------------------------------*
FORM remove_point  USING    u_value
                   CHANGING c_date.
  DATA : l_number(30),
         l_number_out(30),
         l_length     TYPE i,
         l_length2    TYPE i,
         l_one.

  MOVE u_value TO l_number.
  IF l_number CO '0123456789'.
    c_date = u_value.
    EXIT.
  ELSE.
    l_length = STRLEN( l_number ).
    DO l_length TIMES.
      l_one = l_number(1).
      IF l_one BETWEEN '0' AND '9'.
        CONCATENATE l_number_out l_one INTO l_number_out.
      ELSE.

      ENDIF.
      SHIFT l_number.
    ENDDO.
  ENDIF.
  MOVE l_number_out TO c_date.
ENDFORM.                    "remove_point
*&---------------------------------------------------------------------*
*&      Form  remove_other_char
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_VALUE    text
*      -->C_VAL      text
*----------------------------------------------------------------------*
FORM remove_other_char  USING    u_value
                        CHANGING c_val.
  DATA : l_number(30),
         l_number_out(30),
         l_length     TYPE i,
         l_length2    TYPE i,
         l_one.

  MOVE u_value TO l_number.
  IF l_number CO '0123456789.'.
    c_val = u_value.
    EXIT.
  ELSE.
    l_length = STRLEN( l_number ).
    DO l_length TIMES.
      l_one = l_number(1).
      IF l_one BETWEEN '0' AND '9'.
        CONCATENATE l_number_out l_one INTO l_number_out.
      ELSEIF l_one = '.'.
        CONCATENATE l_number_out l_one INTO l_number_out.
      ELSE.

      ENDIF.
      SHIFT l_number.
    ENDDO.
  ENDIF.
  MOVE l_number_out TO c_val.
ENDFORM.                    "remove_other_char
*&---------------------------------------------------------------------*
*&      Form  get_status_old
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_STATU   text
*      -->P_CNTRL    text
*      -->P_ACTION   text
*----------------------------------------------------------------------*
FORM get_status_old TABLES pt_statu STRUCTURE range_c4
                    USING  p_cntrl
                           p_action.

  pt_statu = 'IEQ'.

  SELECT statu_old INTO pt_statu-low
    FROM cvlc04
    WHERE cntrl  = p_cntrl
      AND aktion = p_action.

    APPEND pt_statu.
  ENDSELECT.

  IF pt_statu[] IS INITIAL.
    APPEND pt_statu.
  ENDIF.

ENDFORM.                    "get_status_old
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNBEGIN text
*      -->P_NAME     text
*      -->P_VALUE    text
*----------------------------------------------------------------------*
FORM dynpro  USING p_dynbegin
                   p_name
                   p_value.

  CLEAR it_bdcdata.

  IF p_dynbegin = 'X'.
    it_bdcdata-program  = p_name.
    it_bdcdata-dynpro   = p_value.
    it_bdcdata-dynbegin = 'X'.
  ELSE.
    it_bdcdata-fnam = p_name.
    it_bdcdata-fval = p_value.
  ENDIF.

  APPEND it_bdcdata.

ENDFORM.                    "dynpro
*&---------------------------------------------------------------------*
*&      Form  F4_COMMON_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_DATA      text
*      -->P_FIELDNAME  text
*----------------------------------------------------------------------*
FORM f4_common_screen USING pt_table TYPE STANDARD TABLE
                            p_reffield
                            p_field
                            p_title.

  DATA: lv_repid LIKE sy-repid,
        lv_dynnr LIKE sy-dynnr,
        lv_title(30).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = p_reffield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = p_field
      window_title    = p_title
      value_org       = 'S'
    TABLES
      value_tab       = pt_table
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDFORM.                    "F4_COMMON_screen
*&---------------------------------------------------------------------*
*&      Form  F4_COMMON_ACGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_YEAR     text
*      -->P_MODEL    text
*----------------------------------------------------------------------*
FORM f4_common_online USING pt_table TYPE STANDARD TABLE
                            p_reffield
                            p_field
                            p_title.

  DATA: l_fname(20) TYPE c,
        lt_return LIKE ddshretval OCCURS 0 WITH HEADER LINE,
        l_display TYPE ddbool_d.

  DATA: lt_value TYPE TABLE OF dynpread,
        ls_value LIKE LINE OF lt_value.

  FIELD-SYMBOLS: <fs_any1> TYPE ANY.

  GET CURSOR FIELD l_fname.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = p_reffield
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      window_title    = p_title
      display         = l_display
      value_org       = 'S'
    TABLES
      value_tab       = pt_table
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF NOT lt_return[] IS INITIAL.
    READ TABLE lt_return INDEX 1.
    ASSIGN (l_fname) TO <fs_any1>.
    <fs_any1> = lt_return-fieldval.
  ENDIF.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_value.

ENDFORM.                    "F4_COMMON_color
*&---------------------------------------------------------------------*
*&      Form  CHECK_VALIDATE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATE     text
*      -->C_RLST     text
*----------------------------------------------------------------------*
FORM check_validate_date USING p_date CHANGING c_error.

* Validate date - An invalid date is caused by using the cancel
* button on the calendar
  CLEAR c_error.
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = p_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    c_error = 'X'.
*   User has cancelled the calendar
    EXIT.
  ENDIF.

ENDFORM.                    "CHECK_VALIDATE_DATE
*&---------------------------------------------------------------------*
*&      Form  call_change_history_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_change_history_function USING p_tname
                                        ps_struct
                                        p_del.
  DATA: l_message TYPE msgtxt,
        l_type    TYPE msgtyp.

* Header data
  CALL FUNCTION 'ZHUS_SD_SAVE_HISTORY'
    EXPORTING
      i_tablename = p_tname
      i_structure = ps_struct
      i_del       = p_del
    IMPORTING
      e_message   = l_message
      e_type      = l_type.

ENDFORM.                    "call_change_history_function
*&---------------------------------------------------------------------*
*&      Form  message_text_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_MSGID    text
*      -->U_MSGNR    text
*      -->U_MSGV1    text
*      -->U_MSGV2    text
*      -->U_MSGV3    text
*      -->U_MSGV4    text
*      -->C_TEXT     text
*----------------------------------------------------------------------*
FORM message_text_build  USING u_msgid u_msgnr
                               u_msgv1 u_msgv2 u_msgv3 u_msgv4
                      CHANGING c_text.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = u_msgid
      msgnr               = u_msgnr
      msgv1               = u_msgv1
      msgv2               = u_msgv2
      msgv3               = u_msgv3
      msgv4               = u_msgv4
    IMPORTING
      message_text_output = c_text.

ENDFORM.                    "message_text_build
*&---------------------------------------------------------------------*
*&      Form  SET_PO_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZOQTY    text
*      -->P_ZCQTY    text
*      -->P_ZPSTA    text
*----------------------------------------------------------------------*
FORM set_po_status  USING    p_zoqty
                             p_zcqty
                    CHANGING p_zpsta.

  CHECK p_zcqty > 0.

  IF p_zoqty > p_zcqty.
    p_zpsta = 'PC'.
  ELSEIF p_zoqty = p_zcqty.
    p_zpsta = 'CM'.
  ENDIF.

ENDFORM.                    " SET_PO_STATUS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HISTORY_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DOC      text
*      -->P_TABLE    text
*----------------------------------------------------------------------*
FORM display_history_function USING p_table p_doc p_itm.

  DATA: l_message TYPE msgtxt,
        l_type    TYPE msgtyp.
  RANGES: r_docno FOR shp_vbeln_range,
          r_itmno FOR shp_vbeln_range.

  IF sy-subrc = 0.
    CLEAR r_docno.
    r_docno-sign   = 'I'.
    r_docno-option = 'EQ'.
    r_docno-low    = p_doc.
    APPEND r_docno.

    CLEAR r_docno.
    r_itmno-sign   = 'I'.
    r_itmno-option = 'EQ'.
    r_itmno-low    = p_itm.
    APPEND r_itmno.

    CALL FUNCTION 'ZHUS_SD_DISPLAY_HISTORY'
      EXPORTING
        i_tablename = p_table
      IMPORTING
        e_message   = l_message
        e_type      = l_type
      TABLES
        t_docno     = r_docno
        t_itmno     = r_itmno.

  ENDIF.

ENDFORM.                    "DISPLAY_HISTORY_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  OPEN_DEALER_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABLE    text
*      -->P_FIELD    text
*      -->P_DISPLAY  text
*      -->c_range   text
*----------------------------------------------------------------------*
FORM open_dealer_range USING p_table p_field p_display
                    CHANGING c_range TYPE STANDARD TABLE.

  DATA: lt_field LIKE rstabfield OCCURS 0 WITH HEADER LINE,
        ls_opt LIKE rsoptions.
  DATA: l_disp TYPE char01.
  DATA: l_helpfield LIKE rsscr-dbfield.

  MOVE: p_field TO lt_field-fieldname,
        p_table TO lt_field-tablename.
  APPEND lt_field.
  CLEAR lt_field.

  CONCATENATE p_table p_field INTO l_helpfield SEPARATED BY '-'.

  MOVE: 'X' TO ls_opt-bt,
        'X' TO ls_opt-cp,
        'X' TO ls_opt-ge,
        'X' TO ls_opt-gt,
        'X' TO ls_opt-le,
        'X' TO ls_opt-lt,
        'X' TO ls_opt-nb,
        'X' TO ls_opt-np,
        'X' TO ls_opt-ne.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
       EXPORTING
         title                   = 'Dealer Code'
         text                    = 'Dealer Code'
*         SIGNED                  = 'X'
*         LOWER_CASE              = ' '
*         NO_INTERVAL_CHECK       = ' '
         just_display            = p_display
         just_incl               = 'X'
         excluded_options        = ls_opt
*         DESCRIPTION             =
         help_field              = l_helpfield
*          SEARCH_HELP             = 'BEDI'
         tab_and_field           = lt_field
        TABLES
          range                  = c_range
       EXCEPTIONS
         no_range_tab            = 1
         cancelled               = 2
         internal_error          = 3
         invalid_fieldname       = 4
         OTHERS                  = 5.

ENDFORM.                    " SET_DEALER_RANGE
*&---------------------------------------------------------------------*
*&      Form  BEFORE_OPEN_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->C_RANGE    text
*----------------------------------------------------------------------*
FORM before_open_range USING p_field
                    CHANGING c_range TYPE STANDARD TABLE.

  DATA: ls_line TYPE REF TO data,
        l_subrc LIKE sy-subrc.
  FIELD-SYMBOLS: <fs_range> TYPE STANDARD TABLE,
                 <fs_outwa>,
                 <fs_value>.

  ASSIGN c_range TO <fs_range>.
  CREATE DATA ls_line LIKE LINE OF <fs_range>.
  ASSIGN ls_line->* TO <fs_outwa>.

  READ TABLE <fs_range> INTO <fs_outwa> INDEX 1.
  l_subrc = sy-subrc.

  IF NOT p_field IS INITIAL.
    <fs_outwa> = 'IEQ'.
    ASSIGN ('<FS_OUTWA>-LOW') TO <fs_value>.
    <fs_value> = p_field.
    IF l_subrc = 0.
      MODIFY <fs_range> FROM <fs_outwa> INDEX 1.
    ELSE.
      APPEND <fs_outwa> TO <fs_range>.
    ENDIF.
  ELSE.
    IF sy-subrc = 0.
      DELETE <fs_range> INDEX 1.
    ENDIF.
  ENDIF.

  UNASSIGN: <fs_range>, <fs_outwa>, <fs_value>.

ENDFORM.                    "BEFORE_OPEN_RANGE
*&---------------------------------------------------------------------*
*&      Form  AFTER_OPEN_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD    text
*      -->C_RANGE    text
*----------------------------------------------------------------------*
FORM after_open_range USING p_field
                   CHANGING c_range TYPE STANDARD TABLE.

  DATA: ls_line TYPE REF TO data.
  FIELD-SYMBOLS: <fs_range> TYPE STANDARD TABLE,
                 <fs_outwa>,
                 <fs_value>.

  ASSIGN c_range TO <fs_range>.
  CREATE DATA ls_line LIKE LINE OF <fs_range>.
  ASSIGN ls_line->* TO <fs_outwa>.

  READ TABLE <fs_range> INTO <fs_outwa> INDEX 1.
  IF sy-subrc = 0.
    ASSIGN ('<FS_OUTWA>-LOW') TO <fs_value>.
    p_field = <fs_value>.
  ELSE.
    CLEAR p_field.
  ENDIF.

  UNASSIGN: <fs_range>, <fs_outwa>, <fs_value>.

ENDFORM.                    "AFTER_OPEN_RANGE
*&---------------------------------------------------------------------*
*&      Form  SET_FOCUS_ON_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GRID       text
*      -->P_INDEX      text
*      -->P_FIELDNAME  text
*----------------------------------------------------------------------*
FORM set_focus_on_grid USING    p_grid  TYPE REF TO cl_gui_alv_grid
                                p_index
                                p_fieldname.

  DATA: ls_row  TYPE lvc_s_row,
        ls_col  TYPE lvc_s_col.

  IF p_index IS INITIAL.
    EXIT.
  ENDIF.


  ls_row-index = p_index.
  ls_col-fieldname = p_fieldname.

  CALL METHOD p_grid->set_focus
    EXPORTING
      control = p_grid.

  CALL METHOD p_grid->set_current_cell_via_id
    EXPORTING
      is_row_id    = ls_row
      is_column_id = ls_col.

ENDFORM.                    "SET_FOCUS_ON_GRID
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_DATA_CHANGED  text
*      -->PS_MOD_CELLS     text
*      -->P_MSGNO          text
*      -->P_MSGTY          text
*      -->P_MSGV1          text
*      -->P_FIELDNAME      text
*----------------------------------------------------------------------*
FORM error_message_display USING pr_data_changed TYPE REF TO
                                         cl_alv_changed_data_protocol
                                 ps_mod_cells TYPE lvc_s_modi
                                 p_msgno
                                 p_msgty
                                 p_msgv1
                                 p_fieldname.

* Error Message Display
  CALL METHOD pr_data_changed->add_protocol_entry
    EXPORTING
      i_msgid     = 'ZHUSSD'
      i_msgno     = p_msgno
      i_msgty     = p_msgty
      i_msgv1     = p_msgv1
      i_fieldname = p_fieldname
      i_row_id    = ps_mod_cells-row_id.

ENDFORM.                    "ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  coversion_of_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MONEY    text
*      -->P_KEY      text
*      -->P_TEXT     text
*----------------------------------------------------------------------*
FORM coversion_of_currency  USING    p_money
      p_key
CHANGING p_text.
  WRITE p_money TO p_text CURRENCY p_key.

  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
  EXPORTING
    input            = p_text
*   INTERNAL         = 'X'
  IMPORTING
    output           = p_text
  EXCEPTIONS
    no_numeric       = 1
    OTHERS           = 2 .

ENDFORM.                    "coversion_of_currency
*&---------------------------------------------------------------------*
*&      Form  SAP_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sap_indicator.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = c_percentage
      text       = c_text.

ENDFORM.                    "SAP_indicator
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE_MATNR_BY_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPE     text
*      -->P_SLOPT    text
*      -->C_RANGE    text
*----------------------------------------------------------------------*
FORM set_range_matnr_by_unit  USING    p_count
                                       p_slopt TYPE STANDARD TABLE
                              CHANGING c_range TYPE STANDARD TABLE.

  DATA: ls_line TYPE REF TO data.

  FIELD-SYMBOLS: <fs_slopt_val>,
                 <fs_range_val>,
                 <fs_slopt> TYPE STANDARD TABLE,
                 <fs_slopt_struct>,
                 <fs_range> TYPE STANDARD TABLE,
                 <fs_range_struct>.

  CLEAR c_range.

  ASSIGN p_slopt TO <fs_slopt>.
  CREATE DATA ls_line LIKE LINE OF <fs_slopt>.
  ASSIGN ls_line->* TO <fs_slopt_struct>.

  ASSIGN c_range TO <fs_range>.
  CREATE DATA ls_line LIKE LINE OF <fs_range>.
  ASSIGN ls_line->* TO <fs_range_struct>.

  LOOP AT <fs_slopt> INTO <fs_slopt_struct>.
    ASSIGN ('<FS_SLOPT_STRUCT>-LOW') TO <fs_slopt_val>.
    ASSIGN ('<FS_RANGE_STRUCT>-LOW') TO <fs_range_val>.

    CLEAR <fs_range_struct>.
    <fs_range_struct>+0(1) = <fs_slopt_struct>+0(1).
    IF <fs_slopt_struct>+1(1) EQ 'N'.
      <fs_range_struct>+1(2) = 'NP'.
    ELSE.
      <fs_range_struct>+1(2) = 'CP'.
    ENDIF.

    DO p_count TIMES.
      CONCATENATE '+' <fs_range_val> INTO <fs_range_val>.
    ENDDO.

    CONCATENATE <fs_range_val> <fs_slopt_val> '*' INTO <fs_range_val>.
    APPEND <fs_range_struct> TO <fs_range>.
  ENDLOOP.

ENDFORM.                    " SET_RANGE_MATNR_BY_UNIT
*&---------------------------------------------------------------------*
*&      Form  GET_RANGE_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLOPT    text
*      -->C_RANGE    text
*----------------------------------------------------------------------*
FORM get_range_timestamp  USING p_slopt TYPE STANDARD TABLE
                       CHANGING c_range TYPE STANDARD TABLE.

  DATA: ls_line TYPE REF TO data.

  FIELD-SYMBOLS: <fs_slopt_high_val> TYPE dats,
                 <fs_range_high_val> TYPE vlc_ltstamp,
                 <fs_slopt_low_val>  TYPE dats,
                 <fs_range_low_val>  TYPE vlc_ltstamp,
                 <fs_slopt> TYPE STANDARD TABLE,
                 <fs_slopt_struct>,
                 <fs_range> TYPE STANDARD TABLE,
                 <fs_range_struct>.

  CLEAR c_range.

  ASSIGN p_slopt TO <fs_slopt>.
  CREATE DATA ls_line LIKE LINE OF <fs_slopt>.
  ASSIGN ls_line->* TO <fs_slopt_struct>.

  ASSIGN c_range TO <fs_range>.
  CREATE DATA ls_line LIKE LINE OF <fs_range>.
  ASSIGN ls_line->* TO <fs_range_struct>.


  LOOP AT <fs_slopt> INTO <fs_slopt_struct>.

    ASSIGN ('<FS_SLOPT_STRUCT>-LOW')  TO <fs_slopt_low_val>.
    ASSIGN ('<FS_SLOPT_STRUCT>-HIGH') TO <fs_slopt_high_val>.
    ASSIGN ('<FS_RANGE_STRUCT>-LOW')  TO <fs_range_low_val>.
    ASSIGN ('<FS_RANGE_STRUCT>-HIGH') TO <fs_range_high_val>.


    IF NOT <fs_slopt_low_val> IS INITIAL.
      <fs_range_struct> = 'IBT'.
      PERFORM convert_to_tstmp USING <fs_slopt_low_val> '000000'
                            CHANGING <fs_range_low_val>.

      IF NOT <fs_slopt_high_val> IS INITIAL.
        PERFORM convert_to_tstmp USING <fs_slopt_high_val> '235959'
                              CHANGING <fs_range_high_val>.
      ELSE.
        PERFORM convert_to_tstmp USING <fs_slopt_low_val> '235959'
                              CHANGING <fs_range_high_val>.
      ENDIF.
    ELSE.
*      <FS_RANGE_STRUCT> = 'ILT'.
*      PERFORM CONVERT_TO_TSTMP USING S_CRTDT-LOW '235959'
*                            CHANGING R_TSTMP-LOW.
    ENDIF.

    APPEND <fs_range_struct> TO <fs_range>.
  ENDLOOP.

  UNASSIGN: <fs_slopt_low_val>,
            <fs_slopt_high_val>,
            <fs_range_low_val>,
            <fs_range_high_val>.

ENDFORM.                    "GET_RANGE_TIMESTAMP
*&---------------------------------------------------------------------*
*&      Form  set_vhvin_uppercase
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_VHVIN   text
*----------------------------------------------------------------------*
FORM set_vhvin_uppercase  TABLES   pr_vhvin STRUCTURE range_c35.

  LOOP AT pr_vhvin.
    TRANSLATE pr_vhvin-low  TO UPPER CASE.
    TRANSLATE pr_vhvin-high TO UPPER CASE.
    MODIFY pr_vhvin.
  ENDLOOP.

ENDFORM.                    "set_vhvin_uppercase
*&---------------------------------------------------------------------*
*&      Form  is_valid_zipcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM is_valid_zipcode   USING p_location_data TYPE tax_txjcd_addr
                     CHANGING p_result TYPE char01.

  TABLES: t005.

*  DATA: E_TXJCD LIKE TTXJ-TXJCD.
*  DATA: TXJCD_SHELL  TYPE REF TO CL_TAX_TXJCD_DETERMIN_SHELL.
*  CREATE OBJECT TXJCD_SHELL.
*------ TTXD (Struktur des Steuerstanortcodes) nachlesen --------------
  DATA: ls_address_out TYPE tax_txjcd_addr,
        l_text     TYPE ttxjt-text1,
        l_kalsm    TYPE ttxd-kalsm.
  DATA: ls_jurisdiction TYPE REF TO cl_tax_jurisdiction_code.


  CLEAR p_result.

  CALL METHOD cl_tax_jurisdiction_code=>get_kalsm
    EXPORTING
      im_country       = p_location_data-country
    RECEIVING
      re_kalsm         = l_kalsm
    EXCEPTIONS
      input_incomplete = 1
      no_tax_procedure = 2
      OTHERS           = 3.
*
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  CREATE OBJECT ls_jurisdiction
    EXPORTING
      im_kalsm = l_kalsm
    EXCEPTIONS
      no_ttxd  = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

*  DEFINE MESSAGE_AND_RAISE.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 RAISING &1.
*  END-OF-DEFINITION.

  CALL METHOD ls_jurisdiction->set_txjcd
    EXPORTING
      im_location_data    = p_location_data
      im_text1            = l_text
      im_no_dialog        = 'X'
    EXCEPTIONS
      parameter_error     = 1
      invalid_txjcd       = 2
      communication_error = 3
      system_error        = 4
      txjcd_not_found     = 5
      OTHERS              = 6.

  IF sy-subrc = 0.
    p_result = 'X'.
  ELSE.
    CLEAR p_result.
  ENDIF.


*  IF SY-SUBRC <> 0.
*    CASE SY-SUBRC.
*      WHEN 1.      MESSAGE_AND_RAISE PARAMETER_ERROR .
*      WHEN 2.      MESSAGE_AND_RAISE INVALID_TXJCD.
*      WHEN 3.      MESSAGE_AND_RAISE COMMUNICATION_ERROR.
*      WHEN 4.      MESSAGE_AND_RAISE SYSTEM_ERROR.
*      WHEN 5.      MESSAGE_AND_RAISE TXJCD_NOT_FOUND.
*      WHEN OTHERS. MESSAGE_AND_RAISE OTHERS_ERROR.
*    ENDCASE.
*  ENDIF.

*  CALL METHOD LO_JURISDICTION->GET_TXJCD
*    IMPORTING
*      EX_TXJCD         = E_TXJCD
*      EX_LOCATION_DATA = LS_ADDRESS_OUT
*      EX_TEXT1         = L_TEXT.
*
**IF SCREEN_INPUT = SPACE. "/ space is edit mode !!!
*  TAXJURCODE = E_TXJCD.

ENDFORM.                    "is_valid_zipcode
*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ACT      text
*      -->P_MAX      text
*      -->P_TEXT     text
*----------------------------------------------------------------------*
FORM progress_indicator USING p_act  TYPE i
                              p_max  TYPE i
                              p_text TYPE any.

  DATA: l_percent       TYPE p,
        l_ptext(80)     TYPE c.

  IF p_max NE 0.
    l_percent = p_act / p_max * 100.
  ENDIF.

  l_ptext(3)    = l_percent.
  l_ptext+3     = '%'.
  l_ptext+6(60) = p_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percent
      text       = l_ptext.

ENDFORM.                    "progress_indicator
*&---------------------------------------------------------------------*
*&      Form  CHECK_VLC_VHVIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZHUSSDT7100_VHVIN  text
*      <--P_L_ERROR  text
*----------------------------------------------------------------------*
FORM check_vlc_vhvin  USING p_vhvin
                   CHANGING c_error.

  DATA: lv_strlength TYPE i.
  RANGES: r_vhvin FOR vlcvehicle-vhvin.

  lv_strlength = STRLEN( p_vhvin ).

  IF lv_strlength = 8.
    r_vhvin = 'ICP'.
    r_vhvin-low = p_vhvin.
    SHIFT r_vhvin-low BY 1 PLACES RIGHT.
    r_vhvin-low(1) = '*'.
    APPEND r_vhvin.
  ELSEIF lv_strlength = 17.
    r_vhvin = 'IEQ'.
    r_vhvin-low = p_vhvin.
    APPEND r_vhvin.
  ELSE.
    c_error = 'X'.
    EXIT.
  ENDIF.

  SELECT SINGLE vhvin
    INTO p_vhvin
    FROM vlcvehicle
   WHERE vhvin IN r_vhvin.

  IF sy-subrc <> 0.
    c_error = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_VLC_VHVIN
