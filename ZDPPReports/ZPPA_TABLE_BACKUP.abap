*********************************************************************
* Program Name      : ZPPA_TABLE_BACKUP
* Author            : Furong Wang
* Creation Date     : 02/2011
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zppa_table_backup LINE-SIZE 132 MESSAGE-ID zmpp.

*DATA: it_ztpp_if_status LIKE TABLE OF ztpp_if_status WITH HEADER LINE.

TABLES: dcobjdef.
FIELD-SYMBOLS: <table>,    " like dntab-tabname
               <table_bk>. " like dntab-tabname

*DATA: it_data                LIKE TABLE OF ztppvr      WITH HEADER LINE

DATA: wa_error                 TYPE c,
      wa_check                 TYPE c,
      wa_atinn                 LIKE ausp-atinn,
      wa_atnam                 LIKE cabn-atnam.

DATA: wa_count(2) TYPE n,
      wa_tablename(20),
      wa_tablename_bk(20),
      wa_ch(20),
      wa_cd(20),
      wa_bd(20),
      wa_tn(20),
      wa_df(20),
      it_dd02t LIKE TABLE OF dd02t WITH HEADER LINE.

** define for dynamic itab.
DATA: cond(72) TYPE c,
      it_cond LIKE TABLE OF cond.

DATA: d_ref TYPE REF TO data,
d_ref2 TYPE REF TO data ,
d_refbk TYPE REF TO data ,
new_line TYPE REF TO data,
i_alv_cat TYPE TABLE OF lvc_s_fcat,
i_alv_cat_bk TYPE TABLE OF lvc_s_fcat,
ls_alv_cat LIKE LINE OF i_alv_cat.


DATA: BEGIN OF itab OCCURS 0.
        INCLUDE STRUCTURE dntab.
DATA: END OF itab.

FIELD-SYMBOLS : <f_fs> TYPE table,
<f_fs1> TYPE table,
<f_struc> TYPE table,
<f_archive> TYPE table.

TYPES tabname LIKE dcobjdef-name.
DATA: tabname_t TYPE tabname.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_tname FOR dcobjdef-name NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-014.
PARAMETERS: p_tinput(1).
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
*  PERFORM display_data.

AT SELECTION-SCREEN.
  PERFORM check_data.

START-OF-SELECTION.
  PERFORM backup_processing .
  IF p_tinput = 'X'.
    PERFORM backup_tinput_date.
  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  BACKUP_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM backup_processing.
  DATA: lw_tname LIKE s_tname,
        l_colum_d(20),
        l_colum_t(20),
        l_index LIKE sy-index,
        l_len TYPE i,
        l_times TYPE i.

*  DATA: l_date_cd               TYPE d,
*        l_date_bd               TYPE d,
*        wa_date LIKE sy-datum,
*        w_field(20),
*        wa_backup_tab_exist(1),
*        w_table_line TYPE i,
*        w_backup_line TYPE i.

  FIELD-SYMBOLS: <lw_table>,  <lw_table_bk>, <lw_value>, <lw_value_bk>.

  CLEAR wa_count.

  PERFORM display_progress_bar USING text-010.
*  WRITE: 5(30) 'Table'.
*  WRITE: 35(30) 'Remarks'.
*  ULINE (65).
*
  l_colum_d = 'BACKUP_DATE'.
  l_colum_t = 'BACKUP_TIME'.

  LOOP AT s_tname INTO lw_tname.

    wa_tablename = lw_tname-low.
    ASSIGN wa_tablename TO <table>.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    wa_tablename_bk = wa_tablename.
    l_len = strlen( wa_tablename_bk ).
    IF l_len >= 14.
      wa_tablename_bk =  wa_tablename_bk+0(14).
      CONCATENATE wa_tablename_bk '_H' INTO wa_tablename_bk.
    ELSE.
      CONCATENATE wa_tablename_bk '_BK' INTO wa_tablename_bk.
    ENDIF.

    ASSIGN wa_tablename_bk TO <table_bk>.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    tabname_t = <table>.
    REFRESH: itab, i_alv_cat.
    CALL FUNCTION 'NAMETAB_GET'
         EXPORTING
              langu          = sy-langu
              tabname        = tabname_t
         TABLES
              nametab        = itab
         EXCEPTIONS
              no_texts_found = 1.
    LOOP AT itab .
      ls_alv_cat-fieldname = itab-fieldname.
      ls_alv_cat-ref_table = <table>.
      ls_alv_cat-ref_field = itab-fieldname.
      APPEND ls_alv_cat TO i_alv_cat.
    ENDLOOP.
    DESCRIBE TABLE i_alv_cat LINES l_times.

* internal table build
    CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING it_fieldcatalog = i_alv_cat
    IMPORTING ep_table = d_ref .
    ASSIGN d_ref->* TO <f_fs>.

    tabname_t = <table_bk>.
    REFRESH: itab, i_alv_cat_bk.
    CLEAR: ls_alv_cat.
    CALL FUNCTION 'NAMETAB_GET'
         EXPORTING
              langu          = sy-langu
              tabname        = tabname_t
         TABLES
              nametab        = itab
         EXCEPTIONS
              no_texts_found = 1.
    LOOP AT itab .
      ls_alv_cat-fieldname = itab-fieldname.
      ls_alv_cat-ref_table = <table_bk>.
      ls_alv_cat-ref_field = itab-fieldname.
      APPEND ls_alv_cat TO i_alv_cat_bk.
    ENDLOOP.

    CALL METHOD cl_alv_table_create=>create_dynamic_table
     EXPORTING it_fieldcatalog = i_alv_cat_bk
     IMPORTING ep_table = d_refbk .
    ASSIGN d_refbk->* TO <f_fs1>.

    CREATE DATA new_line LIKE LINE OF <f_fs>.
    ASSIGN new_line->* TO <lw_table>.

    CREATE DATA new_line LIKE LINE OF  <f_fs1>.
    ASSIGN new_line->* TO <lw_table_bk>.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE <f_fs>
     FROM (<table>) .

    LOOP AT <f_fs> INTO <lw_table>.
      DO l_times TIMES.
        l_index = sy-index + 2.
        ASSIGN COMPONENT sy-index OF STRUCTURE <lw_table>
                                 TO <lw_value>.

        IF sy-index = 1.
          ASSIGN COMPONENT sy-index OF STRUCTURE <lw_table_bk>
                                 TO <lw_value_bk>.

        ELSE.

          ASSIGN COMPONENT l_index OF STRUCTURE <lw_table_bk>
                                   TO <lw_value_bk>.
        ENDIF.
        <lw_value_bk> = <lw_value>.
      ENDDO.

      ASSIGN COMPONENT 2 OF STRUCTURE <lw_table_bk>
                                 TO <lw_value_bk>.
      <lw_value_bk> =  sy-datum.
      ASSIGN COMPONENT 3 OF STRUCTURE <lw_table_bk>
                                 TO <lw_value_bk>.
      <lw_value_bk> = sy-uzeit.
      APPEND <lw_table_bk> TO <f_fs1>.
    ENDLOOP.

    MODIFY (<table_bk>) FROM TABLE <f_fs1>.

    IF sy-subrc = 0.
      COMMIT WORK.
      WRITE: /5(30) wa_tablename.
      WRITE: 35(30) 'Successfully Backup'.
    ELSE.
      ROLLBACK WORK.
      WRITE: /5(30) wa_tablename.
      WRITE: 35(30) 'Unsuccessfully Backup'.
    ENDIF.
    UNASSIGN: <table>, <table_bk>, <lw_value>, <lw_value_bk>,
       <lw_table>, <lw_table_bk>,  <f_fs1>,  <f_fs>.
    FREE: d_refbk, d_ref.
  ENDLOOP.
ENDFORM.                    " BACKUP_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  check_screen_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_text LIKE dd02t-ddtext,
        l_tname LIKE s_tname-low,
        l_len TYPE i.
  LOOP AT s_tname.
    SELECT SINGLE ddtext INTO l_text FROM dd02t
          WHERE tabname = s_tname-low
          AND ddlanguage = 'E'.
    IF sy-subrc NE 0.
      MESSAGE e001 WITH text-w04 s_tname-low.
    ENDIF.
    l_tname = s_tname-low.
    l_len = strlen( l_tname ).
    IF l_len >= 14.
      l_tname =  l_tname+0(14).
      CONCATENATE l_tname '_H' INTO l_tname.
    ELSE.
      CONCATENATE l_tname '_BK' INTO l_tname.
    ENDIF.

    SELECT SINGLE ddtext INTO l_text FROM dd02t
          WHERE tabname = l_tname
          AND ddlanguage = 'E'.
    IF sy-subrc NE 0.
      MESSAGE e001 WITH text-w04 l_tname.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_010  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.

*  s_tname-sign = 'I'.
*  s_tname-option = 'EQ'.
*  s_tname-low = 'ZTPP_SEQ_SUM01'.
*  APPEND s_tname.
*
*  s_tname-low = 'ZTPP_SEQ_SUM02'.
*  APPEND s_tname.
*
*  s_tname-low = 'ZTPP_SEQ_SUM03'.
*  APPEND s_tname.
*
*  s_tname-low = 'ZTPP_WIRE_DAY'.
*  APPEND s_tname.
*
*  s_tname-low = 'ZTPP_WIRE_HOUR'.
*  APPEND s_tname.
ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  IF screen-name = 'P_TB_01'.
  ENDIF.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  BACKUP_TINPUT_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM backup_tinput_date.
  DATA: lt_date LIKE TABLE OF ztpp_date_save WITH HEADER LINE,
        lt_7jb_date LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE.
  .
  DATA: wa_datum TYPE d,
        wa_kalid LIKE kako-kalid,
                 w_num(2) TYPE n,
                 w_field(20),
        z_max_date LIKE sy-datum.

  FIELD-SYMBOLS: <fs>.
  lt_date-pname = 'ZRPP208R_TRIM_INPUT_PLAN'.
  lt_date-if_date = sy-datum.
  lt_date-if_time = sy-uzeit.


  SELECT SINGLE dates INTO wa_datum
    FROM ztpp_common_vals
   WHERE jobs = 'ZAPP903R_INPUT_PLAN'
     AND key2 = 'TRIM_INPUT'.


  SELECT SINGLE kalid INTO wa_kalid
   FROM zvpp_capacity
  WHERE arbpl = 'T'   .


  SELECT MAX( sqdt ) INTO z_max_date
  FROM ztpp_pmt07jb_a
   WHERE gubb = 'A'.

  wa_datum = wa_datum - 1.

  PERFORM read_working_date USING '+'  wa_kalid  wa_datum.
  lt_date-DAY01 = wa_datum.
  wa_datum = wa_datum + 1.
  PERFORM read_working_date USING '+'  wa_kalid  wa_datum.
  lt_date-DAY02 = wa_datum.
  lt_date-DAY03 = wa_datum.
  wa_datum = wa_datum + 1.

  CLEAR w_num.
  w_num = 4.
  DO.
    CLEAR w_field.
    IF  wa_datum > z_max_date.
      EXIT.
    ENDIF.
    CONCATENATE 'LT_DATE-DAY' w_num INTO w_field.
    ASSIGN (w_field) TO <fs>.
    PERFORM read_working_date USING '+'  wa_kalid  wa_datum .
    <fs> = wa_datum.
    wa_datum = wa_datum + 1.
    w_num = w_num + 1.
  ENDDO.

  SELECT * INTO TABLE lt_7jb_date
  FROM ztpp_pmt07jb_a
   WHERE gubb EQ 'B'.

  SORT lt_7jb_date BY sqdt.
  DELETE ADJACENT DUPLICATES FROM lt_7jb_date COMPARING sqdt.

  CLEAR w_num.
  w_num = 1.
  LOOP AT lt_7jb_date.
    CLEAR w_field.
    CONCATENATE 'LT_DATE-WEEK' w_num INTO w_field.
    ASSIGN (w_field) TO <fs>.
    PERFORM read_working_date USING '+'  wa_kalid  lt_7jb_date-sqdt .
    <fs> = lt_7jb_date-sqdt.
    w_num = w_num + 1.
  ENDLOOP.
  APPEND lt_date.
  MODIFY ztpp_date_save FROM lt_date.
  IF SY-SUBRC = 0.
     COMMIT WORK.
  ELSE.
     ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " BACKUP_TINPUT_DATE
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
