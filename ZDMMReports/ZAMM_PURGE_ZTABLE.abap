*********************************************************************
* Program Name      : ZAMM_PURGE_ZTABLE
* Author            : Furong Wang
* Creation Date     : 11/02/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zamm_purge_ztable LINE-SIZE 132  MESSAGE-ID zmpp.

*DATA: it_ztpp_if_status LIKE TABLE OF ztpp_if_status WITH HEADER LINE.

FIELD-SYMBOLS: <table>.   " like dntab-tabname
*               <table_bk>. " like dntab-tabname

DATA: it_data                LIKE TABLE OF ztppvr      WITH HEADER LINE.

DATA: wa_error                 TYPE c,
      wa_check                 TYPE c,
      wa_atinn                 LIKE ausp-atinn,
      wa_atnam                 LIKE cabn-atnam.

DATA: wa_count(2) TYPE n,
      wa_tablename(20),
*      wa_tablename_bk(20),
      wa_ch(20),
      wa_cd(20),
      wa_bd(20),
      wa_tn(20),
      wa_df(20),
      it_dd02t LIKE TABLE OF dd02t WITH HEADER LINE.

FIELD-SYMBOLS: <desc>, <check>, <current>, <date_field>.  " , <backup>

** define for dynamic itab.
DATA: cond(72) TYPE c,
      it_cond LIKE TABLE OF cond.

DATA: d_ref TYPE REF TO data,
d_ref2 TYPE REF TO data ,
*d_refbk TYPE REF TO data ,
new_line TYPE REF TO data,
i_alv_cat TYPE TABLE OF lvc_s_fcat,
ls_alv_cat LIKE LINE OF i_alv_cat.


DATA: BEGIN OF itab OCCURS 0.
        INCLUDE STRUCTURE dntab.
DATA: END OF itab.

FIELD-SYMBOLS : <f_fs> TYPE table,
<f_fs1> TYPE table,
<f_struc> TYPE table,
<f_archive> TYPE table.

TYPES tabname LIKE dcobjdef-name .
DATA: tabname_t TYPE tabname.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*SELECTION-SCREEN ULINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(35) text-003.
SELECTION-SCREEN COMMENT 47(15) text-004.
*SELECTION-SCREEN COMMENT 56(7) text-005.
SELECTION-SCREEN COMMENT 64(20) text-006.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 50(6) text-007.
**SELECTION-SCREEN COMMENT 57(6) text-008.
*SELECTION-SCREEN COMMENT 65(8) text-009.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN ULINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_01 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_01(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_01.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_01(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_01(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_01(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_02 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_02(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_02.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_02(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_02(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_02(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_03 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_03(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_03.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_03(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_03(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_03(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_04 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_04(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_04.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_04(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_04(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_04(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_05 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_05(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_05.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_05(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_05(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_05(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_06 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_06(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_06.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_06(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_06(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_06(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_07 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_07(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_07.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_07(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_07(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_07(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_08 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_08(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_08.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_08(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_08(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_08(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_09 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_09(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_09.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_09(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_09(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_09(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_10 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_10(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_10.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_10(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_10(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_10(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_11 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_11(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_11.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_11(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_11(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_11(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_12 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_12(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_12.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_12(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_12(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_12(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_13 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_13(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_13.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_13(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_13(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_13(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_14 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_14(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_14.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_14(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_14(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_14(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_15 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_15(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_15.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_15(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_15(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_15(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_16 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_16(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_16.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_16(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_16(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_16(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_17 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_17(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_17.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_17(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_17(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_17(17).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: p_ch_18 AS CHECKBOX.
SELECTION-SCREEN POSITION 4.
PARAMETERS: p_tb_18(16).
SELECTION-SCREEN COMMENT 21(28) w_tn_18.
SELECTION-SCREEN POSITION 50.
PARAMETERS: p_cd_18(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_18(4) TYPE n.
SELECTION-SCREEN POSITION 63.
PARAMETERS: p_df_18(17).
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS: p_ch_19 AS CHECKBOX.
*SELECTION-SCREEN POSITION 4.
*PARAMETERS: p_tb_19(16).
*SELECTION-SCREEN COMMENT 21(28) w_tn_19.
*SELECTION-SCREEN POSITION 50.
*PARAMETERS: p_cd_19(4) TYPE n.
*SELECTION-SCREEN POSITION 57.
*PARAMETERS: p_bd_19(4) TYPE n.
*SELECTION-SCREEN POSITION 63.
*PARAMETERS: p_df_19(17).
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(79) text-011.
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(79) text-012.
*SELECTION-SCREEN END OF LINE.
PARAMETERS: p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.


AT SELECTION-SCREEN OUTPUT.
*  PERFORM display_data.

AT SELECTION-SCREEN.
  PERFORM check_data.

START-OF-SELECTION.
  PERFORM backup_processing .

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
  DATA: l_date_cd               TYPE d,
        l_date_bd               TYPE d,
        wa_date LIKE sy-datum,
        w_field(20),
*        wa_backup_tab_exist(1),
        w_table_line TYPE i.
*        w_backup_line TYPE i.

  FIELD-SYMBOLS: <wa_table>,
  <wa_archive>,
  <wa_field>.

  CLEAR wa_count.

  PERFORM display_progress_bar USING text-010.
  WRITE: 5 'Table Name'.
  IF p_test IS INITIAL.
    WRITE: 20 'Total Records were deleted:'.
  ELSE.
    WRITE: 22 'Total Records:'.
  ENDIF.
  WRITE: 50 'Retention Date'.
*  WRITE: 50 'Backup Table'.
*  WRITE: 68 'Total Records'.
  ULINE /5(80).
  DO 18 TIMES.
    REFRESH: i_alv_cat.
    CLEAR: ls_alv_cat,d_ref.
    wa_count  = wa_count + 1.

    CONCATENATE 'p_ch_' wa_count INTO wa_ch.
    ASSIGN (wa_ch) TO <check>.
    IF <check> NE 'X'.
      CONTINUE.
    ENDIF.

    CONCATENATE 'p_tb_' wa_count INTO wa_tablename.
    ASSIGN (wa_tablename) TO <table>.
    IF <table> IS INITIAL.
      EXIT.
    ENDIF.

*    wa_tablename_bk = <table>.
*    CONCATENATE wa_tablename_bk '_BK' INTO wa_tablename_bk.
*    ASSIGN wa_tablename_bk TO <table_bk>.
*    IF sy-subrc EQ 0.
*      wa_backup_tab_exist = 'Y'.
*    ELSE.
*      CLEAR wa_backup_tab_exist.
*    ENDIF.
    CONCATENATE 'p_cd_' wa_count INTO wa_cd.
    ASSIGN (wa_cd) TO <current>.
*    CONCATENATE 'p_bd_' wa_count INTO wa_bd.
*    ASSIGN (wa_bd) TO <backup>.
    CONCATENATE 'p_df_' wa_count INTO wa_df.
    ASSIGN (wa_df) TO <date_field>.

    PERFORM check_date USING l_date_cd <current>.
*    PERFORM check_date USING l_date_bd <backup>.
    tabname_t = <table>.
    REFRESH itab.
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
* internal table build
    CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING it_fieldcatalog = i_alv_cat
    IMPORTING ep_table = d_ref .
    ASSIGN d_ref->* TO <f_fs>.

*    CALL METHOD cl_alv_table_create=>create_dynamic_table
*     EXPORTING it_fieldcatalog = i_alv_cat
*     IMPORTING ep_table = d_refbk .
*    ASSIGN d_refbk->* TO <f_fs1>.


    CREATE DATA new_line LIKE LINE OF <f_fs>.
    ASSIGN new_line->* TO <wa_archive>.

    CLEAR: it_cond[], cond.
    CONCATENATE <date_field> '<=' l_date_cd
             INTO cond SEPARATED BY space.
    APPEND cond TO it_cond.

*    SELECT * FROM (<table>) INTO CORRESPONDING FIELDS OF TABLE <f_fs>
*                              WHERE (it_cond).

    SELECT * FROM (<table>) INTO CORRESPONDING FIELDS OF TABLE <f_fs>
                              WHERE (it_cond).

    DESCRIBE TABLE <f_fs> LINES w_table_line.
    WRITE:/5 <table>.
    IF w_table_line > 0 AND p_test IS INITIAL.
*      IF wa_backup_tab_exist = 'Y'.
*        MODIFY (<table_bk>) FROM TABLE <f_fs>.
*      ENDIF.
      DELETE (<table>) FROM TABLE <f_fs>.
      IF sy-subrc = 0.
        WRITE: 22 w_table_line.
      ELSE.
        WRITE: 22 w_table_line, 40 'with errors'.
      ENDIF.
    ELSE.
      WRITE: 22 w_table_line.
    ENDIF.
    WRITE: 50 l_date_cd MM/DD/YY . " NO-ZERO.
*    IF wa_backup_tab_exist = 'Y'.
*      WRITE: 50 <table_bk>.
*      CLEAR: it_cond[], cond.
*      CONCATENATE <date_field> '<=' l_date_bd
*               INTO cond SEPARATED BY space.
*      APPEND cond TO it_cond.
*
*      SELECT * FROM (<table_bk>) INTO CORRESPONDING FIELDS OF
*                     TABLE <f_fs1> WHERE (it_cond).
*      DESCRIBE TABLE <f_fs1> LINES w_backup_line.
*
*      IF w_backup_line > 0.
*        DELETE (<table_bk>) FROM TABLE <f_fs1>.
*        IF sy-subrc EQ 0.
*          WRITE: 67 w_backup_line.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      WRITE: 50 'Backup table not exist'.
*    ENDIF.

*    IF w_table_line > 0.  " OR w_backup_line > 0.
*      CLEAR: it_ztpp_if_status, it_ztpp_if_status[].
*      SELECT SINGLE * INTO it_ztpp_if_status
*                      FROM ztpp_if_status WHERE tabname = <table>.
*      IF sy-subrc EQ 0.
*        it_ztpp_if_status-aedat = sy-datum.
*        it_ztpp_if_status-aezet = sy-uzeit.
*        it_ztpp_if_status-aenam = sy-uname.
*        APPEND it_ztpp_if_status.
*        UPDATE ztpp_if_status FROM TABLE it_ztpp_if_status.
*      ELSE.
*        it_ztpp_if_status-mandt = sy-mandt.
*        it_ztpp_if_status-tabname = <table>.
*        it_ztpp_if_status-erdat = sy-datum.
*        it_ztpp_if_status-erzet = sy-uzeit.
*        it_ztpp_if_status-ernam = sy-uname.
*        it_ztpp_if_status-aedat = sy-datum.
*        it_ztpp_if_status-aezet = sy-uzeit.
*        it_ztpp_if_status-aenam = sy-uname.
*        APPEND it_ztpp_if_status.
*        INSERT ztpp_if_status FROM TABLE it_ztpp_if_status.
*      ENDIF.
*    ENDIF.
*    ENDCASE.
  ENDDO.
  COMMIT WORK.
ENDFORM.                    " BACKUP_PROCESSING


*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM check_date USING    pa_date pa_day.
  IF sy-uzeit < '063000'.
    pa_date = sy-datum - 1 .
  ELSE.
    pa_date = sy-datum.
  ENDIF.

  DO pa_day TIMES.
    pa_date =  pa_date - 1.
    PERFORM read_working_date USING '-'  'HM'  pa_date.
  ENDDO.
ENDFORM.                    " CHECK_DATE

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
*&---------------------------------------------------------------------*
*&      Form  check_screen_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: w_tabname(30),  " LIKE ztpp_if_status-tabname,
        wa_text(33).
  CLEAR wa_count.
  DO 18 TIMES.
    wa_count  = wa_count + 1.
    CONCATENATE 'p_ch_' wa_count INTO wa_ch.
    ASSIGN (wa_ch) TO <check>.
    IF <check> NE 'X'.
      CONTINUE.
    ENDIF.

    CONCATENATE 'p_tb_' wa_count INTO wa_tablename.
    ASSIGN (wa_tablename) TO <table>.
    IF <table> IS INITIAL.
      MESSAGE e001 WITH text-w01.
    ENDIF.

    CONCATENATE 'w_tn_' wa_count INTO wa_tn.
    ASSIGN (wa_tn) TO <desc>.

    SELECT SINGLE ddtext INTO <desc> FROM dd02t
           WHERE tabname = <table>
           AND ddlanguage = 'EN'.
    IF sy-subrc NE 0.
      MESSAGE e001 WITH text-w04 <table>.
    ENDIF.

*    wa_tablename_bk = <table>.
*    CONCATENATE wa_tablename_bk '_BK' INTO wa_tablename_bk.
*    ASSIGN wa_tablename_bk TO <table_bk>.

    CONCATENATE 'p_cd_' wa_count INTO wa_cd.
    ASSIGN (wa_cd) TO <current>.
    IF <current> IS INITIAL.
      MESSAGE e001 WITH text-w01.
    ENDIF.

*    CONCATENATE 'p_bd_' wa_count INTO wa_bd.
*    ASSIGN (wa_bd) TO <backup>.

    CONCATENATE 'p_df_' wa_count INTO wa_df.
    ASSIGN (wa_df) TO <date_field>.
    IF <date_field> IS INITIAL.
      MESSAGE e001 WITH text-w01.
    ENDIF.

    SELECT SINGLE tabname INTO w_tabname
        FROM dd03l WHERE tabname = <table> AND fieldname = <date_field>.
    IF sy-subrc NE 0.
      MESSAGE e002 WITH text-w03 <table> <date_field>.
    ENDIF.

  ENDDO.

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
*  CLEAR: it_ztpp_if_status, it_ztpp_if_status[].
*
*  SELECT a~tabname b~ddlanguage b~as4local b~as4vers b~ddtext
*         INTO TABLE it_dd02t
*         FROM ztpp_if_status AS a INNER JOIN dd02t AS b
*         ON a~tabname = b~tabname
*         WHERE b~ddlanguage = 'EN'.
*
*  SELECT * INTO TABLE it_ztpp_if_status FROM ztpp_if_status.
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
