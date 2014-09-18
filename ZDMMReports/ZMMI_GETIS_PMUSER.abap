************************************************************************
* Program Name      : ZMMI_GETIS_PMUSER
* Creation Date     : 06/04/13
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zmmi_getis_pmuser NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

TYPE-POOLS: slis, vrm.
TABLES: pa0000.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztmm_getis_pmuse.
DATA: END OF it_data.
DATA: it_error LIKE TABLE OF it_data WITH HEADER LINE.

DATA: w_dest(10).

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldname.  "IT_FIELDCAT.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE i,
      w_mtart LIKE mara-mtart.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:  p_datum LIKE sy-datum DEFAULT sy-datum.
PARAMETERS: p_bukrs LIKE t001-bukrs DEFAULT 'H201' OBLIGATORY.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(4) text-v01 FOR FIELD p_sd.
PARAMETERS: p_act RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN COMMENT 12(8) text-v02 FOR FIELD p_eo.
PARAMETERS: p_noact RADIOBUTTON GROUP grp1 MODIF ID abc.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.
PARAMETERS: p_batch AS CHECKBOX  USER-COMMAND chal.
SELECTION-SCREEN  BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-u01 FOR FIELD p_sd.
PARAMETERS: p_sd RADIOBUTTON GROUP grp2 MODIF ID abc.
SELECTION-SCREEN COMMENT 20(8) text-u12 FOR FIELD p_eo.
PARAMETERS: p_eo RADIOBUTTON GROUP grp2 MODIF ID abc.
SELECTION-SCREEN COMMENT 36(15) text-u13 FOR FIELD p_es.
PARAMETERS: p_es RADIOBUTTON GROUP grp2 MODIF ID abc
                  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 60(11) text-u14 FOR FIELD p_rp.
PARAMETERS: p_rp RADIOBUTTON GROUP grp2 MODIF ID abc.
SELECTION-SCREEN  END OF LINE.

SELECTION-SCREEN ULINE.
*SELECTION-SCREEN SKIP.
*PARAMETERS: p_rver  LIKE somlreci1-receiver OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen_all.

START-OF-SELECTION.

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE i009 WITH 'No data found'.
  ELSE.
    IF p_batch = 'X'.
      PERFORM save_send_data.
    ELSE.
      IF p_es = 'X' OR p_rp = 'X'.
        PERFORM save_send_data.
      ENDIF.
      PERFORM display_data.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_kostl LIKE pa0001-kostl,
        l_company_name LIKE t001-butxt,
        l_persnumber LIKE v_username-persnumber.

  DATA: BEGIN OF lt_pernr OCCURS 0,
         pernr LIKE pa0001-pernr,
        END OF lt_pernr.

  DATA: BEGIN OF lt_status OCCURS 0,
        bname LIKE usr02-bname,
        stat2 LIKE pa0000-stat2,
        END OF lt_status.

  DATA: BEGIN OF lt_temp OCCURS 0,
         bname LIKE usr02-bname,
         erdat LIKE usr02-erdat,
         END OF lt_temp.

  IF p_bukrs = 'H201'.
    l_kostl = '55103'.
  ELSEIF p_bukrs = 'K201'.
    l_kostl = 'K6200'.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = l_kostl
    IMPORTING
      output = l_kostl.

  CASE 'X'.
    WHEN p_sd.
      SELECT * INTO TABLE it_data
        FROM ztmm_getis_pmuse
        WHERE if_date = p_datum.

    WHEN p_rp.
      SELECT * INTO TABLE it_data
     FROM ztmm_getis_pmuse
     WHERE if_date = p_datum
       AND rflag <> 'Z'.

      LOOP AT it_data.
        CLEAR: it_data-if_date_change, it_data-if_time_change,
               it_data-rflag, it_data-message.
        MODIFY it_data.
      ENDLOOP.

    WHEN OTHERS.

      SELECT pernr
            INTO TABLE lt_pernr
             FROM pa0001
           WHERE endda >= p_datum
             AND kostl = l_kostl.

      CHECK sy-subrc = 0.

      SELECT pernr AS bname stat2
        INTO TABLE lt_status
         FROM pa0000
      FOR ALL ENTRIES IN lt_pernr
       WHERE pernr = lt_pernr-pernr
         AND endda >= p_datum.

      CHECK sy-subrc = 0.

      LOOP AT lt_status.
        SHIFT lt_status-bname LEFT DELETING LEADING '0'.
        MODIFY lt_status.
      ENDLOOP.

      SORT lt_status BY bname.

      IF p_act = 'X'.
        SELECT bname erdat INTO CORRESPONDING FIELDS OF TABLE lt_temp
         FROM usr02
         FOR ALL ENTRIES IN lt_status
         WHERE bname = lt_status-bname
          AND ( gltgb = '00000000' OR gltgb >= p_datum ).

        REFRESH it_data.
        SELECT SINGLE butxt INTO l_company_name
           FROM t001
          WHERE bukrs = p_bukrs.

        LOOP AT lt_temp.

          SELECT SINGLE name_text persnumber
              INTO (it_data-user_name, l_persnumber)
          FROM v_username
         WHERE bname = lt_temp-bname.

          IF sy-subrc = 0.
            SELECT SINGLE smtp_addr INTO it_data-email_address
              FROM adr6
              WHERE persnumber = l_persnumber.
          ENDIF.

          READ TABLE lt_status WITH KEY bname = lt_temp-bname
                           BINARY SEARCH.
          IF lt_status-stat2 = '3'.
            it_data-user_status = 'Y'.
          ELSE.
            it_data-user_status = 'N'.
          ENDIF.
          it_data-user_id = lt_temp-bname.
          it_data-regist_date = lt_temp-erdat.
          it_data-company_id = p_bukrs.
          it_data-company_name = l_company_name.

          APPEND it_data.
          CLEAR: it_data.
        ENDLOOP.
      ELSE.

** Get new users
        SELECT bname erdat INTO CORRESPONDING FIELDS OF TABLE lt_temp
           FROM usr02
           WHERE erdat = p_datum.

        LOOP AT lt_temp.
          CLEAR: lt_status.
          READ TABLE lt_status WITH KEY bname = lt_temp-bname
                           BINARY SEARCH.
*          IF lt_status-stat2 = '3'.
*            it_data-user_status = 'Y'.
*          ELSE.
*            CONTINUE.
*          ENDIF.
          if sy-subrc <> 0.
            CONTINUE.
          endif.
          it_data-user_status = 'Y'.

          SELECT SINGLE name_text persnumber
              INTO (it_data-user_name, l_persnumber)
          FROM v_username
           WHERE bname = lt_temp-bname.

          IF sy-subrc = 0.
            SELECT SINGLE smtp_addr INTO it_data-email_address
              FROM adr6
              WHERE persnumber = l_persnumber.
          ENDIF.
          it_data-user_id = lt_temp-bname.
          it_data-regist_date = lt_temp-erdat.
          it_data-company_id = p_bukrs.
          it_data-company_name = l_company_name.

          APPEND it_data.
          CLEAR: it_data.
        ENDLOOP.

** Get terminated users
        REFRESH: lt_temp.
        SELECT bname erdat INTO CORRESPONDING FIELDS OF TABLE lt_temp
            FROM usr02
            WHERE gltgb = p_datum.

        LOOP AT lt_temp.
          CLEAR: lt_status.
          READ TABLE lt_status WITH KEY bname = lt_temp-bname
                           BINARY SEARCH.
*          IF lt_status-stat2 <> '3'.
*            it_data-user_status = 'N'.
*          ELSE.
*            CONTINUE.
*          ENDIF.
          if sy-subrc <> 0.
            CONTINUE.
          endif.
          it_data-user_status = 'N'.

          SELECT SINGLE name_text persnumber
              INTO (it_data-user_name, l_persnumber)
          FROM v_username
           WHERE bname = lt_temp-bname.

          IF sy-subrc = 0.
            SELECT SINGLE smtp_addr INTO it_data-email_address
              FROM adr6
              WHERE persnumber = l_persnumber.
          ENDIF.
          it_data-user_id = lt_temp-bname.
          it_data-regist_date = lt_temp-erdat.
          it_data-company_id = p_bukrs.
          it_data-company_name = l_company_name.

          APPEND it_data.
          CLEAR: it_data.
        ENDLOOP.
      ENDIF.
  ENDCASE.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_send_data.
  DATA: l_result(1),
        l_msg TYPE zmsg,
        l_totrec TYPE i,
        l_srec TYPE i,
        l_frec TYPE i,
        l_msgtxt(60).

  IF p_bukrs = 'H201'.
    w_dest = 'WMHR01'.
*  ELSEIF p_bukrs = 'K201'.
*    SELECT SINGLE dest INTO (w_dest)
*      FROM zdest
*     WHERE sy_sysid = sy-sysid
*       AND sy_mandt = sy-mandt.
  ENDIF.

  DESCRIBE TABLE it_data LINES l_totrec.

  CALL FUNCTION 'Z_FMM_GETIS_PMUSER'
    DESTINATION w_dest
    IMPORTING
      flag                  = l_result
      msg                   = l_msg
    TABLES
      it_data               = it_data
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF l_result = 'S' OR l_result = 's'.

    LOOP AT it_data.
      IF it_data-rflag = 'F'.
        l_frec = l_frec + 1.
        it_error = it_data.
        APPEND it_error.
      ELSE.
        l_srec = l_srec + 1.
      ENDIF.
      IF p_rp = 'X'.
        it_data-if_date_change = sy-datum.
        it_data-if_time_change = sy-uzeit.
      ELSE.
        it_data-if_date = sy-datum.
        it_data-if_time = sy-uzeit.
      ENDIF.
      it_data-flag = 'S'.
      MODIFY it_data.
    ENDLOOP.
    WRITE: / 'Total records are : ', l_totrec.
    WRITE: / 'Successfully sent records are : ', l_srec.
    WRITE: / 'Unsuccessfully sent records are : ', l_frec.
    IF l_totrec = l_srec.
      it_data-flag = 'S'.
      MODIFY TABLE it_data TRANSPORTING flag.
    ELSE.
      it_data-flag = 'E'.
      MODIFY TABLE it_data TRANSPORTING flag.
    ENDIF.
  ELSE.
    WRITE: / 'EAI Failed, Total records are: ', l_totrec.
    LOOP AT it_data.
      IF p_rp = 'X'.
        it_data-if_date_change = sy-datum.
        it_data-if_time_change = sy-uzeit.
      ELSE.
        it_data-if_date = sy-datum.
        it_data-if_time = sy-uzeit.
      ENDIF.
      it_data-flag = 'E'.
      it_data-message = l_msg.
      MODIFY it_data.
    ENDLOOP.
    it_error[] = it_data[].
  ENDIF.

  MODIFY ztmm_getis_pmuse FROM TABLE it_data.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
*  IF NOT it_error[] IS INITIAL.
*    PERFORM send_email.
*  ENDIF.

ENDFORM.                    "save_send_data
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen_all.

  LOOP AT SCREEN.
    IF p_batch = 'X' AND screen-group2 EQ 'ABC'.
      screen-invisible = 1.
      screen-active    = 0.
      screen-input     = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_ALL
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 800.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'ST800'.
  SET TITLEBAR 'T800'.

ENDMODULE.                 " STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_DATA'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

*  IT_SORT-SPOS           = 1.
*  IT_SORT-FIELDNAME      = 'MATNR'.
*  IT_SORT-UP             = 'X'.
*  IT_SORT-SUBTOT         = 'X'.
*  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
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

                                  'S' 'USER_ID'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'User ID',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'USER_NAME'       ' ',
                                  ' ' 'COLTEXT'     'User Name',
                                  'E' 'OUTPUTLEN'   '25'.


ENDFORM.                    "build_field_catalog
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
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
      i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[]
      it_sort              = it_sort[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM send_email.
*  DATA: lt_body LIKE TABLE OF solisti1 WITH HEADER LINE.
*
*  DATA: l_subject TYPE p15_text150,
*        l_p_rec_type  LIKE  somlreci1-rec_type.
*
*  MOVE 'Following items with EAI errors:' TO lt_body.
*  APPEND lt_body.
*  CLEAR: lt_body.
*  MOVE '================================' TO lt_body.
*  APPEND lt_body.
*  CLEAR: lt_body.
*
*  MOVE: 'Material No' TO lt_body+0(20),
*        'Steel Material' TO lt_body+20(30).
*
*  APPEND lt_body.
*  CLEAR: lt_body.
*
*  MOVE: '--------------------' TO  lt_body+0(20),
*        '------------------------------' TO  lt_body+20(30).
*  APPEND lt_body.
*  CLEAR: lt_body.
*
*  LOOP AT it_data.
*    MOVE: it_data-user_id TO lt_body+0(20),
*          it_data-user_name TO lt_body+20(30).
*    APPEND lt_body.
*  ENDLOOP.
*
*  CALL FUNCTION 'ZCAF_SEND_EMAIL'
*    EXPORTING
*      p_subject  = 'PM User Data to GETIS'
*      p_rec_type = 'C'
*      p_receiver = p_rver
*    TABLES
*      pt_body    = lt_body.
*
*ENDFORM.                    " SEND_EMAIL
